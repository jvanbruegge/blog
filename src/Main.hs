{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Development.Shake (ShakeOptions(..), shakeOptions, Verbosity (..), Action, forP, getDirectoryFiles, readFile', writeFile', copyFileChanged, liftIO)
import Development.Shake.Forward (forwardOptions, shakeArgsForward, cacheAction)
import GHC.Generics (Generic, Generically(..))
import Development.Shake.Classes (Binary)
import Data.Aeson (FromJSON, ToJSON, Value, toJSON)
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Slick (compileTemplate', substitute)
import Slick.Utils (convert)
import Slick.Pandoc (PandocWriter, PandocReader, defaultHtml5Options, defaultMarkdownOptions)
import Text.Pandoc (Meta(..), MetaValue(..), PandocIO, runIO, Pandoc(..), Block(..), writeHtml5String, readMarkdown, def, writePlain)
import Data.Aeson.KeyMap qualified as KM
import Development.Shake.FilePath (dropDirectory1, (</>), dropExtension)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Data.Aeson.Key (fromText)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Set qualified as Set
import Data.Map.Strict qualified as M

outputFolder :: FilePath
outputFolder = "build/"

data Post = MkPost
  { title :: String
  , author :: String
  , description :: String
  , content :: String
  , url :: String
  , prefix :: String
  , date :: String
  , readableDate :: String
  , tags :: [String]
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving (ToJSON, FromJSON) via Generically Post

instance Binary Post

data Publication = MkPublication
  { title :: String
  , date :: String
  , authors :: [String]
  , conference :: String
  , doi :: String
  , pdf :: String
  , extended :: Maybe String
  , prefix :: String
  , url :: String
  , readableDate :: String
  , openAccess :: Bool
  , content :: String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving (ToJSON, FromJSON) via Generically Publication

instance Binary Publication

mapP :: (a -> Action b) -> [a] -> Action [b]
mapP = flip forP

formatDate :: A.Value -> Maybe T.Text
formatDate (A.String t) |
  [y, m, d] <- T.splitOn "-" t
  , Just m' <- month m = Just $ m' <> " " <> d <> ", " <> y
  where
    month = \case
      "01" -> Just "January"
      "02" -> Just "February"
      "03" -> Just "March"
      "04" -> Just "April"
      "05" -> Just "May"
      "06" -> Just "June"
      "07" -> Just "July"
      "08" -> Just "August"
      "09" -> Just "September"
      "10" -> Just "October"
      "11" -> Just "November"
      "12" -> Just "December"
      _ -> Nothing
formatDate _ = Nothing

renderTemplates :: A.Value -> [FilePath] -> Action A.Value
renderTemplates = foldM go
  where
    go o@(A.Object m) fp = do
      template <- compileTemplate' ("templates" </> fp)
      pure . A.Object $ KM.insert "content" (A.String $ substitute template o) m
    go x _ = pure x

getRendered :: A.Value -> T.Text
getRendered (A.Object m) | Just (A.String x) <- KM.lookup "content" m = x
getRendered _ = error "Could not find content key in object"

buildRules :: Action ()
buildRules = do
  allPosts <- mapP buildPost =<< getDirectoryFiles "." ["articles//*.md"]
  sortedPosts <- buildPostList Nothing allPosts
  buildAtomFeed (take 15 sortedPosts)

  publications <- sortOn (Down . (.date)) <$> (mapP (readMarkdownFile "/../publications") =<< getDirectoryFiles "." ["publications//*.md"])
  buildPublications Nothing publications

  let allTags = Set.toList $ foldr (Set.union . Set.fromList . (.tags)) Set.empty allPosts
  void . forP allTags $ \tag -> buildPostList (Just (tag, "tags" </> tag)) (filter (elem tag . (.tags)) allPosts)

  let allConferences = Set.toList $ Set.fromList (map (.conference) publications)
  void . forP allConferences $ \conf -> buildPublications (Just (conf, "conferences" </> conf)) (filter ((==) conf . (.conference)) publications)

  copyStaticFiles

readMarkdownFile :: (FromJSON a, Typeable a, Binary a, Show a) => T.Text -> FilePath -> Action a
readMarkdownFile prefix srcPath = cacheAction ("read" :: T.Text, srcPath) $ do
  content <- readFile' srcPath
  A.Object postData <- markdownToHTML markdownKeys . T.pack $ content
  let postUrl = T.pack . dropDirectory1 $ dropExtension srcPath
      dateOpt = KM.lookup "date" postData >>= formatDate
      postData' =
        KM.insert "url" (A.String postUrl) $
        KM.insert "prefix" (A.String ("../" <> prefix)) $ -- posts are placed in <year>/<slug>/index.html
        KM.insert "readableDate" (A.String $ fromMaybe "unknown date" dateOpt) postData
  convert $ A.Object postData'
  where
    markdownKeys = Set.fromList ["content", "title", "description", "authors"]

buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  post <- readMarkdownFile ".." srcPath
  rendered <- getRendered <$> renderTemplates (A.toJSON post) ["post.html", "shell.html"]
  writeFile' (outputFolder </> post.url </> "index.html") . T.unpack $ rendered
  pure post

buildPostList :: Maybe (String, FilePath) -> [Post] -> Action [Post]
buildPostList tag posts = do
  let posts' = sortOn (Down . (.date)) posts
  let postData = A.Object $ KM.fromList $
        [ (fromText "posts", A.toJSON posts')
        , (fromText "prefix", A.String (maybe "." (const "../..") tag))
        ] <> maybe [] (\(t, _) -> [(fromText "tag", A.String (T.pack t))]) tag
  rendered <- getRendered <$> renderTemplates postData ["postList.html", "shell.html"]
  writeFile' (outputFolder <> maybe "" snd tag </> "index.html") . T.unpack $ rendered
  pure posts'

buildPublications :: Maybe (String, FilePath) -> [Publication] -> Action ()
buildPublications conference publications = do
  let postData = A.Object $ KM.fromList $
        [ (fromText "publications", A.toJSON publications)
        , (fromText "prefix", A.String (maybe ".." (const "../..") conference))
        ] <> maybe [] (\(t, _) -> [(fromText "conference", A.String (T.pack t))]) conference
  rendered <- getRendered <$> renderTemplates postData ["publications.html", "shell.html"]
  writeFile' (outputFolder </> maybe "publications" snd conference </> "index.html") . T.unpack $ rendered

buildAtomFeed :: [Post] -> Action ()
buildAtomFeed posts = do
  rendered <- getRendered <$> renderTemplates (A.Object $ KM.fromList [(fromText "posts", A.toJSON posts)]) ["atom.xml"]
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ rendered

copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./static" ["*"]
    copyFileChanged ("articles" </> "LICENSE") (outputFolder </> "LICENSE")
    void $ forP filepaths $ \filepath ->
      copyFileChanged ("static" </> filepath) (outputFolder </> filepath)
    citations <- getDirectoryFiles "./publications" ["//*.bib"]
    void $ forP citations $ \filepath ->
      copyFileChanged ("publications" </> filepath) (outputFolder </> "publications" </> filepath)

main :: IO ()
main = do
  let shOpts =
        forwardOptions $ shakeOptions
          { shakeVerbosity = Verbose
          , shakeLintInside = ["./templates", "./articles", "./static", "./publications"]
          }
  shakeArgsForward shOpts buildRules

markdownToHTML :: Set.Set T.Text -> T.Text -> Action A.Value
markdownToHTML keys = loadUsing
    (readMarkdown defaultMarkdownOptions)
    (\key -> if Set.member key keys then
      writeHtml5String defaultHtml5Options
      else writePlain def
    )

loadUsing :: PandocReader textType -- ^ The reader used to load the document
          -> (T.Text -> PandocWriter) -- ^ The writer used to render the document itself
          -> textType
          -> Action Value
loadUsing reader writer text = do
  (pdoc, meta) <- makePandocReaderWithMetaWriter reader writer text
  outText      <- unPandocM $ writer "content" pdoc
  withContent <- case meta of
      A.Object m -> return . A.Object $ KM.insert "content" (A.String outText) m
          -- meta & _Object . at "content" ?~ String outText
      _ -> fail "Failed to parse metadata"
  return withContent

makePandocReaderWithMetaWriter
    :: PandocReader textType
    -> (T.Text -> PandocWriter)
    -> textType
    -> Action (Pandoc, Value)
makePandocReaderWithMetaWriter readerFunc writerFunc text = do
  pdoc@(Pandoc meta _) <- unPandocM $ readerFunc text
  meta' <- flattenMeta writerFunc meta
  return (pdoc, meta')

flattenMeta :: (T.Text -> PandocWriter) -> Meta -> Action Value
flattenMeta writer (Meta meta) = toJSON <$> M.traverseWithKey go meta
 where
  go :: T.Text -> MetaValue -> Action Value
  go key (MetaMap     m) = toJSON <$> traverse (go key) m
  go key (MetaList    m) = A.toJSONList <$> traverse (go key) m
  go _ (MetaBool    m) = pure $ toJSON m
  go _ (MetaString  m) = pure $ toJSON m
  go key (MetaInlines m) = toJSON <$> (unPandocM . writer key . Pandoc mempty . (:[]) . Plain $ m)
  go key (MetaBlocks  m) = toJSON <$> (unPandocM . writer key . Pandoc mempty $ m)

unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) return result
