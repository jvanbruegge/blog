{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as A
import Data.Functor (void)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Development.Shake (ShakeOptions(..), shakeOptions, Verbosity (..), Action, forP, getDirectoryFiles, readFile', writeFile', copyFileChanged)
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath (dropDirectory1, (</>), dropExtension)
import Development.Shake.Forward (forwardOptions, shakeArgsForward, cacheAction)
import GHC.Generics (Generic)
import Slick (compileTemplate', substitute)
import Slick.Pandoc (loadUsingMeta, defaultHtml5Options, defaultMarkdownOptions)
import Slick.Utils (convert)
import Text.Pandoc.Options (writerTableOfContents, writerTemplate)
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Templates (compileTemplate)
import Text.Pandoc.Writers (writeHtml5String)

outputFolder :: FilePath
outputFolder = "build/"

markdownToHTML :: T.Text -> Action A.Value
markdownToHTML txt = do
  Right x <- liftIO $ compileTemplate "" "<h2>Table of Contents</h2>\n$toc$\n$body$"
  loadUsingMeta (readMarkdown defaultMarkdownOptions)
    (writeHtml5String (defaultHtml5Options { writerTableOfContents = True, writerTemplate = Just x }))
    (writeHtml5String defaultHtml5Options)
    txt

data Post = MkPost
  { title :: String
  , author :: String
  , description :: String
  , published_on :: Maybe String
  , content :: String
  , url :: String
  , date :: String
  , readableDate :: String
  , tags :: [String]
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, Binary)

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

  let allTags = Set.toList $ foldr (Set.union . Set.fromList . tags) Set.empty allPosts
  void . forP allTags $ \tag -> buildPostList (Just (tag, "tags" </> tag)) (filter (elem tag . tags) allPosts)

  copyStaticFiles

buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  content <- readFile' srcPath
  A.Object postData <- markdownToHTML . T.pack $ content
  let postUrl = T.pack . dropDirectory1 $ dropExtension srcPath
      dateOpt = KM.lookup "date" postData >>= formatDate
      postData' =
        KM.insert "url" (A.String postUrl) $
        KM.insert "prefix" (A.String "../..") $ -- posts are placed in <year>/<slug>/index.html
        KM.insert "readableDate" (A.String $ fromMaybe "unknown date" dateOpt) postData
  rendered <- getRendered <$> renderTemplates (A.Object postData') ["post.html", "shell.html"]
  writeFile' (outputFolder </> T.unpack postUrl </> "index.html") . T.unpack $ rendered
  convert $ A.Object postData'

buildPostList :: Maybe (String, FilePath) -> [Post] -> Action [Post]
buildPostList tag posts = do
  let posts' = sortOn (Down . date) posts
  let postData = A.Object $ KM.fromList $
        [ (fromText "posts", A.toJSON posts')
        , (fromText "prefix", A.String (maybe "." (const "../..") tag))
        ] <> maybe [] (\(t, _) -> [(fromText "tag", A.String (T.pack t))]) tag
  rendered <- getRendered <$> renderTemplates postData ["postList.html", "shell.html"]
  writeFile' (outputFolder <> maybe "" snd tag </> "index.html") . T.unpack $ rendered
  pure posts'

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

main :: IO ()
main = do
  let shOpts =
        forwardOptions $ shakeOptions
          { shakeVerbosity = Verbose
          , shakeLintInside = ["./templates", "./articles", "./static"]
          }
  shakeArgsForward shOpts buildRules
