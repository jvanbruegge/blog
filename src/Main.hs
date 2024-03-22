{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Development.Shake (ShakeOptions(..), shakeOptions, Verbosity (..), Action, forP, getDirectoryFiles, readFile', writeFile', copyFileChanged)
import Development.Shake.Forward (forwardOptions, shakeArgsForward, cacheAction)
import GHC.Generics (Generic)
import Development.Shake.Classes (Binary)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Slick (markdownToHTML, compileTemplate', substitute)
import Slick.Utils (convert)
import qualified Data.Aeson.KeyMap as KM
import Development.Shake.FilePath (dropDirectory1, (</>), dropExtension)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Data.Aeson.Key (fromText)
import Data.List (sortOn)
import Data.Ord (Down(..))

outputFolder :: FilePath
outputFolder = "build/"

data Post = MkPost
  { title :: String
  , author :: String
  , description :: String
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
  buildPostList allPosts
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

buildPostList :: [Post] -> Action ()
buildPostList posts = do
  let posts' = sortOn ( date) posts
  let postData = A.Object $ KM.fromList
        [ (fromText "posts", A.toJSON posts')
        , (fromText "prefix", A.String ".")
        ]
  rendered <- getRendered <$> renderTemplates postData ["postList.html", "shell.html"]
  writeFile' (outputFolder </> "index.html") . T.unpack $ rendered

copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./static" ["*"]
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
