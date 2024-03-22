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
import Development.Shake.FilePath (dropDirectory1, (-<.>), (</>))
import Data.Functor (void)
import Data.Maybe (fromMaybe)

outputFolder :: FilePath
outputFolder = "build/"

data Post = MkPost
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , date :: String
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

buildRules :: Action ()
buildRules = do
  _allPosts <- mapP buildPost =<< getDirectoryFiles "." ["articles//*.md"]
  copyStaticFiles

buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  content <- readFile' srcPath
  A.Object postData <- markdownToHTML . T.pack $ content
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      dateOpt = KM.lookup "date" postData >>= formatDate
      postData' =
        KM.insert "url" (A.String postUrl) $
        KM.insert "prefix" (A.String "..") $
        KM.insert "readableDate" (A.String $ fromMaybe "" dateOpt) postData
  template <- compileTemplate' "templates/post.html"
  shell <- compileTemplate' "templates/shell.html"
  let postData'' = KM.insert "content" (A.String $ substitute template (A.Object postData')) postData'
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute shell (A.Object postData'')
  convert $ A.Object postData'

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
