{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Pandoc.Shared (headerShift)
import Lib (getRecursiveContents)
import Text.Pandoc
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (HtmlUrl, hamlet)
import Filesystem.Path.CurrentOS (fromText, toText, encode, decode)
import qualified Filesystem.Path.CurrentOS as FSPath
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (hPutStrLn, IOMode(..), withFile)
import Control.Monad (forM_)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Control.Exception.Extensible as E (try, IOException)

import Control.Applicative ( (<$>) )
import Data.Maybe (catMaybes)

import Text.Pandoc.Templates (getDefaultTemplate)
import qualified Text.HTML.TagSoup as TagSoup

import Data.List (isSuffixOf)
import qualified Data.Tree as Tree

markdownToHtml :: String -> IO String
markdownToHtml s = do
    set <- settings
    return $ htmlTransform $ writeHtmlString set $ transform $ readMarkdown def s

-- reduce headers by 1 so that title = h1, # = h2, ## = h3, ...
transform = headerShift 1

-- adds class="table" to all tables produced from the markdown files
-- so that the correct bootstrap table css is applied
htmlTransform :: String -> String
htmlTransform html = TagSoup.renderTags $ map f tags
    where tags = TagSoup.parseTags html
          f (TagSoup.TagOpen "table" xs) = TagSoup.TagOpen "table" $ ("class","table"):xs
          f x = x

stringToFilePath :: String -> FSPath.FilePath
--stringToFilePath = FSPath.decode . BSC8.pack
stringToFilePath = FSPath.decode . Text.pack

filePathToString :: FSPath.FilePath -> String
--filePathToString = BSC8.unpack . FSPath.encode
filePathToString = Text.unpack . FSPath.encode

data MyRoute = Home

render :: MyRoute -> [(Text.Text, Text.Text)] -> Text.Text
render Home _ = "/home"

navFiles :: [(String, String)] -> String
navFiles xs = renderHtml $ [hamlet|
  $forall (url, title) <- xs
    <li role="presentation">
      <a href="#{url}">#{title}
|] render

fileWithTitles :: String -> (String, String)
fileWithTitles path = (path, base)
    where base = filePathToString $ FSPath.basename $ stringToFilePath path


settings :: IO WriterOptions
settings = do
    template <- readFile "tutorial.html"
    files <- filesList
    let htmls :: [String] = catMaybes $ map mdToHtml files
    let filesVars = map ( (,) "nav-files" ) htmls
    let nav = navFiles $ map fileWithTitles htmls
    return $ def { writerVariables = ("siteRoot", siteRoot) : ("nav", nav) : css ++ filesVars
          , writerStandalone = True
          , writerTemplate = template
          , writerHighlight = True
          , writerTableOfContents = True }
    where mdToHtml :: String -> Maybe String
          mdToHtml file = do
            { f <- FSPath.stripPrefix (stringToFilePath "gen/") $ stringToFilePath file
            ; return $ (++) (siteRoot++"site/") $ filePathToString $ flip FSPath.addExtension "html" $ FSPath.dropExtension f
            }

filesList = unsafeInterleaveIO $ getRecursiveContents "gen"

css = map ( (,) "css" ) $ map ((siteRoot ++ "_src/css/")++) ["bootstrap.min.css", "custom.css"]

siteRoot = "http://localhost:8000/"

main = do
  files <- filesList
  forM_ files $ \file -> do
    if ".md" `isSuffixOf` file
      then do
        s <- readFile file
        putStrLn file
        html <- markdownToHtml s
        let fileName = "../site" ++ (snd $ break (=='/') $ reverse $ snd $ break (=='.') $ reverse $ file) ++ "html"
        putStrLn fileName
        let p = stringToFilePath fileName
        let dir = filePathToString $ FSPath.directory p
        putStrLn dir
        createDirectoryIfMissing True dir
        withFile fileName WriteMode $ \h -> do
          hPutStrLn h html
      else return ()
