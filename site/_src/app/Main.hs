{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Pandoc.Shared (headerShift)
import Lib (getRecursiveContents)
import Text.Pandoc
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (HtmlUrl, hamlet)
-- import Filesystem.Path.CurrentOS (fromText, toText, encode, decode)
-- import qualified Filesystem.Path.CurrentOS as FSPath
import qualified System.FilePath.Posix as FP
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (hPutStr, IOMode(..), withFile)
import Control.Monad (forM_)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Control.Exception.Extensible as E (try, IOException)

import Control.Applicative ( (<$>) )
import qualified Data.Maybe as Maybe (catMaybes, fromJust)

import Text.Pandoc.Templates (getDefaultTemplate)
import qualified Text.HTML.TagSoup as TagSoup

import qualified Data.List as List
import qualified Data.Tree as Tree
import qualified Data.Set as Set
import qualified Data.Map as Map

markdownToHtml :: String -> IO String
markdownToHtml s = do
    set <- settings
    return $ htmlTransform $ writeHtmlString set $ transform $ readMarkdown def s

-- reduce headers by 1 so that title = h1, # = h2, ## = h3, ...
transform = headerShift 1

-- adds class="table" to all tables produced from the markdown files
-- so that the correct bootstrap table css is applied
--
-- (hacky) Option to not escape &"<> in order to save our pandoc-generated
-- css which uses >
htmlTransform :: String -> String
htmlTransform html = TagSoup.renderTagsOptions opts $ map f tags
    where opts = TagSoup.renderOptions { TagSoup.optEscape = id }
          tags = TagSoup.parseTags html
          f (TagSoup.TagOpen "table" xs) = TagSoup.TagOpen "table" $ ("class","table"):xs
          f x = x

data MyRoute = Home

render :: MyRoute -> [(Text.Text, Text.Text)] -> Text.Text
render Home _ = "/home"

navFiles :: Map.Map Section (Set.Set (String, String)) -> String
navFiles xs = renderHtml $ [hamlet|
  $forall (section, items) <- Map.toAscList xs
    #{title section}
    <ul class="nav nav-pills nav-stacked">
      $forall (url, title) <- Set.toAscList items
        <li role="presentation">
          <a href="#{url}">#{title}
|] render

-- <li role="presentation" class="active"><a href="#">$title$</a></li>

extractTitle :: String -> String
extractTitle path = dropNumber $ map go $ FP.takeBaseName path
    where go '-' = ' '
          go x   = x

dropNumber :: String -> String
dropNumber = dropWhile (flip elem "0123456789 ")

-- groups files by directory, given a list of (path, title)
groupFilesWithTitles :: [(String,String)] -> Map.Map String (Set.Set (String, String))
groupFilesWithTitles xs = foldl addToMap Map.empty xs
    where addToMap map (path,title) = Map.insertWith Set.union (FP.takeDirectory path) (Set.singleton (path,title)) map

{-
-- groups files by directory, given a list of path
groupFiles :: [String] -> Map.Map String (Set.Set String)
groupFiles xs = foldl addToMap Map.empty xs
    where addToMap map path = Map.insertWith Set.union (FP.takeDirectory path) (Set.singleton path) map
-}

data Section = Section { path :: String, title :: String } deriving Eq

instance Ord Section where
  compare a b = compare (path a) (path b)

mkSection :: FilePath -> Section
mkSection path = Section { path = path, title = title }
    where title = List.intercalate " > " $ filter (not . null) $ map extractTitle $ FP.splitDirectories $ Maybe.fromJust $ List.stripPrefix (siteRoot ++ "site") path

-- makes the navbar from a list of html paths
makeNav :: [String] -> String
makeNav htmls = navFiles $ Map.mapKeys mkSection $ groupFilesWithTitles filesWithTitles
    where filesWithTitles = map (\x -> (x, extractTitle x)) htmls

settings :: IO WriterOptions
settings = do
    template <- readFile "tutorial.html"
    files <- List.sort <$> filesList
    let htmls :: [String] = Maybe.catMaybes $ map mdToHtml files
    let nav = makeNav htmls
    return $ def { writerVariables = ("siteRoot", siteRoot) : ("nav", nav) : css
          , writerStandalone = True
          , writerTemplate = template
          , writerHighlight = True
          , writerTableOfContents = True }
    where mdToHtml :: String -> Maybe String
          mdToHtml file = do
            { f <- List.stripPrefix "gen/" file
            ; return $ siteRoot ++ "site/" ++ (FP.replaceExtension f "html")
            }

filesList = unsafeInterleaveIO $ getRecursiveContents "gen"

css = map ( (,) "css" ) $ map ((siteRoot ++ "_src/css/")++) ["bootstrap.min.css", "custom.css"]

siteRoot = "http://localhost:8000/"

main = do
  files <- filesList
  forM_ files $ \file -> do
    if ".md" `List.isSuffixOf` file
      then do
        putStrLn file
        s <- readFile file
        html <- markdownToHtml s
        let fileName = (++) "../site/" $ Maybe.fromJust $ List.stripPrefix "gen/" $ FP.replaceExtension file ".html"
        putStrLn fileName
        let dir = FP.takeDirectory fileName
        createDirectoryIfMissing True dir
        withFile fileName WriteMode $ \h -> do
          hPutStr h html
      else return ()
