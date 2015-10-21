module Qiita
    ( run
    ) where

import System.Environment
import Text.HTML.Scalpel
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

type Snippet = ByteString

allSnippets :: URL -> IO (Maybe [Snippet])
allSnippets url = scrapeURL url snippets

snippets :: Scraper ByteString [Snippet]
snippets = chroots ("section" @: [hasClass "markdownContent"]) snippet
    where snippet :: Scraper ByteString Snippet
          snippet = text $ "div" @: [hasClass "code-frame"]

run :: IO ()
run = do
  (url:_) <- getArgs
  maybe printError printSnippets =<< allSnippets url
    where
      printError = putStrLn "ERROR: Could not scrape the URL!"
      printSnippets = mapM_ C.putStrLn
