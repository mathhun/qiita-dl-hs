module Qiita
    ( run
    ) where

import System.Environment
import Text.HTML.Scalpel
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

data Site = Site { siteTitle :: ByteString
                 , siteSnippets :: [Snippet]
                 }

data Snippet = Snippet { snptFile :: ByteString
                       , snptCode :: ByteString
                       } deriving Show

allSnippets :: URL -> IO (Maybe [Snippet])
allSnippets url = scrapeURL url snippets

snippets :: Scraper ByteString [Snippet]
snippets = chroots ("div" @: [hasClass "code-frame"]) snippet

snippet :: Scraper ByteString Snippet
snippet = Snippet <$> file <*> code
    where file :: Scraper ByteString ByteString
          file = text $ "div" @: [hasClass "code-lang"]
          code :: Scraper ByteString ByteString
          code = text "pre"

printSnippetList :: [Snippet] -> IO ()
printSnippetList snpts = mapM_ print1 $ zip [1..] snpts
    where print1 (n, snpt) = C.putStrLn $ (C.pack $ show n ++ " ") `C.append` (snptFile snpt)

runDownloadSnippets :: Snippet -> IO ()
runDownloadSnippets snpt = do C.putStrLn $ snptFile snpt
                              C.putStrLn $ snptCode snpt

run :: IO ()
run = do
  (url:_) <- getArgs
  snpts <- allSnippets url
  maybe printError processSnippets snpts
    where
      printError = putStrLn "ERROR: Could not scrape the URL"
      processSnippets snpts = do
          case length snpts of
            0 -> putStrLn "No snippet found"
            1 -> runDownloadSnippets $ head snpts
            _ -> printSnippetList snpts

-- option -o (filename)
-- option -n to specify snpt
-- option -d (directory)
-- option -a to download all
