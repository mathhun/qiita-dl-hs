module Qiita
    ( run
    ) where

import System.Environment
import Text.HTML.Scalpel
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Control.Applicative

data Article = Article { aTitle :: ByteString
                       , aSnippets :: [Snippet]
                       }

data Snippet = Snippet { snptFile :: Maybe ByteString
                       , snptCode :: ByteString
                       } deriving Show

allSnippets :: URL -> IO (Maybe Article)
allSnippets url = scrapeURL url snippets

snippets :: Scraper ByteString Article
snippets = Article
           <$> text "title"
           <*> chroots ("div" @: [hasClass "code-frame"]) snippet

snippet :: Scraper ByteString Snippet
snippet = Snippet <$> file' <*> code
    where file' = fmap Just file <|> return Nothing
          file = text $ "div" @: [hasClass "code-lang"]
          code = text "pre"

printSnippetList :: Article -> IO ()
printSnippetList article = do
  C.putStrLn $ (C.pack "Title: ") `C.append` aTitle article
  mapM_ print1 $ zip [1..] $ aSnippets article
    where print1 (n, snpt) = C.putStrLn $ (C.pack $ show n ++ " ") `C.append`
                             (fromMaybe (C.pack "\"\"") $ snptFile snpt)

runDownloadSnippets :: Article -> IO ()
runDownloadSnippets article = do
  let snpt = head $ aSnippets article
  C.putStrLn $ fromMaybe (C.pack "\"\"") $ snptFile snpt
  C.putStrLn $ snptCode snpt

run :: IO ()
run = do
  (url:_) <- getArgs
  article <- allSnippets url
  maybe printError processSnippets article
    where
      printError = putStrLn "ERROR: Could not scrape the URL"
      processSnippets article = do
          case length (aSnippets article) of
            0 -> putStrLn "No snippet found"
            1 -> runDownloadSnippets article
            _ -> printSnippetList article

-- option -o (filename)
-- option -n to specify snpt
-- option -d (directory)
-- option -a to download all
