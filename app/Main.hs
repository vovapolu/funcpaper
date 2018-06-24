module Main where

import FunctionalImage
import System.Console.GetOpt
import System.Environment
import Data.List

data Flag = Width { width :: Int } |
            Height { height :: Int} |
            Path { path :: String }
          deriving Show

options :: [OptDescr Flag]
options =
  [
    Option ['w'] ["width"] (ReqArg (Width . read) "WIDTH") "width of wallpaper"
  , Option ['h'] ["height"] (ReqArg (Height . read) "HEIGTH") "height of wallpaper"
  , Option ['p'] ["path"] (ReqArg Path "PATH") "destination path of wallpaper"
  ]

funcpaperOpts :: [String] -> IO [Flag]
funcpaperOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return o
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: funcpaper [OPTION...]"

runMaybe :: Maybe (IO ()) -> IO ()
runMaybe (Just io) = io
runMaybe Nothing = pure ()

main :: IO ()
main = do
  args <- getArgs
  opts <- funcpaperOpts args
  runMaybe $ do
    w <- find (\o -> case o of
      Width _ -> True
      _ -> False) opts
    h <- find (\o -> case o of
      Height _ -> True
      _ -> False) opts
    p <- find (\o -> case o of
      Path _ -> True
      _ -> False) opts
    Just $ generateFunctionalImage (path p) (width w) (height h)
