module Main where

import System.Environment(getArgs)

import Data.OSMPBF.Decoder
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  putStrLn $ "Reading from " ++ fileName
  runResourceT $
               CC.sourceFile fileName
                   $$ conduitPbfToPrimitives
                   =$= CC.mapM_ (liftIO . putStrLn)
