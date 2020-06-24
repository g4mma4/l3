#!/usr/bin/env stack
-- stack --resolver lts-14 script

-- --package bits
-- --package bytestring
-- --package network
-- --package optparse-applicative

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- import Control.Applicative
-- import Control.Arrow
-- import Control.Monad
-- import Data.Bits
-- import Data.ByteString (ByteString)
-- import Data.Map (Map)
-- import Data.Maybe
-- import Data.Semigroup ((<>))
-- import GHC.Exts
-- import Numeric (showHex, showIntAtBase)
-- import System.Exit
-- import System.Random
-- import Text.Printf
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as C
-- import qualified Data.Map as Map

import Data.Char (digitToInt, intToDigit)
import Data.Char (ord, isHexDigit)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Options.Applicative
import System.IO
import qualified Control.Exception as E
import qualified Data.List as L


data Config = Config
  { cfgNum1   :: Int
  , cfgPath   :: FilePath
  }

optParser :: Parser Config
optParser = Config
      <$> option auto
          ( long "num"
         <> short 'n'
         <> help "How many keys to generate"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> strOption
        (long "file"
         <> short 'f'
         <> metavar "FILE"
         <> showDefault
         <> value "solver"
         <> help "Output from generate" )

main :: IO ()
main = do
    execParser opts >>= solver
  where
    opts = info (optParser <**> helper)
      (fullDesc
          <> progDesc "XXX"
          <> header "Solver")


-----------------------------------------------------------
-- Solver Instructions
-- 1.

solver :: Config -> IO ()
solver Config{..} = error "TODO: solv'meh"

-----------------------------------------------------------
-- Fetch keys and print them to stdout

-----------------------------------------------------------
-- from the "network-run" package.

_runTCPClient
    :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
_runTCPClient host port client = withSocketsDo $ do
    (addr:_) <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        getAddrInfo
            (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr)
            (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

-----------------------------------------------------------
-- Stuff

-- Strings are evil; but i'm lazzzzzy
hexToInt :: String -> Either String (Integer,String)
hexToInt txt
    | null h  = Left "input does not start with a hexadecimal digit"
    | otherwise = Right (L.foldl' go 0 h, t)
  where (h,t)  = span isHexDigit txt
        go n d = (n * 16 + fromIntegral (hexDigitToInt d))

hexDigitToInt :: Char -> Int
hexDigitToInt c
    | c >= '0' && c <= '9' = ord c - ord '0'
    | c >= 'a' && c <= 'f' = ord c - (ord 'a' - 10)
    | otherwise            = ord c - (ord 'A' - 10)

fromBinary :: String -> Integer
fromBinary str = sum $ zipWith toDec (reverse str) [0 .. length str] where
    toDec a b = toInteger $ digitToInt a * (2 ^ b)
