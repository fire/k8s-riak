#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package riak --package bytestring

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy.Char8 ()

import qualified Network.Riak.Basic as RB
import           Network.Riak.Connection (defaultClient)
import           Network.Riak.Content
import           Network.Riak.Types (Bucket(..), Key(..), Quorum(..))

client :: RB.Client
client = RB.Client "172.17.8.102" "30798" "test"

testBucket :: B.ByteString
testBucket = "testBucket"

testKey :: B.ByteString
testKey = "testKey"

testValue :: B.ByteString
testValue = "{something: 'blah'}"

putSomeData conn bucket key value = do
  putStrLn ("PUT: bucket=" ++ show bucket ++ ", key = " ++ show key ++ ", value = " ++ show value)
  RB.put conn testBucket testKey Nothing (binary testValue) Default Default
  putStrLn "=========================="

getSomeData conn bucket key = do
  putStrLn ("Get: bucket=" ++ show bucket ++ ", key = " ++ show key)
  b <- RB.get conn testBucket key Default
  maybe (print "No value") (print) b
  putStrLn "=========================="

deleteSomeData conn bucket key = do
  putStrLn ("Delete: bucket=" ++ show bucket ++ ", key = " ++ show key)
  RB.delete conn bucket key Default
  putStrLn "=========================="

listBuckets conn = do
  buckets <- RB.listBuckets conn
  print buckets

main = do
  c <- RB.connect client
  RB.ping   c
  print =<< RB.getServerInfo c
  listBuckets    c
  putSomeData    c testBucket testKey (binary testValue)
  getSomeData    c testBucket testKey
  listBuckets    c
  deleteSomeData c testBucket testKey
  getSomeData    c testBucket testKey
