#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package lens --package lens-aeson --package wreq --package envy --package string-conversions  --package errors --package bytestring --package http-client-tls --package text

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
module Main where

import           Control.Lens            hiding ((.=))
import           Data.Aeson              as JSON
import           Data.Foldable
import           Data.Aeson.Lens
import           Data.List               as L (isPrefixOf, partition, zip, null, sortBy)
import           Data.Function           (on)
import qualified Data.Text               as T
import           Data.String.Conversions (cs)
import           Data.ByteString         as BS hiding (isPrefixOf, foldl1)
import           Network.Wreq            as Http
import           Network.HTTP.Client.TLS as TLS
import           Network.Connection      as TLS
import           System.Envy             as Env (Var(..), FromEnv (fromEnv), env, decodeEnv,
                                                 envMaybe, (.!=))
import           Turtle                  as Shell
import           Control.Error
import           Control.Error.Script
import           System.IO               as System


data Endpoint = Endpoint { ip :: Text, name :: Text }
  deriving (Show,Eq)


instance FromJSON Endpoint where
  parseJSON (JSON.Object v) = Endpoint <$> v .: "ip" <*> (v .: "targetRef" >>= (.: "name"))
  parseJSON _ = mzero


instance ToJSON Endpoint where
  toJSON (Endpoint ip name) = JSON.object [ "ip" .= ip, "targetRef" .= ("name" .= name) ]


newtype KubernetesApi = KubernetesApi { getUrl :: Text }
  deriving (Show, Read, Eq, Ord)


data Environment = Environment
  { kubernetesService    :: KubernetesApi
  , riakNamespace        :: Text
  , riakDiscoveryService :: Text
  } deriving (Show,Eq)


instance FromEnv Environment where
  fromEnv =
    let localKubernetesEnv = Env.env "KUBERNETES_MASTER" -- E.g.: when running via Vagrant
        liveKubernetesEnv  = (<>) "https://" <$> Env.env "KUBERNETES_PORT_443_TCP_ADDR"
    in Environment
        <$> (fmap KubernetesApi (liveKubernetesEnv <|> localKubernetesEnv))  -- "https://kubernetes"
        <*> envMaybe "RIAK_NAMESPACE" Env..!= "default"
        <*> envMaybe "RIAK_DISCOVERY_SERVICE" Env..!= "riak-discovery"


data ClusterJoinEvent
  = Joined Endpoint
  | AlreadyJoined
  | CannotJoinItself
  | NotJoined Text
  deriving (Show)


main :: IO ()
main = do
  hSetBuffering System.stdout NoBuffering
  hSetBuffering System.stderr NoBuffering

  endpoints <- cs . endpointQueryUrl <$> Env.decodeEnv `orError` (die.cs)
  echo $ "Query: " <> cs endpoints
  opts <- if "https://" `isPrefixOf` endpoints
          then secureConnectionSettings
          else return defaults

  -- Query service endpoints via http k8s Api
  response <- Http.getWith opts endpoints

  -- Extract first endpoint of the service
  let mEndpoints = response ^? responseBody
                    . key "subsets"
                    ... key "addresses"
                    . _JSON
  case L.sortBy (compare `on` ip) <$> mEndpoints of
    Nothing   -> die "Missing discovery service."
    Just []   -> die "No riak endpoint found."
    Just [e]  -> echo "Single Node cluster, cannot form any cluster."
    Just endpoints -> sh $ do
      echo (repr endpoints)

      tryJoinEndpoint endpoints >>= \case
        NotJoined msg ->
          die msg

        Joined endpoint -> do
          echo ("Joined: " <> repr endpoint)
          sleep 5.0 >> (plan .&&. commit)

        evt -> do
          echo (repr evt)
          exit ExitSuccess


riakAdmin :: [Text] -> Shell (ExitCode, Text)
riakAdmin args = Shell.procStrict "riak-admin" args Shell.empty


plan :: Shell ExitCode
plan = excuse (riakAdmin ["cluster", "plan"])


commit :: Shell ExitCode
commit = excuse (riakAdmin ["cluster", "commit"])


joinCluster :: Endpoint -> Shell (ExitCode, Text)
joinCluster endpoint = riakAdmin ["cluster", "join", "riak@" <> ip endpoint]


tryJoinEndpoint :: [Endpoint] -> Shell ClusterJoinEvent
tryJoinEndpoint = foldlM run (NotJoined "No Riak endpoint.")
  where
  run event endpoint
    | isJoined event = return event
    | otherwise = joinEndpoint endpoint


joinEndpoint :: Endpoint -> Shell ClusterJoinEvent
joinEndpoint endpoint = do
  echo ("Try to join: " <> repr endpoint)
  joinCluster endpoint >>= \case
    (ExitSuccess, _) -> return (Joined endpoint)
    -- Handle join failures
    (ExitFailure i, t)
      | T.strip t == "Failed: This node is already a member of a cluster" -> return AlreadyJoined
      | T.strip t == "Failed: This node cannot join itself in a cluster"  -> return CannotJoinItself
      | otherwise -> return (NotJoined t)


excuse :: Shell (ExitCode, Text) -> Shell ExitCode
excuse s = s >>= \case
  (ExitFailure i, e)  -> Shell.err (T.strip e) >> return (ExitFailure i)
  (ExitSuccess, out)  -> Shell.echo (T.strip out) >> return ExitSuccess


secureConnectionSettings :: IO Options
secureConnectionSettings = do
  -- cert  <- BS.readFile "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt"
  -- HTTPS read bearer token http://stackoverflow.com/a/30739416
  token <- BS.readFile "/var/run/secrets/kubernetes.io/serviceaccount/token"
  let tlsSettings = TLSSettingsSimple True False False -- FIXME: mitm
  return $ defaults
            & auth ?~ oauth2Bearer token
            & manager .~ Left (mkManagerSettings tlsSettings Nothing)


endpointQueryUrl :: Environment -> Text
endpointQueryUrl Environment{..} =
  Shell.format
  (s%"/api/v1/namespaces/"%s%"/endpoints/"%s)
  (getUrl kubernetesService)
  riakNamespace
  riakDiscoveryService


orError :: Monad m => m (Either e a) -> (e -> m a) -> m a
orError action e = action >>= either e return


isJoined (Joined _) = True
isJoined AlreadyJoined = True
isJoined _ = False
