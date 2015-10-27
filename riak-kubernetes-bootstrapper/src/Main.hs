#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package lens --package lens-aeson --package wreq --package envy --package string-conversions  --package errors --package bytestring --package http-client-tls --package text

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
module Main where

import           Control.Lens            hiding ((.=))
import           Data.Aeson              as JSON
import           Data.Aeson.Lens
import           Data.Map                as Map
import           Data.List               as L (isPrefixOf)
import qualified Data.Text               as T
import           Data.String.Conversions (cs)
import           Data.ByteString         as BS hiding (isPrefixOf)
import           Network.Wreq            as Http
import           Network.HTTP.Client.TLS as TLS
import           Network.Connection      as TLS
import           System.Envy             as Env (Var(..), FromEnv (fromEnv), env, decodeEnv,
                                                 envMaybe, (.!=))
import           Turtle                  as Shell
import           Control.Error
import           Control.Error.Script

import Debug.Trace

data Endpoint = Endpoint { ip :: Text }
  deriving (Show,Eq)

instance FromJSON Endpoint where
  parseJSON (JSON.Object v) = Endpoint <$> v .: "ip"
  parseJSON _ = mzero

instance ToJSON Endpoint where
  toJSON (Endpoint ip) = JSON.object [ "ip" .= ip ]

newtype KubernetesApi = KubernetesApi { getUrl :: Text }
  deriving (Show, Read, Eq, Ord)

instance Env.Var KubernetesApi where
  toVar = cs . getUrl
  fromVar = Just . KubernetesApi . (<>) "https://" . cs

data Environment = Environment
  { kubernetesService    :: KubernetesApi
  , riakNamespace        :: Text
  , riakDiscoveryService :: Text
  } deriving (Show,Eq)

instance FromEnv Environment where
  fromEnv =
    Environment
    <$> Env.envMaybe "KUBERNETES_PORT_443_TCP_ADDR" Env..!= KubernetesApi "https://kubernetes"
    <*> envMaybe "RIAK_NAMESPACE" Env..!= "default"
    <*> envMaybe "RIAK_DISCOVERY_SERVICE" Env..!= "riak-discovery"

main :: IO ()
main = do
  endpoints <- cs . endpointQueryUrl <$> Env.decodeEnv `orError` (die.cs)
  echo $ "Query: " <> cs endpoints
  opts <- if "https://" `isPrefixOf` endpoints
          then secureConnectionSettings
          else return defaults

  -- Query service endpoints via http k8s Api
  response <- Http.getWith opts endpoints

  -- Extract first endpoint of the service
  let mEndpoint = response ^? responseBody
                    . key "subsets"
                    ... key "addresses"
                    . nth 0 . _JSON
  riakAdmin ["status"]
  print (mEndpoint :: Maybe Endpoint)
  -- print (mEndpoints :: Maybe [Endpoint])

riakAdmin args = Shell.proc "riak-admin" args Shell.empty

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
