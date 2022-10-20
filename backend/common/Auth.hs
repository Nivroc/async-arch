{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Auth(verifyAuthHeader, verifyAgainstAuthService, actionErr, hasRole) where

import           Data.Aeson (Result, fromJSON)
import           Data.Aeson.Types (Result(..))
import           Web.Scotty.Trans hiding (status)
import           Data.ByteString hiding (elem)
import           Web.Scotty.Internal.Types
import           Control.Monad.IO.Class
import           Control.Monad.Except (MonadError(throwError))
import           Network.HTTP.Types (status400, status500, status422, status401, Status, status412)
import qualified Data.Text.Lazy     as TL
import qualified Data.Text          as T
import qualified Web.JWT            as J
import qualified Network.HTTP.Req   as R
import qualified Data.Text.Encoding as T
import qualified Data.Map

import           Common

instance (MonadError (ActionError TL.Text) (ActionT e m), ScottyError e, MonadIO m) => R.MonadHttp (ActionT e m) where
  handleHttpException :: R.HttpException -> ActionT e m a
  handleHttpException (R.VanillaHttpException he) = throwError $ ActionError status500 (TL.pack $ show he)
  handleHttpException (R.JsonHttpException str) = throwError $ ActionError status500 (TL.pack $ show str)

verifyAuthHeader :: MonadIO m => String -> ActionT TL.Text m (ByteString, J.JWT J.VerifiedJWT)
verifyAuthHeader clientSecret = do
  let err = actionErr "Error: no 'authorization' header" status400 
  tokenText <- TL.toStrict . TL.drop 7 <$> (header "authorization" >>= maybe err pure)
  let er = throwError $ ActionError status422 "Auth JWT cannot be decoded"
  jwt <- maybe er pure (J.decodeAndVerifySignature (J.toVerify . J.hmacSecret . T.pack $ clientSecret ) tokenText)
  return (T.encodeUtf8 tokenText, jwt)

actionErr :: MonadError (ActionError TL.Text) m => String -> Status ->  m a
actionErr mess status = throwError $ ActionError status (TL.pack mess)

hasRole :: MonadError (ActionError TL.Text) m => J.JWT r -> Role -> m Bool
hasRole token role = do roles <- maybe (actionErr "No roles found in claims" status412) pure . Data.Map.lookup "roles" . J.unClaimsMap . J.unregisteredClaims . J.claims $ token
                        case fromJSON roles :: Result [Role] of
                          Error s -> actionErr s status412
                          Success ros -> pure $ elem role ros  

verifyAgainstAuthService :: MonadIO m => String -> String -> ActionT TL.Text m (J.JWT J.VerifiedJWT)
verifyAgainstAuthService clientId clientSecret  = do
  (oauth, t) <- verifyAuthHeader clientSecret
  resp <- R.req R.POST (R.http "localhost" R./: "user" R./: "verify" R./~ clientId) R.NoReqBody R.ignoreResponse 
                (R.header "authorization" ("Bearer " <> oauth) <> R.port 4000)
  case R.responseStatusCode resp of
    200 -> return t
    _   -> throwError $ ActionError status401 "Authorization failure"
