{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Common where

import Data.Aeson (FromJSON, ToJSON, decode)

import           Database.PostgreSQL.Simple.FromField ( FromField(..) )
import           Database.PostgreSQL.Simple.ToField ( ToField(..) )
import           Database.PostgreSQL.Simple.Types 
    (PGArray (PGArray, fromPGArray), Query (Query))
import           Data.Text.Encoding                                          as T ( encodeUtf8 )
import           Data.Text                                                   as T ( pack )
import           Data.ByteString.Lazy.Internal ( ByteString )
import           GHC.Generics ( Generic )
import           Database.PostgreSQL.Simple (Connection, SqlError)
import           Control.Monad.Except
import           Web.Scotty.Internal.Types
    ( ActionError(ActionError), ScottyT )
import           Control.Monad.Reader
import qualified Data.Text.Lazy                                              as TL
import qualified Data.Text.Lazy.Encoding                                     as TL
import           Control.Exception (try)
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.Cors
import           Web.Scotty.Trans (middleware)


data Role = Worker | Admin | Accountant | Manager deriving (Show, Read, Generic, FromJSON, ToJSON)

instance ToField Role where toField = toField . show
instance FromField Role where fromField f dat = read <$> fromField f dat
instance ToField [Role] where toField = toField . PGArray
instance FromField [Role] where fromField f dat = fromPGArray <$> fromField f dat

toQuery :: String -> Query
toQuery = Query . T.encodeUtf8 . T.pack

class DBConnect a where
    schematable :: a -> (a -> String) -> String
    connection :: a -> Connection

type DBConstraints m a = (MonadError (ActionError TL.Text) m, MonadIO m, DBConnect a, MonadReader a m)

simpleDB :: DBConstraints m a => (a -> String) -> (String -> Query) -> (Query -> Connection -> IO b) -> m b
simpleDB c q t = join $ asks (\rt -> (absorbError . liftIO . try . t (q $ schematable rt c) . connection) rt)
                    where absorbError :: (DBConstraints m a) => m (Either SqlError b) -> m b
                          absorbError = (either (throwError . ActionError status422 . TL.pack . show) pure =<<)

decodeOrThrow :: (MonadError (ActionError TL.Text) m, FromJSON a) => m ByteString -> Status -> m a
decodeOrThrow b s = b >>= \bb -> maybe (throwError $ ActionError s ("Error: " <> TL.decodeUtf8 bb <> " cannot be decoded")) pure (decode bb)


midware :: ScottyT e m ()
midware = middleware $ cors $ const $ Just simpleCorsResourcePolicy {
              corsRequestHeaders = "authorization":simpleHeaders,
              corsMethods = "POST":"PUT":"DELETE":simpleMethods
            }