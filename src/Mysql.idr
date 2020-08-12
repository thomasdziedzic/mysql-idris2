module Mysql

import Control.App

import public Mysql.Types
import Mysql.Wrapper

import Data.Vect

export
interface MysqlI e where
  -- TODO move host username, password, database, port into a ConnectionInfo record
  connect : ConnectionInfo ->
            App e (Either MysqlError (Client Connected))
  disconnect : (client : Client Connected) ->
               App e ()
  query : (client : Client Connected) ->
          (query : String) ->
          App e (Either MysqlError (Maybe (nCols ** Vect nCols String)))

  -- TODO implement fetchOne
  -- TODO implement fetchMany

export
Has [PrimIO] e => MysqlI e where
  connect connectionInfo = do
    Right disconnectedClient <- primIO mysqlInit
        | Left err => pure $ Left err
    Right client <- primIO $ mysqlRealConnect disconnectedClient connectionInfo
        | Left err => pure $ Left err
    pure $ Right client

  disconnect client = do
    primIO $ mysqlClose client
    primIO $ mysqlServerEnd

  query client q = do
    Right () <- primIO $ mysqlQuery client q
        | Left err => pure $ Left err
    Right (resultCount ** result) <- primIO $ mysqlStoreResult client
        | Left err => pure $ Left err
    case resultCount of
         None => do
           primIO $ putStrLn "Got no results so not printing anything"
           pure $ Right Nothing
         Many => do
           row <- primIO $ mysqlFetchRow client result
           case row of
                Left err => pure $ Left err
                Right Nothing => do
                  primIO $ putStrLn "Got many results but fetch row returned nothing, I don't think this should happen"
                  pure $ Right Nothing
                Right (Just row) => do
                  --firstColumn <- primIO $ getColumn row 0
                  nCols <- primIO $ mysqlNumFields result
                  fetchedRow <- primIO $ fetchOne nCols row
                  primIO $ mysqlFreeResult result
                  pure $ Right $ Just (nCols ** fetchedRow)
