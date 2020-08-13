module Mysql

import Control.App

import public Mysql.Types
import Mysql.Wrapper

import Data.Vect

export
interface MysqlI e where
  connect : ConnectionInfo ->
            App e (Either MysqlError (Client Connected))
  disconnect : (client : Client Connected) ->
               App e ()
  fetchOne : (client : Client Connected) ->
             (query : String) ->
             App e (Either MysqlError (Maybe (nCols ** Vect nCols String)))
  fetchMany : (client : Client Connected) ->
              (query : String) ->
              (nRows : Nat) ->
              App e (Either MysqlError (Maybe (nCols ** (Vect nRows (Vect nCols String)))))
  fetchAll : (client : Client Connected) ->
             (query : String) ->
             App e (Either MysqlError (Maybe (nRows ** (nCols ** Vect nRows (Vect nCols String)))))

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

  fetchOne client q = do
    Right () <- primIO $ mysqlQuery client q
        | Left err => pure $ Left err
    Right (resultCount ** result) <- primIO $ mysqlStoreResult client
        | Left err => pure $ Left err
    case resultCount of
         None => pure $ Right Nothing
         Many => do
           Right mRow <- primIO $ mysqlFetchRow client result
               | Left err => pure $ Left err
           case mRow of
                Nothing => do
                  pure $ Right Nothing
                Just row => do
                  nCols <- primIO $ mysqlNumFields result
                  fetchedRow <- primIO $ fetchOne nCols row
                  primIO $ mysqlFreeResult result
                  pure $ Right $ Just (nCols ** fetchedRow)

  fetchAll client q = do
    Right () <- primIO $ mysqlQuery client q
        | Left err => pure $ Left err
    Right (resultCount ** result) <- primIO $ mysqlStoreResult client
        | Left err => pure $ Left err
    case resultCount of
         None => do
           pure $ Right Nothing
         Many => do
           rows <- primIO $ ?fetchAllHelper client result
           pure $ Right $ Just rows
