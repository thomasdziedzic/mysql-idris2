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
              (limit : Nat) ->
              (query : String) ->
              App e (Either MysqlError (Maybe (nRows ** (nCols ** (Vect nRows (Vect nCols String))))))
  fetchAll : (client : Client Connected) ->
             (query : String) ->
             App e (Either MysqlError (Maybe (nRows ** (nCols ** Vect nRows (Vect nCols String)))))

fetchHelper : HasIO io =>
              {accRows : Nat} ->
              (limit : Nat) ->
              (nCols : Nat) ->
              (client : Client Connected) ->
              (result : Result Many) ->
              (acc : Vect accRows (Vect nCols String)) ->
              io (Either MysqlError (nRows ** (nCols ** (Vect nRows (Vect nCols String)))))
fetchHelper {accRows} 0 nCols client result acc = pure $ Right (accRows ** (nCols ** acc))
fetchHelper {accRows} (S k) nCols client result acc = do
  Right mRow <- mysqlFetchRow client result
    | Left err => pure $ Left err
  case mRow of
       Nothing => pure $ Right (accRows ** (nCols ** acc))
       Just row => do
         fetchedRow <- fetchOneRow nCols row
         fetchHelper k nCols client result (fetchedRow :: acc)

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

  fetchOne client query = do
    Right () <- primIO $ mysqlQuery client query
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
                  fetchedRow <- primIO $ fetchOneRow nCols row
                  primIO $ mysqlFreeResult result
                  pure $ Right $ Just (nCols ** fetchedRow)

  fetchMany client limit query = do
    Right () <- primIO $ mysqlQuery client query
        | Left err => pure $ Left err
    Right (resultCount ** result) <- primIO $ mysqlStoreResult client
        | Left err => pure $ Left err
    case resultCount of
         None => pure $ Right Nothing
         Many => do
           nCols <- primIO $ mysqlNumFields result
           Right (nRows ** (nCols ** allRows)) <- primIO $ fetchHelper limit nCols client result []
             | Left err => pure $ Left err
           primIO $ mysqlFreeResult result
           -- we need to reverse the fetchHelper results because it accumulates results using a vector
           pure $ Right $ Just (nRows ** (nCols ** (reverse allRows)))

  fetchAll client query = do
    Right () <- primIO $ mysqlQuery client query
        | Left err => pure $ Left err
    Right (resultCount ** result) <- primIO $ mysqlStoreResult client
        | Left err => pure $ Left err
    case resultCount of
         None => do
           pure $ Right Nothing
         Many => do
           rows <- primIO $ ?fetchAllHelper client result
           pure $ Right $ Just rows
