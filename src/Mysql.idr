module Mysql

import Control.App

import Mysql.Bindings
import Support.Bindings

data Status = Disconnected | Connected

data Client : Status -> Type where
  MkClient : AnyPtr -> Client status

data MysqlError =
  MkMysqlError Bits32 String

export
Show MysqlError where
  show (MkMysqlError errno error) = "Mysql Error: (" ++ show errno ++ ") " ++ error

export
interface MysqlI e where
  connect : (host : String) ->
            (username : String) ->
            (password : String) ->
            (database : String) ->
            (port : Bits32) ->
            App e (Either MysqlError (Client Connected))
  disconnect : (client : Client Connected) ->
               App e ()
  query : (client : Client Connected) ->
          (query : String) ->
          App e (Either String String)

export
Has [PrimIO] e => MysqlI e where
  connect host username password database port = do
    -- the first primIO is for the Control.App PrimIO interface, the second is to lift it from a PrimIO type to HasIO io
    mysql <- primIO $ primIO $ mysql_init prim__getNullAnyPtr
    let unix_socket = prim__castPtr prim__getNullAnyPtr
        client_flag = 0
    mysql' <- primIO $ primIO $ mysql_real_connect mysql host username password database port unix_socket client_flag
    if prim__nullAnyPtr mysql' == 1
       then do
         errno <- primIO $ primIO $ mysql_errno mysql
         error <- primIO $ primIO $ mysql_error mysql
         pure (Left (MkMysqlError errno error))
       else pure (Right (MkClient mysql))

  disconnect (MkClient mysql) = primIO $ primIO $ mysql_close mysql

  query (MkClient mysql) q = do
    ret <- primIO $ primIO $ mysql_query mysql q
    if ret == 0
       then pure () -- success
       else pure () -- failure

    result <- primIO $ primIO $ mysql_store_result mysql

    numFields <- primIO $ primIO $ mysql_num_fields result

    row <- primIO $ primIO $ mysql_fetch_row result

    if prim__nullAnyPtr row == 1
       then pure (Left "row is a null pointer, not printing out the row")
       else do
         value <- primIO $ primIO $ get_column row 0
         primIO $ primIO $ mysql_free_result result
         pure (Right value)
