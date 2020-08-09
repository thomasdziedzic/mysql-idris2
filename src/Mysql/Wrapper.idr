module Mysql.Wrapper

import Mysql.Bindings
import Mysql.Types
import Support.Bindings

export
mysqlInit : HasIO io => io (Either MysqlError (Client Disconnected))
mysqlInit = do
  mysql <- primIO $ mysql_init prim__getNullAnyPtr
  if prim__nullAnyPtr mysql == 1
     then pure $ Left OutOfMemory
     else pure $ Right (MkClient mysql)

export
mysqlRealConnect : HasIO io =>
                   (client : Client Disconnected) ->
                   (host : String) ->
                   (username : String) ->
                   (password : String) ->
                   (database : String) ->
                   (port : Bits32) ->
                   io (Either MysqlError (Client Connected))
mysqlRealConnect (MkClient mysql) host username password database port = do
    let unix_socket = prim__castPtr prim__getNullAnyPtr
        client_flag = 0
    mysql' <- primIO $ mysql_real_connect mysql host username password database port unix_socket client_flag
    if prim__nullAnyPtr mysql' == 1
       then do
         errno <- primIO $ mysql_errno mysql
         error <- primIO $ mysql_error mysql
         pure (Left (MkMysqlError errno error))
       else pure (Right (MkClient mysql))

export
mysqlQuery : HasIO io => (client : Client Connected) -> (q : String) -> io (Either MysqlError ())
mysqlQuery (MkClient mysql) q = do
    ret <- primIO $ mysql_query mysql q
    if ret == 0
       then pure $ Right ()
       else do
         errno <- primIO $ mysql_errno mysql
         error <- primIO $ mysql_error mysql
         pure $ Left (MkMysqlError errno error)

export
mysqlStoreResult : HasIO io => (client : Client Connected) -> io (Either MysqlError (a ** (Result a)))
mysqlStoreResult (MkClient mysql) = do
  result <- primIO $ mysql_store_result mysql
  if prim__nullAnyPtr result == 1
     then do
       errno <- primIO $ mysql_errno mysql
       if errno == 0
          then pure $ Right (None ** NoResults)
          else do
            error <- primIO $ mysql_error mysql
            pure $ Left (MkMysqlError errno error)
     else pure $ Right $ (Many ** SomeResults result)

export
mysqlNumFields : HasIO io => Result Many -> io Bits32
mysqlNumFields (SomeResults result) = primIO $ mysql_num_fields result

export
mysqlFetchRow : HasIO io => (client : Client Connected) -> Result Many -> io (Either MysqlError (Maybe Row))
mysqlFetchRow (MkClient mysql) (SomeResults result) = do
  row <- primIO $ mysql_fetch_row result
  if prim__nullAnyPtr row == 1
     then do
       errno <- primIO $ mysql_errno mysql
       if errno == 0
          then pure $ Right $ Nothing
          else do
            error <- primIO $ mysql_error mysql
            pure $ Left (MkMysqlError errno error)
     else pure $ Right $ Just $ MkRow row

-- TODO implement mysqlFetchLengths
mysqlFetchLengths : HasIO io => Result Many -> io (List Bits64)

-- TODO what if index is out of bounds?
-- TODO does getColumn belong in this file or should we move it to a Support.Wrapper module?
export
getColumn : HasIO io => Row -> Bits32 -> io (String)
getColumn (MkRow row) index = primIO $ get_column row index

-- TODO maybe we should have a withResult function replace the mysqlStoreResult and mysqlFreeResult so that we don't forget to free the pointer
export
mysqlFreeResult : HasIO io => Result Many -> io ()
mysqlFreeResult (SomeResults result) = primIO $ mysql_free_result result

export
mysqlClose : HasIO io => (client : Client Connected) -> io ()
mysqlClose (MkClient mysql) = primIO $ mysql_close mysql
