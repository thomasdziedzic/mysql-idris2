module Mysql.Wrapper

import Mysql.Bindings
import Mysql.Types
import Support.Bindings

import Data.Vect

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
                   ConnectionInfo ->
                   io (Either MysqlError (Client Connected))
mysqlRealConnect (MkClient mysql) connectionInfo = do
    let unix_socket = prim__castPtr prim__getNullAnyPtr
        client_flag = 0
    -- cast port from 16 bits to 32 bits. Ports are 16 bits long, but the mysql c api uses a 32 bit unsigned integer
    mysql' <- primIO $ mysql_real_connect
                            mysql
                            (host connectionInfo)
                            (username connectionInfo)
                            (password connectionInfo)
                            (database connectionInfo)
                            (cast16to32 (port connectionInfo))
                            unix_socket
                            client_flag
    if prim__nullAnyPtr mysql' == 1
       then do
         errno <- primIO $ mysql_errno mysql
         error <- primIO $ mysql_error mysql
         pure (Left (MkMysqlError errno error))
       else pure (Right (MkClient mysql))
  where
    cast16to32 : Bits16 -> Bits32
    cast16to32 x = fromInteger (prim__cast_Bits16Integer x)

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
mysqlNumFields : HasIO io => Result Many -> io Nat
mysqlNumFields (SomeResults result) = do
  cols <- primIO $ mysql_num_fields result
  pure $ castBits32ToNat cols
  where
    castBits32ToNat : Bits32 -> Nat
    castBits32ToNat x = fromInteger (prim__cast_Bits32Integer x)

export
mysqlNumRows : HasIO io => Result Many -> io Nat
mysqlNumRows (SomeResults result) = do
  rows <- primIO $ mysql_num_rows result
  pure $ castBits64ToNat rows
  where
    castBits64ToNat : Bits64 -> Nat
    castBits64ToNat x = fromInteger (prim__cast_Bits64Integer x)

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
getColumn : HasIO io => Row -> Nat -> io String
getColumn (MkRow row) index = primIO $ get_column row (castNatTo32 index)
  where
    toIntegerNat : Nat -> Integer
    toIntegerNat Z = 0
    toIntegerNat (S k) = 1 + toIntegerNat k

    -- TODO This might be unsafe if we ever represent a Nat larger than a Bits32
    castNatTo32 : Nat -> Bits32
    castNatTo32 n = fromInteger $ toIntegerNat n

fetchOneReversed : HasIO io => (n : Nat) -> Row -> io (Vect n String)
fetchOneReversed 0 _ = pure []
fetchOneReversed (S k) row = do
  val <- getColumn row k
  rest <- fetchOneReversed k row
  pure $ val :: rest

export
fetchOneRow : HasIO io => (n : Nat) -> Row -> io (Vect n String)
fetchOneRow n row = do
  reversedRow <- fetchOneReversed n row
  pure $ reverse reversedRow

-- TODO maybe we should have a withResult function replace the mysqlStoreResult and mysqlFreeResult so that we don't forget to free the pointer
export
mysqlFreeResult : HasIO io => Result Many -> io ()
mysqlFreeResult (SomeResults result) = primIO $ mysql_free_result result

export
mysqlClose : HasIO io => (client : Client Connected) -> io ()
mysqlClose (MkClient mysql) = primIO $ mysql_close mysql

export
mysqlServerEnd : HasIO io => io ()
mysqlServerEnd = primIO $ mysql_server_end
