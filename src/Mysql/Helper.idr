module Mysql.Helper

import Data.Vect

import Mysql.Types
import Mysql.Wrapper
import Support.Bindings

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

export
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
