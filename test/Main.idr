import Mysql.Bindings
import Support.Bindings

main : IO ()
main = do
  putStrLn "initializing"
  mysql <- primIO $ mysql_init prim__getNullAnyPtr
  putStrLn "initialized"

  let host = "127.0.0.1"
      username = "tom"
      password = "tom123"
      database = "tom"
      port = 3306
      unix_socket = prim__castPtr prim__getNullAnyPtr
      client_flag = 0
  putStrLn "connecting"
  mysql' <-primIO $ mysql_real_connect mysql host username password database port unix_socket client_flag
  -- TODO if mysql' is null then it we got an error
  putStrLn "connected"

  putStrLn "querying"
  ret <- primIO $ mysql_query mysql "select 42"
  putStrLn ("mysql_query returned " ++ show ret)
  if ret == 0
     then pure () -- success
     else pure () -- failure

  putStrLn "storing result"
  result <- primIO $ mysql_store_result mysql
  putStrLn "stored result"

  -- get number of fields from the result
  putStrLn "getting the number of fields from the result"
  numFields <- primIO $ mysql_num_fields result
  putStrLn ("Got " ++ show numFields ++ " field(s) from the result")

  row <- primIO $ mysql_fetch_row result

  if prim__nullAnyPtr row == 1
     then putStrLn "row is a null pointer, not printing out the row"
     else do
       putStrLn "row is not a null pointer, printing out the row"
       value <- primIO $ get_column row 0
       putStrLn ("first column contains: " ++ value)

  putStrLn "freeing result"
  primIO $ mysql_free_result result
  putStrLn "freed result"

  putStrLn "disconnecting"
  primIO $ mysql_close mysql
  putStrLn "disconnected"
