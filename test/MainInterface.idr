import Control.App
import Control.App.Console

import Mysql
import Data.Vect

selectTest : Has [Console, MysqlI] e => App e ()
selectTest = do
  let host = "127.0.0.1"
      username = "tom"
      password = "tom123"
      database = "tom"
      port = 3306
      connectionInfo = MkConnectionInfo host username password database port
  Right client <- connect connectionInfo
        | Left err => putStrLn $ show err
  res <- fetchOne client "select 42, 21"
  putStrLn $ show res
  disconnect client

main : IO ()
main = run selectTest
