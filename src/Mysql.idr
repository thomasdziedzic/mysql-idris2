module Mysql

data Connection = Disconnected | Connected

data Result : Type where

data Client : Type -> Connection -> Connection -> Type where
  Connect : Client () Disconnected Connected
  Disconnect : Client () Connected Disconnected

  Query : Client Result Connected Connected
