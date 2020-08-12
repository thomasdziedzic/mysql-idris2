module Mysql.Types

public export
data MysqlError = OutOfMemory
                | MkMysqlError Bits32 String

export
Show MysqlError where
  show OutOfMemory = "Mysql Error: Out of memory"
  show (MkMysqlError errno error) = "Mysql Error: (" ++ show errno ++ ") " ++ error

public export
data Status = Disconnected | Connected

public export
data Client : Status -> Type where
  MkClient : AnyPtr -> Client status

public export
data ResultCount = None
                 | Many

public export
data Result : ResultCount -> Type where
  NoResults : Result None
  SomeResults : AnyPtr -> Result Many

public export
data Row = MkRow AnyPtr
