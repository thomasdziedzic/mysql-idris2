module Mysql.Bindings

libmysqlclient : String -> String
libmysqlclient fn = "C:" ++ fn ++ ",libmysqlclient"

-- MYSQL *mysql_init(MYSQL *mysql)
%foreign (libmysqlclient "mysql_init")
export
mysql_init : AnyPtr -> PrimIO AnyPtr

-- MYSQL *mysql_real_connect(MYSQL *mysql, const char *host, const char *user, const char *passwd, const char *db, unsigned int port, const char *unix_socket, unsigned long client_flag)
%foreign (libmysqlclient "mysql_real_connect")
export
mysql_real_connect : AnyPtr -> String -> String -> String -> String -> Bits32 -> Ptr String -> Bits64 -> PrimIO AnyPtr

-- int mysql_query(MYSQL *mysql, const char *stmt_str)
%foreign (libmysqlclient "mysql_query")
export
mysql_query : AnyPtr -> String -> PrimIO Int

-- MYSQL_RES *mysql_store_result(MYSQL *mysql)
%foreign (libmysqlclient "mysql_store_result")
export
mysql_store_result : AnyPtr -> PrimIO AnyPtr

-- unsigned int mysql_num_fields(MYSQL_RES *result)
%foreign (libmysqlclient "mysql_num_fields")
export
mysql_num_fields : AnyPtr -> PrimIO Bits32

-- MYSQL_ROW mysql_fetch_row(MYSQL_RES *result)
%foreign (libmysqlclient "mysql_fetch_row")
export
mysql_fetch_row : AnyPtr -> PrimIO AnyPtr

-- unsigned long *mysql_fetch_lengths(MYSQL_RES *result)
%foreign (libmysqlclient "mysql_fetch_lengths")
export
mysql_fetch_lengths : AnyPtr -> PrimIO (Ptr Bits64)

-- void mysql_free_result(MYSQL_RES *result)
%foreign (libmysqlclient "mysql_free_result")
export
mysql_free_result : AnyPtr -> PrimIO ()

-- void mysql_close(MYSQL *mysql)
%foreign (libmysqlclient "mysql_close")
export
mysql_close : AnyPtr -> PrimIO ()