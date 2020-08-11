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

-- TODO this function is deprecated according to:
-- https://dev.mysql.com/doc/c-api/8.0/en/mysql-server-end.html
-- But when I tried to use mysql_library_end, I got an error when running a test:
-- Exception in foreign-procedure: no entry for "mysql_library_end"
-- After investigation, it indeed looks like the library I have installed libmysqlclient21 8.0.21-1ubuntu18.04
-- does not have this symbol installed, verified with the following command which returns no results:
-- nm -gD /usr/lib/x86_64-linux-gnu/libmysqlclient.so.21 | grep mysql_library
-- void mysql_server_end(void)
%foreign (libmysqlclient "mysql_server_end")
export
mysql_server_end : PrimIO ()

-- unsigned int mysql_errno(MYSQL *mysql)
%foreign (libmysqlclient "mysql_errno")
export
mysql_errno : AnyPtr -> PrimIO Bits32

-- const char *mysql_error(MYSQL *mysql)
%foreign (libmysqlclient "mysql_error")
export
mysql_error : AnyPtr -> PrimIO String
