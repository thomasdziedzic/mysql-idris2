module Support

libidrismysql : String -> String
libidrismysql fn = "C:" ++ fn ++ ",libidrismysql"

%foreign (libidrismysql "get_column")
export
get_column : AnyPtr -> Bits32 -> PrimIO String
