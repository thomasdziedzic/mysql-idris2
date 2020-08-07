#include <mysql/mysql.h>

#include "libidrismysql.h"

char *get_column(MYSQL_ROW row, unsigned int index) {
    return row[index];
}
