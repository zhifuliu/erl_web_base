if [ $# -ne 5 ]; then
  echo "format: ./execute_sql.sh host user password db sqlFile"
  exit
fi

host=$1
user=$2
pass=$3
db=$4
sqlFile=$5

mysql --default-character-set=utf8 -h${host} -u${user} -p${pass} -D${db} < ${sqlFile}
