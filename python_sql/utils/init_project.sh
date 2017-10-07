if [ $# -ne 6 ]; then
  echo "format: ./execute_sql.sh host rootUserName rootPassword createUser createPass createDb"
  exit
fi

host=$1
user=$2
pass=$3
createUser=$4
createPass=$5
createDb=$6

initVsSql="./odds/init_vs.sql"

echo "create database ${createDb} DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci;" | mysql -h${host} -u${user} -p${pass}
echo "grant all privileges on ${createDb}.* to ${createUser} identified by '${createPass}';" | mysql -h${host} -u${user} -p${pass}
echo "flush privileges;" | mysql -h${host} -u${user} -p${pass}
mysql -h${host} -u${user} -p${pass} -D${createDb} < ${initVsSql}
