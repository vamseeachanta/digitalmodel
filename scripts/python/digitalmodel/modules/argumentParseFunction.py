import argparse
def argumentParseFunction():

    parser = argparse.ArgumentParser(description='Get AssetID')
    parser.add_argument('--log', dest='log', nargs=1,
 	    help='You must pass the --log value')

    parser.add_argument('--SQLType', dest='SQLType', nargs=1,
 	    help='You must pass the --SQLType value')

    parser.add_argument('--SQLServerName', dest='SQLServerName', nargs=1,
 	    help='You must pass the --SQLServerName Value')
    parser.add_argument('--SQLDatabaseName', dest='SQLDatabaseName', nargs=1,
 	    help='You must pass the --SQLDatabaseName Value')
    parser.add_argument('--SQLUserName', dest='SQLUserName', nargs=1,
 	    help='You must pass the --SQLUserName Value')
    parser.add_argument('--SQLPassword', dest='SQLPassword', nargs=1,
 	    help='You must pass the --SQLPassword Value')
    parser.add_argument('--OracleServerName', dest='OracleServerName', nargs=1,
 	    help='You must pass the --OracleServerName Value')
    parser.add_argument('--OracleUserName', dest='OracleUserName', nargs=1,
 	    help='You must pass the --OracleUserName Value')
    parser.add_argument('--OraclePassword', dest='OraclePassword', nargs=1,
 	    help='You must pass the --OraclePassword Value')
    parser.add_argument('--MySQLServerName', dest='MySQLServerName', nargs=1,
 	    help='You must pass the --SQLServerName Value')
    parser.add_argument('--MySQLDatabaseName', dest='MySQLDatabaseName', nargs=1,
 	    help='You must pass the --SQLDatabaseName Value')
    parser.add_argument('--MySQLUserName', dest='MySQLUserName', nargs=1,
 	    help='You must pass the --SQLUserName Value')
    parser.add_argument('--MySQLPassword', dest='MySQLPassword', nargs=1,
 	    help='You must pass the --SQLPassword Value')
    parser.add_argument('--tableName', dest='tableName', nargs=1,
 	    help='You must pass the --SQLTableName Value')
    parser.add_argument('--tableColumns', dest='tableColumns', nargs=1,
 	    help='You must pass the --SQLTableColumns Value')

    args = parser.parse_args()

    return(args)

