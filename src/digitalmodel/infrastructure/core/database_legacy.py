import logging
import os


def get_db_connection(db_properties):
    dbe = Database(db_properties)
    try:
        dbe.enable_connection_and_cursor()
        connection_status = True
        return dbe, connection_status
    except Exception as e:
        connection_status = False
        print("Error as {}".format(e))
        print("No connection for environment: {}".format(db_properties.get('server', None)))
        return dbe, connection_status


def get_db_properties_for_service(service):
    import yaml
    working_directory = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
    filename = os.path.join(working_directory, 'data', 'database.yml')

    with open(filename, 'r') as ymlfile:
        database_dict = yaml.safe_load(ymlfile)
    db_properties = database_dict[service].copy()
    current_env = os.environ.get('environment', 'stage')
    db_properties.update(database_dict['common'][current_env])
    return db_properties


def test_saving_service_data_to_db():
    pass


class Database():

    def __init__(self, db_properties):
        from common.data import AttributeDict
        self.init_assign_db_properties(db_properties)
        self.analysis = AttributeDict()

    def init_assign_db_properties(self, db_properties):
        self.highAvailability = True
        self.server_type = db_properties.get('server_type', None)
        self.server = db_properties.get('server', None)
        self.database = db_properties.get('database', None)
        self.schema = db_properties.get('schema', None)
        self.user = db_properties.get('user', None)
        self.password = db_properties.get('password', None)
        self.port = db_properties.get('port', None)
        self.connection_string = db_properties.get('connection_string', None)
        self.parse_dates = db_properties.get('pd_parse_dates_columns', True)
        self.cursor = None
        self.conn = None

    def db_retry_decorator(f):
        import logging
        from functools import wraps

        @wraps(f)
        def wrapper(self, *args, **kwargs):
            try:
                return f(self, *args, **kwargs)
            except Exception as e:
                logging.info(str(e))
                return wrapper(self, *args, **kwargs)

        return wrapper

    def get_input_data(self, cfg_input):
        try:
            self.enable_connection_and_cursor()
        except Exception as e:
            print("No connection to input db, error {0}".format(e))
        try:
            if cfg_input['source'] == 'db':
                self.get_input_data_from_db(cfg_input)
            else:
                print("No input data source defined")
        except Exception as e:
            print("Error getting input data in error {0}".format(e))

    def set_up_db_connection(self, db_properties):
        try:
            self.enable_connection_and_cursor()
            return True
        except Exception as e:
            print("Error as {}".format(e))
            print("No connection for environment: {}".format(db_properties))
            return False

    def enable_connection_and_cursor(self):
        from urllib.parse import quote_plus

        from sqlalchemy import create_engine
        if self.server_type == 'mssql':
            cfg_sql_express = {
                'server_type': 'mssql',
                'server': 'localhost\\SQLEXPRESS',
                'user': None,
                'password': None,
                'database': 'master'
            }
            import logging
            logging.info("  ******DATABASE******")
            logging.info("Attempt to connect to Server: {0}, user: {1}, password:not displayed, database: {2}".format(
                self.server, self.user, self.database))
            try:
                if self.user != None:
                    connection_string_driver_specific = "Driver={ODBC Driver 13 for SQL Server};Server=" + self.server + ";Database=" + self.database + ";Uid=" + self.user + ";Pwd=" + self.password + ";"
                    connection_string_generic = "Driver=SQL+Server;Server=" + self.server + ";Database=" + self.database + ";Uid=" + self.user + ";Pwd=" + self.password + ";"
                    if self.highAvailability:
                        connection_string_driver_specific = connection_string_driver_specific + "MultiSubnetFailover=Yes;"
                        connection_string_generic = connection_string_generic + "MultiSubnetFailover=Yes;"
                    connection_string_generic = "mssql+pyodbc:///?odbc_connect=" + quote_plus(
                        connection_string_generic)
                    connection_string_driver_specific = "mssql+pyodbc:///?odbc_connect=" + quote_plus(
                        connection_string_driver_specific)
                    try:
                        self.engine = create_engine(connection_string_generic, encoding='utf-8', echo=False)
                    except:
                        print("Generic driver did not work. Utilizing specific driver")
                        self.engine = create_engine(connection_string_driver_specific, encoding='utf-8', echo=False)
                else:
                    import pyodbc
                    server_database_string = 'Server={0};Database={1};Trusted_Connection=yes;'.format(
                        self.server, self.database)
                    sql_alchemy_connection_string_generic = "mssql+pyodbc:///?odbc_connect=" + quote_plus(
                        'Driver={SQL Server};' + server_database_string)
                    self.engine = create_engine(sql_alchemy_connection_string_generic, encoding='utf-8', echo=False)

                    # pyodbc_connection_string_generic = "Driver={SQL Server};" + server_database_string
                    # self.conn = pyodbc.connect(pyodbc_connection_string_generic)
                    # self.get_db_version()

                self.conn = self.engine.connect()
                print("Connection to Server: {0} Successful by user {1} to database {2}!".format(
                    self.server, self.user, self.database))

            except:
                logging.info("MSSQL connection failed")
                print("MSSQL connection failed")

        if self.server_type == 'postgresql':
            import logging

            import psycopg2
            logging.info("  ******DATABASE******")
            logging.info("Attempt to connect to Server: {0}, user: {1}, password:not displayed, database: {2}".format(
                self.server, self.user, self.database))
            try:
                if self.connection_string is not None:
                    self.engine = create_engine(self.connection_string)
                    self.conn = self.engine.connect()
                    print("Connection to Server: {0} Successful by user {1} to database {2}!".format(
                        self.server, self.user, self.database))
                else:
                    connection_string = "postgresql+psycopg2://{0}:{1}@{2}:{4}/{3}".format(
                        self.user, self.password, self.server, self.database, self.port)
                    self.engine = create_engine(connection_string)
                    self.conn = self.engine.connect()
                    print("Connection to Server: {0} Successful by user {1} to database {2}!".format(
                        self.server, self.user, self.database))
            except (Exception, psycopg2.Error) as error:
                print("Error while connecting to PostgreSQL", error)
                logging.info("Error while connecting to PostgreSQL {}".format(error))

        if self.server_type == 'mongodb':
            from pprint import pprint

            from pymongo import MongoClient
            pprint(serverStatusResult)
            logging.info("  ******DATABASE******")
            logging.info("Attempt to connect to Server: {0}, user: {1}, password:not displayed, database: {2}".format(
                self.server, self.database))
            try:
                client = MongoClient()
                db = client.admin
                # Issue the serverStatus command and print the results
                serverStatusResult = db.command("serverStatus")

                connection_string = "postgresql+psycopg2://{0}:{1}@{2}:{4}/{3}".format(
                    self.user, self.password, self.server, self.database, self.port)
                self.engine = create_engine(connection_string)
                self.conn = self.engine.connect()
            except (Exception, psycopg2.Error) as error:
                print("Error while connecting to PostgreSQL", error)
                logging.info("Error while connecting to PostgreSQL {}".format(error))

        if self.server_type in ['accdb', 'mdb']:
            import logging
            logging.info("  ******DATABASE******")
            logging.info("Attempt to connect to Server: {0}, user: {1}, password:not displayed, database: {2}".format(
                None, None, self.database))
            try:
                # Could not get this working
                from pathlib import Path, PureWindowsPath

                import pyodbc
                dbq = r"DBQ={0}".format(PureWindowsPath(self.database))
                # below not working
                connection_string_generic = r"Driver={Microsoft Access Driver (*.mdb, *.accdb)};" + dbq + ";"
                connection_string_generic = r"Driver={Microsoft Access Driver (*.mdb, *.accdb)};" + r"Dbq=C:\Users\achantv\Documents\Utilities\aceengineer\data_manager\data\bsee\2018_Atlas_Update.accdb;"
                print(connection_string_generic)
                self.conn = pyodbc.connect(connection_string_generic)

                import pypyodbc
                pypyodbc.lowercase = False
                from pathlib import Path, PureWindowsPath
                dbq = r"DBQ={0}".format(PureWindowsPath(self.database))
                # below not working
                connection_string_generic = r"Driver={Microsoft Access Driver (*.mdb, *.accdb)};" + dbq + ";"
                connection_string_generic = r"Driver={Microsoft Access Driver (*.mdb, *.accdb)};" + r"Dbq=C:\Users\achantv\Documents\Utilities\aceengineer\data_manager\data\bsee\2018_Atlas_Update.accdb;"
                print(connection_string_generic)
                self.conn = pypyodbc.connect(connection_string_generic)

            except Exception as e:
                import sys
                logging.info("Access file connection failed: {}".format(e))
                sys.exit()
                print("Access file connection failed")

    def perform_analysis(self, cfg_analysis):
        db_analysis_result = {}
        if cfg_analysis['simple']:
            print("Performing db data analysis")
            db_analysis_result = self.get_db_basic_analysis_outputs()
        if cfg_analysis['table_statistics']['flag']:
            print("Performing db table statistics")
            statistics_df_columns = cfg_analysis['table_statistics']['statistics_df_columns']
            self.get_table_basic_statistics(statistics_df_columns)
        if cfg_analysis['timeline_statistics']['flag']:
            timeline_statistics_cfg = cfg_analysis['timeline_statistics'].copy()
            statistics_df_columns = cfg_analysis['table_statistics']['statistics_df_columns']
            self.get_timeline_statistics(timeline_statistics_cfg, statistics_df_columns)
        if cfg_analysis.__contains__('extreme_events') and cfg_analysis['extreme_events']['flag']:
            print("Performing event identification analysis")
            cfg_extreme_events = cfg_analysis['extreme_events']
            extreme_events_df_array = self.perform_extreme_event_analysis(cfg_extreme_events)
            db_analysis_result.update({
                'cfg_extreme_events': cfg_extreme_events,
                'extreme_events_df_array': extreme_events_df_array
            })
        return db_analysis_result

    def get_db_basic_analysis_outputs(self):
        db_version_df = self.get_db_version()
        db_table_df = self.get_db_tables()
        db_function_df = self.get_db_functions()
        table_column_df_array, column_df_array_tables = self.get_columns_for_all_db_tables(db_table_df)

        db_analysis_result = ({
            'db_version_df': db_version_df,
            'db_table_df': db_table_df,
            'db_function_df': db_function_df,
            'table_column_df_array': table_column_df_array,
            'column_df_array_tables': column_df_array_tables
        })
        return db_analysis_result

    def get_df_from_stored_procedure(self, sp, args):
        import logging

        import pandas as pd

        if len(args) == 1:
            if type(args[0]) is str:
                query = "exec dbo.{0} '{1}'".format(sp, args[0])
            else:
                query = "exec dbo.{0} {1}".format(sp, args[0])

        logging.debug(query)
        df = pd.read_sql_query(query, self.conn)
        return df

    def get_df_from_query(self, query):
        import logging

        import pandas as pd
        logging.debug(query)
        df = pd.read_sql_query(query, self.conn)
        return df

    def get_db_table_analysis_outputs(self):
        self.get_table_statistics()

    def get_db_version(self):
        import os
        filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions', 'sql_version.sql')
        if os.path.isfile(filename):
            df = self.executeScriptsFromFile(filename)
            self.analysis.update({'version': df})
            return df
        else:
            print("Not a valid filename")

    def get_db_tables(self):
        import os
        filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                'tables_in_database.sql')
        if os.path.isfile(filename):
            df = self.executeScriptsFromFile(filename)
            self.analysis.update({'tables': df})
            return df
        else:
            print("Not a valid filename")

    def get_db_functions(self):
        import os
        filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                'functions_in_database.sql')
        if os.path.isfile(filename):
            df = self.executeScriptsFromFile(filename)
            self.analysis.update({'store_procedures': df})
            return df
        else:
            print("Not a valid filename")

    def get_columns_for_all_db_tables(self, db_table_df):
        import pandas as pd
        column_df_array = []
        column_df_array_tables = []
        for row_index in range(0, len(db_table_df)):
            table_name = db_table_df.iloc[row_index].TABLE_NAME
            self.schema = db_table_df.iloc[row_index].TABLE_SCHEMA
            try:
                column_df = self.get_table_columns(table_name)
            except:
                column_df = pd.DataFrame()
                print("          ..... FAILURE DB table columns for table {1} of {2}: {0}".format(
                    table_name, row_index, len(self.analysis.tables)))
            column_df_array.append(column_df)
            column_df_array_tables.append(table_name)

        return column_df_array, column_df_array_tables

    def get_table_basic_statistics(self, statistics_df_columns):
        import pandas as pd
        self.StatisticsClass_df = None
        for table_index in range(0, len(self.analysis.tables)):
            # for table_index in range(0, 1):
            self.table_statistics = pd.DataFrame(columns=statistics_df_columns)
            self.schema = self.analysis.tables.iloc[table_index].TABLE_SCHEMA
            self.StatisticsClass_df = self.get_schema_statistics_class()
            table_name = self.analysis.tables.iloc[table_index].TABLE_NAME
            print("          ..... DB table statistics for table {1} of {2}: {0}".format(
                table_name, table_index, len(self.analysis.tables)))
            try:
                self.get_table_statistics(table_name)
                if len(self.table_statistics) > 0:
                    self.save_to_db(self.table_statistics, 'TableStatistics')
            except:
                print("          ..... FAILURE DB table statistics for table {1} of {2}: {0}".format(
                    table_name, table_index, len(self.analysis.tables)))

    def get_schema_statistics_class(self):
        import os
        schema_table = self.schema + '.' + 'StatisticsClass'
        query_argument_array = [schema_table]
        filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                'get_all_records_from_table.sql')
        if os.path.isfile(filename):
            df = self.executeScriptsFromFile(filename, query_argument_array)
            return df
        else:
            print("Not a valid filename")

    def get_table_statistics(self, table_name, chosen_columns=None):
        column_df = self.get_table_columns(table_name)
        for df_row in range(0, len(column_df)):
            # for df_row in range(0, 1):
            column_name = column_df.iloc[df_row]['Column Name']
            process_column_flag = False
            if chosen_columns is None:
                process_column_flag = True
            elif column_name in chosen_columns:
                process_column_flag = True
            if process_column_flag:
                column_data_type = column_df.iloc[df_row]['Data type']
                StatisticsClassType = 'Time Range'
                StatisticsClassId = self.StatisticsClass_df[self.StatisticsClass_df.StatisticsClassType ==
                                                            StatisticsClassType].StatisticsClassId.iloc[0]

                table_column_statistics_df = self.get_table_column_statistics(table_name, column_name,
                                                                              column_data_type)
                start_time = None
                end_time = None
                table_rows = table_column_statistics_df.table_rows.iloc[0]
                minimum = table_column_statistics_df.minimum.iloc[0]
                maximum = table_column_statistics_df.maximum.iloc[0]
                average = None
                RMS = None
                StandardDeviation = None
                column_statistics_array = [
                    1, self.schema, table_name, column_name, column_data_type, StatisticsClassId, start_time, end_time,
                    table_rows, minimum, maximum, average, RMS, StandardDeviation
                ]
                if table_rows > 0:
                    self.table_statistics.loc[len(self.table_statistics)] = column_statistics_array

    def get_table_timetrace_statistics(self, table_name, start_time, end_time, time_column, chosen_columns=None):
        column_df = self.get_table_columns(table_name)
        for df_row in range(0, len(column_df)):
            # for df_row in range(0, 1):
            column_name = column_df.iloc[df_row]['Column Name']
            process_column_flag = False
            if chosen_columns is None:
                process_column_flag = True
            elif column_name in chosen_columns:
                process_column_flag = True
            if process_column_flag:
                column_data_type = column_df.iloc[df_row]['Data type']
                StatisticsClassType = 'Time Range'
                StatisticsClassId = self.StatisticsClass_df[self.StatisticsClass_df.StatisticsClassType ==
                                                            StatisticsClassType].StatisticsClassId.iloc[0]
                table_column_timetraces_df = self.get_table_column_time_traces(table_name, column_name,
                                                                               column_data_type, start_time, end_time,
                                                                               time_column)
                table_rows = len(table_column_timetraces_df)
                minimum = table_column_timetraces_df[column_name].min()
                maximum = table_column_timetraces_df[column_name].max()
                average = table_column_timetraces_df[column_name].mean()
                RMS = None
                StandardDeviation = table_column_timetraces_df[column_name].std()
                column_statistics_array = [
                    1, self.schema, table_name, column_name, column_data_type, StatisticsClassId, start_time, end_time,
                    table_rows, minimum, maximum, average, RMS, StandardDeviation
                ]
                if table_rows > 0:
                    self.table_statistics.loc[len(self.table_statistics)] = column_statistics_array

    def get_timeline_statistics(self, cfg, statistics_df_columns):
        import datetime

        import pandas as pd
        from dateutil import parser
        self.StatisticsClass_df = None
        for table_index in range(0, len(cfg['sets'])):
            # for table_index in range(0, 1):
            self.table_statistics = pd.DataFrame(columns=statistics_df_columns)
            self.schema = cfg['schema']
            self.StatisticsClass_df = self.get_schema_statistics_class()
            table_name = cfg['sets'][table_index]['table']
            print("          ..... DB table statistics for table {1} of {2}: {0}".format(
                table_name, table_index, len(cfg['sets'])))
            try:
                entire_table_statistics_df = self.get_table_statistics_from_db(table_name)
                if len(entire_table_statistics_df) > 0:
                    table_column_arrays = cfg['sets'][table_index]['columns']
                    time_column = cfg['sets'][table_index]['time']
                    min_time = entire_table_statistics_df[entire_table_statistics_df.ColumnName ==
                                                          time_column].Minimum[0]
                    max_time = entire_table_statistics_df[entire_table_statistics_df.ColumnName ==
                                                          time_column].Maximum[0]
                    min_time = parser.parse(min_time).replace(microsecond=0, second=0, minute=0)
                    time_interval = datetime.timedelta(hours=cfg['time_interval_in_hours'])
                    max_time = parser.parse(max_time).replace(microsecond=0, second=0, minute=0) + time_interval
                    number_of_time_intervals = int((max_time - min_time) / time_interval)
                    # DEBUG
                    # number_of_time_intervals = 2
                    for time_interval_index in range(0, number_of_time_intervals):
                        # DEBUG
                        # for time_interval_index in range(0, 2):
                        start_time = min_time + time_interval_index * time_interval
                        end_time = start_time + time_interval
                        self.get_table_timetrace_statistics(table_name, start_time, end_time, time_column,
                                                            table_column_arrays)
                    if len(self.table_statistics) > 0:
                        self.save_to_db(self.table_statistics, 'TableStatistics')
                    else:
                        print("          ..... No table statistics for table {1} of {2}: {0}".format(
                            table_name, table_index, len(cfg['sets'])))
                else:
                    print("          ..... FAILURE DB table timeline statistics for table {1} of {2}: {0}".format(
                        table_name, table_index, len(cfg['sets'])))
            except Exception as e:
                print("Error is: {}".format(e))
                print("          ..... FAILURE DB table timeline statistics for table {1} of {2}: {0}".format(
                    table_name, table_index, len(cfg['sets'])))

    def get_table_statistics_from_db(self, table_name):
        import os
        schema_table = self.schema + '.' + 'TableStatistics'
        query_argument_array = [schema_table, table_name]
        filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                'get_table_statistics_by_table_name.sql')
        if os.path.isfile(filename):
            df = self.executeScriptsFromFile(filename, query_argument_array)
            return df
        else:
            print("Not a valid filename")

    def save_application_data(self, app_object):
        import pandas as pd
        save_sets = app_object.cfg.save_data['sets']
        for set_index in range(0, len(save_sets)):
            cfg_set = save_sets[set_index]
            df = getattr(app_object, cfg_set['attribute'], pd.DataFrame())
            if df is not None:
                self.save_to_db(df, cfg=cfg_set)

    def save_to_db(self, df, table_name=None, if_exists='append', index=False, cfg={}):
        schema = getattr(self, 'schema', None)
        schema = cfg.get('schema', schema)
        table_name = cfg.get('table_name', table_name)
        if_exists = cfg.get('if_exists', if_exists)
        index = cfg.get('index', False)
        index_label = cfg.get('index_label', None)

        if self.engine.has_table(table_name):
            if cfg.__contains__('pre_conditions') and cfg['pre_conditions']['flag']:
                for set_index in range(0, len(cfg['pre_conditions']['sets'])):
                    set_info = cfg['pre_conditions']['sets'][set_index]
                    query = set_info['sql']
                    arg_array = set_info['arg_array']
                    self.executeQueryWithParameters(query=query, arg_array=arg_array)

        chunk = 1000000
        if len(df.columns) > 0 and len(df) > 0 and len(df) < chunk:
            df.to_sql(name=table_name,
                      con=self.engine,
                      schema=schema,
                      if_exists=if_exists,
                      index=index,
                      index_label=index_label)
        else:
            import math
            number_of_chunks = math.ceil(len(df) / chunk)
            for chuck_index in range(0, number_of_chunks):
                start_index = chuck_index * chunk
                end_index = (chuck_index + 1) * chunk
                df_temp = df.iloc[start_index:end_index].copy()
                if chuck_index == 1:
                    df_temp.to_sql(name=table_name,
                                   con=self.engine,
                                   schema=schema,
                                   if_exists=if_exists,
                                   index=index,
                                   index_label=index_label)
                else:
                    df_temp.to_sql(name=table_name,
                                   con=self.engine,
                                   schema=schema,
                                   if_exists='append',
                                   index=index,
                                   index_label=index_label)

    def save_1_row_df_to_postgresql_db_using_primary_key(self, df, cfg={'table_name': None, 'primary_key': None}):
        if len(df) > 0:

            table_name = cfg.get('table_name', None)
            cfg.update({'table_name': table_name})

            primary_key = cfg.get('primary_key', None)
            cfg.update({'primary_key': primary_key})

            df_columns = list(df.columns)
            columns = ",".join(df_columns)
            cfg.update({'columns': columns})

            values = "({})".format(",".join(
                ["'" + item + "'" if item is not None else "NULL" for item in df.iloc[0].to_list()]))
            cfg.update({'values': values})

            df_columns.remove(primary_key)
            set_code_block = "{}".format(",".join(
                [item + " = excluded." + item if item is not None else "NULL" for item in df_columns]))
            cfg.update({'set_code_block': set_code_block})

            filename = os.path.join('data', self.server_type, 'sql', 'common.upsert_single_record.sql')
            sqlFileQuery = self.read_from_file(filename)
            sqlFileQuery = sqlFileQuery.format(custom_dict=cfg)
        if os.path.isfile(filename):
            self.executeNoDataQuery(sqlFileQuery, [])
        else:
            print("Not a valid filename")

    def get_table_column_statistics(self, table_name, column_name, column_data_type):
        import os
        schema_table = self.schema + '.' + table_name
        query_argument_array = [column_name, schema_table]
        filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                'table_statistics.sql')
        if os.path.isfile(filename):
            print("          ............................ Column : {0}".format(column_name))
            df_statistics = self.executeScriptsFromFile(filename, query_argument_array)
            return df_statistics
        else:
            print("Not a valid filename")

    def get_table_column_time_traces(self, table_name, column_name, column_data_type, start_time, end_time,
                                     time_column):
        import os
        schema_table = self.schema + '.' + table_name
        query_argument_array = [time_column, column_name, schema_table, start_time, end_time]
        if start_time is not None and end_time is not None:
            print("          .....getting timetrace for Column : {0} from {1} to {2}".format(
                column_name, start_time, end_time))
            filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                    'get_column_timetraces.sql')
            if os.path.isfile(filename):
                print("          ............................ Column : {0}".format(column_name))
                df_timetraces = self.executeScriptsFromFile(filename, query_argument_array)
                return df_timetraces
            else:
                print("Not a valid filename")

    def executeScriptsFromFile(self, filename, arg_array=[]):
        import pandas as pd
        sqlFile = self.read_from_file(filename)
        if len(arg_array) == 1:
            sqlFile = sqlFile.format(arg_array[0])
        elif len(arg_array) == 2:
            sqlFile = sqlFile.format(arg_array[0], arg_array[1])
        elif len(arg_array) == 3:
            sqlFile = sqlFile.format(arg_array[0], arg_array[1], arg_array[2])
        elif len(arg_array) == 4:
            sqlFile = sqlFile.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3])
        elif len(arg_array) == 5:
            sqlFile = sqlFile.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3], arg_array[4])
        elif len(arg_array) == 6:
            sqlFile = sqlFile.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3], arg_array[4],
                                     arg_array[5])
        elif len(arg_array) == 7:
            sqlFile = sqlFile.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3], arg_array[4],
                                     arg_array[5], arg_array[6])

        try:
            logging.debug("     .....Executing query: {}".format(sqlFile))
            df = pd.read_sql_query(sqlFile, self.conn, parse_dates=self.parse_dates)
            return df
        except Exception as e:
            logging.error("Command skipped: {}".format(e))

    def read_from_file(self, filename):
        fd = open(filename, 'r')
        sqlFileQuery = fd.read()
        fd.close()
        return sqlFileQuery

    def executeQueryWithParameters(self, query, arg_array=[]):
        import pandas as pd
        sql = query
        if len(arg_array) == 1:
            sql = sql.format(arg_array[0])
        elif len(arg_array) == 2:
            sql = sql.format(arg_array[0], arg_array[1])
        elif len(arg_array) == 3:
            sql = sql.format(arg_array[0], arg_array[1], arg_array[2])
        elif len(arg_array) == 4:
            sql = sql.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3])
        elif len(arg_array) == 5:
            sql = sql.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3], arg_array[4])

        try:
            logging.debug("     .....Executing query: {}".format(sql))
            df = pd.read_sql_query(sql, self.conn, parse_dates=self.parse_dates)
            return df
        except Exception as e:
            # self.conn.execute(query)
            print("Command skipped: ", e)

    def executeNoDataQuery_using_dict(self, query, dict):
        sql = query

    def executeNoDataQuery(self, query, arg_array=[]):
        from sqlalchemy.sql import text
        sql = query
        if len(arg_array) == 1:
            sql = sql.format(arg_array[0])
        elif len(arg_array) == 2:
            sql = sql.format(arg_array[0], arg_array[1])
        elif len(arg_array) == 3:
            sql = sql.format(arg_array[0], arg_array[1], arg_array[2])
        elif len(arg_array) == 4:
            sql = sql.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3])
        elif len(arg_array) == 5:
            sql = sql.format(arg_array[0], arg_array[1], arg_array[2], arg_array[3], arg_array[4])
        try:
            print("     .....Executing query: {}".format(sql))
            from sqlalchemy.orm import scoped_session, sessionmaker
            Session = scoped_session(sessionmaker(bind=self.engine))
            s = Session()
            s.execute(query)
            s.commit()
        except Exception as e:
            print("Command skipped: ", e)

    def get_input_data_from_db(self, cfg_input):
        import logging
        for set_index in range(0, len(cfg_input['sets'])):
            set_info = cfg_input['sets'][set_index]
            logging.info("Preparing input data for: '{}'".format(set_info['label']))
            self.schema = set_info.get('schema')
            if not set_info.__contains__('query'):
                table_name = set_info['table_name']
                df = self.get_input_data_set(table_name)
                if set_info['column_name'] is None:
                    df = df
            else:
                import os
                if set_info['query'].__contains__('filename'):
                    filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                            set_info['query']['filename'])
                    query_argument_array = set_info['query']['arguments']
                    df = self.executeScriptsFromFile(filename, query_argument_array)
                elif set_info['query'].__contains__('sql'):
                    query = set_info['query']['sql']
                    if set_info['query'].__contains__('arg_array'):
                        args_array = set_info['query']['arg_array']
                        df = self.executeQueryWithParameters(query, args_array)
                    else:
                        df = self.get_df_from_query(query)
                else:
                    import pandas as pd
                    df = pd.DataFrame()
            setattr(self, 'input_data_' + set_info['label'], df)
            logging.info("Preparing input data for: '{}'  ... COMPLETE".format(set_info['label']))

    def get_input_data_set(self, table_name):
        import os
        schema_table = self.schema + '.' + table_name
        query_argument_array = [schema_table]
        filename = os.path.join('data_manager\data', self.server_type, 'definitions\\functions',
                                'get_all_records_from_table.sql')
        if os.path.isfile(filename):
            df = self.executeScriptsFromFile(filename, query_argument_array)
            return df
        else:
            print("Not a valid filename")

    def perform_extreme_event_analysis(self, cfg_extreme_events):
        result_df_array = []
        for set_index in range(0, len(cfg_extreme_events['sets'])):
            cfg_setting = cfg_extreme_events['sets'][set_index]
            df = getattr(self, cfg_setting['df'])
            table_name = cfg_setting['table_name']
            temp_df = df[(df.TableName == table_name) & (df.StatisticsClassId == 2)].copy()
            statistics_df = self.format_table_statistics_df_for_extreme_events(cfg_setting, temp_df)
            result_df = statistics_df.sort_values(by=cfg_setting['column_name'] + '_' + cfg_setting['statistic'],
                                                  ascending=False).head(cfg_setting['number_of_events']).copy()
            for quantity_index in range(0, len(cfg_setting['related_quantities'])):
                cfg_related_quantity = cfg_setting['related_quantities'][quantity_index]
                table_name = cfg_related_quantity['table_name']
                temp_df = df[(df.TableName == table_name) & (df.StatisticsClassId == 2)].copy()
                temp_array = []
                quantity_column_name = cfg_related_quantity['column_name'] + '_' + cfg_related_quantity['statistic']
                if len(temp_df) > 0:
                    statistics_df = self.format_table_statistics_df_for_extreme_events(cfg_related_quantity, temp_df)
                    for row_index in range(0, len(result_df)):
                        try:
                            related_quantity = statistics_df[statistics_df.StartTime == result_df.StartTime.
                                                             iloc[row_index]][quantity_column_name].iloc[0]
                        except:
                            related_quantity = None
                        temp_array.append(related_quantity)
                else:
                    temp_array = [None] * len(result_df)
                result_df[quantity_column_name] = temp_array
            result_df_array.append(result_df)

        return result_df_array

    def prepare_input_statistics(self, data_set_cfg):
        df = getattr(self, data_set_cfg['df'])
        table_name = data_set_cfg['table_name']
        temp_df = df[(df.TableName == table_name) & (df.StatisticsClassId == 2)].copy()
        statistics_df = self.format_table_statistics_df(temp_df, data_set_cfg)
        return statistics_df

    def format_table_statistics_df(self, temp_df, data_set_cfg):
        import pandas as pd
        if data_set_cfg.__contains__('x') and data_set_cfg.__contains__('y'):
            statistics_df_column_array = data_set_cfg['x'] + data_set_cfg['y']
        else:
            import sys
            print("Data not defined for formatting table statstics")
            sys.exit()
        statistics_df = pd.DataFrame()
        for column_index in range(0, len(statistics_df_column_array)):
            column_name = statistics_df_column_array[column_index]
            if column_name == 'StartTime':
                import dateutil
                column_df = temp_df[temp_df.ColumnName == statistics_df_column_array[1]].sort_values(by=['StartTime'])
                StartTime_array = column_df.StartTime.to_list()
                StartTime_array = [dateutil.parser.parse(item) for item in StartTime_array]
                statistics_df[column_name] = StartTime_array
            else:
                column_df = temp_df[temp_df.ColumnName == statistics_df_column_array[column_index]].sort_values(
                    by=['StartTime'])
                column_df.fillna(0, inplace=True)
                column_array = column_df[data_set_cfg['statistic']].to_list()
                if (len(column_df) > 0) and (column_df.ColumnDataType.iloc[0] == 'float'):
                    column_array = [float(item) for item in column_array]
                statistics_df[column_name] = column_array

        return statistics_df

    def format_table_statistics_df_for_extreme_events(self, cfg_setting, temp_df):
        import pandas as pd
        statistics_df_column_array = ['StartTime', 'EndTime', cfg_setting['column_name']]
        statistics_df = pd.DataFrame()
        for column_index in range(0, len(statistics_df_column_array)):
            column_name = statistics_df_column_array[column_index]
            if column_name in ['StartTime', 'EndTime']:
                import dateutil
                column_df = temp_df[temp_df.ColumnName == statistics_df_column_array[2]].sort_values(by=['StartTime'])
                time_array = column_df[column_name].tolist()
                time_array = [dateutil.parser.parse(item) for item in time_array]
                statistics_df[column_name] = time_array
            else:
                column_df = temp_df[temp_df.ColumnName == statistics_df_column_array[column_index]].sort_values(
                    by=['StartTime'])
                column_df.fillna(0, inplace=True)
                column_array = column_df[cfg_setting['statistic']].tolist()
                if column_df.ColumnDataType.iloc[0] == 'float':
                    column_array = [float(item) for item in column_array]
                statistics_df[column_name + '_' + cfg_setting['statistic']] = column_array

        return statistics_df

    def get_create_table_sql_code_from_df(self, df, table_name='table_name', file_name='results/filename.txt'):
        from common.data import SaveData
        save_data = SaveData()
        import pandas as pd
        sql_table_code = pd.io.sql.get_schema(df.reset_index(), table_name, con=self.conn)
        save_data.write_ascii_file_from_text(sql_table_code, file_name)
        return sql_table_code
