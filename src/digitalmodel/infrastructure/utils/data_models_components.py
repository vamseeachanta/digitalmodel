import logging


class DataModelsComponents():

    def __init__(self, cfg):
        self.cfg = cfg

    def drop_dependent_tables(self):
        from digitalmodel.infrastructure.utils.database import Database

        db_properties = self.cfg.db
        self.dbe = Database(db_properties)
        self.dbe.set_up_db_connection(db_properties)

        for table in self.cfg.db_tables['drop_first']:
            drop_query = 'DROP TABLE IF EXISTS {}'.format(table)
            self.dbe.executeQueryWithParameters(drop_query)

    def create_db_tables(self):
        from digitalmodel.infrastructure.utils.database import Database

        db_properties = self.cfg.db
        self.dbe = Database(db_properties)
        self.dbe.set_up_db_connection(db_properties)

        for set_index in range(0, len(self.cfg.db_tables['sets'])):
            set_info = self.cfg.db_tables['sets'][set_index]
            if_exists = set_info['if_exists']
            if if_exists == 'drop':
                drop_query = 'DROP TABLE IF EXISTS {}'.format(set_info['table'])
                self.dbe.executeQueryWithParameters(drop_query)
            self.dbe.executeScriptsFromFile(set_info['io'])

    def get_data_and_save_to_target(self):
        result = {}
        if self.cfg.input_data['source'] == 'accdb':
            for set_index in range(0, len(self.cfg.input_data['sets'])):
                db_properties = {'server_type': 'accdb', 'database': self.cfg.input_data['sets'][0]['io']}

                from digitalmodel.infrastructure.utils.database import Database
                self.dbe = Database(db_properties)
                self.dbe.set_up_db_connection(db_properties)
        elif self.cfg.input_data['source'] == 'xlsx':
            for set_index in range(0, len(self.cfg.input_data['sets'])):
                cfg_xlsx = self.cfg.input_data['sets'][set_index]
                result = self.read_from_xlsx(cfg_xlsx)
                self.save_data(cfg_xlsx, result)
        elif self.cfg.input_data['source'] == 'csv':
            self.process_csv_files()
        elif self.cfg.input_data['source'] == 'zip':
            self.process_zip_files()
        elif self.cfg.input_data['source'] == 'url':
            self.process_url_files()
        elif self.cfg.input_data['source'] == 'db':
            self.process_db_source()
        else:
            import sys
            logging.info("No input data sources specified")
            sys.exit()

    def process_csv_files(self):
        import os

        import pandas as pd
        for set_index in range(0, len(self.cfg.input_data['sets'])):
            cfg_csv = self.cfg.input_data['sets'][set_index]

            file_name = cfg_csv['io']
            label = cfg_csv.get('label', None)
            label = os.path.basename(cfg_csv['io']).split('.')[0] if label is None else label
            names = cfg_csv.get('df_columns', None)
            header = cfg_csv.get('header', 'infer')
            logging.info("Processing file: {}".format(file_name))

            try:
                df = pd.read_csv(filepath_or_buffer=file_name, header=header, names=names, encoding='unicode_escape')
                result = {label: df}
                if len(df) >= 1:
                    self.save_data(cfg_csv, result)
                    logging.info("Successfully saved file: {}".format(file_name))
                else:
                    logging.info("No data in DF to save: {}".format(file_name))
            except Exception:
                import sys
                print("Could not process a file: {}".format(file_name))
                print(sys.exc_info())
                logging.error("Could not process a file: {}".format(file_name), exc_info=True)

    def process_db_source(self, input_data=None):
        from digitalmodel.infrastructure.utils.database import Database
        if input_data is None:
            input_data = self.cfg.input_data

        for set_index in range(0, len(input_data['sets'])):
            try:
                cfg_db = input_data['sets'][set_index]
                source_db_properties = cfg_db['db_source']
                source_dbe = Database(source_db_properties)
                source_dbe.set_up_db_connection(source_db_properties)
                query = cfg_db['table']['query']
                dict_key = cfg_db['table']['target']
                df = source_dbe.get_df_from_query(query)
                result = {dict_key: df}
                if len(df) >= 1:
                    self.save_data(cfg_db, result)
            except Exception:
                import sys
                print("Could not process a file: {}".format(cfg_db))
                print(sys.exc_info())
                logging.error("Could not process a file: {}".format(cfg_db), exc_info=True)

    def process_zip_files(self, input_data=None):
        import os
        import zipfile

        import pandas as pd
        if input_data is None:
            input_data = self.cfg.input_data

        for set_index in range(0, len(input_data['sets'])):
            cfg_zip = input_data['sets'][set_index]
            label_array = cfg_zip.get('label', None)
            columns_array = cfg_zip.get('df_columns', None)
            archive = zipfile.ZipFile(cfg_zip['io'], 'r')
            for file_index in range(0, len(archive.filelist)):
                file_name = archive.filelist[file_index].filename
                if file_name[-1] not in ['/']:
                    logging.info("Processing file: {} in zip file: {}".format(file_name, cfg_zip['io']))
                    filecontents = archive.open(file_name, 'r')
                    label = os.path.basename(file_name).split(
                        '.')[0] if label_array is None else label_array[file_index]
                    names = None if columns_array is None else columns_array[file_index]
                    header = cfg_zip.get('header', 'infer')
                    if header is None:
                        header = 'infer'
                    encoding = cfg_zip.get('encoding', 'unicode_escape')
                    try:
                        df = pd.read_csv(filepath_or_buffer=filecontents,
                                         encoding=encoding,
                                         names=names,
                                         header=header)
                        result = {label: df}
                        if len(df) >= 1:
                            self.save_data(cfg_zip, result)
                            logging.info("Successfully saved file: {} in zip file: {}".format(
                                file_name, cfg_zip['io']))
                        else:
                            logging.info("No data in DF to save: {} in zip file: {}".format(file_name, cfg_zip['io']))

                    except Exception:
                        import sys
                        print("Could not process a file: {}".format(file_name))
                        print(sys.exc_info())
                        logging.error("Could not process a file: {}".format(file_name), exc_info=True)
                else:
                    logging.info("Skipping file: {} in zip file: {}".format(file_name, cfg_zip['io']))

    def process_url_files(self):
        import os

        import pandas as pd

        from digitalmodel.infrastructure.utils.data import GetData
        get_data = GetData()
        for set_index in range(0, len(self.cfg.input_data['sets'])):
            cfg_url = self.cfg.input_data['sets'][set_index].copy()
            cfg_get_data = {}
            cfg_get_data['url'] = cfg_url['io']
            cfg_get_data['download_to'] = self.cfg.input_data['download_to']
            logging.info("Processing file: {}".format(cfg_url['io']))
            download_cfg = get_data.download_file_from_url(cfg_get_data)
            if download_cfg['result']:
                cfg_url['io'] = download_cfg['filename']
                input_data = {'sets': [cfg_url]}
                self.process_zip_files(input_data)
            else:
                print("Could not process a file: {}".format(cfg_url))
                logging.error("Could not process a file: {}".format(cfg_url), exc_info=True)

    def save_data(self, cfg, data):
        if cfg.__contains__('output'):
            if cfg['output']['target'] == 'db':
                db_properties = self.cfg.db
                if_exists = cfg['output']['if_exists']
                from digitalmodel.infrastructure.utils.database import Database
                self.dbe = Database(db_properties)
                self.dbe.set_up_db_connection(db_properties)
                keys = list(data.keys())
                for key in keys:
                    df = data[key]
                    try:
                        self.dbe.save_to_db(df, table_name=key, if_exists=if_exists)
                    except Exception:
                        logging.error("Could not save dataframe to DB: {}".format(cfg), exc_info=True)

    def run_example(self):
        pass
        # TBA

    def read_from_xlsx(self, cfg_xlsx):
        from digitalmodel.infrastructure.utils.data import ReadData
        readdata = ReadData()
        result = readdata.from_xlsx(cfg_xlsx)
        return result
