import datetime
import json
import logging
import os
import traceback

import pandas as pd

from common.data import transform_df_datetime_to_str, transform_df_None_to_NULL
from common.database import get_db_connection, get_db_properties_for_service

script_working_dir = os.getcwd()
if 'tests' in script_working_dir:
    script_working_dir = os.path.join(script_working_dir, '..')

data_from_db = [{
    'label': 'field_summary',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.field_summary.sql')
}, {
    'label': 'gom_fields',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.get_gom_fields.sql')
}, {
    'label': 'well_locations',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.well_locations.sql')
}, {
    'label': 'well_location_and_production',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.well_location_and_production.sql')
}, {
    'label': 'well_summary_high_level',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.well_summary_high_level.sql')
}, {
    'label': 'production_by_well',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.production_by_well.sql')
}, {
    'label': 'well_construction',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.well_construction.sql')
}, {
    'label': 'well_paths',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.well_paths.sql')
}, {
    'label': 'well_monthly_production',
    'filename': os.path.join(script_working_dir, 'data', 'postgresql', 'sql', 'bsee.well_monthly_production.sql')
}]


class GoMFieldsComponents():

    def __init__(self, cfg):
        self.cfg = cfg
        self.get_database_connection()

    def get_data_dict(self):
        df_data_array = getattr(self.fdata, 'stock_data_array', [None])
        df_data = df_data_array[0]
        company_info = getattr(self.fdata, 'company_info', None)
        insider_df = getattr(self.fdata, 'insider_df', None)
        option_data = getattr(self.fdata, 'option_data', None)

        data_dict = {
            'df_data': df_data,
            'company_info': company_info,
            'insider_df': insider_df,
            'option_data': option_data,
        }

        return data_dict

    def get_database_connection(self):
        db_properties = get_db_properties_for_service(service='GoMFields')
        self.dbe, self.dbe_connection_status = get_db_connection(db_properties=db_properties)

    def get_gom_field_list_from_db(self):
        gom_field_list_from_db = []
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'bsee.get_gom_fields.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename)
            gom_field_list_from_db = df['Field NickName'].to_list()
        except:
            logging.error("Could not get GoM field list using query: {}".format(filename))

        return gom_field_list_from_db

    def get_plot_cfg(self, data_dict):

        ticker = data_dict.get('ticker', None)
        updated_time = data_dict.get('updated_time', None)
        cfg = {
            'ticker': ticker,
            'updated_time': updated_time,
        }

        return cfg

    def get_gom_field_plot_cfg(self, data_dict):

        ticker = data_dict.get('ticker', None)
        daily_data = data_dict.get('daily_data', None)

        plot_cfg = {
            'df_daily_data': daily_data,
        }

        return plot_cfg

    def get_data_for_UI(self, gom_block):
        data_df, data_dict = self.get_data_from_db(gom_block)
        # Perform data transformations as required

        return data_df, data_dict

    def get_data_from_db(self, gom_block):
        data_df = {}
        data_dict = {}
        for cfg_data in data_from_db:
            df = self.dbe.executeScriptsFromFile(cfg_data['filename'], arg_array=[gom_block])
            df = transform_df_datetime_to_str(df, date_format="%Y-%m-%d")
            df = transform_df_None_to_NULL(df)
            data_df.update({cfg_data['label']: df})
            data_dict.update({cfg_data['label']: df.to_dict(orient='records')})

        return data_df, data_dict
