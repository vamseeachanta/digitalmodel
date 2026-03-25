import datetime
import json
import logging
import os
import traceback

import pandas as pd
from sqlalchemy import text

from common.data import transform_df_datetime_to_str, transform_df_None_to_NULL
from common.database import get_db_connection, get_db_properties_for_service
from common.finance_components_analysis import FinanceAnalysis
from common.finance_components_get_data import FinanceGetData

script_working_dir = os.getcwd()
if 'tests' in script_working_dir:
    script_working_dir = os.path.join(script_working_dir, '..')


class FinanceComponents():

    def __init__(self, cfg):

        self.cfg = cfg

        db_status = True
        self.get_database_connection()
        if cfg is not None:
            data_exists_in_db_flag = self.get_data_exist_status_in_db()
            self.cfg.update({'database': {'status': db_status, 'data_exists': data_exists_in_db_flag}})

            self.fdata = FinanceGetData(self.cfg)
            self.fanalysis = FinanceAnalysis(self.cfg)
            self.status = {}

    def valid_ticker(self, ticker=None):
        return self.fdata.valid_ticker()

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

    def perform_analysis(self, data_dict):
        if not self.cfg['database']['data_exists']:
            self.fanalysis.assign_data(data_dict)
            self.fanalysis.run_long_term_analysis()

        self.status.update({'data': self.fdata.status, 'analysis': self.fanalysis.status})

    def get_database_connection(self):
        db_properties = get_db_properties_for_service(service='StockAnalysis')
        self.dbe, self.dbe_connection_status = get_db_connection(db_properties=db_properties)

    def get_data_exist_status_in_db(self):
        db_latest_data_exist_status = False
        if self.dbe_connection_status:
            try:
                db_latest_data_exist_status = self.get_data_status_from_db(cfg=None)
            except:
                logging.error("Error in database data status. Detailed error {0}".format(traceback.format_exc()))

        return db_latest_data_exist_status

    def get_stock_ticker_list_from_db(self):
        ticker_list_from_db = []
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'stocks.get_ticker_list.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename)
            ticker_list_from_db = df.ticker.to_list()
        except:
            logging.error("Could not get stock ticker data using query: {}".format(filename))

        return ticker_list_from_db

    def get_tickers_list_in_same_sector(self, sector):
        ticker_list_in_sector = []
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.get_ticker_in_sector.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename, [sector])
            ticker_list_in_sector = df.ticker.to_list()
        except:
            logging.error("Could not get stock ticker data using query: {}".format(filename))

        return ticker_list_in_sector

    def get_data_status_from_db(self, cfg=None):
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'stocks.UI.get_data.sql')

        try:
            ticker = self.cfg['stocks'][0]['ticker']
            df = self.dbe.executeScriptsFromFile(filename, [ticker])
        except:
            return False

        analysis_threshold_datetime = datetime.datetime.utcnow() + datetime.timedelta(days=-2)
        if df is not None and len(df) == 1 and (df.updated_time.iloc[0] is not None) and (df.updated_time.iloc[0] >
                                                                                          analysis_threshold_datetime):
            return True
        else:
            return False

    def get_data_for_UI(self, ticker=None):
        data_dict = {}
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'stocks.UI.get_data.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename, [ticker])
            data_dict = df.to_dict(orient='records')[0]
            data_dict['updated_time'] = data_dict['updated_time'].strftime('%m-%d-%Y')
        except:
            print("Error getting data from database for ticker {}".format(ticker))

        return data_dict

    def get_analysis_data_from_db(self, ticker):
        df = pd.DataFrame()
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'stocks.UI.get_data.sql')

        try:
            df = self.dbe.executeScriptsFromFile(filename, [ticker])
        except:
            print("Error getting data from database for ticker {}".format(ticker))

        return df

    def get_stock_analysis_UI_cfg(self, data_dict):

        ticker = data_dict.get('ticker', None)
        daily_data = data_dict.get('daily_data', None)
        ta = data_dict.get('ta', None)
        breakout_trends = data_dict.get('breakout_trends', None)
        status = data_dict.get('status', None)
        updated_time = data_dict.get('updated_time', None)
        sector = data_dict.get('sector', None)

        insider_info = data_dict.get('insider_info', None)
        insider_by_relation = insider_info.get('insider_by_relation', None) if insider_info is not None else None
        insider_by_timeline = insider_info.get('insider_by_timeline', None) if insider_info is not None else None

        ticker_list_in_sector = []
        # ticker_list_in_sector = self.get_tickers_list_in_same_sector(sector=sector)
        # ticker_list_in_sector.remove(ticker)

        option_analysis = data_dict.get('option_analysis', None)
        call_analysis = option_analysis.get('call_analysis', None) if option_analysis is not None else None
        cfg = {
            'ticker': ticker,
            'updated_time': updated_time,
            'sector': sector,
            'daily_data': daily_data,
            'ta': ta,
            'breakout_trends': json.dumps(breakout_trends),
            'insider_by_relation': json.dumps(insider_by_relation),
            'insider_by_timeline': json.dumps(insider_by_timeline),
            'call_analysis': call_analysis,
            'status': status,
            'ticker_list_in_sector': ticker_list_in_sector
        }

        return cfg

    def get_stock_analysis_plot_cfg(self, data_dict):

        ticker = data_dict.get('ticker', None)
        daily_data = data_dict.get('daily_data', None)
        ta = data_dict.get('ta', None)
        breakout_trends = data_dict.get('breakout_trends', None)
        status = data_dict.get('status', None)
        updated_time = data_dict.get('updated_time', None)

        insider_info = data_dict.get('insider_info', None)
        insider_by_relation = insider_info.get('insider_by_relation', None) if insider_info is not None else None
        insider_by_timeline = insider_info.get('insider_by_timeline', None) if insider_info is not None else None
        insider_summary = insider_info.get('insider_summary', None) if insider_info is not None else None
        insider_df_buy = insider_summary.get('insider_df_buy', None) if insider_summary is not None else None
        insider_df_sell = insider_summary.get('insider_df_sell', None) if insider_summary is not None else None

        option_analysis = data_dict.get('option_analysis', None)
        call_analysis = option_analysis.get('call_analysis', None) if option_analysis is not None else None
        institution_data = data_dict.get('institution_data', None)
        institutional_holders = institution_data.get('institutional_holders', None)

        df_daily_data = pd.DataFrame(daily_data)
        df_daily_data.Date = pd.to_datetime(df_daily_data.Date, infer_datetime_format=True)
        df_ta = pd.DataFrame(ta)
        df_insider_buy = pd.DataFrame(insider_df_buy)
        if len(df_insider_buy) > 0:
            df_insider_buy.Date = pd.to_datetime(df_insider_buy.Date, infer_datetime_format=True)
        df_insider_sell = pd.DataFrame(insider_df_sell)
        if len(df_insider_sell) > 0:
            df_insider_sell.Date = pd.to_datetime(df_insider_sell.Date, infer_datetime_format=True)
        df_insider_by_relation = pd.DataFrame(insider_by_relation)
        df_insider_by_timeline = pd.DataFrame(insider_by_timeline)
        df_call_analysis = pd.DataFrame(call_analysis)
        df_institutional_holders = pd.DataFrame(institutional_holders)

        plot_cfg = {
            'df_daily_data': df_daily_data,
            'ticker': ticker,
            'df_ta': df_ta,
            'df_insider_buy': df_insider_buy,
            'df_insider_sell': df_insider_sell,
            'df_insider_by_relation': df_insider_by_relation,
            'df_insider_by_timeline': df_insider_by_timeline,
            'df_call_analysis': df_call_analysis,
            'df_institutional_holders': df_institutional_holders
        }

        return plot_cfg

    def get_data(self, cfg=None):
        if cfg is not None:
            self.__init__(cfg)

        self.fdata.get_data(self.cfg)

    def get_result_df_for_db(self):
        columns = [
            'ticker', 'updated_time', 'sector', 'summary', 'daily_data', 'breakout_trends', 'insider_info',
            'option_analysis', 'status', 'ta'
        ]
        result_df = pd.DataFrame(columns=columns)

        ticker = self.cfg['stocks'][0]['ticker']
        updated_time = datetime.datetime.utcnow().replace(second=0, microsecond=0)

        summary = None

        company_info = getattr(self.fdata, 'company_info')
        sector = company_info['info'].get('sector', 'unknown')
        breakout_trends = company_info.get('breakout_trend_indicators', None)

        daily_data = self.get_daily_data_for_save_to_db()

        insider_info = json.dumps(company_info.get('insider_info', None))
        option_analysis = json.dumps(company_info.get('optionanalysis', None))
        status = json.dumps(self.status)

        ta = None

        df_row = [
            ticker, updated_time, sector, summary, daily_data, breakout_trends, insider_info, option_analysis, status,
            ta
        ]
        result_df.loc[len(result_df)] = df_row

        return result_df

    def get_daily_data_for_save_to_db(self):
        daily_data_raw = self.fdata.stock_data_array[0][['Close', 'Volume']]
        daily_data_raw['Date'] = daily_data_raw.index.to_list()
        daily_data_raw = transform_df_datetime_to_str(daily_data_raw, date_format="%Y-%m-%d")
        daily_data_raw.Close = daily_data_raw.Close.apply(lambda x: round(x, 4))
        daily_data = json.dumps(daily_data_raw.to_dict(orient='records'))
        return daily_data

    def save_analysis_column_to_db(self, cfg_save):
        column = cfg_save.get('column', None)
        data = cfg_save.get('data', None)
        status = cfg_save.get('status', None)
        ticker = cfg_save.get('ticker', None)
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.analysis.update_column.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename, arg_array=[column, data, status, ticker])
        except:
            logging.error("Could not get stock ticker data using query: {}".format(filename))

    def update_sec_info_in_db_and_save_to_repo(self):
        sec_ticker_data = self.fdata.get_sec_ticker_data()
        item_list = sec_ticker_data.keys()

        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.update_high_level_info.sql')
        for item in item_list:
            try:
                df = self.dbe.executeScriptsFromFile(
                    filename, arg_array=[item, item,
                                         json.dumps(sec_ticker_data.get(item), ensure_ascii=False)])
            except:
                filename_json = os.path.join(script_working_dir, 'services', 'StockAnalysis', 'static',
                                             'StockAnalysis', 'data', item + '.json')
                with open(filename_json, 'w') as file_json:
                    json.dump(sec_ticker_data.get(item), file_json)
                logging.error("Could not update Label : {0}. Saved data to file system. Detailed error {1}".format(
                    filename, traceback.format_exc()))

    def save_institution_data_sec(self, result):
        infoTable = result['infoTable']
        reportCalendarOrQuarter = result['reportCalendarOrQuarter']
        signatureDate = result['signatureDate']
        cik = result['cik']
        institution = result['institution']
        summary = result['summary']
        status = result['status']

        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.save_institution_data.sql')
        try:
            df = self.dbe.executeScriptsFromFile(
                filename, arg_array=[cik, institution, summary, reportCalendarOrQuarter, signatureDate, infoTable])
        except:
            logging.error("Could not save institution data using query: {}".format(filename))

    def save_institution_data_yf(self, result):
        infoTable = result['infoTable']
        reportCalendarOrQuarter = result['reportCalendarOrQuarter']
        signatureDate = result['signatureDate']
        cik = result['cik']
        name = result['name']
        summary = result['summary']
        status = result['status']

        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.save_institution_data.sql')
        try:
            df = self.dbe.executeScriptsFromFile(
                filename, arg_array=[cik, name, summary, reportCalendarOrQuarter, signatureDate, infoTable])
        except:
            logging.error("Could not save institution data using query: {}".format(filename))

    def save_keys(self):
        dtf_keys_df = self.get_keys_from_db()
        dtf_keys_tickers = dtf_keys_df.ticker.to_list()
        dtf_keys_ciks = dtf_keys_df.cik.to_list()

        filename_json = os.path.join(script_working_dir, 'services', 'StockAnalysis', 'static', 'StockAnalysis',
                                     'data', 'company_tickers_exchange.json')

        with open(filename_json) as json_file_data:
            company_tickers_exchange = json.load(json_file_data)

        company_tickers_exchange_df = pd.DataFrame(company_tickers_exchange['data'],
                                                   columns=company_tickers_exchange['fields'])

        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'stocks.save_keys.sql')

        for df_row in range(0, len(company_tickers_exchange_df)):
            ticker = company_tickers_exchange_df['ticker'].iloc[df_row]
            cik = company_tickers_exchange_df['cik'].iloc[df_row]
            cik = str(cik).zfill(10)
            if ticker not in dtf_keys_tickers:
                status = {}
                name = company_tickers_exchange_df['name'].iloc[df_row]
                name = name.replace("'", "''")
                exchange = company_tickers_exchange_df['exchange'].iloc[df_row]
                sector = "NULL"

                # valid_sec_forms = self.fdata.sec_form.get_valid_forms(ticker)
                valid_sec_forms = []
                status.update({'valid_sec_forms': valid_sec_forms})
                cusip = "NULL"
                cusip_status = {}
                # cusip, cusip_status = self.fdata.sec_form.get_cusip(ticker, valid_sec_forms)
                status.update(cusip_status)

                status = json.dumps(status)
                df = self.dbe.executeScriptsFromFile(filename,
                                                     arg_array=[cik, ticker, name, exchange, sector, cusip, status])
                logging.debug(f"Keys updated for ticker {ticker}, cik: {cik}".format({'ticker': ticker, 'cik': cik}))

    def get_sec_valid_forms(self, ticker):
        self.fdata.sec_form.get_valid_forms(ticker)

    def save_result_to_db_using_primary_key(self):
        result_df = self.get_result_df_for_db()
        cfg = {'table_name': 'stocks.analysis', 'primary_key': 'ticker'}
        df = transform_df_None_to_NULL(result_df)
        df = transform_df_datetime_to_str(result_df)
        self.dbe.save_1_row_df_to_postgresql_db_using_primary_key(df, cfg=cfg)

    def get_keys_from_db(self):
        df = pd.DataFrame()
        sql = 'SELECT * FROM stocks.keys'

        try:
            df = self.dbe.executeQueryWithParameters(sql)
        except:
            print("Error getting keys from database")

        return df

    def get_institution_data_from_db(self, ticker):
        df = pd.DataFrame()

        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.get_institution_data.sql')

        if ticker is not None:
            try:
                df = self.dbe.executeScriptsFromFile(filename, [ticker])
            except:
                print("Error getting data from database for ticker {}".format(ticker))

        return df

    def save_institution_data_to_db(self, cfg_save):
        column = cfg_save.get('column', None)
        data = cfg_save.get('data', None)
        status = cfg_save.get('status', None)
        ticker = cfg_save.get('ticker', None)
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.institution.update_column.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename, arg_array=[column, data, status, ticker])
        except:
            logging.error("Could not get stock ticker data using query: {}".format(filename))

    def get_insider_data_from_db(self, ticker):
        df = pd.DataFrame()

        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'stocks.get_insider_data.sql')

        if ticker is not None:
            try:
                df = self.dbe.executeScriptsFromFile(filename, [ticker])
            except:
                print("Error getting data from database for ticker {}".format(ticker))

        return df

    def save_insider_data_to_db(self, cfg_save):
        column = cfg_save.get('column', None)
        data = cfg_save.get('data', None)
        status = cfg_save.get('status', None)
        ticker = cfg_save.get('ticker', None)
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.insider.update_column.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename, arg_array=[column, data, status, ticker])
        except:
            logging.error("Could not save data using query: {}".format(filename))

    def get_EOD_data_from_db(self, ticker):
        df = pd.DataFrame()

        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql', 'stocks.EOD.get_data.sql')

        if ticker is not None:
            try:
                df = self.dbe.executeScriptsFromFile(filename, [ticker])
            except:
                print("Error getting data from database for ticker {}".format(ticker))

        return df

    def save_EOD_data_to_db(self, cfg_save):
        column = cfg_save.get('column', None)
        data = cfg_save.get('data', None)
        status = cfg_save.get('status', None)
        ticker = cfg_save.get('ticker', None)
        filename = os.path.join(script_working_dir, 'data', self.dbe.server_type, 'sql',
                                'stocks.EOD.update_column.sql')
        try:
            df = self.dbe.executeScriptsFromFile(filename, arg_array=[column, data, status, ticker])
        except:
            logging.error("Could not save data using query: {}".format(filename))
