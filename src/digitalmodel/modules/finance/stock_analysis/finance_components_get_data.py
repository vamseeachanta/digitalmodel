import datetime
import json

import pandas as pd
import yfinance as yf
from finvizfinance.quote import finvizfinance
from yahoo_fin.stock_info import tickers_dow, tickers_nasdaq, tickers_sp500

from common.finance_components_get_SEC_data import SECDataForm, SECDataTicker

sec_ticker = SECDataTicker()


class FinanceGetData():

    def __init__(self, cfg):
        self.cfg = cfg
        self.status = {'insider': {}}
        self.days_rolling_array = [20, 100, 50, 150, 200]
        self.company_info = {'DataQuality': []}
        self.option_data = {}

        self.sec_form = SECDataForm()
        self.sec_form4 = pd.DataFrame()
        self.sec_data_duration_years = 2

        self.inside_trader_df = pd.DataFrame()
        self.ratings_df = pd.DataFrame()
        self.option_data = {}

    def valid_ticker(self, ticker=None):
        return True

    def get_data(self, cfg=None):
        if cfg is not None:
            self.cfg = cfg

        if not self.cfg['database']['data_exists']:
            self.get_stock_price_data()
            self.get_stats()
            self.insider_df = self.get_insider_information()
            self.get_ratings()
            self.get_options_data()

    def get_stock_price_data(self):
        self.status.update({'price': True})
        if self.cfg['source'] == 'tiingo':
            self.get_data_from_tiingo()
        elif self.cfg['source'] == 'yfinance':
            self.get_data_from_yfinance()

        else:
            self.status.update({'price': False})
            raise ("No valid data source specified")

    def get_insider_information_from_finviz(self, stock_ticker):
        self.status['insider']['finviz'] = {}
        try:
            self.fv_ticker = finvizfinance(stock_ticker)
            insider_info_finviz = self.fv_ticker.TickerInsideTrader()
            current_year = datetime.datetime.now().year
            insider_info_finviz['SEC Form 4'] = insider_info_finviz['SEC Form 4'].apply(
                lambda x: str(current_year) + ' ' + x)
            insider_info_finviz['SEC Form 4'] = pd.to_datetime(insider_info_finviz['SEC Form 4'],
                                                               infer_datetime_format=True)
            insider_info_finviz['SEC Form 4'] = insider_info_finviz['SEC Form 4'].apply(
                lambda x: x if x < datetime.datetime.now() else x + datetime.timedelta(days=-365))
            insider_info_finviz['Date'] = insider_info_finviz['SEC Form 4']
            start_date = insider_info_finviz['Date'].min().strftime("%m-%d-%Y")
            end_date = insider_info_finviz['Date'].max().strftime("%m-%d-%Y")
            updated_time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')
            self.status['insider']['finviz'].update({
                'status': True,
                'start': start_date,
                'end': end_date,
                'updated_time': updated_time
            })
            insider_info_finviz.drop(['SEC Form 4'], axis=1, errors='ignore', inplace=True)
        except:
            insider_info_finviz = pd.DataFrame()
            self.status['insider']['finviz'].update({'status': False})

        return insider_info_finviz

    def get_ratings(self):
        self.status.update({'ratings': True})
        try:
            self.ratings_df = self.fv_ticker.TickerOuterRatings()
        except:
            self.status.update({'ratings': False})
            self.ratings_df = pd.DataFrame()

    def get_options_data(self):
        self.status.update({'options': True})
        try:
            option_dates = list(self.yf_ticker.options)
            for date in option_dates:
                option_chain = self.get_option_data_by_date(date)
                self.option_data.update({date: option_chain})
        except Exception as ex:
            self.status.update({'options': False})
            self.option_data = {}

    def get_option_data_by_date(self, date):
        option_chain = self.yf_ticker.option_chain(date)
        option_chain_dict = {'calls': option_chain.calls, 'puts': option_chain.puts}
        return option_chain_dict

    def get_data_from_tiingo(self):
        import pandas_datareader as pdr
        api_key = '512e3063ad18b5116a83cf7ce7d852af4181917c'
        self.stock_data_array = []
        for stock_info in self.cfg.stocks:
            stock_ticker = stock_info['ticker']
            self.company_info['stock_ticker'] = stock_ticker
            df = pdr.get_data_tiingo(stock_ticker, api_key=api_key)
            df['date'] = [index_value[1] for index_value in df.index]
            for days_rolling in self.days_rolling_array:
                df[str(days_rolling) + '_day_rolling'] = df.close.rolling(window=days_rolling).mean()
            self.stock_data_array.append(df)

    def get_screened_stocks(self):
        from finvizfinance.screener.overview import Overview
        finviz_overview = Overview()
        filters_dict = {'Exchange': 'AMEX', 'Sector': 'Basic Materials'}
        finviz_overview.set_filter(filters_dict=filters_dict)
        df = finviz_overview.ScreenerView()
        df.head()

    def get_data_from_yfinance(self, ticker=None):
        self.stock_data_array = []
        period = self.cfg.get('period', '5y')
        for stock_info in self.cfg.stocks:
            stock_ticker = stock_info['ticker']
            self.yf_ticker = yf.Ticker(str(stock_ticker))
            self.company_info['stock_ticker'] = stock_ticker
            self.company_info['info'] = self.yf_ticker.info
            df = self.yf_ticker.history(period=period)
            for days_rolling in self.days_rolling_array:
                df[str(days_rolling) + '_day_rolling'] = df.Close.rolling(window=days_rolling).mean()
            self.stock_data_array.append(df)

    def get_EOD_data_from_yfinance(self, ticker):
        period = self.cfg.get('period', '5y')
        yf_ticker = yf.Ticker(str(ticker))
        company_info = yf_ticker.info
        df = yf_ticker.history(period=period)

        return df

    def get_EOD_with_rolling_averages(self, df):
        for days_rolling in self.days_rolling_array:
            df[str(days_rolling) + '_day_rolling'] = df.Close.rolling(window=days_rolling).mean()

        return df

    def get_stats(self):
        df = self.stock_data_array[0]
        import datetime
        if self.cfg['source'] == 'tiingo':
            num_years = ((df.date.max() - df.date.min()).days / 365.25).__round__(1)
            start_time = df['date'].iloc[-1] + datetime.timedelta(days=-365)
            df_temp = df[df['date'] > start_time].copy()
            fiftyTwoWeekLow = df_temp.close.max()
            fiftyTwoWeekHigh = df_temp.close.min()
        elif self.cfg['source'] == 'yfinance':
            num_years = ((df.index.max() - df.index.min()).days / 365.25).__round__(1)
            fiftyTwoWeekLow = self.yf_ticker.info['fiftyTwoWeekLow']
            fiftyTwoWeekHigh = self.yf_ticker.info['fiftyTwoWeekHigh']
        self.company_info['DataQuality'].append('Stock data duration used : {} yrs'.format(num_years))

        self.company_info.update({'fiftyTwoWeekLow': fiftyTwoWeekLow})
        self.company_info.update({'fiftyTwoWeekHigh': fiftyTwoWeekHigh})

    def get_data_from_morningstar(self):
        data_source = 'morningstar'

        import datetime

        import pandas_datareader.data as web

        start = datetime.datetime(2010, 1, 1)
        end = datetime.datetime(2013, 1, 27)
        f = web.DataReader('OXY', data_source, start, end)

        print(web.DataReader('OXY', data_source, start, end))

    def get_data_from_iex(self):
        days_rolling_array = self.days_rolling_array
        import os
        os.environ["IEX_API_KEY"] = self.cfg.default['data_sources']['iex']['api_key']
        # {'tiingo': {'flag': None}}
        from datetime import datetime, timedelta

        import pandas_datareader.data as web
        end_date = datetime.now()
        start_date = end_date - timedelta(days=5479)

        self.stock_data_array = []
        for stock_info in self.cfg.stocks:
            stock_ticker = stock_info['ticker']
            df = web.DataReader(stock_ticker, 'iex', start_date, end_date)
            df['date'] = [index_value for index_value in df.index]
            for days_rolling in days_rolling_array:
                df[str(days_rolling) + '_day_rolling'] = df.close.rolling(window=days_rolling).mean()
            self.stock_data_array.append(df)

    def get_sec_data(self, ticker):
        sec_form4 = self.sec_form.get_sec_form_data(ticker, cfg_sec={})
        sec_data = {'sec_form4': sec_form4}
        return sec_data

    def get_tickers_dow(self):
        tickers = tickers_dow()
        return tickers

    def get_tickers_nasdaq(self):
        tickers = tickers_nasdaq()
        return tickers

    def get_tickers_sp500(self):
        tickers = tickers_sp500()
        return tickers

    def get_insider_information(self, ticker):
        insider_info_finviz = self.get_insider_information_from_finviz()
        sec_data = self.get_sec_data(ticker)
        sec_form4 = sec_data.get('sec_form4')
        if len(sec_form4) > 0:
            insider_df = sec_form4
            insider_df = self.insider_data_clean_and_add_share_ratio(insider_df)
        elif len(insider_info_finviz) > 0:
            insider_df = insider_info_finviz
            insider_df = self.insider_data_clean_and_add_share_ratio(insider_df)
        else:
            self.status.update({'insider_info': False})
            insider_df = pd.DataFrame()

        return insider_df

    def insider_data_clean_and_add_share_ratio(self, insider_df):
        insider_df.drop(['SEC Form 4', 'Insider_id'], axis=1, inplace=True, errors='ignore')
        insider_df['start_shares'] = 0
        insider_df['share_holding_ratio'] = 0
        insider_df['average_cost'] = 0

        for row_index in range(0, len(insider_df)):
            df_temp = insider_df.iloc[[row_index]]
            start_shares, share_holding_ratio, average_cost = self.sec_data_get_economic_analysis_for_subset_df(
                df_temp)
            insider_df['start_shares'].iloc[row_index] = start_shares
            insider_df['share_holding_ratio'].iloc[row_index] = share_holding_ratio
            insider_df['average_cost'].iloc[row_index] = average_cost

        return insider_df

    def sec_data_get_economic_analysis_for_subset_df(self, df):
        end_shares = df['#Shares Total'].iloc[0]
        noShares = df['#Shares'].sum()
        if df['Transaction'].iloc[-1] in ['Sell', 'Sale']:
            start_shares = df['#Shares Total'].iloc[-1] + noShares
        else:
            start_shares = df['#Shares Total'].iloc[-1] - noShares

        if start_shares > 0:
            share_holding_ratio = (end_shares / start_shares).__round__(2)
        else:
            share_holding_ratio = 1.01

        try:
            average_cost = (df['#Shares'] * df['Cost']).sum() / df['#Shares'].sum()
            average_cost = round(average_cost, 3)
        except:
            average_cost = None

        return start_shares, share_holding_ratio, average_cost

    def get_sec_ticker_data(self):

        company_tickers_json = sec_ticker.get_company_tickers()
        company_tickers = json.loads(company_tickers_json)

        company_tickers_exchange_json = sec_ticker.get_company_tickers_exchange()
        company_tickers_exchange = json.loads(company_tickers_exchange_json)

        company_tickers_mf_json = sec_ticker.get_company_tickers_mf()
        company_tickers_mf = json.loads(company_tickers_mf_json)

        sec_ticker_data = {
            'company_tickers': company_tickers,
            'company_tickers_exchange': company_tickers_exchange,
            'company_tickers_mf': company_tickers_mf
        }

        return sec_ticker_data

    def get_yf_institutions(self, ticker):
        self.yf_ticker = yf.Ticker(str(ticker))
        institutional_holders = self.yf_ticker.get_institutional_holders()
        major_holders = self.yf_ticker.get_major_holders()
        # mf_holders = self.yf_ticker.get_mutualfund_holders()

        return institutional_holders, major_holders
