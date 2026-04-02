import logging


class FinanceComponents():
    """Components for financial data retrieval, analysis, and visualization.

    Provides methods to fetch stock data from various sources (Tiingo, IEX,
    Morningstar), calculate returns on investment, and generate timeline
    and returns plots.

    Attributes:
        cfg: Configuration object with stock tickers, API keys, and plot settings.
        status: AttributeDict tracking data source availability.
    """

    def __init__(self, cfg):
        """Initialize FinanceComponents with configuration.

        Args:
            cfg: Configuration object with 'stocks', 'default', 'plots',
                and 'Analysis' attributes.
        """
        from digitalmodel.infrastructure.utils.data import AttributeDict
        self.cfg = cfg
        self.status = AttributeDict({'tiingo': None, 'morningstar': None})

    def get_data(self, type='close'):
        """Retrieve stock data from configured data sources.

        Args:
            type: Type of price data to retrieve. Defaults to 'close'.
        """
        # self.get_data_from_morningstar()
        self.get_data_from_tiingo()
        # self.get_data_from_iex()

    def get_data_from_tiingo(self):
        """Fetch stock data from the Tiingo API.

        Retrieves historical stock data for each configured ticker and
        adds 20-day and 100-day rolling averages. Results are stored
        in the stock_data_array attribute.
        """
        # {'tiingo': {'flag': None}}
        import pandas_datareader as pdr
        api_key = self.cfg.default['data_sources']['tiingo']['api_key']
        self.stock_data_array = []
        for stock_info in self.cfg.stocks:
            stock_ticker = stock_info['ticker']
            df = pdr.get_data_tiingo(stock_ticker, api_key=api_key)
            df['date'] = [index_value[1] for index_value in df.index]
            df['20_day_rolling'] = df.close.rolling(window=20).mean()
            df['100_day_rolling'] = df.close.rolling(window=100).mean()
            self.stock_data_array.append(df)

    def get_data_from_iex(self):
        """Fetch stock data from the IEX API.

        Retrieves historical stock data for the last ~15 years for each
        configured ticker. Results are stored in the stock_data_array attribute.
        """
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
            df['20_day_rolling'] = df.close.rolling(window=20).mean()
            df['100_day_rolling'] = df.close.rolling(window=100).mean()
            self.stock_data_array.append(df)

    def prepare_time_line_plot(self):
        """Generate timeline plots for all stocks showing price history.

        Creates both full-history and 1-year timeline plots for each stock
        in the stock_data_array, saving them to the configured result folder.
        """
        from digitalmodel.infrastructure.utils.visualization.visualizations import Visualization
        viz = Visualization()

        for stock_index in range(0, len(self.stock_data_array)):
            plt_settings = self.cfg.plots['timeline'][0].copy()
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_suffix'] + '_' + self.cfg.stocks[stock_index]['ticker'] + '.png'
            })
            plt_settings.update({'title': plt_settings['title'].format(self.cfg.stocks[stock_index]['ticker'])})

            viz.from_df_columns(self.stock_data_array[stock_index], plt_settings)
            viz.add_title_and_axis_labels(plt_settings)
            viz.add_legend()
            print("Saving max {0} plot for {1}...".format(plt_settings['file_suffix'],
                                                          self.cfg.stocks[stock_index]['ticker']))
            viz.save_and_close()

        for stock_index in range(0, len(self.stock_data_array)):
            from datetime import datetime, timedelta

            import pandas as pd
            end_date = datetime.now()
            start_date = pd.Timestamp(end_date - timedelta(days=366), tz="Europe/Brussels")

            plt_settings = self.cfg.plots['timeline'][0].copy()
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_suffix'] + self.cfg.stocks[stock_index]['ticker'] + '_1year_' + '.png'
            })
            plt_settings.update({'title': plt_settings['title'].format(self.cfg.stocks[stock_index]['ticker'])})

            df = self.stock_data_array[stock_index]
            df = df[df.date >= start_date]
            viz.from_df_columns(df, plt_settings)
            viz.add_title_and_axis_labels(plt_settings)
            viz.add_legend()
            # viz.plt.grid()
            print("Saving 1 year {0} plot for {1}...".format(plt_settings['file_suffix'],
                                                             self.cfg.stocks[stock_index]['ticker']))
            viz.save_and_close()

    def get_data_from_morningstar(self):
        """Fetch stock data from Morningstar data source.

        Retrieves OXY stock data between 2010 and 2013 as a demonstration.
        """
        data_source = 'morningstar'

        import datetime

        import pandas_datareader.data as web

        start = datetime.datetime(2010, 1, 1)
        end = datetime.datetime(2013, 1, 27)
        f = web.DataReader('OXY', data_source, start, end)

        print(web.DataReader('OXY', data_source, start, end))

    def evaluate_returns(self):
        """Evaluate investment returns by calculating return on investment."""
        self.get_return_on_investment()

    def evaluate_returns_for_initial_investment(self):
        """Evaluate returns for an initial investment amount.

        Note:
            Currently a placeholder with no implementation.
        """
        pass

    def get_return_on_investment(self):
        """Calculate return on investment including dividend reinvestment.

        Computes share accumulation through dividend reinvestment and
        tracks portfolio value over time. Results are stored in the
        df_returns_arrays attribute.
        """
        import datetime
        import statistics

        import pandas as pd
        import pytz
        self.df_returns_arrays = []

        for stock_index in range(0, len(self.stock_data_array)):
            df = self.stock_data_array[stock_index]
            start_year = df.date.min().year + 1
            end_year = df.date.max().year

            if self.cfg.stocks[stock_index].__contains__(
                    'purchase_date') and self.cfg.stocks[stock_index]['purchase_date'] is not None:
                # TODO convert string to datetime
                start_date = self.cfg.stocks[stock_index]['purchase_date']
            else:
                start_date = pytz.utc.localize(datetime.datetime(start_year, 1, 1))
                purchase_date = start_date + datetime.timedelta(days=1)
            if self.cfg.stocks[stock_index].__contains__(
                    'initial_investment') and self.cfg.stocks[stock_index]['initial_investment'] is not None:
                initial_investment = self.cfg.stocks[stock_index]['initial_investment']
            else:
                initial_investment = 1000
            if self.cfg.stocks[stock_index].__contains__(
                    'yearly_payment') and self.cfg.stocks[stock_index]['yearly_payment'] is not None:
                yearly_payment = self.cfg.stocks[stock_index]['yearly_payment']
            else:
                yearly_payment = 1000

            df_temp = df[df.date >= purchase_date]
            df_for_returns = pd.concat([df_temp.head(1), df_temp[df_temp.divCash > 0].copy(),
                                        df.tail(1)],
                                       ignore_index=True,
                                       copy=True)
            df_for_returns['value'] = 0
            df_for_returns['no_of_shares'] = 0
            df_for_returns['cash_value'] = 0

            if self.cfg.stocks[stock_index].__contains__(
                    'purchase_price') and self.cfg.stocks[stock_index]['purchase_price'] is not None:
                purchase_price = self.cfg.stocks[stock_index]['purchase_price']
            else:
                purchase_price = statistics.mean([df_for_returns.loc[0, 'close'], df_for_returns.loc[0, 'open']])
            no_of_shares_purchased = initial_investment / purchase_price

            for row_index in range(0, len(df_for_returns)):
                purchase_price = statistics.mean(
                    [df_for_returns.loc[row_index, 'close'], df_for_returns.loc[row_index, 'open']])
                if row_index == 0:
                    df_for_returns.loc[row_index, 'no_of_shares'] = no_of_shares_purchased + df_for_returns.loc[
                        row_index, 'divCash'] / purchase_price
                else:
                    df_for_returns.loc[row_index, 'no_of_shares'] = df_for_returns.loc[
                                                                         row_index - 1, 'no_of_shares'] + \
                                                                     df_for_returns.loc[
                                                                         row_index, 'divCash'] / purchase_price
                df_for_returns.loc[row_index, 'value'] = df_for_returns.loc[row_index, 'close'] * \
                                                         df_for_returns.loc[row_index, 'no_of_shares']
            self.df_returns_arrays.append(df_for_returns)

    def prepare_returns_plot(self):
        """Generate return-on-investment plots for all stocks.

        Creates plots showing investment value over time for each stock,
        saving them to the configured result folder.
        """
        from digitalmodel.infrastructure.utils.visualization.visualizations import Visualization
        viz = Visualization()

        for stock_index in range(0, len(self.stock_data_array)):
            plt_settings = self.cfg.plots['returns'][0].copy()
            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_suffix'] + '_' + self.cfg.stocks[stock_index]['ticker'] + '.png'
            })
            plt_settings.update({'title': plt_settings['title'].format(self.cfg.stocks[stock_index]['ticker'])})

            viz.from_df_columns(self.df_returns_arrays[stock_index], plt_settings)
            viz.add_title_and_axis_labels(plt_settings)
            viz.add_legend()
            # viz.plt.grid()
            print("Saving {0} plot for {1}...".format(plt_settings['file_suffix'],
                                                      self.cfg.stocks[stock_index]['ticker']))
            viz.save_and_close()

    def superseded_code(self):
        """Superseded code for fetching data from Morningstar.

        Note:
            This method contains legacy code that has been replaced.
        """
        # {'morningstar': {'flag': None}}
        data_source = 'morningstar'

        import datetime

        import pandas_datareader.data as web

        start = datetime.datetime(2010, 1, 1)
        end = datetime.datetime(2013, 1, 27)
        f = web.DataReader('OXY', data_source, start, end)

        print(web.DataReader('OXY', data_source, start, end))
