import datetime
import pandas as pd

from common.data import get_initials_from_name, transform_df_datetime_to_str


class FinanceAnalysis():

    def __init__(self, cfg):
        import pandas as pd
        self.cfg = cfg
        self.insider_analysis_by_relation_df = pd.DataFrame()
        self.insider_analysis_by_timeline_df = pd.DataFrame()
        self.call_effective_value_df = pd.DataFrame()
        self.call_effective_value_df_filtered = pd.DataFrame()
        self.status = {'insider': {}}

    def assign_data(self, data_dict):
        self.df_data = data_dict['df_data']
        self.company_info = data_dict['company_info']
        self.insider_df = data_dict['insider_df']
        self.option_data = data_dict['option_data']
        if self.df_data is not None:
            self.current_price = self.df_data['Close'].iloc[-1]

    def run_long_term_analysis(self):
        self.add_all_ta_features(self.df_data)
        breakout_trend_json = self.get_breakout_trend(self.df_data)
        self.company_info.update({'breakout_trend_indicators': breakout_trend_json})
        insider_info = self.evaluate_insider_trend(self.insider_df)
        self.company_info.update({'insider_info': insider_info})
        if self.option_data:
            call_effective_value_dict = self.call_analysis(self.option_data, self.current_price)
            no_of_closes_call_records = 10
            self.filter_call_analysis_data(no_of_closes_call_records, self.current_price)
            self.company_info.update({'optionanalysis': {'call_analysis': call_effective_value_dict}})

    def evaluate_insider_trend(self, insider_df):
        self.status['insider'].update({'status': True})
        insider_summary_dict = self.get_insider_summary(insider_df.copy())
        insider_analysis_by_relation_dict = self.get_insider_analysis_by_relation(insider_df.copy())
        insider_analysis_by_timeline_dict = self.get_insider_analysis_by_timeline(insider_df.copy())

        insider_info = {
            'insider_summary': insider_summary_dict,
            'insider_by_relation': insider_analysis_by_relation_dict,
            'insider_by_timeline': insider_analysis_by_timeline_dict
        }

        return insider_info

    def get_insider_summary(self, insider_df):
        if len(insider_df) > 0:
            insider_df = transform_df_datetime_to_str(insider_df, date_format='%Y-%m-%d')
            insider_df['Tooltip'] = insider_df.apply(lambda row: self.get_tootip_for_insider_summary_row(row), axis=1)
            insider_df_buy = insider_df[insider_df.share_holding_ratio >= 1]
            insider_df_sell = insider_df[insider_df.share_holding_ratio < 1]
        else:
            insider_df_buy = insider_df.copy()
            insider_df_sell = insider_df.copy()
        insider_df_buy_dict = insider_df_buy.to_dict(orient='records')
        insider_df_sell_dict = insider_df_sell.to_dict(orient='records')
        insider_summary = {'insider_df_buy': insider_df_buy_dict, 'insider_df_sell': insider_df_sell_dict}
        return insider_summary

    def get_tootip_for_insider_summary_row(self, row):
        Tooltip = "trasactionType: {}, Insider: {}, Relationship: {}".format(row['Transaction'],
                                                                             row['Insider Trading'],
                                                                             row['Relationship'])
        return Tooltip

    def get_insider_analysis_by_relation(self, insider_df):
        self.status['insider'].update({'relation': False})

        try:
            columns = ['Relationship', 'Share Holding Ratio', 'Actions', 'Average Cost', 'Tooltip']
            self.insider_analysis_by_relation_df = pd.DataFrame(columns=columns)
            inside_traders = insider_df['Insider Trading'].unique()
            for trader_index in range(0, len(inside_traders)):
                inside_trader = inside_traders[trader_index]
                df_insider = insider_df[insider_df['Insider Trading'] == inside_trader].copy()

                end_shares = df_insider['#Shares Total'].iloc[0]
                noShares = df_insider['#Shares'].sum()
                if df_insider['Transaction'].iloc[-1] in ['Sell', 'Sale']:
                    start_shares = df_insider['#Shares Total'].iloc[-1] + noShares
                else:
                    start_shares = df_insider['#Shares Total'].iloc[-1] - noShares

                if start_shares > 0:
                    share_holding_ratio = (end_shares / start_shares).__round__(2)
                else:
                    share_holding_ratio = 1.01

                try:
                    average_cost = (df_insider['#Shares'] * df_insider['Cost']).sum() / df_insider['#Shares'].sum()
                    average_cost = round(average_cost, 3)
                except:
                    average_cost = None

                transaction_list = list(df_insider['Transaction'].unique())
                trader_intials = get_initials_from_name(inside_trader)
                relationship = df_insider['Relationship'].iloc[0] + '_' + trader_intials

                actions = "; ".join(transaction_list)
                data_tooltip = "{0}, Share End/Start # of {1}; End Shares of {2}; Average Cost: {4}, Actions: {3}".format(
                    df_insider['Relationship'].iloc[0], share_holding_ratio, end_shares, actions, average_cost)
                row_array = [relationship, share_holding_ratio, actions, average_cost, data_tooltip]
                self.insider_analysis_by_relation_df.loc[len(self.insider_analysis_by_relation_df)] = row_array
            insider_analysis_by_relation_dict = self.insider_analysis_by_relation_df.to_dict(orient='records')
            self.status['insider'].update({'relation': True})
        except:
            self.status['insider'].update({'relation': "Error"})
            return {}

        return insider_analysis_by_relation_dict

    def get_insider_analysis_by_timeline(self, insider_df):
        self.status['insider'].update({'timeline': False})

        try:
            columns = ['tradeDate', 'Actions', 'Share Holding Ratio', 'Average Cost', 'Tooltip']
            self.insider_analysis_by_timeline_df = pd.DataFrame(columns=columns)

            trade_dates = insider_df['Date'].unique()
            for trade_date_index in range(0, len(trade_dates)):
                trade_date = trade_dates[trade_date_index]
                df_trade_date = insider_df[insider_df['Date'] == trade_date].copy()

                unique_transactions = df_trade_date['Transaction'].unique()
                for transaction_type in unique_transactions:
                    df_transaction = df_trade_date[df_trade_date['Transaction'] == transaction_type].copy()

                    end_shares = df_transaction['#Shares Total'].sum()
                    shares = df_transaction['#Shares'].sum()
                    if transaction_type in ['Sell', 'Sale']:
                        start_shares = end_shares + shares
                    else:
                        start_shares = end_shares - shares

                    if start_shares > 0:
                        share_holding_ratio = (end_shares / start_shares).__round__(2)
                    else:
                        share_holding_ratio = 1.01

                    try:
                        average_cost = (df_transaction['#Shares'] *
                                        df_transaction['Cost']).sum() / df_transaction['#Shares'].sum()
                        average_cost = round(average_cost, 3)
                    except:
                        average_cost = None

                    data_tooltip = ''
                    data_tooltip = data_tooltip + 'Average Cost: {}; '.format(average_cost)
                    transaction_tooltip = ''
                    for transaction_index in range(0, len(df_transaction)):
                        inside_trader = df_transaction['Insider Trading'].iloc[transaction_index]
                        trader_intials = get_initials_from_name(inside_trader)
                        relationship = df_trade_date['Relationship'].iloc[0] + '_' + trader_intials
                        transaction_tooltip = transaction_tooltip + relationship + "; "
                    data_tooltip = data_tooltip + transaction_tooltip
                    row_array = [trade_date, transaction_type, share_holding_ratio, average_cost, data_tooltip]
                    self.insider_analysis_by_timeline_df.loc[len(self.insider_analysis_by_timeline_df)] = row_array

            insider_analysis_by_timeline_df_dict = self.insider_analysis_by_timeline_df.to_dict(orient='records')
            return insider_analysis_by_timeline_df_dict
        except:
            self.status['insider'].update({'timeline': "Error"})
            return {}

    def call_analysis(self, option_data, current_price):
        self.status.update({'options': True})
        option_dates = list(option_data.keys())
        for option_date in option_dates:
            call_data = option_data[option_date]['calls']
            put_data = option_data[option_date]['puts']
            self.evaluate_call_data(option_date, call_data, current_price)
            self.evaluate_put_data(option_date, put_data, current_price)

        call_analysis_dict = self.call_effective_value_df.to_dict(orient='records')
        return call_analysis_dict

    def add_all_ta_features(self, df):
        from ta import add_all_ta_features
        from ta.utils import dropna
        self.status.update({'technical': True})
        df = df.copy()
        df = df.drop(columns=['Dividends', 'Stock Splits'])
        df = dropna(df)
        if self.cfg['source'] == 'yfinance':
            try:
                self.ta = add_all_ta_features(df, open="Open", high="High", low="Low", close="Close", volume="Volume")
            except:
                import pandas as pd
                self.ta = pd.DataFrame()

    def get_breakout_trend(self, df_data):
        self.status.update({'breakout_trend': True})
        import pandas as pd
        self.breakout_summary_array = []
        columns = ['Description', 'Value']
        df = pd.DataFrame(columns=columns)
        df.loc[len(df)] = self.check_if_price_above_150_and_200_moving(df_data)
        df.loc[len(df)] = self.check_if_150_moving_above_200_moving(df_data)
        df.loc[len(df)] = self.check_if_200_moving_up_for_1mo(df_data)
        df.loc[len(df)] = self.check_if_50_day_above_150_and_200_moving(df_data)
        df.loc[len(df)] = self.check_if_price_above_50_moving(df_data)
        df.loc[len(df)] = self.check_if_price_above_1p3_52wk_low(df_data)
        df.loc[len(df)] = self.check_if_price_near_52wk_high_range(df_data)

        return df

    def check_if_price_above_150_and_200_moving(self, df, close="Close"):
        description = 'Price Above 150 & 200 day avgs.'
        if df.iloc[-1][close] > df.iloc[-1]['150_day_rolling'] and df.iloc[-1][close] > df.iloc[-1]['200_day_rolling']:
            value = True
        else:
            value = False
        return [description, value]

    def check_if_150_moving_above_200_moving(self, df):
        description = '150 day avg. above 200 day avg.'
        if df.iloc[-1]['150_day_rolling'] > df.iloc[-1]['200_day_rolling']:
            value = True
        else:
            value = False
        return [description, value]

    def check_if_200_moving_up_for_1mo(self, df):
        description = '200 day avg. uptrend for 1 mo [n mo.]'
        no_of_months_trend_above = self.get_200_moving_up_for_n_mo(df)
        if no_of_months_trend_above >= 1:
            value = True
        else:
            value = False

        return [description, str(value) + " [{} mo.]".format(no_of_months_trend_above)]

    def get_200_moving_up_for_n_mo(self, df):
        import datetime
        df['200_day_diff'] = df['200_day_rolling'].diff(periods=1).values

        no_of_months_trend_above = 0
        half_months = 11
        for no_of_half_months in range(1, half_months):
            no_of_months_trend_above = 0.5 * (no_of_half_months - 1)
            days_period = 0.5 * no_of_half_months * 30
            start_time = df.Date.iloc[-1] + datetime.timedelta(days=-days_period)
            if start_time > df.Date.iloc[0]:
                df_temp = df[df.Date > start_time].copy()
                if df_temp['200_day_diff'].min() > 0:
                    no_of_months_trend_above = 0.5 * (no_of_half_months - 1)
                else:
                    break
            else:
                break

        return no_of_months_trend_above

    def check_if_50_day_above_150_and_200_moving(self, df):
        description = '50 day avg. Above 150 & 200 day avgs.'
        if df.iloc[-1]['50_day_rolling'] > df.iloc[-1]['150_day_rolling'] and df.iloc[-1]['50_day_rolling'] > df.iloc[
                -1]['200_day_rolling']:
            value = True
        else:
            value = False
        return [description, value]

    def check_if_price_above_50_moving(self, df, close='Close'):
        description = 'Price Above 50 day avg. [x; y %]'
        price_above_50_moving = (df.iloc[-1][close] - df.iloc[-1]['50_day_rolling']).__round__(1)
        percent_price_above_50_moving = ((df.iloc[-1][close] - df.iloc[-1]['50_day_rolling']) /
                                         df.iloc[-1]['50_day_rolling'] * 100).__round__(0)
        if price_above_50_moving > 0:
            value = True
        else:
            value = False
        return [description, str(value) + " [{}; {}%]".format(price_above_50_moving, percent_price_above_50_moving)]

    def check_if_price_above_1p3_52wk_low(self, df, close='Close'):
        description = 'Price 30% Above 52 wk low [% above]'
        fiftyTwoWeekLow = df[df['Date'] > datetime.datetime.now() + datetime.timedelta(days=-365)].Close.min()
        percent_above_52wklow = ((df.iloc[-1][close] / fiftyTwoWeekLow - 1) * 100).__round__(0)
        if percent_above_52wklow > 30:
            value = True
        else:
            value = False

        return [description, str(value) + " [{} %]".format(percent_above_52wklow)]

    def check_if_price_near_52wk_high_range(self, df, close='Close'):
        description = 'Price within 25% of 52 wk high range [% value]'
        fiftyTwoWeekHigh = df[df['Date'] > datetime.datetime.now() + datetime.timedelta(days=-365)].Close.max()
        percent_above_52wkhigh = ((df.iloc[-1][close] / fiftyTwoWeekHigh - 1) * 100).__round__(0)
        if percent_above_52wkhigh > -25:
            value = True
        else:
            value = False

        return [description, str(value) + " [{} %]".format(percent_above_52wkhigh)]

    def evaluate_call_data(self, option_date, option_data, current_price):
        # TODO Options transaction costs are not taken into account
        if self.call_effective_value_df.empty:
            import pandas as pd
            columns = ['expirationDate', 'strike', 'effectiveValuePerShare', 'Tooltip']
            self.call_effective_value_df = pd.DataFrame(columns=columns)

        for od_index in range(0, len(option_data)):
            strike = option_data['strike'].iloc[od_index]
            lastPrice = option_data['lastPrice'].iloc[od_index]
            bid = option_data['bid'].iloc[od_index]
            contract_cost = max(lastPrice, bid)
            tool_tip = "{0}, {1}".format(strike, option_date)
            if contract_cost > 0:
                effectiveValuePerShare = contract_cost + (current_price - strike)
                if effectiveValuePerShare < 0:
                    effectiveValuePerShare = 0
            else:
                effectiveValuePerShare = 0
            call_analysis_array = [option_date, strike, effectiveValuePerShare, tool_tip]
            self.call_effective_value_df.loc[len(self.call_effective_value_df)] = call_analysis_array

    def evaluate_put_data(self, option_date, put_data, current_price):
        pass

    def evaluate_returns(self):
        self.get_return_on_investment()

    def evaluate_returns_for_initial_investment(self):
        pass

    def get_return_on_investment(self):
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

    def filter_call_analysis_data(self, n, current_price):
        from common.data import getClosestIntegerInList
        current_price = int(current_price)
        strike_prices = list(self.call_effective_value_df.strike.unique())
        strike_prices.sort()
        closest_value, closest_value_index = getClosestIntegerInList(strike_prices, current_price)
        nieghbours = int(n / 2)
        start_index = closest_value_index - nieghbours
        if start_index < 0:
            start_index = 0
        end_index = closest_value_index + nieghbours
        if end_index > len(strike_prices):
            end_index = len(strike_prices)
        strike_prices_filtered = strike_prices[start_index:end_index]
        self.call_effective_value_df_filtered = self.call_effective_value_df[
            self.call_effective_value_df['strike'].isin(strike_prices_filtered)].copy()
