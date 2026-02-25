import datetime
import json


class StockCharts:

    def __init__(self):
        pass

    def get_plot_data(self, cfg):
        insider = self.create_insider_chart(cfg)
        insider_relative_sale = self.create_insider_relative_sale_chart(cfg)
        insider_relative_buy = self.create_insider_relative_buy_chart(cfg)
        insider_by_relation = self.create_insider_by_relation_chart(cfg)
        insider_by_timeline = self.create_insider_by_timeline_chart(cfg)

        institution = self.create_institution_chart(cfg)

        price = self.create_price_chart(cfg)
        ob_volume = self.create_ob_volume_chart(cfg)
        cfm = self.create_cfm_chart(cfg)
        eom = self.create_eom_chart(cfg)
        wt_price = self.create_wt_price_chart(cfg)

        volatility_width = self.create_volatility_width_chart(cfg)
        volatility_hi_low = self.create_volatility_high_low_band_chart(cfg)
        ulcer = self.create_ulcer_chart(cfg)
        strength = self.create_strength_all_chart(cfg)
        call_analysis = self.create_call_analysis_chart(cfg)

        plot_data = {
            'insider': insider,
            'insider_relative_sale': insider_relative_sale,
            'insider_relative_buy': insider_relative_buy,
            'insiderByRelation': insider_by_relation,
            'insiderByTimeline': insider_by_timeline,
            'institution': institution,
            'price': price,
            'ob_volume': ob_volume,
            'cfm': cfm,
            'eom': eom,
            'wt_price': wt_price,
            'volatility_width': volatility_width,
            'volatility_hi_low': volatility_hi_low,
            'ulcer': ulcer,
            'strength': strength,
            'call_analysis': call_analysis
        }
        return plot_data

    def create_insider_chart(self, cfg):
        plotly_data1 = json.loads(self.create_insider_buy_chart(cfg))
        plotly_data2 = json.loads(self.create_insider_sell_chart(cfg))
        plotly_data3 = json.loads(self.create_insider_share_price_chart(cfg))

        plotly_data = plotly_data1.copy()
        plotly_data['data'] = plotly_data1['data'] + plotly_data2['data'] + plotly_data3['data']
        plotly_data_json_str = json.dumps(plotly_data)

        return plotly_data_json_str

    def create_insider_buy_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        ticker = cfg['ticker']

        df = cfg['df_insider_buy']
        df_insider_sell = cfg['df_insider_sell']
        if len(df) > 0:
            df_ref1 = df['#Shares'].max()
        else:
            df_ref1 = 100
        if len(df_insider_sell) > 0:
            df_ref2 = df_insider_sell['#Shares'].max()
        else:
            df_ref2 = 100
        sizeref_max = max(df_ref1, df_ref2)
        sizeref = viz.get_custom_size_ref(sizeref_max, sizemax=40)

        layout = {
            'title': 'Insider Buy Cost and Volumes: {}'.format(ticker),
            'yaxis': {
                'title': 'Cost Basis (USD)'
            },
        }
        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': '#Shares',
                'sizerefcolumn': '#Shares',
                'sizemin': 4,
                'sizemax': 40,
                'color': 'rgb(44, 160, 101)'
            },
            'name': ['Bought'],
            'x': ['Date'],
            'y': ['Cost'],
            'text': ['Tooltip']
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_insider_relative_sale_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()
        plotly_group_data = None

        ticker = cfg['ticker']

        df = cfg['df_insider_sell']
        if len(df) > 0:
            sizeref_max = df['start_shares'].max()
        else:
            sizeref_max = 100

        sizeref = viz.get_custom_size_ref(sizeref_max, sizemax=40)

        layout = {
            'title': 'Insider Sale Cost and Volumes: {}'.format(ticker),
            'yaxis': {
                'title': 'Cost Basis (USD)'
            },
        }

        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': '#Shares Total',
                'sizerefcolumn': 'start_shares',
                'sizemin': 4,
                'sizemax': 40,
                'color': 'rgb(161, 161, 157)'
            },
            'name': ['Shares at Start'],
            'x': ['Date'],
            'y': ['Cost'],
            'text': ['Tooltip']
        }
        cfg_plot_data.update({'layout': layout})
        plotly_data = viz.get_plotly_data(cfg_plot_data)

        if plotly_group_data is None:
            plotly_group_data = json.loads(plotly_data).copy()
        else:
            plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': '#Shares',
                'sizerefcolumn': '#Shares',
                'sizemin': 4,
                'sizemax': 40,
                'color': 'rgb(255, 65, 54)'
            },
            'name': ['Sold'],
            'x': ['Date'],
            'y': ['Cost'],
        }
        cfg_plot_data.update({'layout': layout})
        plotly_data = viz.get_plotly_data(cfg_plot_data)

        if plotly_group_data is None:
            plotly_group_data = json.loads(plotly_data).copy()
        else:
            plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        plotly_data = self.create_insider_share_price_chart(cfg)
        if plotly_group_data is None:
            plotly_group_data = json.loads(plotly_data).copy()
        else:
            plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        return plotly_group_data

    def create_insider_relative_buy_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()
        plotly_group_data = None

        ticker = cfg['ticker']

        df = cfg['df_insider_buy']
        if len(df) > 0:
            sizeref_max = df['#Shares Total'].max()
        else:
            sizeref_max = 100

        sizeref = viz.get_custom_size_ref(sizeref_max, sizemax=40)

        layout = {
            'title': 'Insider Buy Cost and Volumes: {}'.format(ticker),
            'yaxis': {
                'title': 'Cost Basis (USD)'
            },
        }

        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': '#Shares Total',
                'sizerefcolumn': '#Shares Total',
                'sizemin': 4,
                'sizemax': 40,
                'color': 'rgb(161, 161, 157)'
            },
            'name': ['Shares at End'],
            'x': ['Date'],
            'y': ['Cost'],
            'text': ['Tooltip']
        }
        cfg_plot_data.update({'layout': layout})
        plotly_data = viz.get_plotly_data(cfg_plot_data)

        if plotly_group_data is None:
            plotly_group_data = json.loads(plotly_data).copy()
        else:
            plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': '#Shares',
                'sizerefcolumn': '#Shares',
                'sizemin': 4,
                'sizemax': 40,
                'color': 'rgb(44, 160, 101)'
            },
            'name': ['Buy'],
            'x': ['Date'],
            'y': ['Cost'],
            'text': ['Tooltip']
        }
        cfg_plot_data.update({'layout': layout})
        plotly_data = viz.get_plotly_data(cfg_plot_data)

        if plotly_group_data is None:
            plotly_group_data = json.loads(plotly_data).copy()
        else:
            plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        plotly_data = self.create_insider_share_price_chart(cfg)
        if plotly_group_data is None:
            plotly_group_data = json.loads(plotly_data).copy()
        else:
            plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        return plotly_group_data

    def create_insider_sell_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        ticker = cfg['ticker']

        df = cfg['df_insider_sell']
        df_insider_buy = cfg['df_insider_buy']
        if len(df_insider_buy) > 0:
            df_ref1 = df_insider_buy['#Shares'].max()
        else:
            df_ref1 = 100
        if len(df) > 0:
            df_ref2 = df['#Shares'].max()
        else:
            df_ref2 = 100
        sizeref_max = max(df_ref1, df_ref2)
        sizeref = viz.get_custom_size_ref(sizeref_max, sizemax=40)

        layout = {
            'title': 'Insider Sale Cost and Volumes: {}'.format(ticker),
            'yaxis': {
                'title': 'Cost Basis (USD)'
            },
        }
        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': '#Shares',
                'sizerefcolumn': '#Shares',
                'sizemin': 4,
                'sizemax': 40,
                'color': 'rgb(255, 65, 54)'
            },
            'name': ['Sold'],
            'x': ['Date'],
            'y': ['Cost'],
            'text': ['Tooltip']
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_insider_share_price_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        ticker = cfg['ticker']

        df = cfg['df_daily_data']
        df_insider_buy = cfg['df_insider_buy']
        df_insider_sell = cfg['df_insider_sell']
        if len(df_insider_buy) > 0:
            df_insider_buy_min_date = df_insider_buy.Date.min()
        else:
            df_insider_buy_min_date = datetime.datetime.now()
        if len(df_insider_sell) > 0:
            df_insider_sell_min_date = df_insider_sell.Date.min()
        else:
            df_insider_sell_min_date = datetime.datetime.now()

        min_date = min(df_insider_buy_min_date, df_insider_sell_min_date)
        df = df[df.Date >= min_date]

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Share Price, Close'],
            'x': ['Date'],
            'y': ['Close'],
            'line': {
                'color': None
            }
        }
        layout = {
            'title': 'Stock Price Timeline: {}'.format(ticker),
            'xaxis': {
                'title': 'Date',
            },
            'yaxis': {
                'title': 'Value'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_insider_by_relation_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg['df_insider_by_relation']
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': 'bar',
            'name': ['Share Holding Ratio'],
            'x': ['Relationship'],
            'y': ['Share Holding Ratio'],
            'text': ['Tooltip']
        }
        layout = {
            'title':
                'Share Holding (End/Start) Ratio by Insider: {}'.format(ticker),
            'xaxis': {
                'tickangle': -45
            },
            'yaxis': {
                'range': [0, 3],
                'title': 'Share Ratio (End/Start)'
            },
            'shapes': [{
                'type': 'line',
                'xref': 'paper',
                'x0': 0,
                'y0': 1,
                'x1': 1,
                'y1': 1,
                'line': {
                    'color': 'rgb(255,0,0)',
                    'width': 1,
                    'dash': 'dash'
                }
            }]
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_insider_by_timeline_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg['df_insider_by_timeline']
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': 'bar',
            'name': ['Share Holding Ratio'],
            'name_column_legend_groups': ['Actions'],
            'x': ['tradeDate'],
            'y': ['Share Holding Ratio'],
            'text': ['Tooltip']
        }
        layout = {
            'title': 'Share Holding (End/Start) Ratio by Timeline : {}'.format(ticker),
            'xaxis': {
                'tickangle': -45,
                'type': 'category',
                'categoryorder': 'category ascending'
            },
            'yaxis': {
                'range': [0, 3],
                'title': 'Share Ratio (End/Start)'
            },
            'shapes': [{
                'type': 'line',
                'xref': 'paper',
                'x0': 0,
                'y0': 1,
                'x1': 1,
                'y1': 1,
                'line': {
                    'color': 'rgb(255,0,0)',
                    'width': 1,
                    'dash': 'dash'
                }
            }],
            'barmode': 'group'
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_institution_chart(self, cfg):
        plotly_data1 = json.loads(self.create_institution_volume_chart(cfg))
        plotly_data2 = json.loads(self.create_insider_share_price_chart(cfg))

        plotly_data = plotly_data1.copy()
        plotly_data['data'] = plotly_data1['data'] + plotly_data2['data']
        plotly_data_json_str = json.dumps(plotly_data)

        return plotly_data_json_str

    def create_institution_volume_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        ticker = cfg['ticker']

        df = cfg['df_institutional_holders']
        if len(df) > 0:
            sizeref_max = df['Shares'].max()
        else:
            sizeref_max = 100

        sizeref = viz.get_custom_size_ref(sizeref_max, sizemax=40)

        layout = {
            'title': 'Institution Volumes: {}'.format(ticker),
            'yaxis2': {
                'title': 'Shares',
                'titlefont': {
                    'color': 'rgb(148, 103, 189)'
                },
                'tickfont': {
                    'color': 'rgb(148, 103, 189)'
                },
            },
        }
        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': 'Shares',
                'sizerefcolumn': 'Shares',
                'sizemin': 4,
                'sizemax': 40,
            },
            'name': ['Institution Volume(#)'],
            'x': ['end_date'],
            'y': ['Shares'],
            'text': ['Holder']
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_price_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg['df_daily_data']
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Close', '50 day avg.', '150 day avg.', '200 day avg.'],
            'x': ['Date'],
            'y': ['Close', '50_day_rolling', '150_day_rolling', '200_day_rolling'],
            'line': {
                'color': None
            }
        }
        layout = {
            'title': 'Stock Price Timeline: {}'.format(ticker),
            'xaxis': {
                'title': 'Date',
            },
            'yaxis': {
                'title': 'Value'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_volume_all_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': [
                'On Balance Volume', 'Chaikin Money Flow', 'Ease of Movement (EoM)', 'EoM SMA.', 'Vol. Wt. Avg Price'
            ],
            'x': ['index'],
            'y': ['volume_obv', 'volume_cmf', 'volume_em', 'volume_sma_em', 'volume_vwap'],
            'line': {
                'color': None
            }
        }
        layout = {'title': 'Stock : {}'.format(ticker), 'xaxis': {'title': 'Date'}, 'yaxis': {'title': 'Value'}}
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_ob_volume_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['On Balance Volume'],
            'x': ['index'],
            'y': ['volume_obv'],
            'line': {
                'color': None
            }
        }
        layout = {
            'title': 'On balance Volume : {}'.format(ticker),
            'xaxis': {
                'title': 'Date'
            },
            'yaxis': {
                'title': 'Value'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_cfm_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Chaikin Money Flow'],
            'x': ['index'],
            'y': ['volume_cmf'],
            'line': {
                'color': None
            }
        }
        layout = {'title': 'Money Flow : {}'.format(ticker), 'xaxis': {'title': 'Date'}, 'yaxis': {'title': 'Value'}}
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_eom_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Ease of Movement (EoM)', 'EoM SMA.'],
            'x': ['index'],
            'y': ['volume_em', 'volume_sma_em'],
            'line': {
                'color': None
            }
        }
        layout = {
            'title': 'Ease of Movement : {}'.format(ticker),
            'xaxis': {
                'title': 'Date'
            },
            'yaxis': {
                'title': 'Value'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_wt_price_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Vol. Wt. Avg Price'],
            'x': ['index'],
            'y': ['volume_vwap'],
            'line': {
                'color': None
            }
        }
        layout = {'title': 'Stock : {}'.format(ticker), 'xaxis': {'title': 'Date'}, 'yaxis': {'title': 'Value'}}
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_volatility_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': [
                'Bollinger (Boll.) % Width', 'Boll Highband Ind.', 'Boll Lowband Ind.',
                'Keltner ATR Channel (KC) % Width', 'KC Highband Ind.', 'KC Lowband Ind.', 'Doncian Channel % Width',
                'Ulcer Index'
            ],
            'x': ['index'],
            'y': [
                'volatility_bbp', 'volatility_bbhi', 'volatility_bbli', 'volatility_kcp', 'volatility_kchi',
                'volatility_bbli', 'volatility_dcp', 'volatility_ui'
            ],
            'line': {
                'color': None
            }
        }

        layout = {'title': 'Stock : {}'.format(ticker), 'xaxis': {'title': 'Date'}, 'yaxis': {'title': 'Value'}}
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_volatility_width_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Bollinger (Boll.) % Width', 'Keltner ATR Channel (KC) % Width', 'Doncian Channel % Width'],
            'x': ['index'],
            'y': ['volatility_bbp', 'volatility_kcp', 'volatility_dcp'],
            'line': {
                'color': None
            }
        }

        layout = {
            'title': 'Volatility Width : {}'.format(ticker),
            'xaxis': {
                'title': 'Date'
            },
            'yaxis': {
                'title': 'Value'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_volatility_high_low_band_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Boll Highband Ind.', 'Boll Lowband Ind.', 'KC Highband Ind.', 'KC Lowband Ind.'],
            'x': ['index'],
            'y': ['volatility_bbhi', 'volatility_bbli', 'volatility_kchi', 'volatility_kcli'],
            'line': {
                'color': None
            }
        }
        layout = {
            'title': 'Volatility Hi-Low Bands : {}'.format(ticker),
            'xaxis': {
                'title': 'Date'
            },
            'yaxis': {
                'title': 'Value'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_ulcer_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': ['Ulcer Index'],
            'x': ['index'],
            'y': ['volatility_ui'],
            'line': {
                'color': None
            }
        }

        layout = {'title': 'Ulcer Index : {}'.format(ticker), 'xaxis': {'title': 'Date'}, 'yaxis': {'title': 'Value'}}
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_strength_all_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg.get('ta', None)
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines",
            'name': [
                'RSIIndicator', 'StochRSIIndicator', 'TSIIndicator', 'UltimateOscillator', 'WilliamsRIndicator',
                'AwesomeOscillatorIndicator', 'ROCIndicator'
            ],
            'x': ['index'],
            'y': [
                'momentum_rsi', 'momentum_stoch_rsi', 'momentum_tsi', 'momentum_uo', 'momentum_wr', 'momentum_ao',
                'momentum_roc'
            ],
            'line': {
                'color': None
            }
        }

        layout = {'title': 'Stock : {}'.format(ticker), 'xaxis': {'title': 'Date'}, 'yaxis': {'title': 'Value'}}
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_call_analysis_chart(self, cfg):
        from common.visualization import Visualization
        viz = Visualization()

        df = cfg['df_call_analysis']
        ticker = cfg['ticker']

        cfg_plot_data = {
            'data_source': df,
            'type': 'bar',
            'name': ['Share Holding Ratio'],
            'name_column_legend_groups': ['strike'],
            'x': ['expirationDate'],
            'y': ['effectiveValuePerShare'],
            'text': ['Tooltip']
        }
        layout = {
            'title': 'Share Holding Ratio : {}'.format(ticker),
            'xaxis': {
                'type': 'category',
                'tickangle': -45
            },
            'barmode': 'group'
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data
