import datetime
import json
from common.visualization import Visualization
import pandas as pd


class GoMFieldsCharts:

    def __init__(self):
        pass

    def get_plot_data(self, cfg):
        well_location = self.create_well_location_chart(cfg)
        block_cummulative_production_by_month = self.create_block_cummulative_production_by_month_chart(cfg)
        block_production_rate_by_month = self.create_block_production_rate_by_month_chart(cfg)
        production_by_well = self.create_production_by_well_chart(cfg)
        production_rate_by_well = self.create_production_rate_by_well_chart(cfg)
        well_path_ES = self.create_well_path_ES_chart(cfg)
        well_path = self.create_well_path_chart(cfg)

        plot_data = {
            'well_location': well_location,
            'block_cummulative_production_by_month': block_cummulative_production_by_month,
            'block_production_rate_by_month': block_production_rate_by_month,
            'production_by_well': production_by_well,
            'production_rate_by_well': production_rate_by_well,
            'well_path': well_path,
            'well_path_ES': well_path_ES
        }
        return plot_data

    def create_well_location_chart(self, cfg):
        viz = Visualization()

        df = cfg.get('well_location_and_production', pd.DataFrame())

        sizeref_max = df['Cumulative Oil Production to Date'].max()
        sizeref = viz.get_custom_size_ref(sizeref_max, sizemax=40)

        layout = {
            'title': 'Block Well Locations',
            'xaxis': {
                'title': 'Easting (ft)'
            },
            'yaxis': {
                'title': 'Northing (ft)'
            },
        }

        cfg_plot_data = {
            'data_source': df,
            'mode': 'markers',
            'marker': {
                'size': [],
                'sizemode': 'area',
                'sizeref': sizeref,
                'sizecolumn': 'Cumulative Oil Production to Date',
                'sizerefcolumn': 'Cumulative Oil Production to Date',
                'sizemin': 4,
                'sizemax': 40,
                'color': 'rgb(44, 160, 101)'
            },
            'name': ['Bought'],
            'x': ['Wellhead Rel X'],
            'y': ['Wellhead Rel Y'],
            'text': ['plot_text']
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)

        return plotly_data

    def create_block_cummulative_production_by_month_chart(self, cfg):
        viz = Visualization()

        df_summary = cfg.get('field_summary', pd.DataFrame())
        production = df_summary.production.iloc[0]
        if production != 'NULL':
            df = pd.DataFrame(production)
        else:
            df = pd.DataFrame()

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines+markers",
            'name': ['Cummulative Production'],
            'x': ['PRODUCTION_DATETIME'],
            'y': ['CUMULATIVE_MONTLY_PRODUCTION_MMbbl'],
            'line': {
                'color': None
            }
        }
        layout = {
            'title': 'Cummulative Production by Month',
            'xaxis': {
                'title': 'Date',
            },
            'yaxis': {
                'title': 'Cummulative Production, MMbbl'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_block_production_rate_by_month_chart(self, cfg):
        viz = Visualization()

        df_summary = cfg.get('field_summary', pd.DataFrame())
        production = df_summary.production.iloc[0]
        if production != 'NULL':
            df = pd.DataFrame(production)
        else:
            df = pd.DataFrame()

        cfg_plot_data = {
            'data_source': df,
            'type': "scatter",
            'mode': "lines+markers",
            'name': ['Production Rate'],
            'x': ['PRODUCTION_DATETIME'],
            'y': ['Production Rate, BOPD'],
            'line': {
                'color': None
            }
        }
        layout = {
            'title': 'Monthly Production Rates',
            'xaxis': {
                'title': 'Date',
            },
            'yaxis': {
                'title': 'Production Rate, BOPD'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_production_by_well_chart(self, cfg):
        viz = Visualization()

        df = cfg.get('production_by_well', pd.DataFrame())

        cfg_plot_data = {
            'data_source': df,
            'type': 'bar',
            'name': ['Cummulative Production'],
            'x': ['Well Name'],
            'y': ['O_CUMMULATIVE_PROD_MMBBL'],
            'text': ['plot_text']
        }
        layout = {
            'title': 'Cummulative Production by Well ',
            'xaxis': {
                'type': 'category'
            },
            'yaxis': {
                'title': 'Production, MMbbl'
            }
        }
        cfg_plot_data.update({'layout': layout})

        plotly_data = viz.get_plotly_data(cfg_plot_data)
        return plotly_data

    def create_production_rate_by_well_chart(self, cfg):
        viz = Visualization()
        plotly_group_data = None

        df_group = cfg.get('well_monthly_production', pd.DataFrame())

        for row_index in range(0, len(df_group)):
            name = df_group['Well Name'].iloc[row_index]
            df = pd.DataFrame(df_group['monthly_production'].iloc[row_index])

            cfg_plot_data = {
                'data_source': df,
                'type': "scatter",
                'mode': "lines+markers",
                'name': [name],
                'x': ['date_time'],
                'y': ['O_PROD_RATE_BOPD'],
                'line': {
                    'color': None
                }
            }
            layout = {
                'title': 'Monthly Production Rates',
                'xaxis': {
                    'title': 'Date',
                },
                'yaxis': {
                    'title': 'Production Rate, BOPD'
                }
            }
            cfg_plot_data.update({'layout': layout})

            plotly_data = viz.get_plotly_data(cfg_plot_data)

            if plotly_group_data is None:
                plotly_group_data = json.loads(plotly_data).copy()
            else:
                plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        return plotly_group_data

    def create_well_path_ES_chart(self, cfg):
        viz = Visualization()
        plotly_group_data = {}

        df_group = cfg.get('well_paths', pd.DataFrame())

        for row_index in range(0, len(df_group)):
            name = df_group['Well Name'].iloc[row_index]
            df = pd.DataFrame(df_group['xyz'].iloc[row_index]['data'])

            cfg_plot_data = {
                'data_source': df,
                'type': "scatter",
                'mode': "lines+markers",
                'name': [name],
                'x': ['x'],
                'y': ['y'],
                'z': ['z'],
                'line': {
                    'color': None
                }
            }
            layout = {
                'title': 'Well Path Profiles',
                'xaxis': {
                    'title': 'Easting (ft)',
                },
                'yaxis': {
                    'title': 'Northing (ft)'
                },
                'zaxis': {
                    'title': 'TVD (ft)'
                }
            }
            cfg_plot_data.update({'layout': layout})

            plotly_data = viz.get_plotly_data(cfg_plot_data)

            if len(plotly_group_data) == 0:
                plotly_group_data = json.loads(plotly_data).copy()
            else:
                plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        return plotly_group_data

    def create_well_path_chart(self, cfg):
        viz = Visualization()
        plotly_group_data = {}

        df_group = cfg.get('well_paths', pd.DataFrame())

        for row_index in range(0, len(df_group)):
            name = df_group['Well Name'].iloc[row_index]
            df = pd.DataFrame(df_group['xyz'].iloc[row_index]['data'])

            cfg_plot_data = {
                'data_source': df,
                'type': "scatter3d",
                'mode': "lines",
                'name': [name],
                'x': ['x'],
                'y': ['y'],
                'z': ['z'],
                'line': {
                    'color': None
                }
            }
            layout = {
                'title': 'Well Path Profiles',
                'width': 900,
                'height': 900,
                'xaxis': {
                    'title': 'Easting (ft)',
                },
                'yaxis': {
                    'title': 'Northing (ft)'
                },
                'zaxis': {
                    'title': 'TVD (ft)',
                    'autorange': "reversed"
                }
            }
            cfg_plot_data.update({'layout': layout})

            plotly_data = viz.get_plotly_data(cfg_plot_data)

            if len(plotly_group_data) == 0:
                plotly_group_data = json.loads(plotly_data).copy()
            else:
                plotly_group_data['data'] = plotly_group_data['data'] + json.loads(plotly_data)['data']

        return plotly_group_data
