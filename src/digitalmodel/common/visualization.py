import logging


class Visualization():

    def __init__(self, cfg=None):
        self.plot_data = {'data': [], 'layout': {}}
        self.colors = self.get_colors()
        self.current_color_index = 0
        if cfg is not None:
            self.cfg = cfg

            self.default_cfg = {
                'data_source': None,
                'type': "scatter",
                'mode': "lines",
                'name': None,
                'x': [],
                'y': [],
                'line': {
                    'color': None
                }
            }

        self.default_layout = {
            'title': 'Title',
            'xaxis': {
                'title': 'X Label'
            },
            'yaxis': {
                'title': 'Y Label'
            }
        }

    def get_plotly_data(self, cfg):
        import json

        import plotly
        if cfg is not None:
            self.cfg = cfg
        elif self.cfg is None:
            self.cfg = self.default_cfg

        if not self.cfg.__contains__('name_column_legend_groups'):
            plot_data = self.assign_simple_data_by_DataFrame_source()
            # data_source = type(self.cfg['data_source'])
            # self.assign_simple_data_by_array_source()
            self.plot_data['data'] = plot_data
        else:
            self.assign_grouped_data()
        self.assign_layout()

        if format in cfg.keys() and cfg['format'] == 'json':
            plotly_data = json.dumps(self.plot_data,
                                     cls=plotly.utils.PlotlyJSONEncoder)
        else:
            plotly_data = self.plot_data

        return plotly_data

    def assign_simple_data_by_array_source(self):
        pass

    def assign_simple_data_by_DataFrame_source(self):

        def get_x_data(df, column):
            data = []
            if column in df.columns.to_list():
                data = df[column]
            elif column == 'index':
                data = df.index.to_list()
            else:
                logging.debug("X Data does not exist")
                raise ("X Data does not exist")
            return data

        def get_y_data(df, column):
            data = []
            if column in df.columns.to_list():
                data = df[column]
            elif column == 'index':
                data = df.index.to_list()
            else:
                logging.debug("Y Data does not exist")
                raise ("Y Data does not exist")
            return data

        def get_z_data(df, column):
            data = []
            if column in df.columns.to_list():
                data = df[column]
            elif column == 'index':
                data = df.index.to_list()
            else:
                logging.debug("Z Data does not exist")
                raise ("Z Data does not exist")
            return data

        def get_text_data(df, column):
            data = []
            if column is not None:
                if column in df.columns.to_list():
                    data = df[column]
                elif column == 'index':
                    data = df.index.to_list()
                else:
                    logging.debug("Text Data does not exist")
                    raise ("Text Data does not exist")

            return data

        def get_data_name(cfg):
            pass

        def get_data_item(cfg,
                          x_data,
                          y_data,
                          z_data,
                          series_name,
                          text_data=[]):
            cfg.update({
                'x': x_data,
                'y': y_data,
                'z': z_data,
                'name': series_name
            })
            if cfg.__contains__('text'):
                cfg.update({'text': text_data})
            return cfg

        def get_data_cfg():
            import copy
            cfg = copy.deepcopy(self.cfg)
            keys_to_drop = ['data_source', 'x', 'y']
            for key in keys_to_drop:
                if key in cfg:
                    del cfg[key]
            return cfg

        try:
            plot_data = []
            x_data = []
            y_data = []
            z_data = []

            df = self.cfg['data_source']

            if len(self.cfg['x']) == 1:
                x_column = self.cfg['x'][0]
                x_data = get_x_data(df, column=x_column)

            if len(self.cfg['y']) >= 1:
                for y_data_index in range(0, len(self.cfg['y'])):
                    y_column = self.cfg['y'][y_data_index]
                    y_data = get_y_data(df, y_column)
                    if self.cfg.__contains__('z'):
                        z_column = self.cfg['z'][y_data_index]
                        z_data = get_z_data(df, z_column)
                    if self.cfg.__contains__('text'):
                        text_column = self.cfg['text'][y_data_index]
                    else:
                        text_column = None
                    text_data = get_text_data(df, text_column)
                    series_name = self.cfg['name'][y_data_index]
                    cfg_temp = get_data_cfg()
                    if cfg_temp.__contains__('marker') and cfg_temp[
                            'marker'].__contains__('sizerefcolumn'):
                        sizecolumn = cfg_temp['marker'].get('sizecolumn', None)
                        sizedata = get_y_data(df, sizecolumn)
                        sizerefcolumn = cfg_temp['marker'].get(
                            'sizerefcolumn', None)
                        sizerefdata = get_y_data(df, sizerefcolumn)
                    else:
                        sizerefdata = []
                        sizedata = []
                    cfg_data = self.assign_custom_properties(
                        cfg_temp, sizedata, sizerefdata)
                    data = get_data_item(cfg_data, x_data, y_data, z_data,
                                         series_name, text_data)
                    plot_data.append(data.copy())

            return plot_data
        except:
            logging.debug("Data does not exist")

    def assign_grouped_data(self):
        try:
            df = self.cfg['data_source']
            groupby_columns = self.cfg['name_column_legend_groups']
            grouped_df = df.groupby(groupby_columns)
            names = list(grouped_df.groups.keys())
            plot_type = self.cfg['type']

            for name in names:
                x_data = grouped_df.get_group(name)[self.cfg['x'][0]]
                y_data = grouped_df.get_group(name)[self.cfg['y'][0]]
                data = {
                    'x': x_data,
                    'y': y_data,
                    'name': name,
                    'type': plot_type
                }
                if 'text' in list(df.columns):
                    text_data = grouped_df.get_group(name)[self.cfg['y'][0]]
                    data.update({'text': text_data})
                self.plot_data['data'].append(data.copy())
        except:
            logging.debug("Data does not exist")

    def assign_custom_properties(self, cfg, sizedata, sizerefdata):
        cfg = self.get_cfg_with_custom_color(cfg, sizedata)
        cfg = self.get_cfg_with_custom_marker(cfg, sizedata, sizerefdata)
        return cfg

    def get_cfg_with_custom_marker(self, cfg, sizedata, sizerefdata):
        if cfg.__contains__('marker'):
            marker = cfg.get('marker', None)
            marker['size'] = sizedata
            sizeref = marker.get('sizeref', None)
            if sizeref is None:
                sizemax = marker['sizemax']
                sizeref_max = sizerefdata.max()
                marker['sizeref'] = self.get_custom_size_ref(
                    sizeref_max, sizemax)
                cfg['marker'] = marker

        return cfg

    def get_cfg_with_custom_color(self, cfg, sizedata):
        color = None
        if 'marker' in self.cfg:
            color = self.cfg['marker'].get('color', None)
        if 'line' in self.cfg:
            color = self.cfg['line'].get('color', None)
        if color is None:
            color = self.colors[self.current_color_index]
            self.current_color_index = self.current_color_index + 1

        if cfg.__contains__('line'):
            cfg['line']['color'] = color
        elif cfg.__contains__('marker'):
            cfg['marker']['color'] = color

        return cfg

    def assign_layout(self):
        layout = self.cfg.get('layout', self.default_layout)
        self.plot_data['layout'] = layout

    def get_colors(self, n=15):
        from webcolors import rgb_to_hex
        if n <= 8:
            colors = [
                "#75968f", "#a5bab7", "#c9d9d3", "#e2e2e2", "#dfccce",
                "#ddb7b1", "#cc7878", "#933b41", "#550b1d"
            ]
        else:
            # Tableau 20 Colors
            colors = [(31, 119, 180), (174, 199, 232), (255, 127, 14),
                      (255, 187, 120), (44, 160, 44), (152, 223, 138),
                      (214, 39, 40), (255, 152, 150), (148, 103, 189),
                      (197, 176, 213), (140, 86, 75), (196, 156, 148),
                      (227, 119, 194), (247, 182, 210), (127, 127, 127),
                      (199, 199, 199), (188, 189, 34), (219, 219, 141),
                      (23, 190, 207), (158, 218, 229)]
            colors = [rgb_to_hex(color) for color in colors]
        return colors

    def get_custom_size_ref(self, sizeref_max, sizemax=40):
        sizeref = 2 * sizeref_max / (sizemax**2)

        return sizeref
