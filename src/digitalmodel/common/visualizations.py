class Visualization():

    def __init__(self, plt_settings=None):
        import matplotlib
        # matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        self.plt = plt
        self.plot_object = self.plt
        self.fig = self.plt.gcf()
        self.polar_fig = None
        self.current_ax = None
        self.cfg_mult = None
        self.fig_suptitle_flag = False
        if plt_settings != None:
            self.plt_settings = plt_settings
            self.set_plot_size_orientation()

    def generate_time_line(self, data, plt_settings=None):
        """Generates timeline from a pandas dataframe
        data : Pandas dataframe with datetimeindex
        titile  Title of the plot
        zaxis_format ="%d %b %Y" valids strdate format
        dayinterval =1 default, can be anything
        Insipred from Matplotlib's excellent examples
        author: sukhbinder
        date 5/6/2018
        """
        import matplotlib
        # matplotlib.use('Agg')
        import matplotlib.dates as mdates
        import matplotlib.pyplot as plt
        import numpy as np

        self.plt = plt

        levels = np.array([-5, 5, -3, 3, -1, 1])
        self.fig, ax = self.plt.subplots(figsize=(plt_settings['size']['length'], plt_settings['size']['height']))
        # Create the base line
        start = min(data.index)
        stop = max(data.index)
        ax.plot((start, stop), (0, 0), 'k', alpha=.5)
        # Iterate through data annoting each one
        for ii, (idate, iname) in enumerate(data.itertuples()):
            level = levels[ii % 6]
            vert = 'top' if level < 0 else 'bottom'

            ax.scatter(idate, 0, s=100, facecolor='w', edgecolor='k', zorder=9999)
            # Plot a line up to the text
            ax.plot((idate, idate), (0, level), c='r', alpha=.7)
            # Give the text a faint background and align it properly
            ax.text(idate, level, iname, ha='right', va=vert, fontsize=14, backgroundcolor=(1., 1., 1., .3))
        ax.set(title=plt_settings['title'])
        # Set the xticks formatting
        # format xaxis with days intervals
        ax.get_xaxis().set_major_locator(mdates.DayLocator(interval=plt_settings['day_interval']))
        ax.get_xaxis().set_major_formatter(mdates.DateFormatter(plt_settings['xaxis_format']))
        self.fig.autofmt_xdate()
        # Remove components for a cleaner look
        self.plt.setp((ax.get_yticklabels() + ax.get_yticklines() + list(ax.spines.values())), visible=False)

        self.plt.savefig(plt_settings['file_name'], dpi=800)
        return ax

    def from_df_array(self, df_array, plt_settings=None):
        import matplotlib
        # matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        self.plt = plt
        plot_type = 'from_df_array'

        for df_index in range(0, len(df_array)):
            if len(df_array[df_index]) > 0:
                plt_settings['label'] = plt_settings['array_label'][df_index]
                self.from_df_columns(df_array[df_index], plt_settings)

    def from_df_columns(self, df, plt_settings=None):
        sample_plt_settings = {}
        if plt_settings == None:
            plt_settings = self.plt_settings
        else:
            self.plt_settings = plt_settings

        if len(plt_settings['x']) == 1:
            x = df[plt_settings['x'][0]]
            for column_index in range(0, len(plt_settings['y'])):
                y = df[plt_settings['y'][column_index]]
                label = plt_settings['label'][column_index]
                if plt_settings.__contains__('linewidth'):
                    linewidth = plt_settings['linewidth']
                else:
                    linewidth = 0.5
                if plt_settings.__contains__('color'):
                    color = plt_settings['color']
                else:
                    color = None
                if plt_settings.__contains__('linestyle'):
                    linestyle = plt_settings['linestyle']
                else:
                    linestyle = None

                markerfacecolor = None
                markersize = None
                if plt_settings.__contains__('marker'):
                    if (plt_settings['marker'] is not None) and (plt_settings['marker'].__contains__('edge_color')):
                        markerfacecolor = plt_settings['marker']['edge_color']
                    if plt_settings.__contains__('color'):
                        markerfacecolor = plt_settings['color']
                    if plt_settings['marker'] is not None and plt_settings['marker'].__contains__('size'):
                        markersize = plt_settings['marker']['size']
                    else:
                        markersize = 2

                if not plt_settings.__contains__('plt_kind'):
                    self.plot_object.plot(x, y, label=label)
                elif plt_settings['plt_kind'] == 'line':
                    if self.current_ax is None:
                        self.plot_object.plot(x, y, label=label)
                    else:
                        if (not plt_settings.__contains__('marker')) or (plt_settings['marker'] is None):
                            self.plot_object.plot(x,
                                                  y,
                                                  label=label,
                                                  linewidth=linewidth,
                                                  linestyle=linestyle,
                                                  color=color)
                        else:
                            self.plot_object.plot(x,
                                                  y,
                                                  label=label,
                                                  linewidth=linewidth,
                                                  linestyle=linestyle,
                                                  color=color,
                                                  marker=plt_settings['marker']['type'],
                                                  markersize=markersize,
                                                  markerfacecolor=markerfacecolor)
                elif plt_settings['plt_kind'] == 'scatter':
                    try:
                        # https://jakevdp.github.io/PythonDataScienceHandbook/04.02-simple-scatter-plots.html
                        self.plot_object.scatter(x, y, label=label, s=markersize)
                        self.annotate(df, x, y)
                    except Exception as e:
                        print(e)
                        if 'datetime.date' in str(e):
                            import sys
                            print(
                                "Error: When using datetime as X-axis, consider changing plot kind from scatter to line "
                            )
                            sys.exit()

                elif plt_settings['plt_kind'] == 'polar':
                    # Radial line
                    self.plot_object.polar(x, y, label=label)
                elif plt_settings['plt_kind'] == 'polar_scatter':
                    # Radial scatter
                    import plotly.express as px
                    self.plot_object = px.scatter_polar(df,
                                                        r=plt_settings['y'][column_index],
                                                        theta=plt_settings['x'][0])

                elif plt_settings['plt_kind'] == 'bar':
                    if len(plt_settings['y']) == 1:
                        self.plot_object.bar(x, y, label=label)
                    else:
                        import numpy as np
                        no_of_bar_groups = len(x)
                        ind = np.arange(no_of_bar_groups)
                        width = 0.3
                        self.plot_object.bar(ind + width * column_index, y, label=label)
                        self.plot_object.xticks(ind + width / 2, x)

        if len(plt_settings['y']) == 1 and len(plt_settings['x']) > 1:
            for column_index in range(0, len((plt_settings['x']))):
                if not plt_settings.__contains__('plt_kind'):
                    self.plot_object.plot(df[plt_settings['x'][column_index]],
                                          df[plt_settings['y'][0]],
                                          label=plt_settings['label'][column_index])
                elif plt_settings['plt_kind'] == 'line':
                    self.plot_object.plot(df[plt_settings['x'][column_index]],
                                          df[plt_settings['y'][0]],
                                          label=plt_settings['label'][column_index])
                elif plt_settings['plt_kind'] == 'bar':
                    self.plot_object.bar(df[plt_settings['x'][column_index]],
                                         df[plt_settings['y'][0]],
                                         label=plt_settings['label'][column_index])

        self.add_x_y_scale_formats()
        self.add_x_y_lim_formats()

    def set_plot_size_orientation(self):
        if self.plt_settings['size'] == 'A4' and self.plt_settings['orientation'] == 'landscape':
            self.fig.set_size_inches(11.69, 8.27)
        elif self.plt_settings['size'] == 'A4' and self.plt_settings['orientation'] == 'portrait':
            self.fig.set_size_inches(8.27, 11.69)
        elif self.plt_settings['size'] == 'A3' and self.plt_settings['orientation'] == 'landscape':
            self.fig.set_size_inches(16.53, 11.69)
        elif self.plt_settings['size'] == 'A3' and self.plt_settings['orientation'] == 'portrait':
            self.fig.set_size_inches(11.69, 16.53)
        elif self.plt_settings['size'] == 'half_letter' and self.plt_settings['orientation'] == 'landscape':
            self.fig.set_size_inches(8.5, 5.5)
        elif self.plt_settings['size'] == 'half_letter' and self.plt_settings['orientation'] == 'portrait':
            self.fig.set_size_inches(5.5, 8.5)

    def from_df_get_plot(self, df, plt_settings):
        # TODO Rename function to 'from_df'
        import matplotlib
        # matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        self.plt = plt
        self.plt_settings = plt_settings

        if self.plt_settings['marker'] != None:
            df.plot(kind=self.plt_settings['plt_kind'],
                    marker=self.plt_settings['marker']['type'],
                    markersize=self.plt_settings['marker']['size'],
                    markerfacecolor=self.plt_settings['marker']['edge_color'])
        else:
            df.plot(kind=self.plt_settings['plt_kind'], rot=0)

        if self.plt_settings['ylim'] != None:
            self.plt.ylim(self.plt_settings['ylim'])

        self.add_x_y_scale_formats()

        self.save_and_close()

    def add_x_y_scale_formats(self):
        if self.plt_settings.__contains__('yscale'):
            if self.plt_settings['yscale']['log']:
                self.plt.yscale('log')
        if self.plt_settings.__contains__('xscale'):
            if self.plt_settings['xscale']['log']:
                self.plt.xscale('log')

    def add_x_y_lim_formats(self):
        if self.plt_settings.__contains__('ylim'):
            if self.plt_settings['ylim'] != None:
                if self.cfg_mult is None:
                    self.plot_object.ylim(self.plt_settings['ylim'])
                else:
                    self.plot_object.set_ylim(self.plt_settings['ylim'])
        if self.plt_settings.__contains__('xlim'):
            if self.plt_settings['xlim'] != None:
                if self.cfg_mult is None:
                    self.plot_object.xlim(self.plt_settings['xlim'])
                else:
                    self.plot_object.set_xlim(self.plt_settings['xlim'])

    def resolution_adjust(self, ax=None):
        """
        Send in an axis
        """
        import numpy as np
        if ax is None:
            ax = self.current_ax

        if self.plt_settings.__contains__('xres'):
            xres = self.plt_settings['xres']
            start, stop = ax.get_xlim()
            ticks = np.arange(start, stop + xres, xres)
            ax.set_xticks(ticks)
        if self.plt_settings.__contains__('yres'):
            yres = self.plt_settings['yres']
            start, stop = ax.get_ylim()
            ticks = np.arange(start, stop + yres, yres)
            ax.set_yticks(ticks)

    def save_and_close(self):
        if self.cfg_mult is not None:
            file_name = self.cfg_mult['file_name']
            self.fig.savefig(file_name, dpi=500)
        else:
            file_name = self.plt_settings['file_name']
            try:
                self.plt.savefig(file_name, dpi=800)
            except:
                self.plt.savefig(file_name, dpi=100)
            self.plt.close()
            if self.polar_fig is not None:
                self.polar_fig.write_image(file_name)

    def gist1(self):
        pass
        # fig, ax = plt.subplots(figsize=(10, 5))
        # df_unstack = grouped_items.size().unstack()
        # # color_list = self.color_lists(len(df_unstack.columns))
        # color_list = self.color_lists(len(df_unstack.columns))
        # pos = list(range(len(df_unstack)))
        # width = 1/(len(df_unstack.columns)+1)
        # for column_index in range(0, len(df_unstack.columns)):
        #     column = df_unstack.columns[column_index]
        #     plt.bar([p + width*column_index for p in pos], df_unstack[column], width,
        #             alpha=0.5, color=color_list[column_index],label=column)
        # ax.set_ylabel(cfg['Analysis']['group_by'][group_by_index]['ylabel'])
        # ax.set_title(cfg['Analysis']['group_by'][group_by_index]['title'])
        # ax.set_xticks([p + len(df_unstack.columns)/2 * width for p in pos])
        # plt.xlim(min(pos), max(pos) + width * len(df_unstack.columns))
        # if cfg['Analysis']['group_by'][group_by_index]['xticklabels'] is None:
        #     ax.set_xticklabels(df_unstack.index)
        # else:
        #     ax.set_xticklabels(cfg['Analysis']['group_by'][group_by_index]['xticklabels'])
        # # plt.ylim([0, max(grouped_items.size().unstack()[column])])
        # plt.legend(df_unstack.columns, loc='best')
        # plt.grid()
        # self.plt.savefig(cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'] +'_' + '{0}'.format(group_by_index) + '.png',
        #                  dpi=800)
        # self.plt.close()

    def add_title_and_axis_labels(self, plt_settings=None):
        sample_plt_settings = {}
        if plt_settings == None:
            plt_settings = self.plt_settings
        else:
            self.plt_settings = plt_settings

        if self.plt_settings.__contains__('suptitle'):
            self.plot_object.suptitle(self.plt_settings['suptitle'], fontsize=12, fontweight='bold', color='black')
        if self.cfg_mult is not None:
            if not self.fig_suptitle_flag:
                self.fig.suptitle(self.cfg_mult['suptitle'], fontsize=12, fontweight='bold', color='black')
                # self.fig_suptitle_flag = True

        if self.plt_settings.__contains__('title'):
            if self.cfg_mult is None:
                self.plot_object.title(self.plt_settings['title'], fontsize=11, fontweight='bold', color='black')
            else:
                self.plot_object.set_title(self.plt_settings['title'], fontsize=10, color='black')

        try:
            ax = self.plot_object.axes()
            if max(ax.get_yticks().tolist()) < 0.001:
                ax.set_yticklabels(['{:.1e}'.format(item) for item in ax.get_yticks().tolist()])
        except:
            print("Axis not formatted")
        try:
            ax = self.plot_object.axes()
            if max(ax.get_xticks().tolist()) < 0.001:
                ax.set_xticklabels(['{:.1e}'.format(item) for item in ax.get_xticks().tolist()])
        except:
            print("Axis not formatted")

        if self.cfg_mult is None:
            self.plot_object.xlabel(self.plt_settings['xlabel'], fontsize=8, fontweight='bold', color='black')
            self.plot_object.ylabel(self.plt_settings['ylabel'], fontsize=8, fontweight='bold', color='black')
        else:
            self.plot_object.set(xlabel=self.plt_settings['xlabel'], ylabel=self.plt_settings['ylabel'])
            self.plot_object.label_outer()

        self.axis_autoformat()

        invert_xaxis = self.plt_settings.get('invert_xaxis', False)
        if invert_xaxis:
            self.plt.gca().invert_xaxis()
        invert_yaxis = self.plt_settings.get('invert_yaxis', False)
        if invert_yaxis:
            self.plt.gca().invert_yaxis()

        if self.plt_settings['grid']:
            if self.cfg_mult is None:
                self.plt.grid()
            else:
                self.plot_object.grid()

        if self.plt_settings.__contains__('grid_minor'):
            if self.plt_settings['grid_minor']['x']['flag']:
                if self.cfg_mult is None:
                    self.plt.grid(which='minor', linestyle='--', axis='x')
                else:
                    self.plot_object.grid(which='minor', linestyle='--', axis='x')
            elif self.plt_settings['grid_minor']['y']['flag']:
                if self.cfg_mult is None:
                    self.plt.grid(which='minor', linestyle='--', axis='y')
                else:
                    self.plot_object.grid(which='minor', linestyle='--', axis='y')

            self.plt.minorticks_on()

    def add_legend(self):
        if self.plt_settings.__contains__('legend') and self.plt_settings['legend']:
            if self.plt_settings.__contains__('legend_location'):
                legend_location = self.plt_settings['legend_location']
            else:
                legend_location = None

            if legend_location == "lower center":
                if self.cfg_mult is None:
                    self.fig = self.plt.gcf()
                    self.plt.legend(bbox_to_anchor=(0.5, 0),
                                    loc=legend_location,
                                    bbox_transform=self.fig.transFigure,
                                    ncol=5,
                                    fontsize=8)
                    self.fig.subplots_adjust(bottom=0.2)
                else:
                    self.plot_object.legend(loc='best', fontsize=8)
            elif (legend_location is None) or (legend_location == "best"):
                self.plot_object.legend(loc='best', fontsize=8)

    def axis_autoformat(self):
        if self.plt_settings.__contains__('autofmt'):
            if self.cfg_mult is None:
                if self.plt_settings['autofmt']['xdate']:
                    self.plot_object.gcf().autofmt_xdate()
                elif self.plt_settings['autofmt']['ydate']:
                    self.plot_object.gcf().autofmt_ydate()
            else:
                if self.plt_settings['autofmt']['xdate']:
                    # self.fig.autofmt_xdate(bottom=0.2, rotation=30, ha='right', which=None)
                    self.fig.autofmt_xdate(bottom=0.2, rotation=30, ha='right')
                    try:
                        self.plot_object.locator_params(nbins=10, axis='x')
                    except Exception as e:
                        print("Error encountered while autoformat : {}".format(e))

    def add_text_fields(self):
        if self.plt_settings.__contains__('text_fields'):
            for text_fields_index in range(0, len(self.plt_settings['text_fields'])):
                text_field = self.plt_settings['text_fields'][text_fields_index]
                if text_field.__contains__('color') and (text_field['color'] is not None):
                    color = text_field['color']
                else:
                    color = 'k'
                if self.cfg_mult is None:
                    self.plt.text(text_field['x'], text_field['y'], text_field['text'], color=color)
                else:
                    self.plot_object.text(text_field['x'],
                                          text_field['y'],
                                          text_field['text'],
                                          color=color,
                                          fontsize=12)

    def annotate(self, df, x, y):
        if self.plt_settings.__contains__('annotate'):
            annotate_label_column = self.plt_settings['annotate']['column']
            if annotate_label_column is None:
                label_column = x
            else:
                label_column = df[annotate_label_column]
            for xa, ya, label in zip(x, y, label_column):
                if annotate_label_column is None:
                    label = "({:.1f}, {:.1f})".format(xa, ya)

                self.plt.annotate(
                    label,  # this is the text
                    (xa, ya),  # this is the point to label
                    textcoords="offset points",  # how to position the text
                    xytext=(0, 10),  # distance from text to points (x,y)
                    ha='center')  # horizontal alignment can be left, right or center

    def add_reference_lines_and_spans(self):
        if self.plt_settings.__contains__('axhline'):
            self.plt.ayvline(y=self.plt_settings['axhline'], color='r', dashes=[4, 2])
        if self.plt_settings.__contains__('ayhline'):
            self.plt.axhline(y=self.plt_settings['ayhline'], color='r', dashes=[4, 2])

        if self.plt_settings.__contains__('axvspan'):
            facecolor = self.plt_settings['axvspan']['facecolor']
            alpha = self.plt_settings['axvspan']['alpha']
            label = self.plt_settings['axvspan']['label']
            x_datatype = self.plt_settings['axvspan']['x_datatype']
            x0 = self.plt_settings['axvspan']['x'][0]
            x1 = self.plt_settings['axvspan']['x'][1]

            if x_datatype == 'datetime':
                from matplotlib.dates import date2num
                x0 = date2num(x0)
                x1 = date2num(x1)

            self.plt.axvspan(x0, x1, facecolor=facecolor, alpha=alpha, label=label)
            self.plt.axes().legend()

    def multiple_plots(self, cfg_mult):
        self.cfg_mult = cfg_mult
        if self.cfg_mult.__contains__('nrows'):
            self.nrows = self.cfg_mult['nrows']
        else:
            self.nrows = len(self.cfg_mult['sets'])
        if self.cfg_mult.__contains__('ncols'):
            self.ncols = self.cfg_mult['ncols']
        else:
            self.ncols = 1

        self.fig, self.ax = self.plt.subplots(nrows=self.nrows,
                                              ncols=self.ncols,
                                              sharex=True,
                                              figsize=(8, self.nrows * 4))

    def autoselect_current_subplot(self, plt_index):
        self.mult_row = plt_index // self.ncols
        self.mult_column = plt_index % self.ncols
        if self.nrows > 1 or self.ncols > 1:
            ax_shape = self.ax.shape
            if len(ax_shape) > 1:
                self.current_ax = self.ax[self.mult_row][self.mult_column]
            else:
                self.current_ax = self.ax[self.mult_row]
        else:
            self.current_ax = self.ax

        self.plot_object = self.current_ax

    def plot_subplot(self, df, subplot_settings):
        self.from_df_columns(df, subplot_settings)

    def color_lists(self, n=15):
        from webcolors import rgb_to_hex
        if n <= 8:
            colors = [
                "#75968f", "#a5bab7", "#c9d9d3", "#e2e2e2", "#dfccce", "#ddb7b1", "#cc7878", "#933b41", "#550b1d"
            ]
        else:
            # Tableau 20 Colors
            colors = [(31, 119, 180), (174, 199, 232), (255, 127, 14), (255, 187, 120), (44, 160, 44), (152, 223, 138),
                      (214, 39, 40), (255, 152, 150), (148, 103, 189), (197, 176, 213), (140, 86, 75), (196, 156, 148),
                      (227, 119, 194), (247, 182, 210), (127, 127, 127), (199, 199, 199), (188, 189, 34),
                      (219, 219, 141), (23, 190, 207), (158, 218, 229)]
            colors = [rgb_to_hex(color) for color in colors]
        return colors

    def orcaflex_range_plot(self, RangeAllFiles, cfg):
        import matplotlib
        # matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        self.plt = plt
        self.plt_settings = {}
        for RangeGraphIndex in range(0, len(cfg['RangeGraph'])):
            self.plt_settings['title'] = cfg['PlotSettings']['SupTitle']
            self.plt_settings['suptitle'] = cfg['RangeGraph'][RangeGraphIndex]['Title']

            for fileIndex in range(0, len(cfg['Files'])):
                x = RangeAllFiles[fileIndex][RangeGraphIndex]['X']
                VariableName = cfg['RangeGraph'][RangeGraphIndex]['Variable']
                y = RangeAllFiles[fileIndex][RangeGraphIndex][VariableName]
                label = cfg['Files']['data'][fileIndex]['Label']

                self.plt.plot(x, y, label=label)

            self.plt_settings['xlabel'] = cfg['RangeGraph'][RangeGraphIndex]['xlabel']
            self.plt_settings['ylabel'] = cfg['RangeGraph'][RangeGraphIndex]['ylabel']
            FileName = cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'] + '_' + \
                       cfg['RangeGraph'][RangeGraphIndex]['FileName']

            if cfg['RangeGraph'][RangeGraphIndex]['axhline'] != None:
                self.plt.axhline(y=cfg['RangeGraph'][RangeGraphIndex]['axhline'], color='r', dashes=[4, 2])

            self.plt_settings['grid'] = True
            self.add_title_and_axis_labels(self.plt_settings)
            self.plt.legend(fontsize=8)
            try:
                self.plt_settings['ylim'] = cfg['RangeGraph'][RangeGraphIndex]['ylim']
                self.plt.ylim(self.plt_settings['ylim'])
            except:
                pass

            self.plt.savefig(FileName, dpi=800)
            self.plt.close()

        print("Saved {0} Range Graphs".format(len(cfg['RangeGraph'])))

    def example_group_bar_plot1_gist(self):
        import matplotlib.pyplot as plt
        import pandas as pd

        raw_data = {
            'first_name': ['Jason', 'Molly', 'Tina', 'Jake', 'Amy'],
            'pre_score': [4, 24, 31, 2, 3],
            'mid_score': [25, 94, 57, 62, 70],
            'post_score': [5, 43, 23, 23, 51]
        }
        df = pd.DataFrame(raw_data, columns=['first_name', 'pre_score', 'mid_score', 'post_score'])

        # Setting the positions and width for the bars
        pos = list(range(len(df['pre_score'])))
        width = 0.25

        # Plotting the bars
        fig, ax = plt.subplots(figsize=(10, 5))
        plt.bar(
            pos,
            # using df['pre_score'] data,
            df['pre_score'],
            # of width
            width,
            # with alpha 0.5
            alpha=0.5,
            # with color
            color='#EE3224',
            # with label the first value in first_name
            label=df['first_name'][0])

        # Create a bar with mid_score data,
        # in position pos + some width buffer,
        plt.bar(
            [p + width for p in pos],
            # using df['mid_score'] data,
            df['mid_score'],
            # of width
            width,
            # with alpha 0.5
            alpha=0.5,
            # with color
            color='#F78F1E',
            # with label the second value in first_name
            label=df['first_name'][1])

        # Create a bar with post_score data,
        # in position pos + some width buffer,
        plt.bar(
            [p + width * 2 for p in pos],
            # using df['post_score'] data,
            df['post_score'],
            # of width
            width,
            # with alpha 0.5
            alpha=0.5,
            # with color
            color='#FFC222',
            # with label the third value in first_name
            label=df['first_name'][2])

        # Set the y axis label
        ax.set_ylabel('Score')

        # Set the chart's title
        ax.set_title('Test Subject Scores')

        # Set the position of the x ticks
        ax.set_xticks([p + 1.5 * width for p in pos])

        # Set the labels for the x ticks
        ax.set_xticklabels(df['first_name'])

        # Setting the x-axis and y-axis limits
        plt.xlim(min(pos) - width, max(pos) + width * 4)
        plt.ylim([0, max(df['pre_score'] + df['mid_score'] + df['post_score'])])

        # Adding the legend and showing the plot
        plt.legend(['Pre Score', 'Mid Score', 'Post Score'], loc='upper left')
        plt.grid()
        plt.show()

    def plotCustomFatigue(self, DF, data):
        import matplotlib
        # matplotlib.use('Agg')
        import matplotlib.pyplot as plt

        # Define plot settings
        plt.suptitle(data['PltSupTitle'], fontsize=14, color='black')
        #it creates plot Sub-tittle, color and size
        plt.title(data['PltTitle'], fontsize=12, color='black')
        #it creates plot X-axis name, fontsize and colour
        plt.xlabel(data['PltXLabel'], fontsize=12, color='black')
        #it creates plot Y-axis name, fontsize and colour
        plt.ylabel(data['PltYLabel'], fontsize=12, color='black')
        plt.yscale('log')

        UniqueSNCurves = DF['S-N Curve'].unique()
        for SNCurveIndex in range(0, len(UniqueSNCurves)):
            plt.plot(DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][0]], \
             DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][1]], label=data['Label'][SNCurveIndex])
        # plt.ylim(data['YLim'])
        plt.legend()
        #     plt.legend(loc='best', bbox_to_anchor=(0.5, -0.05),
        #             fancybox=True, shadow=True, ncol=5)
        plt.axhline(y=data['Axhline'], color='r', dashes=[4, 2])
        for TextFieldsIndex in range(0, len(data['TextFields'])):
            plt.text(data['TextFields'][TextFieldsIndex]['x'],
                     data['TextFields'][TextFieldsIndex]['y'],
                     data['TextFields'][TextFieldsIndex]['Text'],
                     color='b')
        # plt.axvline(x=cfg['design']['wallThickness'], color='m',dashes=[4, 2])
        # plt.text(10000,'Thickness Fabrication Limit : {0} inch' .format(cfg['design']['wallThickness']), color='m', rotation=90)
        plt.grid()
        for plotIndex in range(0, len(data['XLimRanges'])):
            plt.xlim(data['XLimRanges'][plotIndex])
            plt.ylim(data['YLimRanges'][plotIndex])
            plt.savefig(data['FileName'] + '_' + str(plotIndex) + '.png', dpi=800)

        plt.close()

    def plotCustomFatigueComparison(self, DFArray, data, cfg):
        import matplotlib
        # matplotlib.use('Agg')
        import matplotlib.pyplot as plt

        # Define plot settings
        plt.suptitle(data['PltSupTitle'], fontsize=14, color='black')
        #it creates plot Sub-tittle, color and size
        plt.title(data['PltTitle'], fontsize=12, color='black')
        #it creates plot X-axis name, fontsize and colour
        plt.xlabel(data['PltXLabel'], fontsize=12, color='black')
        #it creates plot Y-axis name, fontsize and colour
        plt.ylabel(data['PltYLabel'], fontsize=12, color='black')
        plt.yscale('log')

        for fileIndex in range(0, len(DFArray)):
            DF = DFArray[fileIndex]
            UniqueSNCurves = DF['S-N Curve'].unique()
            for SNCurveIndex in range(0, len(UniqueSNCurves)):
                plt.plot(DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][0]], \
                DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][1]], \
                label=cfg['FatigueLifeReadingSets'][fileIndex]['Label'][SNCurveIndex], linestyle = data['LineStyles'][SNCurveIndex], color = data['Colors'][fileIndex])

        plt.legend(fontsize=8)
        #     plt.legend(loc='best', bbox_to_anchor=(0.5, -0.05),
        #             fancybox=True, shadow=True, ncol=5)
        plt.axhline(y=data['Axhline'], color='r', dashes=[4, 2])
        for TextFieldsIndex in range(0, len(data['TextFields'])):
            plt.text(data['TextFields'][TextFieldsIndex]['x'],
                     data['TextFields'][TextFieldsIndex]['y'],
                     data['TextFields'][TextFieldsIndex]['Text'],
                     color='b')
        # plt.axvline(x=cfg['design']['wallThickness'], color='m',dashes=[4, 2])
        # plt.text(10000,'Thickness Fabrication Limit : {0} inch' .format(cfg['design']['wallThickness']), color='m', rotation=90)
        plt.grid()
        for plotIndex in range(0, len(data['XLimRanges'])):
            plt.xlim(data['XLimRanges'][plotIndex])
            plt.ylim(data['YLimRanges'][plotIndex])
            plt.savefig(data['FileName'] + '_' + str(plotIndex) + '.png', dpi=800)

        plt.close()


if __name__ == "__main__":
    import pandas as pd
    data = pd.read_csv(r'C:\Users\achantv\Documents\Utilities\aceengineer\data_manager\TimeLine.csv',
                       parse_dates=True,
                       index_col=0)
    ax = GenerateTimeLine(data)
    plt.show()
