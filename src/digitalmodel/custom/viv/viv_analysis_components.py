import logging


class VIVAnalysisComponents():

    def __init__(self, cfg):
        self.cfg = cfg

    def modal_analysis(self):
        import math

        import pandas as pd
        self.ModesSummarydfArray = []
        self.ModalSolutiondfArray = []
        for fileIndex in range(0, len(self.cfg['modal_sets'])):
            customdata = {"FileName": self.cfg['modal_sets'][fileIndex]['io']}

            FirstLine = self.readFirstLine(customdata['FileName'])
            FirstLineArray = FirstLine.split(' ')
            NoOfModes = int(FirstLineArray[0])
            NoOfNodes = int(FirstLineArray[1])
            print(FirstLine)

            customdata = {"FileName": self.cfg['modal_sets'][fileIndex]['io']
                , "skiprows": 1
                , "skipfooter": NoOfNodes * NoOfModes
                , "sep": " "
                , "header": None
                , "engine": "python"
                , "Label": self.cfg['modal_sets'][fileIndex]['label']
                          }

            df = pd.read_table(customdata['FileName'], sep=customdata["sep"], skiprows=customdata["skiprows"],
                               skipfooter=customdata["skipfooter"], engine=customdata["engine"],
                               header=customdata["header"])
            print(df.head(2))
            ModesSummaryDF = pd.DataFrame()
            ModesSummaryDF['Mode Number'] = df[0]
            ModesSummaryDF['Frequency'] = df[1] / 2 / math.pi

            df = pd.read_table(customdata['FileName'], sep=customdata["sep"],
                               skiprows=customdata["skiprows"] + len(ModesSummaryDF), engine=customdata["engine"],
                               header=customdata["header"])
            df.rename(
                columns={0: 'Mode Number', 1: 'Node Number', 2: 'Mode Shape', 3: 'Mode Slope', 4: 'Mode Curvature',
                         5: 'Mode Unknown'}, inplace=True)

            model_length = self.cfg.model_state_information['from_dict']['model_length']
            model_start = self.cfg.model_state_information['from_dict']['model_start']
            if self.cfg['modal_sets'][fileIndex].__contains__('x_by_L_range'):
                start_node = math.ceil(self.cfg['modal_sets'][fileIndex]['x_by_L_range'][0] * (NoOfNodes - 1))
                end_node = math.floor(self.cfg['modal_sets'][fileIndex]['x_by_L_range'][1] * (NoOfNodes - 1))
                df = df[(df['Node Number'] > start_node) & (df['Node Number'] < end_node)]
            df['x'] = df['Node Number'] * model_length / (NoOfNodes - 1) + model_start

            ModeCurvature = []
            ModeShape = []
            ModeSlope = []
            ModeUnknown = []
            for ModeNumber in range(1, NoOfModes + 1):
                ModeCurvature.append(df[df['Mode Number'] == ModeNumber]['Mode Curvature'].max())
                ModeShape.append(df[df['Mode Number'] == ModeNumber]['Mode Shape'].max())
                ModeSlope.append(df[df['Mode Number'] == ModeNumber]['Mode Slope'].max())
                ModeUnknown.append(df[df['Mode Number'] == ModeNumber]['Mode Unknown'].max())

            ModesSummaryDF['Mode Curvature'] = ModeCurvature
            ModesSummaryDF['Mode Shape'] = ModeShape
            ModesSummaryDF['Mode Slope'] = ModeSlope
            ModesSummaryDF['Mode Unknown'] = ModeUnknown

            print(df.head(2))
            print(ModesSummaryDF.head())
            self.ModesSummarydfArray.append(ModesSummaryDF)
            self.ModalSolutiondfArray.append(df)

    def readFirstLine(self, FileName):
        with open(FileName) as f:
            line = f.readline()

        return line

    def save_modal_visualizations(self):
        from common.visualizations import Visualization
        viz = Visualization()

        for plt_index in range(0, len(self.cfg['modal_plots'])):
            plt_settings = self.cfg['modal_plots'][plt_index]
            plt_settings.update(
                {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                              plt_settings['file_extension'] + '.png'})
            plt_settings.update({'array_label': [[modal_set['label']] for modal_set in self.cfg.modal_sets]})
            viz.from_df_array(self.ModesSummarydfArray, plt_settings)
            viz.add_title_and_axis_labels()
            viz.add_legend()
            # viz.plt.grid()
            print("Saving {} plot ..." .format(plt_settings['file_extension']))
            viz.save_and_close()

    def save_mode_shapes(self):
        from common.visualizations import Visualization
        viz = Visualization()

        for plt_index in range(0, len(self.cfg['mode_shape_plots']['each_modal_set'])):
            plt_settings = self.cfg['mode_shape_plots']['each_modal_set'][plt_index]
            for file_index in range(0, len(self.cfg['modal_sets'])):
                extension_plt_settings = plt_settings.copy()
                extension_plt_settings.update(
                    {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  plt_settings['file_extension'] + '_file_' + str(file_index) + '_' + str(plt_index) + '.png'})
                for mode_number in plt_settings['label']:
                    temp_df = self.ModalSolutiondfArray[file_index][self.ModalSolutiondfArray[file_index]['Mode Number'] == mode_number]
                    mode_plt_settings = extension_plt_settings.copy()
                    mode_plt_settings.update({'label' : ['mode ' + str(mode_number)]})
                    mode_plt_settings.update({'title':
                        extension_plt_settings['title'] + ' ' + self.cfg['modal_sets'][file_index]['label']})
                    viz.from_df_columns(temp_df, mode_plt_settings)

                viz.add_title_and_axis_labels()
                viz.add_legend()
                viz.add_text_fields()
                print("Saving {} plot ...".format(plt_settings['file_extension']))
                viz.save_and_close()

        for plt_index in range(0, len(self.cfg['mode_shape_plots']['between_modal_sets'])):
            plt_settings = self.cfg['mode_shape_plots']['between_modal_sets'][plt_index]
            for file_index in range(0, len(self.cfg['modal_sets'])):
                extension_plt_settings = plt_settings.copy()
                extension_plt_settings.update(
                    {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  plt_settings['file_extension'] + '_comp_plt_' + str(file_index) + '_' + str(plt_index) + '.png'})
                for mode_number in plt_settings['label']:
                    temp_df = self.ModalSolutiondfArray[file_index][self.ModalSolutiondfArray[file_index]['Mode Number'] == mode_number]
                    mode_plt_settings = extension_plt_settings.copy()
                    mode_plt_settings.update({'label' : ['mode ' + str(mode_number) + ', ' + self.cfg['modal_sets'][file_index]['label']]})
                    viz.from_df_columns(temp_df, mode_plt_settings)

            viz.add_title_and_axis_labels()
            viz.add_legend()
            viz.add_text_fields()
            print("Saving {} plot ...".format(plt_settings['file_extension']))
            viz.save_and_close()

    def get_current_data(self):
        from common.data import ReadData
        read_data = ReadData()
        self.current_data = []
        for current_data_index in range(0, len(self.cfg.current_data['from_xlsx'])):
            cfg_temp = {'files': {'from_xlsx': self.cfg.current_data['from_xlsx']}}
            df = read_data.from_xlsx(cfg_temp, current_data_index)
            df = read_data.df_filter_by_column_values(cfg_temp, df, current_data_index)
            self.current_data.append(df)

    def plot_current_non_exceedance(self, reference_depth = 0):

        for plot_index in range(0, len(self.cfg.current_data['plot']['non_exceedance'])):
            plt_settings = self.cfg.current_data['plot']['exceedance'][plot_index]
            plt_settings['title'] = plt_settings['title']
            plt_settings.update(
                {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                              plt_settings['file_suffix'] + '.png'})
            for current_data_index in range(0, len(self.cfg.current_data['from_xlsx'])):
                if not self.current_data[current_data_index].probability.isnull().values.any():
                    from common.visualizations import Visualization
                    viz = Visualization()

                    surface_current_speeds = self.current_data[current_data_index][self.current_data[current_data_index].depth == 0].copy()
                    surface_current_speeds.sort_values(by = ['current_speed'], ascending=True, inplace=True)
                    # QC currents at exceedabce depth
                    current_profile_id_array = self.current_data[
                        current_data_index].current_profile_id.unique().tolist()
                    if len(current_profile_id_array) > len(surface_current_speeds):
                        print("All current profiles may not be captured for this exceedance depth")
                    surface_current_speeds['exceedance'] = 0
                    surface_current_speeds['exceedance'].iloc[0] = surface_current_speeds['probability'].iloc[
                        0]
                    for row_index in range(1, len(surface_current_speeds)):
                        surface_current_speeds['exceedance'].iloc[row_index] = \
                            surface_current_speeds['probability'].iloc[row_index] + \
                            surface_current_speeds['exceedance'].iloc[row_index - 1]

                    surface_current_speeds.to_csv(self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  'exceedance' + '_' + str(current_data_index) + '.csv')
                    plt_settings['label'] = [self.cfg.current_data['from_xlsx'][current_data_index][
                    'label']]
                    viz.from_df_columns(surface_current_speeds, plt_settings)
                else:
                    print("No exceedance plot due to missing probabilities for some currrent profiles")

            viz.add_title_and_axis_labels()
            viz.add_legend()
            print("Saving {0} plot for current data {1}...".format(plt_settings['file_suffix'], current_data_index))
            viz.save_and_close()

    def plot_current_exceedance(self, reference_depth = 0):
        from common.visualizations import Visualization
        viz = Visualization()

        for plot_index in range(0, len(self.cfg.current_data['plot']['exceedance'])):
            plt_settings = self.cfg.current_data['plot']['non_exceedance'][plot_index]
            plt_settings['title'] = plt_settings['title']
            plt_settings.update(
                {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                              plt_settings['file_suffix'] + '.png'})
            for current_data_index in range(0, len(self.cfg.current_data['from_xlsx'])):
                if not self.current_data[current_data_index].probability.isnull().values.any():

                    surface_current_speeds = self.current_data[current_data_index][self.current_data[current_data_index].depth == 0].copy()
                    surface_current_speeds.sort_values(by = ['current_speed'], ascending=False, inplace=True)
                    # QC currents at exceedabce depth
                    current_profile_id_array = self.current_data[
                        current_data_index].current_profile_id.unique().tolist()
                    if len(current_profile_id_array) > len(surface_current_speeds):
                        print("All current profiles may not be captured for this exceedance depth")
                    surface_current_speeds['non_exceedance'] = 0
                    surface_current_speeds['non_exceedance'].iloc[0] = surface_current_speeds['probability'].iloc[
                        0]
                    for row_index in range(1, len(surface_current_speeds)):
                        surface_current_speeds['non_exceedance'].iloc[row_index] = \
                            surface_current_speeds['probability'].iloc[row_index] + \
                            surface_current_speeds['non_exceedance'].iloc[row_index - 1]

                    surface_current_speeds.to_csv(self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  'non_exceedance' + '_' + str(current_data_index) + '.csv')
                    plt_settings['label'] = [self.cfg.current_data['from_xlsx'][current_data_index][
                    'label']]
                    viz.from_df_columns(surface_current_speeds, plt_settings)
                else:
                    print("No Non exceedance plot due to missing probabilities for some currrent profiles")

            viz.add_title_and_axis_labels()
            viz.add_legend()
            print("Saving {0} plot for current data {1}...".format(plt_settings['file_suffix'], current_data_index))
            viz.save_and_close()

    def plot_current_profiles(self):
        from common.visualizations import Visualization
        viz = Visualization()

        for current_data_index in range(0, len(self.cfg.current_data['from_xlsx'])):
            current_profile_id_array = self.current_data[current_data_index].current_profile_id.unique().tolist()
            for plot_index in range(0, len(self.cfg.current_data['plot']['individual'])):
                plt_settings = self.cfg.current_data['plot']['individual'][plot_index].copy()
                plt_settings['title'] = plt_settings['title'] + ', ' + self.cfg.current_data['from_xlsx'][current_data_index][
                    'label']
                plt_settings.update(
                    {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  plt_settings['file_suffix'] + '_' + str(current_data_index) + '.png'})
                for current_index in range(0, len(current_profile_id_array)):
                    temp_df = self.current_data[current_data_index][
                        self.current_data[current_data_index].current_profile_id == current_profile_id_array[
                            current_index]]
                    plt_settings['label'] = [temp_df.Label.iloc[0]]
                    viz.from_df_columns(temp_df, plt_settings)
                viz.add_title_and_axis_labels()
                if len(current_profile_id_array) <= 15:
                    viz.add_legend()
                print("Saving {0} plot for current data {1}...".format(plt_settings['file_suffix'], current_data_index))
                viz.save_and_close()

    def get_wave_data(self):
        from common.data import ReadData
        read_data = ReadData()
        self.wave_data = []
        for wave_data_index in range(0, len(self.cfg.wave_data['from_xlsx'])):
            cfg_temp = {'files': {'from_xlsx': self.cfg.wave_data['from_xlsx']}}
            df = read_data.from_xlsx(cfg_temp, wave_data_index)
            df = read_data.df_filter_by_column_values(cfg_temp, df, wave_data_index)
            self.wave_data.append(df)

    def plot_wave_data(self):
        from common.visualizations import Visualization
        viz = Visualization()

        for wave_data_index in range(0, len(self.wave_data)):
            for plot_index in range(0, len(self.cfg.wave_data['plot']['individual'])):
                plt_settings = self.cfg.wave_data['plot']['individual'][plot_index].copy()
                plt_settings['title'] = self.cfg.wave_data['from_xlsx'][wave_data_index][
                    'label']
                plt_settings.update(
                    {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  plt_settings['file_suffix'] + '_' + str(wave_data_index) + '.png'})
                temp_df = self.wave_data[wave_data_index]
                temp_df.sort_values(by=['Hs'], ascending=True, inplace=True)
                temp_df.reset_index(inplace=True)
                temp_df['index'] = temp_df.index + 1
                viz.from_df_columns(temp_df, plt_settings)
                viz.add_title_and_axis_labels()
                print("Saving {0} plot for wave data {1}...".format(plt_settings['file_suffix'], wave_data_index))
                viz.save_and_close()

    def plot_wave_exceedance(self):
        from common.visualizations import Visualization
        viz = Visualization()

        for plot_index in range(0, len(self.cfg.wave_data['plot']['exceedance'])):
            plt_settings = self.cfg.wave_data['plot']['exceedance'][plot_index]
            plt_settings['title'] = plt_settings['title']
            plt_settings.update(
                {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                              plt_settings['file_suffix'] + '.png'})
            for wave_data_index in range(0, len(self.cfg.wave_data['from_xlsx'])):
                if not self.wave_data[wave_data_index].probability.isnull().values.any():

                    df = self.wave_data[wave_data_index].copy()
                    df.sort_values(by = ['Hs'], ascending=False, inplace=True)
                    df['non_exceedance'] = 0
                    df['non_exceedance'].iloc[0] = df['probability'].iloc[
                        0]
                    for row_index in range(1, len(df)):
                        df['non_exceedance'].iloc[row_index] = \
                            df['probability'].iloc[row_index] + \
                            df['non_exceedance'].iloc[row_index - 1]

                    df.to_csv(self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  'non_exceedance' + '_' + str(wave_data_index) + '.csv')
                    plt_settings['label'] = [self.cfg.wave_data['from_xlsx'][wave_data_index][
                    'label']]
                    viz.from_df_columns(df, plt_settings)
                else:
                    print("No Non exceedance plot due to missing probabilities for some currrent profiles")

            viz.add_title_and_axis_labels()
            viz.add_legend()
            print("Saving {0} plot for wave data {1}...".format(plt_settings['file_suffix'], wave_data_index))
            viz.save_and_close()

