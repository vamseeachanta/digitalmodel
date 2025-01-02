import os
import pandas as pd
from pathlib import Path
from plotly.subplots import make_subplots
import plotly.graph_objects as go

from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.utilities import get_colors
from assetutilities.common.saveData import saveDataYaml


class RAOAnalysis:

    def __init__(self):
        pass

    def read_orcaflex_displacement_raos(self, cfg=None):
        self.cfg = cfg
        if self.cfg['rao_plot']['displacement']['flag']:
            vessel_data_all_files = []
            for file in cfg['Files']:
                if os.path.isfile(file['Name']):
                    file_vessel_data = self.read_vessel_data_from_file(
                        file['Name'])
                    vessel_cfg = file
                    self.plot_single_vessel_data(file_vessel_data, vessel_cfg)

                    vessel_data_all_files.append(
                        {file['Label']: file_vessel_data})

            self.vessel_data_all_files = vessel_data_all_files
            self.plot_multiple_vessel_data()
        else:
            print("No RAO displacement plots requested")

        return self.cfg

    def read_vessel_data_from_file(self, file_name):
        print('Reading file: {}'.format(file_name))
        yml_data = ymlInput(file_name, updateYml=None)
        vessel_data = yml_data['VesselTypes']
        return vessel_data

    def write_filtered_orcaflex_seastate_rao_file(self, file_name,
                                                  filter_SeaStateRAOs_data,
                                                  filtered_file_vessel_data):
        print('Reading file: {}'.format(file_name))
        yml_data = ymlInput(file_name, updateYml=None)
        yml_data['VesselTypes'] = filtered_file_vessel_data

        p = Path(file_name)
        file_name_stem = p.stem

        filtered_vessel_data_name = file_name.replace(
            file_name_stem + '.yml', 'filter_SeaStateRAOs_data')
        saveDataYaml(filter_SeaStateRAOs_data,
                     filtered_vessel_data_name,
                     default_flow_style=False)

        file_name_filtered = file_name.replace(file_name_stem + '.yml',
                                               file_name_stem + '_filtered')
        saveDataYaml(yml_data, file_name_filtered, default_flow_style=True)

    def plot_multiple_vessel_data(self):
        pass

    def plot_single_vessel_data(self, file_vessel_data, vessel_cfg):
        vessel_names = [vessel_data['Name'] for vessel_data in file_vessel_data]
        vessel_data = self.get_vessel_data(file_vessel_data, vessel_cfg,
                                           vessel_names)

        draught_data = self.get_draught_indices(vessel_cfg, vessel_data)

        RAOs_draughts = self.get_rao_data_for_vessel(vessel_data, draught_data)
        self.plot_displacement_RAOs(RAOs_draughts, draught_data)

    def plot_displacement_RAOs(self, RAOs_draughts, draught_data):
        draughts = draught_data['draughts']

        for draught in draughts:
            RAOs = RAOs_draughts[draught]
            self.plot_displacement_RAOs_amplitudes_for_draught(RAOs, draught)
            self.plot_displacement_RAOs_phases_for_draught(RAOs, draught)

    def plot_displacement_RAOs_amplitudes_for_draught(self, RAOs, draught):
        '''
        key references:
        https://stackoverflow.com/questions/60751008/sharing-same-legends-for-subplots-in-plotly
        '''
        nrows = 6
        ncols = 1

        RAODirections = self.cfg['rao_plot']['displacement']['RAODirections']
        if len(RAODirections) == 0:
            RAODirections = RAOs['RAODirection'].unique()

        title_text = f"Amplitude RAOs, Draught: {draught}"

        xaxes_title_text = [
            "Period (s)", "Period (s)", "Period (s)", "Period (s)",
            "Period (s)", "Period (s)"
        ]
        yaxes_title_text = [
            "Amp, m/m",
            "Amp, m/m",
            "Amp, m/m",
            "Amp, deg/m",
            "Amp, deg/m",
            "Amp, deg/m",
        ]

        if 'RAOPeriodOrFreq' in list(RAOs.keys()):
            RAODirection_x_columns = 6 * ['RAOPeriodOrFreq']
        else:
            RAODirection_x_columns = 6 * ['RAOPeriodOrFrequency']

        RAODirection_y_columns = [
            'RAOHeaveAmp', 'RAOSurgeAmp', 'RAOSwayAmp', 'RAOYawAmp',
            'RAORollAmp', 'RAOPitchAmp'
        ]

        fig = make_subplots(rows=nrows,
                            cols=ncols,
                            shared_xaxes=True,
                            subplot_titles=("Heave", "Surge", "Sway", "Yaw",
                                            "Roll", "Pitch"))
        fig.update_layout(title_text=title_text)
        fig.update_layout(paper_bgcolor='rgba(0,0,0,0)',
                          plot_bgcolor='rgba(0,0,0,0)')

        xaxis_range = self.cfg['rao_plot']['displacement']['xaxis_range']
        if len(xaxis_range) > 0:
            RAOPeriodOrFreq_column = RAODirection_x_columns[0]
            RAOs = RAOs[(RAOs[RAOPeriodOrFreq_column] >= xaxis_range[0]) & (
                RAOs[RAOPeriodOrFreq_column] <= xaxis_range[1])].copy()

        showlegend_array = [True, False, False, False, False, False]
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name_for_overwrite']
        file_name = file_name + '_rao_amp'

        col = 1
        for row_index in range(0, nrows):
            fig.update_yaxes(title_text=yaxes_title_text[row_index],
                             row=row_index + 1,
                             col=col)

        fig.update_xaxes(title_text=xaxes_title_text[nrows - 1],
                         row=nrows,
                         col=ncols)

        num_colors = len(RAODirections)
        plot_colors = get_colors(set='single', n=num_colors)

        for RAODirection_index in range(0, len(RAODirections)):
            RAODirection = RAODirections[RAODirection_index]
            RAOs_direction = RAOs[RAOs['RAODirection'] == RAODirection]

            name = RAODirection
            line_color = plot_colors[RAODirection_index]
            legend_group = RAODirection
            mode = 'lines'

            for row_index in range(0, nrows):
                x = RAOs_direction[RAODirection_x_columns[row_index]]
                y = RAOs_direction[RAODirection_y_columns[row_index]]
                showlegend = showlegend_array[row_index]
                row = row_index + 1
                col = 1

                add_trace_cfg = {
                    'x': x,
                    'y': y,
                    'name': name,
                    'line_color': line_color,
                    'legendgroup': legend_group,
                    'showlegend': showlegend,
                    'mode': mode,
                    'row': row,
                    'col': col
                }
                self.add_trace_to_fig(fig, add_trace_cfg)

        # fig.write_image(file_name + '_rao_amp' + '.png')
        fig.write_html(file_name + '.html')

    def plot_displacement_RAOs_phases_for_draught(self, RAOs, draught):
        '''
        key references:
        https://stackoverflow.com/questions/60751008/sharing-same-legends-for-subplots-in-plotly
        '''
        nrows = 6
        ncols = 1

        RAODirections = self.cfg['rao_plot']['displacement']['RAODirections']
        if len(RAODirections) == 0:
            RAODirections = RAOs['RAODirection'].unique()

        title_text = f"Amplitude RAOs, Draught: {draught}"

        xaxes_title_text = [
            "Period (s)", "Period (s)", "Period (s)", "Period (s)",
            "Period (s)", "Period (s)"
        ]
        yaxes_title_text = [
            "Ph, deg", "Ph, deg", "Ph, deg", "Ph, deg", "Ph, deg", "Ph, deg"
        ]

        if 'RAOPeriodOrFreq' in list(RAOs.keys()):
            RAODirection_x_columns = 6 * ['RAOPeriodOrFreq']
        else:
            RAODirection_x_columns = 6 * ['RAOPeriodOrFrequency']

        RAODirection_y_columns = [
            'RAOHeavePhase', 'RAOSurgePhase', 'RAOSwayPhase', 'RAOYawPhase',
            'RAORollPhase', 'RAOPitchPhase'
        ]

        fig = make_subplots(rows=nrows,
                            cols=ncols,
                            shared_xaxes=True,
                            subplot_titles=("Heave", "Surge", "Sway", "Yaw",
                                            "Roll", "Pitch"))
        fig.update_layout(title_text=title_text)
        fig.update_layout(paper_bgcolor='rgba(0,0,0,0)',
                          plot_bgcolor='rgba(0,0,0,0)')

        xaxis_range = self.cfg['rao_plot']['displacement']['xaxis_range']
        if len(xaxis_range) > 0:
            RAOPeriodOrFreq_column = RAODirection_x_columns[0]
            RAOs = RAOs[(RAOs[RAOPeriodOrFreq_column] >= xaxis_range[0]) & (
                RAOs[RAOPeriodOrFreq_column] <= xaxis_range[1])].copy()

        showlegend_array = [True, False, False, False, False, False]
        file_name = self.cfg['Analysis']['result_folder'] + self.cfg[
            'Analysis']['file_name_for_overwrite']
        file_name = file_name + '_rao_phase'

        col = 1
        for row_index in range(0, nrows):
            fig.update_yaxes(title_text=yaxes_title_text[row_index],
                             row=row_index + 1,
                             col=col)

        fig.update_xaxes(title_text=xaxes_title_text[nrows - 1],
                         row=nrows,
                         col=ncols)

        num_colors = len(RAODirections)
        plot_colors = get_colors(set='single', n=num_colors)

        for RAODirection_index in range(0, len(RAODirections)):
            RAODirection = RAODirections[RAODirection_index]
            RAOs_direction = RAOs[RAOs['RAODirection'] == RAODirection]

            name = RAODirection
            line_color = plot_colors[RAODirection_index]
            legend_group = RAODirection
            mode = 'lines'

            for row_index in range(0, nrows):
                x = RAOs_direction[RAODirection_x_columns[row_index]]
                y = RAOs_direction[RAODirection_y_columns[row_index]]
                showlegend = showlegend_array[row_index]
                row = row_index + 1
                col = 1

                add_trace_cfg = {
                    'x': x,
                    'y': y,
                    'name': name,
                    'line_color': line_color,
                    'legendgroup': legend_group,
                    'showlegend': showlegend,
                    'mode': mode,
                    'row': row,
                    'col': col
                }
                self.add_trace_to_fig(fig, add_trace_cfg)

        # fig.write_image(file_name  + '.png')
        fig.write_html(file_name + '.html')

    def add_trace_to_fig(self, fig, add_trace_cfg):

        x = add_trace_cfg['x']
        y = add_trace_cfg['y']
        name = add_trace_cfg['name']
        line_color = add_trace_cfg['line_color']
        legendgroup = add_trace_cfg['legendgroup']
        showlegend = add_trace_cfg['showlegend']
        mode = add_trace_cfg['mode']
        row = add_trace_cfg['row']
        col = add_trace_cfg['col']

        fig.add_trace(go.Scatter(x=x,
                                 y=y,
                                 name=name,
                                 line_color=line_color,
                                 legendgroup=legendgroup,
                                 showlegend=showlegend,
                                 mode=mode),
                      row=row,
                      col=col)

    def get_rao_data_for_vessel(self, vessel_data, draught_data):
        draughts = draught_data['draughts']
        draughts_index = draught_data['draughts_index']
        RAOs_draughts = {}
        for i in range(0, len(draughts_index)):
            RAOs = vessel_data['Draughts'][
                draughts_index[i]]['DisplacementRAOs']['RAOs']
            RAOs_df = self.get_DisplacementRAOs_for_draught(RAOs)
            RAOs_draughts.update({draughts[i]: RAOs_df})

        return RAOs_draughts

    def get_DisplacementRAOs_for_draught(self, RAOs):
        columns_set_0 = list(RAOs[0].keys())[0]
        columns_set_1 = list(RAOs[0].keys())[1]
        columns_set_1_columns = columns_set_1.split(',')
        columns_set_1_strip = [
            column.strip() for column in columns_set_1_columns
        ]
        RAOs_df_columns = [columns_set_0] + columns_set_1_strip

        RAOs_df = pd.DataFrame(columns=RAOs_df_columns)

        for RAODirection_data in RAOs:
            RAODirection = RAODirection_data[columns_set_0]
            data = RAODirection_data[columns_set_1]

            for data_row in data:
                RAOs_df_row = [RAODirection] + data_row
                RAOs_df.loc[len(RAOs_df)] = RAOs_df_row

        return RAOs_df

    def get_draught_indices(self, vessel_cfg, vessel_data):
        if len(vessel_cfg['draughts']) == 0:
            draughts = [
                vessel_draught_data['Name']
                for vessel_draught_data in vessel_data['Draughts']
            ]
        else:
            draughts = vessel_cfg['draughts']

        vessel_draughts_available = [
            draught['Name'] for draught in vessel_data['Draughts']
        ]
        draughts_index = []
        for i in range(0, len(draughts)):
            if draughts[i] in vessel_draughts_available:
                draughts_index.append(i)
            else:
                raise ValueError(
                    f"Draught :'{draughts[i]}' not found for vessel:'{vessel_cfg['Name']}'"
                )

        draught_data = {'draughts': draughts, 'draughts_index': draughts_index}

        return draught_data

    def get_vessel_data(self, file_vessel_data, vessel_cfg, vessel_names):
        if vessel_cfg['vessel_name'] in vessel_names:
            vessel_index = vessel_names.index(vessel_cfg['vessel_name'])
            vessel_data = file_vessel_data[vessel_index]
        else:
            raise ValueError(
                f"Vessel name :'{vessel_cfg['vessel_name']}' not found in file :'{vessel_cfg['Name']}'"
            )

        return vessel_data

    def check_amplitudes(self):
        pass

    def check_phases(self):
        pass

    def check_locations(self):
        pass

    def check_definitions(self):
        pass

    def check_units(self):
        pass

    def assess_orcaflex_seastate_raos(self, cfg):
        self.cfg = cfg
        if self.cfg['rao_plot']['seastate']['flag']:
            vessel_data_all_files = []
            for file in cfg['Files']:
                if os.path.isfile(file['Name']):
                    # file_vessel_data = self.read_vessel_data_from_file(
                    #     file['Name'])
                    # filter_SeaStateRAOs_data, filtered_file_vessel_data = self.filter_orcaflex_seastate_raos(
                    #     file_vessel_data)
                    filter_SeaStateRAOs_data, filtered_file_vessel_data = self.filter_orcaflex_seastate_raos_visual_grid(
                        file['Name'])
                    self.write_filtered_orcaflex_seastate_rao_file(
                        file['Name'], filter_SeaStateRAOs_data,
                        filtered_file_vessel_data)

        else:
            print("No Seastate RAO plots requested")

        return self.cfg

    def filter_orcaflex_seastate_raos(self, file_vessel_data):
        filter_settings = self.cfg['rao_plot']['seastate']['filter']

        x_low = filter_settings['cog'][
            'x'] - filter_settings['l'] / 2 - filter_settings['beyond_boundary']
        x_high = filter_settings['cog'][
            'x'] + filter_settings['l'] / 2 + filter_settings['beyond_boundary']
        y_low = filter_settings['cog'][
            'y'] - filter_settings['w'] / 2 - filter_settings['beyond_boundary']
        y_high = filter_settings['cog'][
            'y'] + filter_settings['w'] / 2 + filter_settings['beyond_boundary']

        SeaStateRAOs_key = 'SeaStateRAODirection, SeaStateRAOPeriodOrFrequency, SeaStateRAOX, SeaStateRAOY, SeaStateRAOZ, SeaStateRAOPotentialAmp, SeaStateRAOPotentialPhase, SeaStateRAOGradientXAmp, SeaStateRAOGradientXPhase, SeaStateRAOGradientYAmp, SeaStateRAOGradientYPhase, SeaStateRAOGradientZAmp, SeaStateRAOGradientZPhase'
        SeaStateRAOs = file_vessel_data[0]['Draughts'][0]['SeaStateRAOs'][
            SeaStateRAOs_key]
        df_columns = [
            'SeaStateRAODirection', 'SeaStateRAOPeriodOrFrequency',
            'SeaStateRAOX', 'SeaStateRAOY', 'SeaStateRAOZ',
            'SeaStateRAOPotentialAmp', 'SeaStateRAOPotentialPhase',
            'SeaStateRAOGradientXAmp', 'SeaStateRAOGradientXPhase',
            'SeaStateRAOGradientYAmp', 'SeaStateRAOGradientYPhase',
            'SeaStateRAOGradientZAmp', 'SeaStateRAOGradientZPhase'
        ]
        SeaStateRAOs_df = pd.DataFrame(SeaStateRAOs, columns=df_columns)
        filter_SeaStateRAOs_df = SeaStateRAOs_df[
            (SeaStateRAOs_df['SeaStateRAOX'] < x_high) &
            (SeaStateRAOs_df['SeaStateRAOX'] > x_low) &
            (SeaStateRAOs_df['SeaStateRAOY'] < y_high) &
            (SeaStateRAOs_df['SeaStateRAOY'] > y_low)]

        filter_SeaStateRAOs_data = filter_SeaStateRAOs_df.to_dict(
            'tight')['data']
        filtered_file_vessel_data = file_vessel_data.copy()
        filtered_file_vessel_data[0]['Draughts'][0]['SeaStateRAOs'][
            SeaStateRAOs_key] = filter_SeaStateRAOs_data

        return filter_SeaStateRAOs_data, filtered_file_vessel_data

    def filter_orcaflex_seastate_raos_visual_grid(self, file_name):
        filter_settings = self.cfg['rao_plot']['seastate']['filter']

        x_low = filter_settings['cog'][
            'x'] - filter_settings['l'] / 2 - filter_settings['beyond_boundary']
        x_high = filter_settings['cog'][
            'x'] + filter_settings['l'] / 2 + filter_settings['beyond_boundary']
        y_low = filter_settings['cog'][
            'y'] - filter_settings['w'] / 2 - filter_settings['beyond_boundary']
        y_high = filter_settings['cog'][
            'y'] + filter_settings['w'] / 2 + filter_settings['beyond_boundary']

        self.get_shape_data(file_name)

        SeaStateRAOs_df = pd.DataFrame(SeaStateRAOs, columns=df_columns)
        filter_SeaStateRAOs_df = SeaStateRAOs_df[
            (SeaStateRAOs_df['SeaStateRAOX'] < x_high) &
            (SeaStateRAOs_df['SeaStateRAOX'] > x_low) &
            (SeaStateRAOs_df['SeaStateRAOY'] < y_high) &
            (SeaStateRAOs_df['SeaStateRAOY'] > y_low)]

        filter_SeaStateRAOs_data = filter_SeaStateRAOs_df.to_dict(
            'tight')['data']
        filtered_file_vessel_data = file_vessel_data.copy()
        filtered_file_vessel_data[0]['Draughts'][0]['SeaStateRAOs'][
            SeaStateRAOs_key] = filter_SeaStateRAOs_data

        return filter_SeaStateRAOs_data, filtered_file_vessel_data

    def get_shape_data(self, file_name):
        print('Reading file: {}'.format(file_name))
        yml_data = ymlInput(file_name, updateYml=None)
        shapes = yml_data['Shapes']
        shape_names = [shape['Name'] for shape in shapes]
        rao_visual_grid_shape = self.cfg['rao_plot']['seastate']['filter'][
            'grid_label']
        rao_visual_grid_shape_index = shape_names.index(rao_visual_grid_shape)
        shape_data = shapes[rao_visual_grid_shape_index]

        l_grid = self.cfg['rao_plot']['seastate']['filter']['l'] + 2 * self.cfg[
            'rao_plot']['seastate']['filter']['beyond_boundary']
        w_grid = self.cfg['rao_plot']['seastate']['filter']['w'] + 2 * self.cfg[
            'rao_plot']['seastate']['filter']['beyond_boundary']

        filtered_origin = [
            self.cfg['rao_plot']['seastate']['filter']['cog']['x'] - l_grid / 2,
            self.cfg['rao_plot']['seastate']['filter']['cog']['y'] - w_grid / 2,
            shape_data['Origin'][2]
        ]
        shape_data['Origin'] = filtered_origin
        filtered_size = [l_grid, w_grid, shape_data['Size'][2]]
        shape_data['Size'] = filtered_size

        p = Path(file_name)
        file_name_stem = p.stem

        filtered_vessel_data_name = file_name.replace(file_name_stem + '.yml',
                                                      'shape_data')
        saveDataYaml(shape_data,
                     filtered_vessel_data_name,
                     default_flow_style=False)

        return shape_data
