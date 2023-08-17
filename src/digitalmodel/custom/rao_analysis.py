import os
import pandas as pd
from plotly.subplots import make_subplots
import plotly.graph_objects as go

from digitalmodel.common.yml_utilities import ymlInput


class RAOAnalysis:

    def __init__(self):
        pass

    def read_orcaflex_raos(self, cfg=None):
        self.cfg = cfg
        vessel_data_all_files = []
        for file in cfg['Files']:
            if os.path.isfile(file['Name']):
                file_vessel_data = self.read_vessel_data_from_file(file['Name'])
                vessel_cfg = file
                self.plot_single_vessel_data(file_vessel_data, vessel_cfg)

                vessel_data_all_files.append({file['Label']: file_vessel_data})

        self.vessel_data_all_files = vessel_data_all_files
        self.plot_amplitudes()

    def read_vessel_data_from_file(self, file_name):
        print('Reading file: {}'.format(file_name))
        yml_data = ymlInput(file_name, updateYml=None)
        vessel_data = yml_data['VesselTypes']
        return vessel_data

    def plot_amplitudes(self):
        vessel_data_all_files = self.vessel_data_all_files

        for vessel_data in vessel_data_all_files:
            keys = list(vessel_data.keys())
            for key in keys:
                if key == 'RAOs':
                    vessel_data[key]['Amplitude'].plot()

        self.vessel_data_all_files['Vessel1']['RAOs']['Amplitude'].plot()

    def plot_single_vessel_data(self, file_vessel_data, vessel_cfg):
        vessel_names = [vessel_data['Name'] for vessel_data in file_vessel_data]
        vessel_data = self.get_vessel_data(file_vessel_data, vessel_cfg,
                                           vessel_names)

        draught_data = self.get_draught_indices(vessel_cfg, vessel_data)

        RAOs_draughts = self.get_rao_data_for_vessel(vessel_data, draught_data)
        self.plot_RAOs(RAOs_draughts, draught_data)

    def plot_RAOs(self, RAOs_draughts, draught_data):
        draughts = draught_data['draughts']

        for draught in draughts:
            RAOs = RAOs_draughts[draught]
            self.plot_RAOs_for_draught(RAOs, draught)

    def plot_RAOs_for_draught(self, RAOs, draught):
        '''
        key references:
        https://stackoverflow.com/questions/60751008/sharing-same-legends-for-subplots-in-plotly
        '''
        nrows = 3
        ncols = 2
        fig = make_subplots(rows=nrows,
                            cols=ncols,
                            shared_xaxes=True,
                            subplot_titles=("Heave", "Heave", "Surge", "Surge",
                                            "Yaw", "Yaw"))
        fig.update_layout(title_text=f"RAOs for Draught: {draught}")

        for row in range(0, nrows):
            col = 1
            fig.update_yaxes(title_text="Amplitude (m)", row=row, col=col)
            col = 2
            fig.update_yaxes(title_text="Phase (deg)", row=row, col=col)

        for row in range(0, nrows):
            for col in range(0, ncols):
                fig.update_xaxes(title_text="Period (s)", row=nrows, col=ncols)

        RAODirections = RAOs['RAODirection'].unique()
        for RAODirection_index in range(0, len(RAODirections)):
            RAODirection = RAODirections[RAODirection_index]
            legend_group = RAODirection
            RAOs_direction = RAOs[RAOs['RAODirection'] == RAODirection]
            fig.add_trace(go.Scatter(x=RAOs_direction['RAOPeriodOrFreq'],
                                     y=RAOs_direction['RAOSurgeAmp'],
                                     name=RAODirection,
                                     legendgroup=legend_group,
                                     mode='lines'),
                          row=1,
                          col=1)
            fig.add_trace(go.Scatter(x=RAOs_direction['RAOPeriodOrFreq'],
                                     y=RAOs_direction['RAOSwayAmp'],
                                     name=RAODirection,
                                     legendgroup=legend_group,
                                     showlegend=False,
                                     mode='lines'),
                          row=2,
                          col=1)
            fig.add_trace(go.Scatter(x=RAOs_direction['RAOPeriodOrFreq'],
                                     y=RAOs_direction['RAOYawAmp'],
                                     name=RAODirection,
                                     legendgroup=legend_group,
                                     showlegend=False,
                                     mode='lines'),
                          row=3,
                          col=1)
            fig.add_trace(go.Scatter(x=RAOs_direction['RAOPeriodOrFreq'],
                                     y=RAOs_direction['RAOSurgePhase'],
                                     name=RAODirection,
                                     legendgroup=legend_group,
                                     showlegend=False,
                                     mode='lines'),
                          row=3,
                          col=2)
            fig.add_trace(go.Scatter(x=RAOs_direction['RAOPeriodOrFreq'],
                                     y=RAOs_direction['RAOSwayPhase'],
                                     name=RAODirection,
                                     legendgroup=legend_group,
                                     showlegend=False,
                                     mode='lines'),
                          row=3,
                          col=2)
            fig.add_trace(go.Scatter(x=RAOs_direction['RAOPeriodOrFreq'],
                                     y=RAOs_direction['RAOYawPhase'],
                                     name=RAODirection,
                                     legendgroup=legend_group,
                                     showlegend=False,
                                     mode='lines'),
                          row=3,
                          col=2)

        fig.show()

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
