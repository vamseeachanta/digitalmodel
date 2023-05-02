import os
import pandas as pd

from digitalmodel.common.yml_utilities import ymlInput


class RAOAnalysis:

    def __init__(self):
        pass

    def read_orcaflex_raos(self, cfg=None):
        self.cfg = cfg
        vessel_data_all_files = {}
        for file in cfg['Files']:
            if os.path.isfile(file['Name']):
                vessel_data = self.read_vessel_data_from_file(file['Name'])
                vessel_data_all_files.update({file['Label']: vessel_data})

        self.vessel_data_all_files = vessel_data_all_files

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


# check amplitudes,
# check phases,
# check locations
