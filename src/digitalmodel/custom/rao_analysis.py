import os
import pandas as pd

from digitalmodel.common.yml_utilities import ymlInput


class RAOAnalysis:

    def __init__(self):
        pass

    def read_orcaflex_raos(self, cfg=None):
        self.cfg = cfg
        for file in cfg['Files']:
            if os.path.isfile(file['Name']):
                self.read_rao_file(file['Name'])

    def read_rao_file(self, file_name):
        print('Reading file: {}'.format(file_name))
        yml_data = ymlInput(file_name, updateYml=None)
        rao_data = yml_data['VesselTypes'][0]
        print(rao_data)
