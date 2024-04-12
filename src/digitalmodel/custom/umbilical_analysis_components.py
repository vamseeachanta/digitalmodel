import os
import math
import pandas as pd
from assetutilities.common.update_deep import update_deep_dictionary

from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.data import SaveData

from assetutilities.common.utilities import is_file_valid_func

from digitalmodel.common.orcaflex_linetypes import OrcaflexLineTypes
from digitalmodel.custom.umbilical_installation_IV_to_host import InstallationVtoHost

save_data = SaveData()
olt = OrcaflexLineTypes()
iv_to_host = InstallationVtoHost()
class UmbilicalAnalysis():

    def __init__(self):
        pass

    def perform_analysis(self, cfg):
        if cfg['installation_phases']:
            # TODO program assumes host at 0 deg and No Y coordinate for installation vessel reference point. 
            # Program this feature for future analysis
            self.installation_phases(cfg)
        elif cfg['line_properties']:
            olt.get_umbilical_lines(cfg)
        else:
            raise NotImplementedError("Analysis not implemented.")

        return cfg

    def first_end_analysis(self):
        pass

    def second_end_analysis(self):
        pass

    def installation_phases(self, cfg):
        for phase in cfg['installation']['phase']:
            if cfg['installation']['phase_type'] == 'installationV_to_host':
                iv_to_host.installation_phase(cfg, phase)
            elif cfg['installation']['phase_type'] == '2nd_end_installation':
                #TODO
                self.installation_phase(cfg, phase)

        return cfg

