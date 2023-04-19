import OrcFxAPI

import copy
import os
from collections import OrderedDict

from digitalmodel.common.saveData import saveDataYaml
from digitalmodel.common.yml_utilities import ymlInput


class OrcModalAnalysis:

    def __init__(self):
        pass

    def run_modal_analysis(self, cfg=None):
        if cfg['default']['Analysis']['Analyze']['modal']:
            files = cfg['Files']
            for file in files:
                self.run_by_file(file=file['Name'])

    def run_by_file(self, file=None):
        model = OrcFxAPI.Model()
        model.LoadData(file)
        model.CalculateStatics()

        spec = OrcFxAPI.ModalAnalysisSpecification(calculateShapes=False,
                                                   lastMode=5)

        modes = OrcFxAPI.Modes(model, spec)
        print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
        print(modes.period)
