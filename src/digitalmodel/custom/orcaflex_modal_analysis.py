import OrcFxAPI

import copy
import os
from collections import OrderedDict

from digitalmodel.common.saveData import saveDataYaml
from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


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
        ou.save_sim_file(model, file)
        
        
        spec = OrcFxAPI.ModalAnalysisSpecification(calculateShapes=True,
                                                   lastMode=5)

        modes = OrcFxAPI.Modes(model, spec)
        
        print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
        print(modes.period)
        
        for modeIndex in range(modes.modeCount):
            details = modes.modeDetails(modeIndex)
            print(details.modeNumber)
            print(details.period)
            # Nodal 3 dof displacements
            print(details.shapeWrtGlobal)
            # TODO Investigate why same value for all modes?
            print(details.percentageInAxialDirection)
            # TODO Investigate why same value for all modes?
            print(details.percentageInAxialDirection)

            # TODO Investigate why same value for all modes?
            for dofIndex in range(modes.dofCount):
                print(f"{modes.owner[dofIndex].name} node {modes.nodeNumber[dofIndex]:2} " \
                f"dof {modes.dof[dofIndex]}: {details.shapeWrtGlobal[dofIndex]}")
