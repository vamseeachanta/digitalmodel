from collections import OrderedDict

from assetutilities.common.yml_utilities import WorkingWithYAML
from modulefinder import ModuleFinder

wwy = WorkingWithYAML()


class LineSetUpWizard:
    def __init__(self):
        pass

    def define_calculation_parameters(self):
        model = OrcFxAPI.Model("inputfile.dat")

        model.general.LineSetupCalculationMode = "Calculate line lengths"

        model.general.LineSetupMaxDamping = 20

        model["Line1"].LineSetupTargetVariable = "Tension"

        model["Line1"].LineSetupLineEnd = "End A"

        model["Line1"].LineSetupTargetValue = 830.0

        model["Line2"].LineSetupIncluded = "No"

        model.InvokeLineSetupWizard()
