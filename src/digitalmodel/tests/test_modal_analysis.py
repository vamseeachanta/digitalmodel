# Modal analysis test

import os
import sys

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.custom.orcaflex_modal_analysis import OrcModalAnalysis

ymlfile = 'C:/Users/ss7a2365/Documents/CVX_Ballymore/04-RES/dz_45deg_modal_analysis.yml'
sys.argv.append(ymlfile)
# print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

oma = OrcModalAnalysis()

cfg_base = oma.run_modal_analysis(cfg)
