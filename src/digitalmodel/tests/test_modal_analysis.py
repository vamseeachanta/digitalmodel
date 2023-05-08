# Modal analysis test

import os
import sys

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.common.orcaflex_model_components import OrcaflexUtilities
from digitalmodel.custom.orcaflex_modal_analysis import OrcModalAnalysis

ymlfile = 'test_data/modal_analysis.yml'
sys.argv.append(ymlfile)
if not os.path.isfile(ymlfile):
    ymlfile = os.path.join(os.path.dirname(__file__), ymlfile)
    print(os.path.isfile(ymlfile))
print(os.path.isfile(ymlfile))

cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'modal_analysis'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

ou = OrcaflexUtilities()
orcaflex_license_flag = ou.is_orcaflex_available()

oma = OrcModalAnalysis()
cfg_base = oma.run_modal_analysis(application_manager.cfg)
