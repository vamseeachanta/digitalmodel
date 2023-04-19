# Modal analysis test

import os
import sys

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.custom.orcaflex_modal_analysis import OrcModalAnalysis

ymlfile = 'src/digitalmodel/tests/test_data/orcaflex_modal_analysis.yml'
sys.argv.append(ymlfile)
print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'fea_model'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

oma = OrcModalAnalysis()

cfg_base = oma.run_modal_analysis(application_manager.cfg)
