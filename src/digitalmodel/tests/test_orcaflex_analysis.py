# Fatigue analysis is not functional

import os
import sys

from digitalmodel.common.ymlInput import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.orcaflex_analysis import orcaflex_analysis

ymlfile = 'src/digitalmodel/tests/test_data/orcaflex_analysis.yml'
sys.argv.append(ymlfile)
print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'orcaflex_analysis'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

cfg_base = orcaflex_analysis(application_manager.cfg)
