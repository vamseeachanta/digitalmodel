# Modal analysis test

import os
import sys

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.custom.rao_analysis import RAOAnalysis

ymlfile = 'C:/Users/ss7a2365/Documents/github/digitalmodel/src/digitalmodel/tests/test_data/rao_analysis.yml'
sys.argv.append(ymlfile)
# print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'rao_analysis'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

rao = RAOAnalysis()

cfg_base = rao.read_orcaflex_raos(application_manager.cfg)
