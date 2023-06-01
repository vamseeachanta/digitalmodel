# Modal analysis test

import os
import sys

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.custom.rao_analysis import RAOAnalysis

ymlfile = 'test_data/rao_analysis.yml'

if not os.path.isfile(ymlfile):
    ymlfile = os.path.join(os.path.dirname(__file__), ymlfile)
    print(os.path.isfile(ymlfile))
print(os.path.isfile(ymlfile))

cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'rao_analysis'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

rao = RAOAnalysis()

cfg_base = rao.read_orcaflex_raos(application_manager.cfg)
