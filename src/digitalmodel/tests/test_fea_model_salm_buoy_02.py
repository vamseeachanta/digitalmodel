# Fatigue analysis is not functional

import os
import sys

from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.fea_model import fea_model

ymlfile = 'src/digitalmodel/tests/test_data/fea_model/SALM_Rev1.yml'
sys.argv.append(ymlfile)
print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'fea_model'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

cfg_base = fea_model(application_manager.cfg)
