import os
import sys

from digitalmodel.common.ymlInput import ymlInput
from digitalmodel.common.update_deep import AttributeDict
from digitalmodel.common.ApplicationManager import ConfigureApplicationInputs
from digitalmodel.vertical_riser import vertical_riser

ymlfile = 'src/digitalmodel/tests/test_data/vertical_riser.yml'
sys.argv.append(ymlfile)
print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'vertical_riser'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

cfg_base = vertical_riser(application_manager.cfg)
