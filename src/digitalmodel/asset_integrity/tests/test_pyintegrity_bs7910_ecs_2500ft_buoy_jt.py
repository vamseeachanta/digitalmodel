import os
import sys

from ..common.ymlInput import ymlInput
from ..common.update_deep import AttributeDict
from ..common.ApplicationManager import ConfigureApplicationInputs
from ..fracture_mechanics import fracture_mechanics

ymlfile = 'src/asset_integrity/tests/test_data/fracture_mechanics/fracture_mechanics_py_ecs_2500ft_buoy_jt.yml'
sys.argv.append(ymlfile)
print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'fracture_mechanics'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

cfg_base = fracture_mechanics(application_manager.cfg)
