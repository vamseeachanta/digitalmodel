import os
import sys

from pyintegrity.common.ymlInput import ymlInput
from pyintegrity.common.update_deep import AttributeDict
from pyintegrity.common.ApplicationManager import ConfigureApplicationInputs
from pyintegrity.fracture_mechanics import fracture_mechanics

ymlfile = 'src/pyintegrity/tests/test_data/fracture_mechanics/fracture_mechanics_py_ecs_2500ft_buoy_jt.yml'
sys.argv.append(ymlfile)
print(os.path.isfile(ymlfile))
cfg = ymlInput(ymlfile, updateYml=None)
cfg = AttributeDict(cfg)

basename = 'fracture_mechanics'
application_manager = ConfigureApplicationInputs(basename)
application_manager.configure(run_dict=None)

cfg_base = fracture_mechanics(application_manager.cfg)
