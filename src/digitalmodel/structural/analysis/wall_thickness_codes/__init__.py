# ABOUTME: Package init that imports all code strategy modules to trigger auto-registration
# ABOUTME: Adding a new code requires only adding a new module and importing it here

from .base import CODE_REGISTRY, CodeStrategy, register_code
from .dnv_st_f101 import DnvStF101Strategy
from .api_rp_1111 import ApiRp1111Strategy
from .api_rp_2rd import ApiRp2rdStrategy
from .api_std_2rd import ApiStd2rdStrategy
from .pd_8010_2 import Pd80102Strategy
from .asme_b31_8 import AsmeB318Strategy
from .iso_13623 import Iso13623Strategy
