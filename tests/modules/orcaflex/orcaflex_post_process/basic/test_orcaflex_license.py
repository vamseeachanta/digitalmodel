# Reader imports
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()

def test_orcaflex_license():
    orcaflex_license_flag = ou.is_orcaflex_available()
    assert (orcaflex_license_flag)

test_orcaflex_license()