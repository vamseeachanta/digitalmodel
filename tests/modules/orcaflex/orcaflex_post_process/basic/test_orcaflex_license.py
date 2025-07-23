# Reader imports
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()

def test_orcaflex_license():
    orcaflex_license_flag = ou.is_orcaflex_available()
    if not orcaflex_license_flag:
        import pytest
        pytest.skip("OrcaFlex license not available")
    assert (orcaflex_license_flag)