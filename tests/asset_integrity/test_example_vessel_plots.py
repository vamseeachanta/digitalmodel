import pytest
from digitalmodel.asset_integrity.assessment.example_vessel_plots import peak_by_element


def test_display_retains_element_peak_instead_of_averaging():
    rows = [dict(element_id=1, sx=v, sy=0, sz=0, sxy=0, syz=0, sxz=0) for v in (10,100,20,30)]
    assert peak_by_element(rows) == {1: 100}
    rows[0]['sx'] = float('nan')
    with pytest.raises(ValueError):
        peak_by_element(rows)
