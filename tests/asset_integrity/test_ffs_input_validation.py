"""Design-pressure domain regressions for the standalone FFS helper.

The positive, finite pressure requirement is the API contract requested in
issue 2598, not a standards-derived acceptance limit:
https://github.com/vamseeachanta/digitalmodel/issues/2598
"""

import math
import runpy
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.rsf_calculations import check_ffs_level1


@pytest.mark.parametrize("pressure", [0.0, -1.0, math.nan, math.inf, -math.inf])
def test_invalid_design_pressure_is_rejected(pressure):
    """Zero/negative pressures violate the cited contract; NaN/inf lack a ratio.

    The geometry and strength are synthetic inputs, not published design data.
    """
    with pytest.raises(ValueError, match="design_pressure_psi.*positive.*finite"):
        check_ffs_level1(0.500, 0.250, 0.500, pressure, 12000.0, 12.0)


@pytest.mark.parametrize(
    "pressure, fraction, reduced", [(500.0, 2.0, False), (1000.0, 1.0, False),
                                   (2000.0, 0.5, True)]
)
def test_positive_design_pressure_preserves_pressure_ratio(pressure, fraction, reduced):
    """Independent hand calculation using synthetic inputs, in inches and psi.

    Source: rsf_calculations.check_ffs_level1's documented Barlow relation.
    Substitution gives P = 2 * 12000 * 1 * 0.500 / 12 = 1000 psi.
    For design pressures 500, 1000 and 2000 psi, P/P_design is 2, 1 and 0.5;
    only the last case has P < P_design. This checks arithmetic and validation,
    without asserting qualification against an engineering standard.
    """
    result = check_ffs_level1(0.500, 0.250, 0.500, pressure, 12000.0, 12.0)

    assert result["mawp_psi"] == pytest.approx(1000.0)
    assert result["remaining_pressure_fraction"] == pytest.approx(fraction)
    assert result["mawp_reduced"] is reduced


def test_worked_example_reports_interval_and_check_dispositions(capsys):
    """Independent df=2 calculation for the executable documentation example.

    NIST/SEMATECH e-Handbook, online edition accessed 2026-10-21, sections
    1.3.5.8 and 1.3.6.6.6 (URLs in test_campaign_scatter_interval.py):
    differences [-1, 0, 1] have mean 0, sum of squares 2 and s=sqrt(2/2)=1.
    At df=2 the CDF is 1-exp(-x/2), so q(p)=-2*ln(1-p). Thus the SD
    endpoints are sqrt(2/(-2*ln(0.025))) and sqrt(2/(-2*ln(0.975))),
    which print as 0.5 and 6.3 mm to one decimal place. The pressure ratio
    of 1.0 follows the independent hand calculation above. Zero pressure is
    rejected by the API contract, and a disabled check reports NOT_EVALUATED.
    """
    example = Path(__file__).resolve().parents[2] / "examples/asset_integrity/check_status.py"

    runpy.run_path(str(example), run_name="__main__")

    output = capsys.readouterr().out
    assert "Sample count: 3; degrees of freedom: 2" in output
    assert "Difference SD (mm): 1.0" in output
    assert "95% SD interval (mm): [0.5, 6.3]" in output
    assert "Pressure ratio (synthetic): 1.0" in output
    assert "Rejected pressure: design_pressure_psi must be positive and finite" in output
    assert "Fracture mechanics NOT_EVALUATED: flaw_limits is disabled" in output
