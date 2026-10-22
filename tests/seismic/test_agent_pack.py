# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent synthetic arithmetic for the seismic knowledge pack.

Numerical tolerances below allow floating point roundoff only; they are not
engineering acceptance limits. Inputs are synthetic, in seconds and SI units.
"""

import numpy as np
import pytest

from digitalmodel.seismic.baseline import polynomial_detrend, pre_event_mean
from digitalmodel.seismic.readers.ascii import read_ascii_accelerogram
from digitalmodel.seismic.response_spectrum.calculations import compute_kinematics
from digitalmodel.seismic.response_spectrum.render import render_report
from digitalmodel.seismic.response_spectrum.workflow import router


def test_centimetre_conversion(tmp_path):
    """SI prefix centi is 1/100: [0,100,-200] cm/s² -> [0,1,-2] m/s².

    Basis: NIST SP 811 (2008), Appendix B.8, acceleration conversion table.
    Time [0,1,2] gives two 1 s intervals; max absolute acceleration is 2.
    """
    path = tmp_path / "motion.asc"
    path.write_text("0 0\n1 100\n2 -200\n", encoding="utf-8")
    record = read_ascii_accelerogram(path, units="cm_s2")
    assert record.acceleration_m_s2.tolist() == [0.0, 1.0, -2.0]
    assert record.dt_s == 1.0
    assert record.npts == 3
    assert record.pga_m_s2 == 2.0


def test_mean_subtraction():
    """Hand mean (2+4)/2=3; [2,4,8]-3=[-1,1,5] m/s².

    Definition of arithmetic mean and subtraction; no empirical coefficient.
    """
    corrected, offset = pre_event_mean([2.0, 4.0, 8.0], samples=2)
    assert offset == 3.0
    assert corrected.tolist() == [-1.0, 1.0, 5.0]


def test_linear_detrend():
    """Hand line a=2t+1 through (0,1),(1,3),(2,5): slope 2, intercept 1.

    Least-squares residuals are zero. Basis: NumPy 2.2 polyfit, Notes.
    The absolute tolerance 1e-12 covers roundoff about exact zero only.
    """
    corrected, coefficients = polynomial_detrend([0, 1, 2], [1, 3, 5], order=1)
    assert coefficients.tolist() == pytest.approx([2.0, 1.0], abs=1e-12)
    np.testing.assert_allclose(corrected, [0.0, 0.0, 0.0], atol=1e-12)


def test_two_trapezoids(tmp_path):
    """With a=[0,2,2], dt=1: v=[0,1,3], d=[0,0.5,2.5].

    Each increment is dt*(left+right)/2; both initial values are zero.
    Basis: SciPy 1.15.3 cumulative_trapezoid, composite trapezoidal rule.
    This is a discrete kinematic example, not an oscillator pulse solution.
    """
    path = tmp_path / "motion.asc"
    path.write_text("0 0\n1 2\n2 2\n", encoding="utf-8")
    result = compute_kinematics(
        read_ascii_accelerogram(path),
        baseline={"method": "none"},
        frequency_filter={"enabled": False},
    )
    assert result.timeseries.velocity_m_s.tolist() == [0.0, 1.0, 3.0]
    assert result.timeseries.displacement_m.tolist() == [0.0, 0.5, 2.5]
    assert result.method == "kinematic_cumtrapz"


def test_render_requires_provenance(tmp_path):
    """An undeclared data source must not produce a report."""
    from digitalmodel.reporting import ProvenanceError

    path = tmp_path / "motion.asc"
    path.write_text("0 0\n1 2\n2 2\n", encoding="utf-8")
    result = compute_kinematics(read_ascii_accelerogram(path))
    with pytest.raises(ProvenanceError):
        render_report(result, data_source=None)


def test_direct_router_contract(tmp_path):
    """Exercise file outputs without importing the general solver engine.

    Trapezoid hand values are derived in test_two_trapezoids above.
    """
    path = tmp_path / "motion.asc"
    path.write_text("0 0\n1 2\n2 2\n", encoding="utf-8")
    cfg = {
        "_config_dir_path": str(tmp_path),
        "basename": "synthetic",
        "response_spectrum": {
            "record": {"file": "motion.asc"},
            "baseline": {"method": "none"},
            "filter": {"enabled": False},
            "output_dir": "results",
        },
    }
    assert router(cfg) is cfg
    assert cfg["response_spectrum"]["summary"]["pga_m_s2"] == 2.0
    assert (tmp_path / "results/synthetic_timeseries.csv").is_file()
    report = (tmp_path / "results/synthetic_report.html").read_text()
    assert "Kinematic acceleration integration" in report
    assert str(tmp_path) not in report
