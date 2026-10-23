"""Synthetic unit-RAO workflow checks with hand-worked spectral area.

Source: Orcina web help, 'Waves: Wave spectra', ISSC spectrum section,
accessed 2026-10-22 (live edition):
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Waves,Wavespectra.htm
In angular frequency after change of variables w=2*pi*f:
Q(w)=(5/16)*Hs²*wp⁴*w^-5*exp[-(5/4)*(wp/w)^4].
All states use Tp=2*pi s (wp=1), grid [1/2,1,3/2] rad/s, unit RAO.
At Hs=2 m the three raw ordinates, worked by substitution, are:
q0=40*exp(-20), q1=(5/4)*exp(-5/4),
q2=(40/243)*exp(-20/81). Trapezoidal raw area is
A=q0/4+q1/2+q2/4. The code's declared discrete normalisation
multiplies these ordinates by (Hs²/16)/A, so m0=Hs²/16 exactly.
The significant single amplitude definition is 2*sqrt(m0)=Hs/2.
At Hs=4 and Hs=6 raw ordinates scale by 4 and 9 respectively;
normalised areas are 1 and 9/4 m². Therefore amplitudes are 1,2,3 m.
JONSWAP with explicit gamma=1 reduces to the same base shape.

This checks the normalised discrete contract, not physical spectral resolution.
Criteria, occurrence weights and required percentages are synthetic inputs;
pytest.approx permits arithmetic roundoff, not an engineering tolerance.
"""

import csv
import math

import pytest

from digitalmodel.vessel_seakeeping.workflow import router


def _config(tmp_path, hs=2, spectrum="pm", criterion=1.5):
    return {
        "basename": "vessel_seakeeping",
        "_config_dir_path": str(tmp_path),
        "vessel_seakeeping": {
            "spectrum_type": spectrum,
            "gamma": 1,
            "required_operability_pct": 90,
            "rao_frequency_rad_s": [0.5, 1, 1.5],
            "sea_states": [{"hs": hs, "tp": 2 * math.pi, "probability": 1}],
            "dofs": [{"name": "heave", "rao_amplitude": [1, 1, 1],
                      "criterion": criterion, "unit": "m"}],
            "output_dir": str(tmp_path / "results"),
        },
    }


def _response_rows(tmp_path):
    path = tmp_path / "results" / "vessel_seakeeping_vessel_seakeeping.csv"
    with path.open() as stream:
        return list(csv.DictReader(stream))


def test_unit_rao_pm_hs2(tmp_path):
    """Hand area 2²/16=1/4 m², amplitude 2*sqrt(1/4)=1 m; passes 1.5 m."""
    cfg = _config(tmp_path)
    returned = router(cfg)
    assert returned is cfg
    result = returned["vessel_seakeeping"]
    assert float(_response_rows(tmp_path)[0]["significant_amplitude"]) == pytest.approx(1.0)
    assert result["dofs"][0]["operability_pct"] == pytest.approx(100.0)
    assert result["governing_operability_pct"] == pytest.approx(100.0)
    assert result["screening_status"] == "pass"
    assert returned["screening_status"] == "pass"


def test_unit_rao_bretschneider_hs4(tmp_path):
    """Hand area 4²/16=1 m², amplitude 2 m; exceeds synthetic 1.5 m criterion."""
    result = router(_config(tmp_path, hs=4, spectrum="bretschneider"))["vessel_seakeeping"]
    assert float(_response_rows(tmp_path)[0]["significant_amplitude"]) == pytest.approx(2.0)
    assert result["dofs"][0]["operability_pct"] == pytest.approx(0.0)
    assert result["governing_operability_pct"] == pytest.approx(0.0)
    assert result["screening_status"] == "fail"


def test_unit_rao_jonswap_hs6(tmp_path):
    """gamma=1; hand area 6²/16=9/4 m², amplitude 3 m; passes 3.5 m."""
    result = router(_config(tmp_path, hs=6, spectrum="jonswap", criterion=3.5))[
        "vessel_seakeeping"
    ]
    assert float(_response_rows(tmp_path)[0]["significant_amplitude"]) == pytest.approx(3.0)
    assert result["dofs"][0]["operability_pct"] == pytest.approx(100.0)
    assert result["screening_status"] == "pass"


def test_scatter_weighting_literal_percentage(tmp_path):
    """Amplitudes 1 and 2 m vs 1.5 m; weights 3 and 1: 100*3/(3+1)=75%."""
    cfg = _config(tmp_path)
    cfg["vessel_seakeeping"]["sea_states"] = [
        {"hs": 2, "tp": 2 * math.pi, "probability": 3},
        {"hs": 4, "tp": 2 * math.pi, "probability": 1},
    ]
    result = router(cfg)["vessel_seakeeping"]
    assert result["governing_operability_pct"] == pytest.approx(75.0)
    assert result["screening_status"] == "fail"
    rows = _response_rows(tmp_path)
    assert [float(row["significant_amplitude"]) for row in rows] == pytest.approx([1, 2])


def test_all_dofs_must_pass(tmp_path):
    """At m0_wave=1/4, RAOs 1 and 2 give amplitudes 1 and 2 m respectively."""
    cfg = _config(tmp_path)
    cfg["vessel_seakeeping"]["dofs"].append(
        {"name": "synthetic_rotation", "rao_amplitude": [2, 2, 2],
         "criterion": 1.5, "unit": "deg"}
    )
    result = router(cfg)["vessel_seakeeping"]
    assert [row["operability_pct"] for row in result["dofs"]] == [100.0, 0.0]
    assert result["governing_dof"] == "synthetic_rotation"
    assert result["screening_status"] == "fail"


def test_missing_rao_fails_closed(tmp_path):
    """A missing frequency/RAO basis must raise before any passing result."""
    cfg = _config(tmp_path)
    del cfg["vessel_seakeeping"]["rao_frequency_rad_s"]
    with pytest.raises(ValueError, match="rao_frequency_rad_s"):
        router(cfg)
    assert "screening_status" not in cfg
    assert not (tmp_path / "results").exists()


def test_nan_criterion_fails_closed(tmp_path):
    """NaN criterion is refused; infinity is a separately documented code gap."""
    cfg = _config(tmp_path, criterion=float("nan"))
    with pytest.raises(ValueError, match="criterion"):
        router(cfg)
    assert "screening_status" not in cfg
    assert not (tmp_path / "results").exists()
