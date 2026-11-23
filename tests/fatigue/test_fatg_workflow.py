"""Workflow (router) tests for the fatg_spectral_fatigue basename."""

import csv

import pytest

from digitalmodel.fatg_spectral_fatigue.workflow import router
from digitalmodel.fatigue.fatg import FatgSeaState, fatg_annual_damage

OMEGA = [0.2 + 0.1 * i for i in range(18)]
RAO = [
    120.0, 260.0, 450.0, 700.0, 1050.0, 1500.0,
    2050.0, 2600.0, 2950.0, 3000.0, 2750.0, 2300.0,
    1800.0, 1350.0, 950.0, 620.0, 380.0, 210.0,
]
SCATTER = [
    (3.0, 5.0, 0.55, 0.6, 0.3, 0.1),
    (8.0, 7.0, 0.35, 0.5, 0.35, 0.15),
    (15.0, 9.5, 0.10, 0.4, 0.4, 0.2),
]


def _cfg(tmp_path, **overrides):
    settings = {
        "design_life_years": 20.0,
        "dff": 1.0,
        "sn_curve": "F",
        "member_type": "chord",
        "rao": {
            "omega_rad_per_s": OMEGA,
            "stress_per_wave_height": RAO,
        },
        "sea_states": [
            {
                "hs": hs,
                "tz": tz,
                "occurrence_fraction": occ,
                "spreading": {"p0": p0, "p45": p45, "p90": p90},
            }
            for hs, tz, occ, p0, p45, p90 in SCATTER
        ],
        "output_dir": str(tmp_path / "results"),
    }
    settings.update(overrides)
    return {
        "basename": "fatg_spectral_fatigue",
        "fatg_spectral_fatigue": settings,
        "_config_dir_path": str(tmp_path),
    }


def _library_reference(sn_curve="F", member_type="chord", spreading=True):
    states = [
        FatgSeaState(
            hs=hs,
            tz=tz,
            occurrence=occ,
            spreading=(p0, p45, p90) if spreading else None,
        )
        for hs, tz, occ, p0, p45, p90 in SCATTER
    ]
    return fatg_annual_damage(
        OMEGA, RAO, states, sn_curve=sn_curve, member_type=member_type
    )


def test_router_matches_library(tmp_path):
    cfg = router(_cfg(tmp_path))
    expected = _library_reference()
    result = cfg["fatg_spectral_fatigue"]
    assert result["annual_damage"] == pytest.approx(expected.annual_damage, rel=1e-12)
    assert result["fatigue_life_years"] == pytest.approx(
        expected.fatigue_life_years, rel=1e-12
    )
    assert result["life_used_percent_1yr"] == pytest.approx(
        expected.annual_damage * 100.0, rel=1e-12
    )
    assert result["life_used_percent_10yr"] == pytest.approx(
        expected.annual_damage * 1000.0, rel=1e-12
    )
    per_state = [row["damage_per_year"] for row in result["sea_states"]]
    assert per_state == pytest.approx(
        [item.damage_per_year for item in expected.sea_states], rel=1e-12
    )


def test_router_screening_status(tmp_path):
    # the synthetic deck uses ~3.3 lives per year -> fails a 20-year check
    cfg = router(_cfg(tmp_path))
    assert cfg["fatg_spectral_fatigue"]["screening_status"] == "fail"
    assert cfg["screening_status"] == "fail"
    assert cfg["fatg_spectral_fatigue"]["margin"] < 1.0

    # a much softer RAO passes
    soft = {"omega_rad_per_s": OMEGA, "stress_per_wave_height": [v / 200.0 for v in RAO]}
    cfg = router(_cfg(tmp_path, rao=soft))
    assert cfg["fatg_spectral_fatigue"]["screening_status"] == "pass"
    assert cfg["screening_status"] == "pass"


def test_router_writes_csv_outputs(tmp_path):
    cfg = router(_cfg(tmp_path))
    result = cfg["fatg_spectral_fatigue"]
    results_csv = tmp_path / "results" / "fatg_spectral_fatigue_fatg_spectral_fatigue.csv"
    summary_csv = (
        tmp_path / "results" / "fatg_spectral_fatigue_fatg_spectral_fatigue_summary.csv"
    )
    assert results_csv.exists()
    assert summary_csv.exists()
    with results_csv.open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == len(SCATTER)
    assert float(rows[0]["significant_stress_range"]) == pytest.approx(
        result["sea_states"][0]["significant_stress_range"]
    )


def test_router_explicit_sn_mapping_equals_named_curve(tmp_path):
    named = router(_cfg(tmp_path))["fatg_spectral_fatigue"]["annual_damage"]
    explicit = router(
        _cfg(tmp_path, sn_curve={"log10_a": 18.2845, "slope": 3.0})
    )["fatg_spectral_fatigue"]["annual_damage"]
    assert explicit == pytest.approx(named, rel=1e-12)


def test_router_uniform_mode_when_spreading_omitted(tmp_path):
    sea_states = [
        {"hs": hs, "tz": tz, "occurrence_fraction": occ}
        for hs, tz, occ, *_ in SCATTER
    ]
    cfg = router(_cfg(tmp_path, sea_states=sea_states, member_type="general"))
    expected = _library_reference(member_type="general", spreading=False)
    assert cfg["fatg_spectral_fatigue"]["annual_damage"] == pytest.approx(
        expected.annual_damage, rel=1e-12
    )


def test_router_rejects_bad_inputs(tmp_path):
    with pytest.raises(ValueError):
        router(_cfg(tmp_path, sn_curve="Z"))
    with pytest.raises(ValueError):
        router(_cfg(tmp_path, sea_states=[]))
    with pytest.raises(ValueError):
        router(_cfg(tmp_path, rao={"omega_rad_per_s": OMEGA}))
    missing_dff = _cfg(tmp_path)
    del missing_dff["fatg_spectral_fatigue"]["dff"]
    with pytest.raises(ValueError):
        router(missing_dff)


def test_engine_registers_basename():
    """The engine dispatch table includes basename fatg_spectral_fatigue.

    Checked against the source file (not an engine import) to keep this
    test lightweight; the router itself is exercised above.
    """
    import importlib.util
    from pathlib import Path

    spec = importlib.util.find_spec("digitalmodel.engine")
    source = Path(spec.origin).read_text(encoding="utf-8", errors="ignore")
    assert 'basename == "fatg_spectral_fatigue"' in source
    assert "from digitalmodel.fatg_spectral_fatigue.workflow import" in source
