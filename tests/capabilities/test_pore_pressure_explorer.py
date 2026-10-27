"""Regression checks for the synthetic pore-pressure explorer.

Input basis: synthetic illustrative inputs defined in the builder, assumed
and unsourced, based on previous experience rather than measured well data.
Equation source: Eaton (1975), SPE 5544, sonic relation documented in
src/digitalmodel/well/drilling/pore_pressure.py (Methods).
"""

import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/capabilities/build_pore_pressure_explorer.py"
OUTPUT = ROOT / "docs/api/structural"


@pytest.fixture
def non_utf8_default(monkeypatch):
    """Emulate a platform whose default text decoding uses a legacy codepage."""
    original = Path.read_text

    def read_text(path, encoding=None, errors=None):
        return original(path, encoding=encoding or "cp1252", errors=errors)

    monkeypatch.setattr(Path, "read_text", read_text)


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("pore_pressure_explorer", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


# Depths are the existing synthetic grid up to and including its onset.
@pytest.mark.parametrize("depth", [5000, 5500, 6000, 6500, 7000, 7500, 8000])
def test_normal_interval_uses_normal_pressure(builder, depth):
    """Hand calculation: dto=dtn, so Sv-(Sv-8.6)*1**3=8.6 ppg.

    The illustrative mud indicator adds the existing assumed 0.3 ppg margin:
    8.6+0.3=8.9 ppg. Exact decimal equality checks the serialized precision;
    no engineering acceptance tolerance is introduced.
    """
    point = next(p for p in builder.build_study()["series"] if p["z"] == depth)
    assert point["pp"] == 8.6
    assert point["mw"] == 8.9


@pytest.mark.parametrize("overburden_ppg", [12.0, 16.5, 19.0])
def test_on_trend_pressure_is_independent_of_synthetic_overburden(
    builder, monkeypatch, overburden_ppg
):
    """Synthetic inputs for issue #2707, assumed, based on previous experience.

    Depths are 5,000 through 8,000 ft at 500 ft spacing, including the
    8,000 ft onset. Normal sonic is 170 - 0.004*z us/ft; observed sonic
    equals normal throughout this interval. Each case holds overburden at
    12.0, 16.5 or 19.0 ppg. Pn is independently declared as 8.6 ppg and
    the illustrative mud margin is 0.3 ppg; no calibration is supplied.

    The sonic relation cited in pore_pressure.py (Methods, SPE 5544) gives
    observed/normal = 1, hence Pp = Sv - (Sv - Pn)*1**n = Pn for any n.
    Thus every overburden case gives 8.6 ppg pressure and 8.6+0.3=8.9 ppg
    mud indicator. Exact equality checks the builder's decimal output.
    """
    depths = [5000, 5500, 6000, 6500, 7000, 7500, 8000]
    monkeypatch.setattr(builder, "_DEPTH", depths)
    monkeypatch.setattr(builder, "_PN", 8.6)
    monkeypatch.setattr(builder, "_KICK_MARGIN", 0.3)
    monkeypatch.setattr(builder, "_overburden", lambda z: overburden_ppg)

    study = builder.build_study()

    assert study["meta"]["pn"] == 8.6
    assert study["meta"]["kick_margin"] == 0.3
    assert study["meta"]["governing_method"] == "eaton"
    assert study["meta"]["bowers_status"] == "NOT_EVALUATED"
    assert study["meta"]["bowers_reason"] == (
        "No Bowers calibration is supplied for the synthetic trend."
    )
    assert study["meta"]["data_basis"] == "assumed, based on previous experience"
    assert study["series"] == [
        {"z": z, "sv": overburden_ppg, "eaton": 8.6, "bowers": None,
         "pp": 8.6, "mw": 8.9}
        for z in depths
    ]


def test_missing_calibration_survives_serialization(builder):
    """Round-trip the synthetic study for issue #2707 without a calibration.

    Inputs from build_pore_pressure_explorer.py are assumed, based on
    previous experience: depths 5,000..14,000 ft every 500 ft; Sv rises
    linearly from 16.5 to 19.0 ppg; Pn=8.6 ppg; margin=0.3 ppg; normal
    sonic=170-0.004*z us/ft. Observed/normal is unity through the 8,000 ft
    onset, then rises linearly to 1.35 at 14,000 ft. No calibration is given.

    Using the sonic relation cited in pore_pressure.py (Methods, SPE 5544),
    unity gives Pp=Sv-(Sv-Pn)=8.6 ppg and mud=8.6+0.3=8.9 ppg. At the
    endpoint, Pp=19-(19-8.6)/1.35**3 rounds to 14.77 ppg and the mud
    indicator rounds to 15.07 ppg. These are independent hand calculations.
    """
    study = builder.build_study()
    encoded = json.dumps(study, allow_nan=False)
    restored = json.loads(encoded)

    assert restored == study
    assert restored["meta"]["governing_method"] == "eaton"
    assert restored["meta"]["bowers_status"] == "NOT_EVALUATED"
    assert restored["meta"]["bowers_reason"] == (
        "No Bowers calibration is supplied for the synthetic trend."
    )
    assert restored["meta"]["data_basis"] == "assumed, based on previous experience"
    assert all(point["bowers"] is None for point in restored["series"])
    assert all(point["pp"] == point["eaton"] for point in restored["series"])
    normal_points = [point for point in restored["series"] if point["z"] <= 8000]
    assert [(point["z"], point["pp"], point["mw"]) for point in normal_points] == [
        (z, 8.6, 8.9) for z in [5000, 5500, 6000, 6500, 7000, 7500, 8000]
    ]
    endpoint = next(point for point in restored["series"] if point["z"] == 14000)
    assert (endpoint["pp"], endpoint["mw"]) == (14.77, 15.07)


def test_overpressure_ramp_retains_eaton_response(builder):
    """Hand calculation at the existing synthetic endpoint (14,000 ft).

    Sv=19, Pn=8.6, dto/dtn=1.35. Eaton: 19-(19-8.6)/1.35**3
    rounds to 14.77 ppg; adding the assumed 0.3 margin rounds to 15.07.
    These are arithmetic expectations, independent of the implementation.
    """
    point = next(p for p in builder.build_study()["series"] if p["z"] == 14000)
    assert point["eaton"] == 14.77
    assert point["pp"] == 14.77
    assert point["mw"] == 15.07


def test_uncalibrated_bowers_is_not_evaluated(builder, monkeypatch):
    """A missing calibration supplies no numeric prediction or governing vote."""
    from digitalmodel.well.drilling import pore_pressure

    def reject_uncalibrated_call(**kwargs):
        pytest.fail("The explorer must not evaluate uncalibrated Bowers defaults")

    monkeypatch.setattr(pore_pressure, "bowers_pore_pressure", reject_uncalibrated_call)
    # Also catch the old builder's direct import before it is removed.
    if hasattr(builder, "bowers_pore_pressure"):
        monkeypatch.setattr(builder, "bowers_pore_pressure", reject_uncalibrated_call)
    study = builder.build_study()
    assert study["meta"]["bowers_status"] == "NOT_EVALUATED"
    assert "calibrat" in study["meta"]["bowers_reason"].lower()
    assert study["meta"]["governing_method"] == "eaton"
    assert study["meta"]["data_basis"] == "assumed, based on previous experience"
    assert all(p["bowers"] is None for p in study["series"])
    assert all(p["pp"] == p["eaton"] for p in study["series"])


def test_generated_page_and_json_expose_missing_calibration(
    builder, monkeypatch, tmp_path, non_utf8_default
):
    """Generated files agree and the page handles a missing Bowers number."""
    monkeypatch.setattr(builder, "_REPO", tmp_path)
    monkeypatch.setattr(builder, "_OUT_DIR", tmp_path)
    builder.main()
    study = json.loads((tmp_path / "pore-pressure-explorer.json").read_text(encoding="utf-8"))
    page = (tmp_path / "pore-pressure-explorer.html").read_text(encoding="utf-8")
    payload = page.split("const DATA = ", 1)[1].split(";\nconst M", 1)[0]
    assert page == builder._HTML.replace(
        "{data}", json.dumps(study, separators=(",", ":"))
    )
    assert json.loads(payload) == study
    assert "p.bowers === null" in page
    assert "NOT_EVALUATED" in page
    assert "Bowers is not evaluated" in page
    assert "assumed, based on previous experience" in page


def test_committed_outputs_match_builder(builder, non_utf8_default):
    """A regenerated page and JSON must carry the same prediction and status."""
    study = builder.build_study()
    assert (OUTPUT / "pore-pressure-explorer.json").read_text(encoding="utf-8") == (
        json.dumps(study, indent=2) + "\n"
    )
    assert (OUTPUT / "pore-pressure-explorer.html").read_text(encoding="utf-8") == (
        builder._HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    )


def test_page_labels_band_as_illustrative(builder):
    """The display supplies no evaluated fracture or stability boundary."""
    page = builder._HTML
    assert 'Mud-weight indicator<b id="rM"' in page
    assert "Shaded: illustrative band" in page
    assert "assumed 0.3 ppg margin" in page
    assert "no fracture or stability criterion is evaluated" in page
    assert "safe band" not in page
    assert "ceiling" not in page
    assert "Min. mud weight" not in page
