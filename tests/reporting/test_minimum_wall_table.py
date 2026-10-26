# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Check grid sizing independently, then reproduce the pinned table cells."""

import csv
import hashlib
import io
import json
from pathlib import Path
import subprocess

import pytest

from scripts.reporting import build_minimum_wall_table as table
from digitalmodel.structural.analysis.wall_thickness import DesignCode

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "docs/reports/minimum-wall-table"


def config():
    return json.loads(table.INPUT.read_text())


@pytest.mark.parametrize("code", list(DesignCode))
def test_pressure_minimum_is_first_passing_grid_point(code):
    cfg = config()
    minimum = table.pressure_minimum(cfg, code, 20.0)
    # Exhaust every earlier point: no monotonicity assumption in this oracle.
    for tick in range(1, round(minimum["wall_mm"] * 10)):
        wall = tick / 10
        if wall <= cfg["pipe"]["corrosion_allowance_mm"]:
            continue
        checks = table.pressure_checks(cfg, code, wall, 20.0)
        assert max(checks.values()) > 1
    checks = table.pressure_checks(cfg, code, minimum["wall_mm"], 20.0)
    assert max(checks.values()) <= 1


@pytest.mark.parametrize("code", [
    DesignCode.ASME_B31_4, DesignCode.PD_8010_2, DesignCode.ISO_13623,
    DesignCode.DNV_ST_F101, DesignCode.DNV_ST_F201,
])
def test_corrosion_is_deducted_once_on_all_five_native_routes(code):
    cfg = config()
    cfg["pipe"]["corrosion_allowance_mm"] = 0
    without = table.pressure_minimum(cfg, code, 20.0)["wall_mm"]
    cfg["pipe"]["corrosion_allowance_mm"] = 3
    with_allowance = table.pressure_minimum(cfg, code, 20.0)["wall_mm"]
    assert with_allowance == pytest.approx(without + 3, abs=1e-10)


@pytest.mark.parametrize("code", [
    DesignCode.API_RP_1111, DesignCode.API_RP_2RD,
    DesignCode.API_STD_2RD, DesignCode.ASME_B31_8,
])
def test_corrosion_is_added_once_on_nominal_pressure_routes(code):
    cfg = config()
    cfg["pipe"]["corrosion_allowance_mm"] = 0
    without = table.pressure_minimum(cfg, code, 20.0)["wall_mm"]
    cfg["pipe"]["corrosion_allowance_mm"] = 3
    assert table.pressure_minimum(cfg, code, 20.0)["wall_mm"] == pytest.approx(
        without + 3, abs=1e-10)


@pytest.mark.parametrize("code", [DesignCode.DNV_ST_F101, DesignCode.DNV_ST_F201])
def test_fabrication_tolerance_is_multiplicative_fraction(code):
    cfg = config()
    # Equal effective wall: (19 - 3) * (1 - 0.125) = 14 mm.
    reduced = table.pressure_checks(cfg, code, 19.0, 20.0)
    cfg["pipe"]["fabrication_tolerance_fraction"] = 0
    unreduced = table.pressure_checks(cfg, code, 17.0, 20.0)
    assert reduced == pytest.approx(unreduced)


def test_independent_membrane_equilibrium_anchor():
    """First principles: a diametral cut gives 2*t*L*sigma = p*D*L.

    With D=400 mm, yield=400 MPa and the implementation's fixed 0.72
    factor, allowable membrane stress is 288 MPa. At p=18 MPa,
    t=18*400/(2*288)=12.5 mm; add 3 mm corrosion once: 15.5 mm.
    This is an independent hand calculation, not an output of the strategy.
    """
    cfg = config()
    cfg["external_pressure_mpa"] = 0
    result = table.pressure_minimum(cfg, DesignCode.ASME_B31_4, 18.0)
    assert result["wall_mm"] == 15.5
    assert result["governing_check"] == "burst"
    assert table.pressure_checks(cfg, DesignCode.ASME_B31_4, 15.5, 18)["burst"] == 1


def test_b31_4_ignores_external_pressure_and_fixed_factors():
    cfg = config()
    a = table.pressure_checks(cfg, DesignCode.ASME_B31_4, 20, 20)
    cfg["external_pressure_mpa"] = 0
    assert table.pressure_checks(cfg, DesignCode.ASME_B31_4, 20, 20) == a


def test_stock_search_steps_until_collapse_and_propagation_pass(monkeypatch):
    """An independent discrete oracle makes collapse and propagation govern."""
    cfg = config()
    cfg["stock_walls_mm"] = [10, 12, 14, 16]
    monkeypatch.setattr(table, "pressure_minimum", lambda *a: {
        "wall_mm": 9.0, "governing_check": "burst"})
    monkeypatch.setattr(table, "pressure_checks", lambda *a: {"burst": 0.5})

    def external(_cfg, _code, wall, _pressure):
        return {"collapse": 1.1 if wall < 12 else 0.7,
                "propagation": 1.1 if wall < 14 else 0.7}

    monkeypatch.setattr(table, "external_checks", external)
    rows = table.rows_for(cfg, DesignCode.ASME_B31_4, 20)
    assert [row["stock_wall_mm"] for row in rows] == [10, 12, 14]
    assert [row["failed_checks"] for row in rows] == [
        "collapse;propagation", "propagation", ""]
    assert rows[-1]["selection_status"] == "selected"
    assert rows[-1]["selection_set_by"] == "propagation"


@pytest.mark.parametrize("code", [DesignCode.ISO_13623, DesignCode.PD_8010_2])
def test_faulty_collapse_is_never_called_or_published(code, monkeypatch):
    strategy = table.CODE_REGISTRY[code]

    def forbidden(*args):
        pytest.fail("Suppressed collapse must not be evaluated")

    monkeypatch.setattr(strategy, "_check_collapse", forbidden)
    rows = table.rows_for(config(), code, 20)
    assert rows
    for row in rows:
        assert row["collapse_status"] == "collapse not shown (#1920)"
        assert row["collapse_utilisation_percent"] == ""
        assert row["selection_status"] != "selected"


@pytest.mark.parametrize("code", [
    DesignCode.API_RP_2RD, DesignCode.API_STD_2RD, DesignCode.DNV_ST_F201,
])
def test_missing_external_checks_do_not_become_a_pass(code):
    rows = table.rows_for(config(), code, 20)
    assert rows[-1]["propagation_status"] == "not implemented"
    assert rows[-1]["selection_status"] == "incomplete check set"


def test_unreachable_pressure_and_stock_are_explicit():
    cfg = config()
    assert table.pressure_minimum(cfg, DesignCode.ASME_B31_4, 1e6) is None
    cfg["stock_walls_mm"] = [4]
    rows = table.rows_for(cfg, DesignCode.ASME_B31_4, 25)
    assert rows[-1]["selection_status"] == "stock list exhausted"
    assert rows[-1]["failed_checks"] == "burst;collapse;propagation"


@pytest.mark.parametrize("field,value", [
    ("pressures_mpa", [float("nan")]), ("pressures_mpa", [-1]),
    ("pressures_mpa", [4]), ("pressures_mpa", []),
    ("stock_walls_mm", [20, 10]), ("stock_walls_mm", [10, 10]),
    ("stock_walls_mm", [200]), ("stock_walls_mm", []),
    ("external_pressure_mpa", -1), ("pressure_basis", "confirmed gauge"),
])
def test_invalid_input_fails_before_writing(tmp_path, field, value):
    cfg = config()
    cfg[field] = value
    source = tmp_path / "input.json"
    source.write_text(json.dumps(cfg))
    with pytest.raises(ValueError):
        table.build(source, tmp_path / "output")
    assert not (tmp_path / "output").exists()


def test_pinned_sources_and_every_table_value_reproduce(tmp_path):
    manifest = json.loads((DATA / "minimum-wall.manifest.json").read_text())
    # Verify the source bytes against git, then recompute with those same bytes.
    for path, evidence in manifest["strategy_sources"].items():
        pinned = subprocess.check_output([
            "git", "show", f'{manifest["source_commit"]}:{path}'], cwd=ROOT)
        assert hashlib.sha256(pinned).hexdigest() == evidence["sha256"]
        assert (ROOT / path).read_bytes() == pinned
    table.build(table.INPUT, tmp_path)
    for name in ("minimum-wall.csv", "minimum-wall.manifest.json"):
        assert (tmp_path / name).read_bytes() == (DATA / name).read_bytes()
    payload = (DATA / "minimum-wall.csv").read_bytes()
    assert hashlib.sha256(payload).hexdigest() == manifest["csv_sha256"]
    rows = list(csv.DictReader(io.StringIO(payload.decode())))
    assert rows == manifest["rows"]
    assert {row["code"] for row in rows} == {code.value for code in DesignCode}
    assert {float(row["design_pressure_mpa"]) for row in rows} == set(config()["pressures_mpa"])


def test_source_digest_mismatch_rejects_generation(tmp_path, monkeypatch):
    monkeypatch.setattr(table.subprocess, "check_output", lambda *a, **kw: b"changed source bytes")
    with pytest.raises(ValueError, match="Source digest mismatch"):
        table.build(table.INPUT, tmp_path / "output")
    assert not (tmp_path / "output").exists()


def test_all_deliverables_are_not_ignored():
    for path in (table.INPUT, Path(table.__file__), DATA / "minimum-wall.csv",
                 DATA / "minimum-wall.manifest.json", Path(__file__)):
        result = subprocess.run(["git", "check-ignore", "-q", str(path)], cwd=ROOT)
        assert result.returncode == 1


def test_no_editions_and_pressure_basis_remains_assumed():
    cfg = config()
    manifest = json.loads((DATA / "minimum-wall.manifest.json").read_text())
    assert cfg["pressure_basis"] == "ASSUMED gauge"
    assert "edition" not in manifest["rows"][0]
    assert all(row["pressure_basis"] == "ASSUMED gauge" for row in manifest["rows"])
    assert "no edition claim" in manifest["limitations"]
    assert "SCREENING" in manifest["combined_loading_notes"]["API-RP-2RD"]
    assert "passed review record" in manifest["combined_loading_notes"]["API-RP-1111"]
