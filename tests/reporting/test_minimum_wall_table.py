# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Check grid sizing independently, then reproduce the pinned table cells."""

import ast
import csv
import hashlib
import inspect
import io
import json
from pathlib import Path
import subprocess

import pytest

from scripts.reporting import build_minimum_wall_table as table
from digitalmodel.structural.analysis.wall_thickness import DesignCode

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "docs/reports/minimum-wall-table"
REBUILD = "Rebuild with PYTHONPATH=src python scripts/reporting/build_minimum_wall_table.py"


def config():
    return json.loads(table.INPUT.read_text(encoding="utf-8"))


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


@pytest.mark.parametrize("pressure,wall", [(18, 15.5), (27, 21.8), (36, 28.0)])
def test_independent_membrane_equilibrium_anchor(pressure, wall):
    """First principles: a diametral cut gives 2*t*L*sigma = p*D*L.

    With D=400 mm, yield=400 MPa and the implementation's fixed 0.72
    factor, allowable membrane stress is 288 MPa. At p=18 MPa,
    t=18*400/(2*288)=12.5 mm; add 3 mm corrosion once: 15.5 mm.
    At 27 MPa, 27*400/(2*288)+3=21.75 mm, rounded up to 21.8 mm.
    At 36 MPa, 36*400/(2*288)+3=28.0 mm on the grid.
    This is an independent hand calculation, not an output of the strategy.
    The factor is an unqualified implementation input, not a standard claim.
    """
    cfg = config()
    cfg["external_pressure_mpa"] = 0
    result = table.pressure_minimum(cfg, DesignCode.ASME_B31_4, pressure)
    assert result["wall_mm"] == pytest.approx(wall, abs=1e-10)
    assert result["governing_check"] == "burst"
    utilisation = table.pressure_checks(cfg, DesignCode.ASME_B31_4, wall, pressure)["burst"]
    if pressure in (18, 36):
        assert utilisation == 1
    else:
        # The 27 MPa exact wall is 18.75 mm remaining; grid wall is 18.8 mm.
        assert utilisation == pytest.approx(375 / 376, rel=1e-12)


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
    source.write_text(json.dumps(cfg), encoding="utf-8", newline="\n")
    with pytest.raises(ValueError):
        table.build(source, tmp_path / "output")
    assert not (tmp_path / "output").exists()


def test_pinned_sources_and_every_table_value_reproduce(tmp_path):
    manifest = json.loads((DATA / "minimum-wall.manifest.json").read_text(encoding="utf-8"))
    # Verify the source bytes against git, then recompute with those same bytes.
    for path, evidence in manifest["strategy_sources"].items():
        pinned = subprocess.check_output([
            "git", "show", f'{manifest["source_commit"]}:{path}'], cwd=ROOT)
        assert hashlib.sha256(pinned).hexdigest() == evidence["sha256"], REBUILD
        assert (ROOT / path).read_bytes() == pinned, REBUILD
    table.build(table.INPUT, tmp_path)
    for name in ("minimum-wall.csv", "minimum-wall.manifest.json"):
        assert (tmp_path / name).read_bytes() == (DATA / name).read_bytes(), REBUILD
    payload = (DATA / "minimum-wall.csv").read_bytes()
    assert hashlib.sha256(payload).hexdigest() == manifest["csv_sha256"], REBUILD
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
    manifest = json.loads((DATA / "minimum-wall.manifest.json").read_text(encoding="utf-8"))
    assert cfg["pressure_basis"] == "ASSUMED gauge"
    assert "edition" not in manifest["rows"][0]
    assert all(row["pressure_basis"] == "ASSUMED gauge" for row in manifest["rows"])
    assert "no edition claim" in manifest["limitations"]
    assert "SCREENING" in manifest["combined_loading_notes"]["API-RP-2RD"]
    assert "passed review record" in manifest["combined_loading_notes"]["API-RP-1111"]


@pytest.fixture(scope="module")
def rebuilt_manifest(tmp_path_factory):
    return table.build(table.INPUT, tmp_path_factory.mktemp("provenance"))


@pytest.mark.parametrize("code", list(DesignCode))
def test_stock_trials_resolve_all_check_citations_and_gaps(rebuilt_manifest, code):
    """Losing a route or check must not erase its qualification gaps."""
    record = rebuilt_manifest["check_provenance"][code.value]
    assert set(record["checks"]) == set(table.PRESSURE_METHODS[code]) | {"collapse", "propagation"}
    for row in rebuilt_manifest["rows"]:
        if row["code"] == code.value:
            assert record["strategy_source"] == row["strategy_source"]
    for check in record["checks"].values():
        assert check["qualification"] == "unqualified"
        assert check["qualification_gaps"]
        assert check["source_reference"]["standard"] == code.value
        assert check["criterion_applied"] == (check["evaluation_status"] == "evaluated")
        if check["evaluation_status"] == "not implemented":
            assert check["source_reference"]["edition"] is None
            assert check["source_reference"]["clause"] is None
            assert check["criterion"] is None and check["limit"] is None
        else:
            assert check["source_reference"]["edition"].startswith("Unqualified repository metadata: ")
            assert check["source_reference"]["clause"].startswith("Unqualified repository metadata: ")
            assert check["criterion"] and check["limit"] == 1.0
            assert check["coefficient_symbols"]


@pytest.mark.parametrize("name,code", [
    ("API RP 1111", "API-RP-1111"), ("API RP 2RD", "API-RP-2RD"),
    ("API STD 2RD", "API-STD-2RD"),
])
def test_comparison_qualification_evidence_is_preserved(rebuilt_manifest, name, code):
    criteria = json.loads((ROOT / "scripts/reporting/riser_pipe_comparison.criteria.json").read_text(
        encoding="utf-8"))
    assert rebuilt_manifest["inherited_criteria_context"] == {
        key: value for key, value in criteria.items() if key != "codes"}
    record = rebuilt_manifest["check_provenance"][code]
    for field, value in criteria["codes"][name].items():
        if field not in {"code_id", "source_sibling", "source_project", "module"}:
            assert record["inherited_criteria"][field] == value
    for check in record["checks"].values():
        assert criteria["qualification"] in check["qualification_gaps"]
        if "metadata_source" in criteria["codes"][name] and check["evaluation_status"] == "evaluated":
            assert check["source_reference"]["metadata_source"] == criteria["codes"][name]["metadata_source"]
    for symbol in criteria["codes"][name]["constants"]:
        if "combined" in symbol.lower():
            continue  # Combined loading is outside the pressure-and-stock table.
        if symbol.startswith("_check_"):
            expected = {symbol.split(".")[0].removeprefix("_check_")}
        elif symbol.startswith("EDITION_FACTORS."):
            expected = {{"d": "burst", "c": "collapse", "p": "propagation"}[symbol[-1]]}
        else:
            assert symbol == "C_A (burst and collapse)"
            expected = {"burst", "collapse"}
        assert {key for key, check in record["checks"].items()
                if symbol in check["coefficient_symbols"]} == expected


def test_missing_and_suppressed_checks_retain_unqualified_status(rebuilt_manifest):
    records = rebuilt_manifest["check_provenance"]
    assert "_check_hoop.sigma_allow coefficient" in records["API-RP-2RD"]["checks"]["hoop"]["coefficient_symbols"]
    for code in ("ISO-13623", "PD-8010-2"):
        collapse = records[code]["checks"]["collapse"]
        assert collapse["evaluation_status"] == "suppressed (#1920)"
        assert any("#1920" in gap for gap in collapse["qualification_gaps"])
    for code in ("API-RP-2RD", "API-STD-2RD", "DNV-ST-F201"):
        propagation = records[code]["checks"]["propagation"]
        assert propagation["evaluation_status"] == "not implemented"
        assert "Check is not implemented" in propagation["qualification_gaps"]


def test_citation_inventory_is_bound_to_pinned_criteria(rebuilt_manifest):
    path = "scripts/reporting/riser_pipe_comparison.criteria.json"
    assert path in rebuilt_manifest["strategy_sources"]
    assert hashlib.sha256((ROOT / path).read_bytes()).hexdigest() == (
        rebuilt_manifest["strategy_sources"][path]["sha256"]), REBUILD


@pytest.mark.parametrize("code", [DesignCode.API_RP_1111, DesignCode.API_RP_2RD, DesignCode.API_STD_2RD])
def test_stale_criteria_module_digest_rejects_output(tmp_path, monkeypatch, code):
    digests = table.source_digests(config()["source_commit"])
    source = Path(inspect.getfile(table.CODE_REGISTRY[code])).relative_to(ROOT).as_posix()
    digests[source] = {"sha256": "0" * 64}
    monkeypatch.setattr(table, "source_digests", lambda commit: digests)
    with pytest.raises(ValueError, match="Criterion module digest mismatch"):
        table.build(table.INPUT, tmp_path / "output")
    assert not (tmp_path / "output").exists()


@pytest.mark.parametrize("code", list(DesignCode))
def test_inventory_covers_coded_coefficients(rebuilt_manifest, code):
    """Detect missing coefficient-bearing assignments using actual strategy ASTs."""
    strategy = table.CODE_REGISTRY[code]
    source = ast.parse(Path(inspect.getfile(strategy)).read_text(encoding="utf-8"))
    source_symbols = {node.id for node in ast.walk(source) if isinstance(node, ast.Name)}
    source_symbols.update(node.attr for node in ast.walk(source) if isinstance(node, ast.Attribute))
    for check in rebuilt_manifest["check_provenance"][code.value]["checks"].values():
        if check["implementation_method"] is None:
            continue
        method = next(node for node in ast.walk(source)
                      if isinstance(node, ast.FunctionDef) and node.name == check["implementation_method"])
        numeric_assignments = {
            target.id for node in ast.walk(method) if isinstance(node, ast.Assign)
            if any(isinstance(value, ast.Constant) and type(value.value) in (int, float)
                   and value.value != 0 for value in ast.walk(node.value))
            for target in node.targets if isinstance(target, ast.Name)
        }
        reported = {symbol.rsplit(".", 1)[-1].split()[0] for symbol in check["coefficient_symbols"]}
        assert numeric_assignments <= reported
        assert reported <= source_symbols


def test_factor_sources_resolve_applied_objects_and_edition_keys(rebuilt_manifest):
    records = rebuilt_manifest["check_provenance"]
    common = "src/digitalmodel/structural/analysis/wall_thickness.py"
    for code in (DesignCode.DNV_ST_F101, DesignCode.API_RP_1111):
        record = records[code.value]
        for check in record["checks"].values():
            for symbol in check["coefficient_symbols"]:
                if symbol.startswith("EDITION_FACTORS."):
                    source = check["coefficient_sources"][symbol]
                    assert source["edition_key"] == table.CODE_REGISTRY[code]().edition_year
                    assert source["path"] == record["strategy_source"]
    f101 = records["DNV-ST-F101"]["checks"]
    for check in f101.values():
        for symbol in ("DesignFactors.gamma_sc", "DesignFactors.gamma_m", "DesignFactors.gamma_inc"):
            assert check["coefficient_sources"][symbol]["path"] == common
    for check in (f101["collapse"], f101["propagation"], records["ISO-13623"]["checks"]["collapse"]):
        assert check["coefficient_sources"]["PipeMaterial.alpha_fab"]["path"] == common
