# ABOUTME: Tests for API STD 2RD combined M-T check + riser load-case screening.
# ABOUTME: Includes cross-validation against the legacy APISTD2RDAnalyzer Method 1.

import importlib.util
import copy
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path

import pytest
import yaml

from digitalmodel import __version__
from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes.api_std_2rd import (
    ApiStd2rdStrategy,
)
from digitalmodel.subsea.riser.riser_case_screening import (
    CaseScreenResult,
    RiserLoadCase,
    build_calc_note,
    screen_riser_cases,
    run_riser_screening,
)


# ---------------------------------------------------------------------------
# Legacy analyzer loader (bypasses a stale package __init__ in the legacy tree)
# ---------------------------------------------------------------------------

def _load_legacy_stress_calc():
    """Import the legacy stress_calculations module directly from its file.

    The legacy calculation package's ``__init__`` references a pre-existing
    moved import path, so we load the module file in isolation to reach
    ``calculate_method1_limits`` / ``calculate_utilization`` for cross-checking.
    """
    src = (
        Path(__file__).resolve().parents[1]
        / "src"
        / "digitalmodel"
        / "subsea"
        / "pipeline"
        / "calculations"
        / "stress_calculations.py"
    )
    spec = importlib.util.spec_from_file_location("legacy_stress_calc", src)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def geometry():
    return PipeGeometry(outer_diameter=0.27305, wall_thickness=0.02144)


@pytest.fixture
def material():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


@pytest.fixture
def strategy():
    return ApiStd2rdStrategy()


# ---------------------------------------------------------------------------
# Combined check: presence and basic behaviour
# ---------------------------------------------------------------------------

def test_check_names_includes_combined(strategy):
    assert strategy.check_names == ["burst", "collapse", "combined"]


def test_run_checks_returns_combined(strategy, geometry, material):
    loads = DesignLoads(
        internal_pressure=20e6, external_pressure=10e6,
        bending_moment=250e3, effective_tension=1500e3,
    )
    results = strategy.run_checks(geometry, material, loads, DesignFactors())
    assert set(results) == {"burst", "collapse", "combined"}
    util, details = results["combined"]
    assert util > 0
    assert details["f_d_corrected"] > 0
    assert details["yield_tension"] > 0
    assert details["yield_moment"] > 0


def test_combined_zero_loads(strategy, geometry, material):
    """No moment/tension and balanced pressure -> zero utilisation."""
    loads = DesignLoads(internal_pressure=0.0, external_pressure=0.0)
    util, _ = strategy._check_combined(geometry, material, loads)
    assert util == 0.0


def test_combined_monotonic_in_moment(strategy, geometry, material):
    """Utilisation rises with bending moment, all else equal."""
    base = DesignLoads(internal_pressure=20e6, external_pressure=10e6,
                       bending_moment=100e3, effective_tension=1000e3)
    more = DesignLoads(internal_pressure=20e6, external_pressure=10e6,
                       bending_moment=300e3, effective_tension=1000e3)
    u_base, _ = strategy._check_combined(geometry, material, base)
    u_more, _ = strategy._check_combined(geometry, material, more)
    assert u_more > u_base


def test_combined_pressure_reduces_capacity(strategy, geometry, material):
    """Higher net pressure shrinks F_d_corr, raising M-T utilisation."""
    lo = DesignLoads(internal_pressure=10e6, external_pressure=5e6,
                     bending_moment=200e3, effective_tension=1200e3)
    hi = DesignLoads(internal_pressure=40e6, external_pressure=5e6,
                     bending_moment=200e3, effective_tension=1200e3)
    u_lo, d_lo = strategy._check_combined(geometry, material, lo)
    u_hi, d_hi = strategy._check_combined(geometry, material, hi)
    assert d_hi["f_d_corrected"] < d_lo["f_d_corrected"]
    assert u_hi > u_lo


# ---------------------------------------------------------------------------
# Cross-validation against the legacy APISTD2RDAnalyzer Method 1
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    "od,t,smys,smts,pi,pe,moment,tension",
    [
        (0.27305, 0.02144, 448e6, 531e6, 20e6, 10e6, 250e3, 1500e3),
        (0.5334, 0.0254, 448e6, 531e6, 30e6, 15e6, 500e3, 3000e3),
        (0.3239, 0.0190, 358e6, 455e6, 15e6, 5e6, 100e3, 800e3),
        (0.27305, 0.02144, 552e6, 621e6, 25e6, 8e6, 400e3, 2000e3),
    ],
)
def test_combined_matches_legacy_method1(od, t, smys, smts, pi, pe, moment, tension):
    """New SI combined check == legacy Method 1 utilisation (machine precision).

    The legacy ``calculate_method1_limits`` carries inconsistent internal unit
    conversions (yield_tension via a kip->kN factor; yield_moment via a
    lb*ft->kN*m factor), so its absolute T_y/M_y are not physical SI values.
    The *dimensionless* utilisation is nonetheless correct: feeding the legacy
    ``calculate_utilization`` the loads expressed in those same internal unit
    scales reproduces our clean SI result exactly.
    """
    legacy = _load_legacy_stress_calc()

    geom = PipeGeometry(outer_diameter=od, wall_thickness=t)
    mat = PipeMaterial(grade="X", smys=smys, smts=smts)
    loads = DesignLoads(
        internal_pressure=pi, external_pressure=pe,
        bending_moment=moment, effective_tension=tension,
    )
    util_new, _ = ApiStd2rdStrategy()._check_combined(geom, mat, loads)

    inner_d = od - 2 * t
    calc = legacy.APISTD2RDCalculations()
    burst = calc.calculate_burst_pressure(od, inner_d, smys, smts, 0.0)
    m1 = calc.calculate_method1_limits(
        od, inner_d, t, smys, burst,
        {"internalPressure": {"design": ApiStd2rdStrategy.COMBINED_DESIGN_FACTOR}},
        "design", pi, pe,
    )

    # Express the applied loads in the legacy's internal unit scales so the
    # T/T_y and M/M_y ratios are computed consistently.
    from digitalmodel.units import Q_
    tension_legacy = Q_(tension / 1000.0, "kip").to("kN").magnitude
    moment_legacy = moment / 1000.0 / 12 * 1.355818
    util_legacy = legacy.calculate_utilization(
        tension_legacy, moment_legacy,
        m1["yield_tension"], m1["yield_moment"],
        m1["pressure_corrected_factor"], method="method1",
    )

    assert util_new == pytest.approx(util_legacy, rel=1e-9, abs=1e-12)


def test_pressure_corrected_factor_matches_legacy():
    """F_d_corr (the pressure-corrected design factor) matches legacy exactly."""
    legacy = _load_legacy_stress_calc()
    od, t, smys, smts, pi, pe = 0.27305, 0.02144, 448e6, 531e6, 20e6, 10e6
    inner_d = od - 2 * t

    geom = PipeGeometry(outer_diameter=od, wall_thickness=t)
    mat = PipeMaterial(grade="X65", smys=smys, smts=smts)
    loads = DesignLoads(internal_pressure=pi, external_pressure=pe)
    _, details = ApiStd2rdStrategy()._check_combined(geom, mat, loads)

    calc = legacy.APISTD2RDCalculations()
    burst = calc.calculate_burst_pressure(od, inner_d, smys, smts, 0.0)
    m1 = calc.calculate_method1_limits(
        od, inner_d, t, smys, burst,
        {"internalPressure": {"design": ApiStd2rdStrategy.COMBINED_DESIGN_FACTOR}},
        "design", pi, pe,
    )
    assert details["f_d_corrected"] == pytest.approx(
        m1["pressure_corrected_factor"], rel=1e-12
    )


# ---------------------------------------------------------------------------
# Case screening + determinism
# ---------------------------------------------------------------------------

def _representative_cases():
    return [
        RiserLoadCase("operating", "operating", 34.5e6, 12e6, 350e3, 2500e3),
        RiserLoadCase("extreme_100yr", "extreme", 34.5e6, 12e6, 900e3, 3800e3),
        RiserLoadCase("survival_1000yr", "survival", 38e6, 12e6, 1250e3, 4600e3),
        RiserLoadCase("accidental_drift_off", "accidental", 31e6, 12.3e6, 1450e3, 2100e3),
    ]


def test_screen_ranks_by_max_utilisation(geometry, material):
    cases = _representative_cases()
    results = screen_riser_cases(geometry, material, cases)
    assert all(isinstance(r, CaseScreenResult) for r in results)
    # Sorted descending by max utilisation
    utils = [r.max_utilisation for r in results]
    assert utils == sorted(utils, reverse=True)
    # Every case is screened and has a governing check among the three names
    assert {r.name for r in results} == {c.name for c in cases}
    for r in results:
        assert r.governing_check in {"burst", "collapse", "combined"}
        assert r.status in {"PASS", "FAIL"}


def test_screen_deterministic(geometry, material):
    cases = _representative_cases()
    r1 = screen_riser_cases(geometry, material, cases)
    r2 = screen_riser_cases(geometry, material, cases)
    assert [(x.name, x.max_utilisation, x.governing_check) for x in r1] == [
        (x.name, x.max_utilisation, x.governing_check) for x in r2
    ]


def test_screen_uses_api_std_2rd_by_default(geometry, material):
    results = screen_riser_cases(geometry, material, _representative_cases())
    # All three API STD 2RD checks present per case
    for r in results:
        assert set(r.checks) == {"burst", "collapse", "combined"}


def test_screen_explicit_api_std_2rd(geometry, material):
    """Passing API STD 2RD explicitly behaves like the default."""
    cases = _representative_cases()
    default = screen_riser_cases(geometry, material, cases)
    explicit = screen_riser_cases(
        geometry, material, cases, code=DesignCode.API_STD_2RD
    )
    assert [r.max_utilisation for r in default] == [
        r.max_utilisation for r in explicit
    ]


@pytest.mark.parametrize("code,names,basis", [
    (DesignCode.DNV_ST_F201, ["pressure_containment", "combined"], "Eqs 5.25-5.26"),
    (DesignCode.API_STD_2RD, ["burst", "collapse", "combined"], "API STD 2RD Method 1"),
])
def test_screen_and_calc_note_follow_selected_code(geometry, material, code, names, basis):
    results = screen_riser_cases(geometry, material, _representative_cases(), code=code)
    assert len(results) == 4
    assert [r.max_utilisation for r in results] == sorted(
        (r.max_utilisation for r in results), reverse=True)
    assert all(list(r.checks) == names for r in results)
    assert results == screen_riser_cases(geometry, material, _representative_cases(), code=code)
    note = build_calc_note(geometry, material, code, results, "screening")
    assert basis in note
    assert "not implemented" not in note
    assert "nan" not in note.lower()
    for name in names:
        assert name.replace("_", " ").title() in note
    if code == DesignCode.DNV_ST_F201:
        assert "| Burst |" not in note and "| Collapse |" not in note
        for text in ("DNV-OS-F201 (2010)", "Sec. 5", "clause D", "alpha_c",
                     "design load effects", "p_min_internal", "DNV-ST-F101 Eq. 5.9",
                     "1.14", "1.15", "1.138", "0.96"):
            assert text in note
    else:
        assert "F_d_corr" in note and "APISTD2RDAnalyzer" in note


@pytest.mark.parametrize("workflow", ["riser-screening", "riser-screening-f201"])
@pytest.mark.parametrize("fallback", [False, True])
def test_workflow_output_anchor_report_contract_and_determinism(tmp_path, monkeypatch, workflow, fallback):
    root = Path(__file__).resolve().parents[1]
    source = root / "examples/workflows" / workflow / "input.yml"
    input_dir = tmp_path / "input"
    input_dir.mkdir()
    input_file = input_dir / "input.yml"
    input_file.write_bytes(source.read_bytes())
    cfg = yaml.safe_load(input_file.read_text())
    cfg.update(_config_dir_path=str(input_dir), _config_file_path=str(input_file))
    if fallback:
        cfg["riser_screening"].pop("output_dir")
        cfg["Analysis"] = {"result_folder": "results"}
    elsewhere = tmp_path / "elsewhere"
    elsewhere.mkdir()
    monkeypatch.chdir(elsewhere)
    run = run_riser_screening(copy.deepcopy(cfg))
    result = run["riser_screening"]["result"]
    assert result["screening_status"] == result["overall_status"].lower() == run["screening_status"]
    assert result["screening_status"] in {"pass", "fail"}
    echo = result["input_echo"]
    assert echo["geometry"]["outer_diameter"] == cfg["riser_screening"]["geometry"]["outer_diameter"]
    assert echo["material"]["smys"] == cfg["riser_screening"]["material"]["smys"]
    assert echo["load_cases"] == cfg["riser_screening"]["load_cases"]
    assert echo["code"] == cfg["riser_screening"]["code"]
    assert echo["factors"]["safety_class"] == "medium"
    provenance = result["provenance"]
    assert provenance["package_version"] == __version__
    source_record = provenance["sources"][0]
    assert source_record["digest"] == "sha256:" + hashlib.sha256(input_file.read_bytes()).hexdigest()
    assert source_record["retrieved_at"] == datetime.fromtimestamp(input_file.stat().st_mtime, timezone.utc).isoformat()
    assert provenance["timestamp_basis"] == "input file modification time (UTC)"
    assert source_record["identifier"] == "input.yml"
    canonical = json.dumps(echo, sort_keys=True, separators=(",", ":"),
                           ensure_ascii=False, allow_nan=False).encode()
    assert provenance["sources"][-1]["digest"] == "sha256:" + hashlib.sha256(canonical).hexdigest()
    stem = input_dir / "results" / cfg["riser_screening"]["label"]
    json_file = stem.with_name(stem.name + "_results.json")
    before = json_file.read_bytes()
    assert json.loads(before) == result
    second = run_riser_screening(copy.deepcopy(cfg))
    assert second["riser_screening"]["result"] == result
    assert json_file.read_bytes() == before
    assert not (elsewhere / "results").exists()
    note = stem.with_name(stem.name + "_calcnote.md").read_text()
    for text in ("Input echo", "Provenance", __version__, source_record["digest"],
                 source_record["retrieved_at"], result["screening_status"]):
        assert text in note
    if workflow.endswith("f201"):
        assert echo["factors"]["strategy"]["combined_gamma_SC"]["normal"] == 1.14
        assert all("Mk" in row["details"]["combined"] for row in result["cases"])


def test_screening_examples_registered():
    root = Path(__file__).resolve().parents[1]
    registry = yaml.safe_load((root / "docs/registry/workflows.yaml").read_text())
    rows = {row["id"]: row for row in registry["workflows"]}
    for name in ("riser-screening", "riser-screening-f201"):
        row = rows[name]
        assert row["basename"] == "riser_screening"
        assert row["runtime"] == "offline"
        assert row["input"] == f"examples/workflows/{name}/input.yml"
        assert row["test"].endswith(f"test_workflow_registry[{name}]")
        assert len(row["outputs"]) == 2
        assert all(path.startswith(f"examples/workflows/{name}/results/")
                   for path in row["outputs"])


def test_workflow_honours_high_safety_class(tmp_path):
    source = Path(__file__).resolve().parents[1] / "examples/workflows/riser-screening-f201/input.yml"
    cfg = yaml.safe_load(source.read_text())
    cfg["riser_screening"].update(safety_class="high", output_dir=str(tmp_path))
    result = run_riser_screening(cfg)["riser_screening"]["result"]
    assert result["input_echo"]["factors"]["safety_class"] == "high"
    for row in result["cases"]:
        assert row["details"]["combined"]["gamma_sc"] == 1.26
        assert row["details"]["pressure_containment"]["gamma_sc"] == 1.308
