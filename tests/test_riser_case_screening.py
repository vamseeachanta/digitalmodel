# ABOUTME: Tests for API STD 2RD combined M-T check + riser load-case screening.
# ABOUTME: Includes cross-validation against the legacy APISTD2RDAnalyzer Method 1.

import importlib.util
import math
from pathlib import Path

import pytest

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
    screen_riser_cases,
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
