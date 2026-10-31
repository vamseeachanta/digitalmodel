"""Slice 6: independent arithmetic in the six-case synthetic pipe example.

Input basis: on-bottom-stability-analysis.md, Synthetic input basis (2026).
All geometry, densities, coefficients and sea states are assumed, based on
previous experience; no real asset is represented. Geometry and static force
balance, not inaccessible standard coefficients, provide expected values.
"""

from dataclasses import replace
import math
from pathlib import Path

import pytest

from digitalmodel.subsea.on_bottom_stability.e305_simplified import (
    Case,
    evaluate_case,
    wave_from_sea_state,
)
from digitalmodel.subsea.on_bottom_stability.example import (
    build_example,
    example_report,
)


ROOT = Path(__file__).resolve().parents[3]


def test_example_weight_against_independent_annular_ledger():
    """Steel OD=.3048, bore=.2794, corrosion OD=.3108, concrete OD=.4108,
    grown OD=.4308 m. Each annulus area is pi*(OD²-ID²)/4.
    Densities steel=7850, corrosion=950, concrete=3000, growth=1300,
    contents=800, water=1025 kg/m³; g=9.80665 m/s². No joint or added mass.
    The independently written ledger below is the expected mass and force;
    it does not call the weight function or read its intermediate results.
    """
    mass = (
        math.pi
        / 4
        * (
            (0.3048**2 - 0.2794**2) * 7850
            + (0.3108**2 - 0.3048**2) * 950
            + (0.4108**2 - 0.3108**2) * 3000
            + (0.4308**2 - 0.4108**2) * 1300
            + 0.2794**2 * 800
        )
    )
    expected = 9.80665 * (mass - math.pi / 4 * 0.4308**2 * 1025)
    example = build_example()
    assert example.weight.submerged_weight_n_m == pytest.approx(expected)
    assert example.weight.hydrodynamic_diameter_m == pytest.approx(0.4308)
    assert len(example.rows) == 6
    assert len({row.depth_m for row in example.rows}) == 3
    assert len({(row.hs_m, row.tp_s) for row in example.rows}) == 2


def test_example_load_utilisation_and_weight_root_by_independent_equations():
    """Each row supplies spectral-derived Uw and Tu to a sampled deterministic
    force screen. At reported theta: U=.5+Uw*cos(theta), a=-Uw*2*pi/Tu*sin(theta).
    FD=1025*.4308*U*abs(U)/2 (CD=1); FI=1025*2*pi*.4308²*a/4 (CM=2);
    FL=1025*.5*.4308*U²/2 (CL=.5). With assumed mu=.6, gamma=1.5,
    required absolute W=1.5*(abs(FD+FI)/.6+FL). These are independent
    equations at the reported phase, not expected values from another API.
    The sampled maximum is also bounded against a separate half-step grid.
    """
    example = build_example()
    for row in example.rows:
        velocity = 0.5 + row.wave_velocity_m_s * math.cos(row.phase_rad)
        acceleration = (
            -row.wave_velocity_m_s
            * 2
            * math.pi
            / row.wave_period_s
            * math.sin(row.phase_rad)
        )
        drag = 1025 * 0.4308 * velocity * abs(velocity) / 2
        inertia = 1025 * 2 * math.pi * 0.4308**2 * acceleration / 4
        lift = 1025 * 0.5 * 0.4308 * velocity**2 / 2
        required = 1.5 * (abs(drag + inertia) / 0.6 + lift)
        assert row.static_case.horizontal_n_m == pytest.approx(drag + inertia)
        assert row.static_case.lift_n_m == pytest.approx(lift)
        assert row.required_weight_n_m == pytest.approx(required, abs=1e-5)
        assert row.absolute_utilisation == pytest.approx(
            required / example.weight.submerged_weight_n_m
        )
        # Numerical sampling control, not a standard acceptance margin.
        other_max = 0
        for i in range(1440):
            theta = 2 * math.pi * (i + 0.5) / 1440
            u = 0.5 + row.wave_velocity_m_s * math.cos(theta)
            a = (
                -row.wave_velocity_m_s
                * 2
                * math.pi
                / row.wave_period_s
                * math.sin(theta)
            )
            h = 1025 * 0.4308 * u * abs(u) / 2 + 1025 * 2 * math.pi * 0.4308**2 * a / 4
            phase_lift = 1025 * 0.5 * 0.4308 * u**2 / 2
            other_max = max(other_max, 1.5 * (abs(h) / 0.6 + phase_lift))
        assert required == pytest.approx(other_max, rel=1e-4)


def test_wave_height_and_current_sensitivity():
    """With fixed period/depth and spectral shape the linear spectrum scales
    with Hs², so Uw scales with Hs (helper contract, knowledge pack §3;
    no F105 calibration). The assumed Hs 1->2, Tp=12 pair retains the same
    peak enhancement in the helper. Doubling Hs doubles Uw without changing Tu.
    In a drag-only wave/current case the maximum velocity is Uc+Uw, so
    demand increases quadratically as either velocity increases.
    """
    first = wave_from_sea_state(
        Hs_m=1, Tp_s=12, depth_m=20, elevation_m=0.2154, g=9.80665
    )
    second = wave_from_sea_state(
        Hs_m=2, Tp_s=12, depth_m=20, elevation_m=0.2154, g=9.80665
    )
    assert second[0] == pytest.approx(2 * first[0])
    assert second[1] == pytest.approx(first[1])
    c = Case("extreme_wave", 1, 1, 8, 1, 1, 1, 0, 0.5, 0.5, 1, 1, "wave_velocity")
    kwargs = dict(diameter_m=1, rho_water_kg_m3=1, available_weight_n_m=100)
    baseline = evaluate_case(c, **kwargs).required_weight_n_m
    # Hand value: .5*(1+1)²/.5 + .5*.5*(1+1)² = 5 N/m.
    assert baseline == pytest.approx(5)
    assert (
        evaluate_case(replace(c, current_m_s=2), **kwargs).required_weight_n_m
        > baseline
    )
    assert (
        evaluate_case(replace(c, wave_velocity_m_s=2), **kwargs).required_weight_n_m
        > baseline
    )


def test_committed_report_is_reproducible_and_linked():
    report = example_report()
    target = ROOT / "docs/domains/subsea/pipeline/on-bottom-stability-analysis.md"
    assert target.read_text() == report
    assert report == example_report()
    assert (
        "on-bottom-stability-analysis.md" in (target.parent / "README.md").read_text()
    )
    assert "12 inch" in report and "NOT_EVALUATED" in report
    assert "assumed, based on previous experience" in report
    assert "## Revision history" in report
    assert "A1" in report and "A5" in report
