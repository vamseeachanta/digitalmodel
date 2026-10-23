"""Independent arithmetic tests for dimensional, single-frequency roll coupling.

All numbers below are declared arithmetic inputs, not measured vessel data.
For m=10 kg, g=10 m/s², GM=1 m, T=2*pi s, zeta=0.1:
C=100 N m/rad; omega_n=1 rad/s; I=100 kg m²; B=20 N m s/rad.
At omega=1, bare impedance is 20j. A tank moment/roll of -20j
makes coupled impedance 40j, so response ratio=20/40 and reduction=50%.
Equations: public online help, 'Modal analysis: Theory', outline theory;
'Results: Roll damping', percentage of critical (accessed 2026-10-19).
See module SOURCES for exact links and restoring-equation source.
"""

from dataclasses import replace
import math

import pytest

from digitalmodel.hydrodynamics.vessel_roll_reduction import (
    TankImpedance,
    VesselRollBasis,
    reduction_sweep,
    render_roll_reduction_html,
)


def basis():
    return VesselRollBasis(10, 1, 2 * math.pi, 0.1, 10, "Declared arithmetic inputs")


def point(area=1, moment=-20j):
    return TankImpedance(area, 2 * math.pi, moment, 0.01, "Hand calculation above")


def test_dimensional_vessel_basis():
    vessel = basis()
    assert vessel.restoring_stiffness == pytest.approx(100)
    assert vessel.effective_inertia == pytest.approx(100)
    assert vessel.linear_damping == pytest.approx(20)


def test_resonant_damping_reduction_and_neutral_control():
    result = reduction_sweep(basis(), [point(1, 0j), point(2)])
    assert result.status == "EVALUATED"
    assert [r.reduction_percent for r in result.rows] == pytest.approx([0, 50])
    assert result.rows[1].bare_response == pytest.approx(-0.05j)
    assert result.rows[1].coupled_response == pytest.approx(-0.025j)


def test_stiffness_and_off_resonant_moment_use_complex_equation():
    """At omega=0.5, Z0=75+10j; tank=25-10j gives Z=50+20j.

    Ratio=sqrt(5725/2900), so reduction=100*(1-sqrt(5725/2900)),
    negative: roll amplification must not be clipped to zero.
    """
    tank = replace(point(), forcing_period_s=4 * math.pi, moment_per_roll=25 - 10j)
    row = reduction_sweep(basis(), [tank]).rows[0]
    assert row.reduction_percent == pytest.approx(100 * (1 - math.sqrt(5725 / 2900)))
    assert row.reduction_percent < 0


@pytest.mark.parametrize(
    "vessel,points,missing",
    [
        (None, [], "vessel basis"),
        (basis(), [], "tank impedance"),
        (None, [point()], "vessel basis"),
    ],
)
def test_missing_data_never_becomes_a_zero_result(vessel, points, missing):
    result = reduction_sweep(vessel, points)
    assert result.status == "NOT_EVALUATED"
    assert missing in " ".join(result.missing)
    assert result.rows == ()


@pytest.mark.parametrize(
    "field",
    [
        "displacement_mass_kg",
        "gm_m",
        "natural_period_s",
        "gravity_m_s2",
    ],
)
@pytest.mark.parametrize("bad", [0, -1, float("nan"), float("inf")])
def test_invalid_vessel_inputs(field, bad):
    with pytest.raises(ValueError, match=field):
        replace(basis(), **{field: bad})


@pytest.mark.parametrize("bad", [-1, float("nan"), float("inf")])
def test_invalid_damping(bad):
    with pytest.raises(ValueError, match="hull_damping_ratio"):
        replace(basis(), hull_damping_ratio=bad)


@pytest.mark.parametrize(
    "field", ["conduit_area_m2", "forcing_period_s", "roll_amplitude_rad"]
)
@pytest.mark.parametrize("bad", [0, -1, float("nan"), float("inf")])
def test_invalid_tank_inputs(field, bad):
    with pytest.raises(ValueError, match=field):
        replace(point(), **{field: bad})


@pytest.mark.parametrize("obj", [basis(), point()])
def test_provenance_is_required(obj):
    with pytest.raises(ValueError, match="source"):
        replace(obj, source=" ")


def test_nonfinite_complex_moment_is_rejected():
    with pytest.raises(ValueError, match="moment_per_roll"):
        point(moment=complex(0, math.nan))


def test_duplicate_areas_and_mixed_operating_points_are_rejected():
    for points in (
        [point(), point()],
        [point(), replace(point(2), forcing_period_s=7)],
        [point(), replace(point(2), roll_amplitude_rad=0.02)],
    ):
        with pytest.raises(ValueError):
            reduction_sweep(basis(), points)


def test_undamped_resonance_is_not_a_finite_answer():
    with pytest.raises(ValueError, match="singular"):
        reduction_sweep(replace(basis(), hull_damping_ratio=0), [point()])


def test_cancelled_coupled_impedance_is_rejected():
    with pytest.raises(ValueError, match="singular"):
        reduction_sweep(basis(), [point(moment=20j)])


def test_report_has_basis_sources_revision_and_curve():
    result = reduction_sweep(basis(), [point(), point(2, -60j)])
    html = render_roll_reduction_html(result, revision="A", date="2026-10-19")
    assert "50.0" in html and "75.0" in html
    assert "Revision history" in html
    assert html.index("Summary") < html.index("Revision history") < html.index("Method")
    assert "N m/rad" in html and "m²" in html
    assert "Declared arithmetic inputs" in html
    assert "Hand calculation above" in html
    assert "<svg" in html
    assert "single-frequency" in html


def test_missing_report_and_html_escaping():
    result = reduction_sweep(None, [])
    html = render_roll_reduction_html(result, revision="<script>", date="2026-10-19")
    assert "NOT_EVALUATED" in html and "vessel basis" in html
    assert "&lt;script&gt;" in html and "<script>" not in html
    result = reduction_sweep(replace(basis(), source="<script>"), [point()])
    assert "<script>" not in render_roll_reduction_html(result, revision="A", date="x")


def test_nonfinite_derived_response_is_rejected():
    with pytest.raises(ValueError, match="finite"):
        reduction_sweep(replace(basis(), displacement_mass_kg=1e308), [point()])


def test_report_reissue_preserves_prior_revision():
    html = render_roll_reduction_html(
        reduction_sweep(None, []),
        revision="B",
        date="2026-10-20",
        previous_revisions=(("A", "2026-10-19", "Initial issue for data readiness"),),
    )
    assert "Initial issue for data readiness" in html
    assert html.index("2026-10-19") < html.index("2026-10-20")
