# ABOUTME: Independent geometry and qualification-boundary tests for envelope requests.
# ABOUTME: Synthetic inputs never establish API 579 acceptance or allowable walls.
"""Study basis: issue 2287 (2026), Parametric basis and Output contract.

All dimensions and pressures below are synthetic test inputs, not code limits.
"""

import hashlib
import json
import math

import pytest

from digitalmodel.asset_integrity import uniform_loss_envelope as envelope


def inputs(**overrides):
    """Return explicit synthetic study assumptions in inches and psi."""
    values = dict(
        od_in=20.0,
        nominal_wall_in=0.5,
        sound_wall_in=0.5,
        axial_length_in=8.0,
        width_fraction=0.25,
        pressure_psi=100.0,
        lower_wall_in=0.1,
        upper_wall_in=0.5,
        wall_tolerance_in=0.0001,
    )
    return values | overrides


def test_geometry_has_independently_calculated_extent_and_normalization():
    """Hand calculation from issue 2287 (2026) geometry definitions.

    Dm=20-0.5=19.5 in; Di=20-2*0.5=19 in.
    A quarter circumference is c=19.5*pi/4=4.875*pi in and angle=90 deg.
    The requested geometric length ratio is 8/sqrt(19*0.5)=8/sqrt(9.5).
    This ratio uses current sound wall, not an asserted Part 5 Tc definition.
    """
    cell = envelope.prepare_envelope_cell(**inputs())
    assert cell["geometry"]["mean_diameter_in"] == 19.5
    assert cell["geometry"]["inside_diameter_in"] == 19.0
    assert cell["geometry"]["width_arc_in"] == pytest.approx(4.875 * math.pi)
    assert cell["geometry"]["width_angle_deg"] == 90.0
    assert cell["geometry"]["sound_wall_normalized_length"] == pytest.approx(
        8 / math.sqrt(9.5)
    )
    assert cell["inputs"] == inputs()


def test_levels_cannot_be_mistaken_for_qualified_acceptance():
    """Source audit (2026-10-06), Tables 1 and 3: neither level is qualified."""
    cell = envelope.prepare_envelope_cell(**inputs())
    assert cell["loss_surface"] == "external"
    assert cell["assessment_status"] == "UNQUALIFIED"
    assert [row["level"] for row in cell["levels"]] == [1, 2]
    for row in cell["levels"]:
        assert row["status"] == "NOT_EVALUATED"
        assert row["allowable_remaining_wall_in"] is None
        assert row["criterion"] is None
        assert row["margin"] is None
        assert row["reason_codes"]
        assert all(check["status"] == "NOT_EVALUATED" for check in row["checks"])
        assert all(check["source"] for check in row["checks"])
    assert cell["asset_acceptance"] is None


def test_arc_matches_independent_textbook_exercise_calculation():
    """OpenStax, Precalculus 2e (2021), section 5.1, exercise 40.

    https://openstax.org/books/precalculus-2e/pages/5-1-angles
    For radius 12 in and angle pi/4 rad, hand calculation gives
    arc=12*pi/4=3*pi in. Synthetic OD=25 in, sound wall=1 in yields
    Dm=24 in, radius=12 in; fraction=(pi/4)/(2*pi)=1/8 and angle=45 deg.
    The other request inputs are synthetic and do not come from the exercise.
    """
    cell = envelope.prepare_envelope_cell(
        **inputs(
            od_in=25.0, nominal_wall_in=1.0, sound_wall_in=1.0, width_fraction=0.125
        )
    )
    assert cell["geometry"]["width_arc_in"] == pytest.approx(3 * math.pi)
    assert cell["geometry"]["width_angle_deg"] == 45.0


@pytest.mark.parametrize("field", list(inputs()))
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -1.0, 0.0, True, "1"])
def test_nonphysical_or_nonnumeric_inputs_are_rejected(field, value):
    """Finite positive dimensions, demand and resolution are request requirements."""
    with pytest.raises(ValueError):
        envelope.prepare_envelope_cell(**inputs(**{field: value}))


@pytest.mark.parametrize(
    "overrides",
    [
        {"nominal_wall_in": 10.0},  # No bore: Di=20-2*10=0 in.
        {"sound_wall_in": 0.6},  # Sound wall exceeds nominal study wall.
        {"width_fraction": 1.1},  # Arc exceeds one full circumference.
        {"lower_wall_in": 0.6},  # Reversed search bracket.
        {"upper_wall_in": 0.6},  # Search exceeds current sound wall.
        {"upper_wall_in": 0.1},  # No bracket width.
        {"wall_tolerance_in": 0.5},  # Resolution exceeds bracket width.
        {"wall_tolerance_in": 0.0002},  # Issue 2287 study resolution exceeded.
        {"od_in": 1e308, "width_fraction": 1.0},  # pi*Dm overflows.
        {
            "od_in": 1e-200,
            "nominal_wall_in": 1e-201,
            "sound_wall_in": 1e-201,
            "lower_wall_in": 1e-202,
            "upper_wall_in": 1e-201,
            "wall_tolerance_in": 1e-205,
        },  # Underflow.
    ],
)
def test_inconsistent_geometry_and_unrepresentable_derivatives_are_rejected(overrides):
    """Boundary cases follow dimensional definitions, not normative method limits."""
    with pytest.raises(ValueError):
        envelope.prepare_envelope_cell(**inputs(**overrides))


def test_one_full_circumference_is_a_request_not_an_applicability_pass():
    """A full turn is 360 degrees; source-qualified applicability remains unknown."""
    cell = envelope.prepare_envelope_cell(**inputs(width_fraction=1.0))
    assert cell["geometry"]["width_angle_deg"] == 360.0
    assert cell["assessment_status"] == "UNQUALIFIED"


def test_unrepresentable_integer_is_rejected_as_invalid_input():
    """An artificial 401-digit dimension cannot be represented as binary64."""
    with pytest.raises(ValueError):
        envelope.prepare_envelope_cell(**inputs(od_in=10**400))


def test_matrix_covers_four_geometries_without_duplicating_qualified_results():
    """Issue 2287 (2026) supplies synthetic geometry/length/width inputs.

    Independent count: four pipes * seven lengths * four widths = 112 cells;
    two requested levels per cell = 224 NOT_EVALUATED level records.
    Pressure=100 psi and lower-wall fraction=1/5 are explicit test assumptions.
    No B31.8 pressure equation or material-strength value is inferred.
    """
    cases = [
        inputs(
            od_in=od,
            nominal_wall_in=wall,
            sound_wall_in=wall,
            lower_wall_in=wall / 5,
            upper_wall_in=wall,
            axial_length_in=length,
            width_fraction=width,
        )
        for od, wall in ((12.75, 0.375), (16.0, 0.625), (20.0, 0.5), (30.0, 0.375))
        for length in (0.5, 1, 2, 4, 8, 16, 32)
        for width in (0.02, 0.10, 0.25, 0.50)
    ]
    result = envelope.prepare_envelope_study(cases)
    assert len(result["cells"]) == 112
    assert sum(len(cell["levels"]) for cell in result["cells"]) == 224
    assert result["meta"]["qualified_cases"] == 0
    assert result["meta"]["units"]["pressure"] == "psi"
    assert all(cell["asset_acceptance"] is None for cell in result["cells"])
    assert result == envelope.prepare_envelope_study(iter(cases))
    json.dumps(result, allow_nan=False)


def test_digest_pins_inputs_and_changes_when_width_or_pressure_changes():
    """SHA-256 covers canonical input JSON, independently serialized here."""
    original = envelope.prepare_envelope_study([inputs()])
    canonical = json.dumps(
        [inputs()],
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
        allow_nan=False,
    ).encode("utf-8")
    assert original["meta"]["config_sha256"] == hashlib.sha256(canonical).hexdigest()
    for change in ({"width_fraction": 0.5}, {"pressure_psi": 101.0}):
        changed = envelope.prepare_envelope_study([inputs(**change)])
        assert original["meta"]["config_sha256"] != changed["meta"]["config_sha256"]


def test_empty_study_is_rejected_and_invalid_case_cannot_produce_partial_output():
    """A study must contain valid requests; preparation does not publish partials."""
    with pytest.raises(ValueError):
        envelope.prepare_envelope_study([])
    with pytest.raises(ValueError):
        envelope.prepare_envelope_study([inputs(), inputs(width_fraction=2.0)])
