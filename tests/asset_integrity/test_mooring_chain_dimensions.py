"""Independent dimensional arithmetic; source scope is pinned in each case."""

from dataclasses import replace

import pytest

from digitalmodel.asset_integrity.mooring_chain_dimensions import (
    ChainInspection,
    DiameterReading,
    VisualInspection,
    screen_dimensions,
)

SOURCE = (
    "https://iacs.s3.af-south-1.amazonaws.com/wp-content/"
    "uploads/2022/05/20093306/rec38rev2.pdf"
)


@pytest.fixture
def inspection():
    """Hand inputs: nominal 100 mm, axes 96/98 mm, limits 8% and 0.8.

    The limits are synthetic operator inputs, not standard discard criteria.
    IACS Recommendation 38, sections 1, 5.1/5.2, source URL above: mobile
    offshore drilling unit; R3 stud welded away from the flash-butt weld.
    """
    return ChainInspection(
        nominal_diameter_mm=100.0,
        application="mobile_offshore_drilling_unit",
        grade="R3",
        stud_attachment="mechanical_and_other_end_fillet_weld",
        min_area_ratio=0.8,
        max_diameter_loss_fraction=0.08,
        operator_limit_reference="Synthetic limits for hand calculations",
        readings=(DiameterReading("bend-reference", "bend", 96.0, 98.0, 0.0),),
        uniform_loss_confirmed=True,
        perpendicular_readings_confirmed=True,
        coverage_confirmed=True,
        visual=VisualInspection(False, False, False, False, False),
    )


def test_uniform_loss_hand_calculation(inspection):
    """Hand: d=(96+98)/2=97; loss=1-97/100=0.03; area=(97/100)^2=0.9409.

    Both meet synthetic limits 0.08 and 0.8. The area ratio is an equivalent
    circular proxy, not a breaking-strength factor. Perpendicular average method:
    https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf
    sections 5.2 and 5.4, pages 4 and 5.
    """
    result = screen_dimensions(inspection)
    sample = result.samples[0]
    assert sample.mean_diameter_range_mm == pytest.approx((97, 97))
    assert sample.diameter_loss_range_fraction == pytest.approx((0.03, 0.03))
    assert sample.area_ratio_range == pytest.approx((0.9409, 0.9409))
    assert sample.status == "WITHIN_LIMITS"
    assert result.scope == "inspected_locations_only"
    assert result.overall_verdict is None
    assert result.citations[0]["url"] == SOURCE


def test_fairlead_reading_hand_calculation(inspection):
    """Hand: mean(80,80)=80; loss=0.2>0.08; area=0.64<0.8.

    IACS sections 5.2 and 2.2.3 identify fairlead shoulder measurements:
    https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf
    This is a local dimensional failure, not a fairlead wear-rate model.
    """
    reading = DiameterReading("fairlead-reference", "fairlead_shoulder", 80, 80, 0)
    result = screen_dimensions(replace(inspection, readings=(reading,)))
    assert result.samples[0].status == "EXCEEDS_LIMITS"
    assert result.overall_verdict is None


def test_uncertainty_overlap_is_indeterminate(inspection):
    """Hand: axes 92+/-1 -> mean [91,93], loss [0.07,0.09], area [0.8281,0.8649].

    The 0.08 diameter-loss limit lies inside the loss interval; no pass is
    established. Bounds are synthetic metrology inputs. Arithmetic interval
    propagation assumes both axes may reach their bounds simultaneously.
    """
    reading = DiameterReading("bend-reference", "bend", 92, 92, 1)
    sample = screen_dimensions(replace(inspection, readings=(reading,))).samples[0]
    assert sample.status == "INDETERMINATE"
    assert sample.mean_diameter_range_mm == pytest.approx((91, 93))
    assert sample.area_ratio_range == pytest.approx((0.8281, 0.8649))


def test_limit_equality_and_area_overlap(inspection):
    """Hand: axes 90/90 -> loss 0.1, area 0.81, equal supplied limits passes.

    With +/-1 mm, area [0.7921,0.8281] straddles 0.81; the supplied diameter
    limit 0.2 passes but the area comparison is indeterminate.
    """
    reading = DiameterReading("bend-reference", "bend", 90, 90, 0)
    exact = replace(inspection, readings=(reading,), min_area_ratio=0.81,
                    max_diameter_loss_fraction=0.1)
    assert screen_dimensions(exact).samples[0].status == "WITHIN_LIMITS"
    uncertain = replace(exact, readings=(replace(reading, uncertainty_mm=1),),
                        max_diameter_loss_fraction=0.2)
    assert screen_dimensions(uncertain).samples[0].status == "INDETERMINATE"


def test_small_loss_equality_uses_stable_arithmetic(inspection):
    """Hand: mean 97; (100-97)/100=0.03, exactly the supplied loss limit.

    Area 97*97/(100*100)=0.9409 exceeds synthetic area limit 0.8.
    Subtraction before division avoids cancellation in 1-(97/100).
    """
    exact = replace(inspection, max_diameter_loss_fraction=0.03)
    assert screen_dimensions(exact).samples[0].status == "WITHIN_LIMITS"


@pytest.mark.parametrize("field", ["crack", "loose_stud", "missing_stud",
                                   "geometry_anomaly", "pitting"])
@pytest.mark.parametrize("observation", [True, None])
def test_visual_anomaly_or_unknown_blocks_screen(inspection, field, observation):
    """No arithmetic verdict: defect present or uninspected is not all-clear.

    Geometry and stud defects require review; cracks do not establish repair
    eligibility. IACS sections 2.2.2, 5.3/5.4 (pitting classification absent):
    https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf
    """
    result = screen_dimensions(replace(
        inspection, visual=replace(inspection.visual, **{field: observation})))
    assert not result.applicability.ok
    assert all(sample.status == "OUT_OF_SCOPE" for sample in result.samples)


@pytest.mark.parametrize("changes", [
    {"application": "permanent_floating_installation"},
    {"grade": "R5"},
    {"stud_attachment": "studless"},
    {"grade": "R4", "stud_attachment": "mechanical_and_other_end_fillet_weld"},
    {"coverage_confirmed": False},
    {"perpendicular_readings_confirmed": False},
    {"uniform_loss_confirmed": False},
])
def test_outside_source_scope_never_passes(inspection, changes):
    """IACS sections 1/5.1 scope and section 5.2 measurement prerequisite:
    https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf
    The implemented subset admits only the two illustrated R3/R4 attachments;
    no strength or section-shape extrapolation is permitted.
    """
    result = screen_dimensions(replace(inspection, **changes))
    assert not result.applicability.ok
    assert result.samples[0].status == "OUT_OF_SCOPE"
    assert result.samples[0].area_ratio_range is None


def test_r4_mechanical_scope_and_independent_samples(inspection):
    """Hand: original mean 97 passes; second mean 80 fails synthetic limits.

    R4 mechanically locked both ends is an illustrated IACS section 5.1 type:
    https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf
    Sample results remain separate rather than averaging across locations.
    """
    extra = DiameterReading("second-bend", "bend", 80, 80, 0)
    scoped = replace(inspection, grade="R4", stud_attachment="mechanical_both_ends",
                     readings=inspection.readings + (extra,))
    assert [s.status for s in screen_dimensions(scoped).samples] == [
        "WITHIN_LIMITS", "EXCEEDS_LIMITS"]


@pytest.mark.parametrize("changes", [
    {"nominal_diameter_mm": 0}, {"nominal_diameter_mm": True},
    {"nominal_diameter_mm": float("nan")}, {"min_area_ratio": 0},
    {"min_area_ratio": 1.1}, {"min_area_ratio": True},
    {"min_area_ratio": float("nan")}, {"max_diameter_loss_fraction": -0.1},
    {"max_diameter_loss_fraction": float("inf")},
    {"max_diameter_loss_fraction": 1}, {"operator_limit_reference": " "},
    {"readings": ()}, {"coverage_confirmed": 1},
])
def test_invalid_inspection_rejected(inspection, changes):
    """Hand input domain: positive finite reference; ratios within unity.

    Zero reference divides by zero; negative losses and area > unity cannot
    represent loss. Booleans are observations, not dimensional values.
    """
    with pytest.raises(ValueError):
        screen_dimensions(replace(inspection, **changes))


@pytest.mark.parametrize("changes", [
    {"diameter_a_mm": 0}, {"diameter_b_mm": float("inf")},
    {"uncertainty_mm": -1}, {"uncertainty_mm": 96},
    {"uncertainty_mm": float("nan")}, {"diameter_a_mm": True},
    {"location_id": ""},
])
def test_invalid_measurement_rejected(inspection, changes):
    """Hand: axes are positive finite; uncertainty is nonnegative and below
    both axes, so every diameter bound stays positive. Empty IDs cannot trace
    inspection locations. Negative uncertainty reverses interval endpoints.
    """
    reading = replace(inspection.readings[0], **changes)
    with pytest.raises(ValueError):
        screen_dimensions(replace(inspection, readings=(reading,)))


def test_interlink_and_above_reference_are_out_of_scope(inspection):
    """Hand: double-diameter 196 cannot identify two individual worn links;
    axes 100 with +/-1 include growth, outside the uniform-loss interval.

    Double-diameter/baseline prerequisite: regulator integrity report Table 7,
    page 40: https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program//730-aa.pdf
    No negative corrosion verdict or interlink area is extrapolated.
    """
    for reading in (DiameterReading("contact", "interlink", 196, 196, 0),
                    DiameterReading("bend", "bend", 100, 100, 1)):
        result = screen_dimensions(replace(inspection, readings=(reading,)))
        assert result.samples[0].status == "OUT_OF_SCOPE"
        assert result.samples[0].area_ratio_range is None


def test_duplicate_locations_and_nonboolean_observations_rejected(inspection):
    """Traceability requires unique location IDs; 'False' is a nonempty string,
    not an inspected negative finding. Neither has a numerical hand result.
    """
    with pytest.raises(ValueError):
        screen_dimensions(replace(inspection, readings=inspection.readings * 2))
    with pytest.raises(ValueError):
        screen_dimensions(replace(inspection, visual=replace(
            inspection.visual, crack="False")))
