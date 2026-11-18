# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent manufactured calculations, basis H1 in the companion guide.

All fixture numbers are artificial inputs, not engineering defaults. Formula
sources and numerical comparison tolerances are documented in H1. Expected
values below are derived by hand, without querying the implementation.
"""

from dataclasses import replace
import json
from math import log, pi, sqrt
from pathlib import Path

import pytest

from digitalmodel.subsea.lift_design_basis import (
    BASIS_FIELDS,
    CASE_FIELDS,
    LiftDesignBasis,
    LoadCase,
    Source,
    SourcedQuantity,
    screen_case,
)
from digitalmodel.subsea.lift_flow_screening import (
    Calibration,
    Interval,
    airlift_energy_screen,
    deposition_screen,
    settling_screen,
    slugging_screen,
    vertical_transport_screen,
)
from digitalmodel.subsea.mining_riser import (
    BundleSection,
    Tube,
    string_weight_inventory,
)


def source():
    """H1: assumed synthetic inputs; the URL supports equations only."""
    return Source(
        reference="https://eng.libretexts.org/@go/page/29202",
        locator="Companion guide, synthetic basis H1; no measured data",
        conditions="Manufactured arithmetic only; no physical calibration",
        assumed=True,
    )


def quantity(value, unit):
    return SourcedQuantity(value, unit, source())


def case(name="operating", kind="operating", **overrides):
    """H1 case: c=1/4 mixes densities 1 and 5 to give density 2."""
    values = (
        dict(
            liquid_density_kg_m3=1.0,
            solids_density_kg_m3=5.0,
            solids_volume_fraction=0.25,
            mixture_velocity_m_s=2.0,
            significant_wave_height_m=1.0,
            peak_wave_period_s=5.0,
            current_speed_m_s=1.0,
            top_heave_amplitude_m=0.5,
            top_heave_period_s=5.0,
            lower_end_force_n=0.0,
            lower_end_mass_kg=0.0,
            pump_differential_pressure_pa=10.0,
        )
        | overrides
    )
    return LoadCase(
        name, kind, {k: quantity(v, CASE_FIELDS[k].unit) for k, v in values.items()}
    )


def basis(limit=350.0):
    """H1 uniform tube: Aw=3*pi/4, Ai=pi/4, EA=600*pi."""
    values = dict(
        water_depth_m=2.0,
        gravity_m_s2=10.0,
        external_density_kg_m3=1.0,
        outer_diameter_m=2.0,
        wall_thickness_m=0.5,
        material_density_kg_m3=8.0,
        youngs_modulus_pa=800.0,
        contents_inertia_fraction=1.0,
        additional_mass_kg_m=0.0,
        additional_displaced_area_m2=0.0,
        axial_added_mass_kg_m=0.0,
    )
    return LiftDesignBasis(
        {k: quantity(v, BASIS_FIELDS[k].unit) for k, v in values.items()},
        (case(), case("survival", "survival", mixture_velocity_m_s=0.0)),
        None if limit is None else quantity(limit, "N"),
    )


def calibration(model, values, **derived):
    """H1 closed point domain tests arithmetic, not physical applicability."""
    return Calibration(
        model, source(), {k: Interval(v, v) for k, v in (values | derived).items()}
    )


def settling_inputs():
    return dict(
        particle_diameter_m=0.25,
        solids_density_kg_m3=10.0,
        liquid_density_kg_m3=2.0,
        dynamic_viscosity_pa_s=0.5,
        gravity_m_s2=6.0,
        drag_coefficient=2.0,
    )


def deposition_inputs():
    return dict(
        pipe_id_m=2.0,
        particle_diameter_m=0.25,
        solids_density_kg_m3=10.0,
        liquid_density_kg_m3=2.0,
        dynamic_viscosity_pa_s=0.5,
        solids_volume_fraction=0.25,
        gravity_m_s2=6.0,
        deposition_factor=0.5,
    )


def airlift_inputs():
    return dict(
        injection_pressure_abs_pa=8.0,
        outlet_pressure_abs_pa=2.0,
        gas_flow_at_outlet_m3_s=3.0,
        liquid_density_kg_m3=2.0,
        gravity_m_s2=6.0,
        lift_above_surface_m=4.0,
        lift_efficiency=0.5,
    )


def test_weight_inventory_separates_wall_contents_and_buoyancy():
    """H1: per metre wall=6*pi, extra=pi, contents=pi/2, displaced=pi.

    For L=2 and g=10: dry=140*pi N, contents=10*pi N, filled=150*pi N,
    buoyancy=20*pi N and submerged=130*pi N. Added inertia is excluded.
    """
    tube = Tube(2.0, 0.5, 8.0, 800.0, 2.0, 1.0, True)
    section = BundleSection(2.0, (tube,), 1.0, pi, 0.0, 100.0)
    result = string_weight_inventory([section], gravity_m_s2=10.0)
    assert result.dry_weight_n == pytest.approx(140 * pi)
    assert result.contents_weight_n == pytest.approx(10 * pi)
    assert result.filled_weight_n == pytest.approx(150 * pi)
    assert result.buoyancy_n == pytest.approx(20 * pi)
    assert result.submerged_weight_n == pytest.approx(130 * pi)
    buoyant = replace(section, additional_displaced_area_m2=10 * pi)
    assert string_weight_inventory([buoyant], gravity_m_s2=10).submerged_weight_n < 0


@pytest.mark.parametrize(
    "sections,gravity", [([], 10), ([object()], 10), (None, 10), ([], 0)]
)
def test_invalid_weight_inventory(sections, gravity):
    with pytest.raises(ValueError):
        string_weight_inventory(sections, gravity_m_s2=gravity)


def test_basis_connects_sourced_cases_to_existing_string_functions():
    """H1: rho=2, w=(6.5*pi-pi)*10=55*pi N/m; L=2 => top=110*pi.

    Integral T/EA is w*L**2/(2*EA)=11/60 m. The caller's artificial
    350 N limit exceeds 110*pi; it is not a design acceptance criterion.
    """
    b = basis()
    result = screen_case(b, "operating")
    assert result.static.effective_tension_n == pytest.approx((110 * pi, 0.0))
    assert result.static.extension_m == pytest.approx(11 / 60)
    assert result.weights.submerged_weight_n == pytest.approx(110 * pi)
    assert result.tension_status == "PASS"
    assert screen_case(basis(300), "operating").tension_status == "FAIL"
    assert screen_case(basis(None), "operating").tension_status == "NOT_EVALUATED"
    assert b.section("survival").tubes[0].contents_density_kg_m3 == 2.0
    with pytest.raises(ValueError, match="unknown case"):
        screen_case(b, "absent")


def test_basis_reports_compression_even_with_a_large_top_limit():
    b = basis()
    cases = (case(lower_end_force_n=-1.0), b.cases[1])
    assert screen_case(replace(b, cases=cases), "operating").tension_status == "FAIL"


@pytest.mark.parametrize("field", list(BASIS_FIELDS))
def test_basis_requires_every_field(field):
    b = basis()
    with pytest.raises(ValueError, match="fields"):
        replace(b, values={k: v for k, v in b.values.items() if k != field})


@pytest.mark.parametrize("field", list(CASE_FIELDS))
def test_load_case_requires_every_field(field):
    c = case()
    with pytest.raises(ValueError, match="fields"):
        replace(c, values={k: v for k, v in c.values.items() if k != field})


def test_basis_requires_operating_survival_unique_names_and_units():
    b = basis()
    with pytest.raises(ValueError, match="operating and survival"):
        replace(b, cases=(case(),))
    with pytest.raises(ValueError, match="unique"):
        replace(b, cases=(case(), case("operating", "survival")))
    with pytest.raises(ValueError, match="unit"):
        replace(b, values=dict(b.values) | {"water_depth_m": quantity(2, "ft")})
    with pytest.raises(ValueError, match="unit"):
        replace(b, top_tension_limit=quantity(350, "Pa"))
    with pytest.raises(ValueError):
        replace(b, values=dict(b.values) | {"wall_thickness_m": quantity(1, "m")})
    with pytest.raises(ValueError):
        case(kind="unknown")
    with pytest.raises(ValueError):
        case(solids_volume_fraction=1.1)


@pytest.mark.parametrize("value", [True, "2", float("nan"), float("inf")])
def test_quantities_reject_nonfinite_or_non_numeric_values(value):
    with pytest.raises(ValueError):
        quantity(value, "m")


@pytest.mark.parametrize(
    "changes",
    [
        {"reference": ""},
        {"reference": "http://example.org"},
        {"reference": "https://" + "name:password" + "@" + "example.org"},
        {"reference": "https://" + ":password" + "@" + "example.org"},
        {"locator": " "},
        {"conditions": ""},
        {"assumed": "yes"},
    ],
)
def test_source_requires_a_public_reference_and_explicit_assumption_status(changes):
    with pytest.raises(ValueError):
        replace(source(), **changes)


def test_records_defensively_freeze_caller_mappings():
    b = basis()
    values = dict(b.values)
    frozen = replace(b, values=values)
    values.clear()
    assert len(frozen.values) == len(BASIS_FIELDS)
    with pytest.raises(TypeError):
        frozen.values["water_depth_m"] = quantity(3, "m")
    inputs = settling_inputs()
    ranges = {k: Interval(v, v) for k, v in inputs.items()} | {
        "particle_reynolds": Interval(2, 2)
    }
    cal = Calibration("isolated_sphere", source(), ranges)
    ranges.clear()
    assert settling_screen(**inputs, calibration=cal).status == "EVALUATED"


def test_settling_matches_independent_force_balance_and_reynolds():
    """F1: w**2=4*6*.25*(10/2-1)/(3*2)=4, so w=2 m/s.

    Re=rho_l*w*d/mu=2*2*.25/.5=2. Non-unity rho and Cd expose dropped
    terms. This is an isolated sphere; no hindered-settling claim is made.
    """
    inputs = settling_inputs()
    result = settling_screen(
        **inputs,
        calibration=calibration("isolated_sphere", inputs, particle_reynolds=2.0),
    )
    assert result.status == "EVALUATED"
    assert result.value == pytest.approx(2.0)
    assert result.unit == "m/s"
    bad_re = calibration("isolated_sphere", inputs, particle_reynolds=3.0)
    assert settling_screen(**inputs, calibration=bad_re).status == "NOT_EVALUATED"


def test_horizontal_deposition_matches_independent_froude_definition():
    """F2: v=.5*sqrt(2*6*2*(10/2-1))=sqrt(24) m/s.

    The factor is synthetic and belongs to a horizontal run, never a vertical
    deposition rule. Neither concentration nor viscosity is guessed away:
    both must be inside the caller's factor-calibration envelope.
    """
    inputs = deposition_inputs()
    result = deposition_screen(
        **inputs, calibration=calibration("horizontal_deposition", inputs)
    )
    assert result.status == "EVALUATED"
    assert result.value == pytest.approx(sqrt(24))


def test_vertical_transport_matches_phase_continuity():
    """F3: with slip=2, c=.25, target solids speed=1, j=.75*2+1=2.5."""
    inputs = dict(
        slip_velocity_m_s=2.0,
        solids_volume_fraction=0.25,
        required_solids_velocity_m_s=1.0,
    )
    result = vertical_transport_screen(
        **inputs, calibration=calibration("vertical_slip", inputs)
    )
    assert result.value == pytest.approx(2.5)
    wrong = calibration("horizontal_deposition", inputs)
    assert (
        vertical_transport_screen(**inputs, calibration=wrong).status == "NOT_EVALUATED"
    )


def test_airlift_energy_matches_independent_isothermal_work():
    """F4: gas power=2*3*ln(8/2)=6*ln(4) W.

    Liquid flow=.5*6*ln(4)/(2*6*4)=ln(4)/16 m3/s. Pressures are absolute,
    gas volume is at the outlet; this is an energy bound, not capacity.
    """
    inputs = airlift_inputs()
    result = airlift_energy_screen(
        **inputs, calibration=calibration("isothermal_energy", inputs)
    )
    assert result.status == "EVALUATED"
    assert result.gas_power_w == pytest.approx(6 * log(4))
    assert result.liquid_flow_m3_s == pytest.approx(log(4) / 16)


def test_airlift_denominator_underflow_is_not_evaluated():
    """H1 numeric-scale probe: three finite 1e-200 factors underflow to zero.

    No finite positive energy-flow answer is representable by this direct
    arithmetic. The screen must expose the missing result instead of raising
    a division error or claiming a result. These are not physical inputs.
    """
    inputs = airlift_inputs() | dict(
        liquid_density_kg_m3=1e-200,
        gravity_m_s2=1e-200,
        lift_above_surface_m=1e-200,
    )
    result = airlift_energy_screen(
        **inputs, calibration=calibration("isothermal_energy", inputs)
    )
    assert result.status == "NOT_EVALUATED"
    assert result.gas_power_w is None
    assert result.liquid_flow_m3_s is None


def test_inventory_of_nonuniform_sections_matches_total_static_load():
    """H1 two sections: lengths 2 and 3, contents densities 2 and 4.

    Dry mass=5*6*pi=30*pi kg; contents=(2*.5+3*1)*pi=4*pi kg;
    displacement=5*pi m3. With g=10, filled=340*pi and buoyancy=50*pi N.
    Static top force includes an independently supplied 7 N lower load.
    """
    from digitalmodel.subsea.mining_riser import free_hanging_screen

    tube = Tube(2, 0.5, 8, 800, 2, 1, True)
    top = BundleSection(2, (tube,), 1, 0, 0, 0)
    bottom = replace(top, length_m=3, tubes=(replace(tube, contents_density_kg_m3=4),))
    weights = string_weight_inventory([top, bottom], gravity_m_s2=10)
    static = free_hanging_screen([top, bottom], gravity_m_s2=10, lower_end_force_n=7)
    assert weights.filled_weight_n == pytest.approx(340 * pi)
    assert weights.buoyancy_n == pytest.approx(50 * pi)
    assert static.effective_tension_n[0] == pytest.approx(290 * pi + 7)
    assert weights.submerged_weight_n == pytest.approx(290 * pi)


def test_caller_criterion_and_calibration_boundaries_are_inclusive():
    """H1 exact criterion T=110*pi N; an interval endpoint includes its value."""
    assert screen_case(basis(110 * pi), "operating").tension_status == "PASS"
    interval = Interval(1, 2)
    assert interval.contains(1) and interval.contains(2)
    assert not interval.contains(0) and not interval.contains(3)


@pytest.mark.parametrize(
    "changes",
    [
        {"model": ""},
        {"source": None},
        {"ranges": {}},
        {"ranges": {"speed": (0, 1)}},
    ],
)
def test_invalid_calibration_metadata_is_rejected(changes):
    with pytest.raises(ValueError):
        replace(calibration("vertical_slip", {"speed": 1.0}), **changes)


@pytest.mark.parametrize(
    "screen,inputs,model",
    [
        (settling_screen, settling_inputs(), "isolated_sphere"),
        (deposition_screen, deposition_inputs(), "horizontal_deposition"),
        (airlift_energy_screen, airlift_inputs(), "isothermal_energy"),
    ],
)
def test_missing_or_incomplete_calibration_is_not_a_pass(screen, inputs, model):
    assert screen(**inputs, calibration=None).status == "NOT_EVALUATED"
    cal = calibration(model, inputs)
    incomplete = replace(cal, ranges={"unrelated": Interval(0, 1)})
    assert screen(**inputs, calibration=incomplete).status == "NOT_EVALUATED"
    outside = replace(
        cal, ranges={k: Interval(v + 1, v + 2) for k, v in inputs.items()}
    )
    assert screen(**inputs, calibration=outside).status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "screen,inputs,field,bad",
    [
        (settling_screen, settling_inputs(), "drag_coefficient", 0),
        (settling_screen, settling_inputs(), "solids_density_kg_m3", 1),
        (settling_screen, settling_inputs(), "particle_diameter_m", True),
        (settling_screen, settling_inputs(), "dynamic_viscosity_pa_s", float("nan")),
        (deposition_screen, deposition_inputs(), "solids_volume_fraction", 1),
        (deposition_screen, deposition_inputs(), "particle_diameter_m", 2),
        (deposition_screen, deposition_inputs(), "deposition_factor", -1),
        (airlift_energy_screen, airlift_inputs(), "injection_pressure_abs_pa", 1),
        (airlift_energy_screen, airlift_inputs(), "outlet_pressure_abs_pa", 0),
        (airlift_energy_screen, airlift_inputs(), "lift_efficiency", 1.1),
        (airlift_energy_screen, airlift_inputs(), "lift_above_surface_m", 0),
    ],
)
def test_invalid_flow_inputs_are_rejected_even_without_calibration(
    screen, inputs, field, bad
):
    with pytest.raises(ValueError):
        screen(**(inputs | {field: bad}), calibration=None)


@pytest.mark.parametrize(
    "lower,upper", [(2, 1), (float("nan"), 2), (0, float("inf")), (True, 2)]
)
def test_invalid_applicability_intervals(lower, upper):
    with pytest.raises(ValueError):
        Interval(lower, upper)


def test_slugging_is_explicitly_not_evaluated_without_readable_source():
    result = slugging_screen()
    assert result.status == "NOT_EVALUATED"
    assert result.value is None
    assert "source" in result.reason
    assert "regime" in result.reason


def test_worked_example_has_independently_calculated_answers():
    """H1 example repeats the 110*pi static balance and 2 m/s drag balance."""
    from runpy import run_path

    path = (
        Path(__file__).resolve().parents[2] / "examples/mining_lift_riser/analysis.py"
    )
    results = run_path(str(path))["run_example"]()
    assert results["top_tension_n"] == pytest.approx(110 * pi)
    assert results["settling_velocity_m_s"] == pytest.approx(2)
    assert results["tension_status"] == "PASS"
    assert results["slugging_status"] == "NOT_EVALUATED"
    assert results["first_axial_period_s"] > 0
    assert "NaN" not in json.dumps(results, allow_nan=False)
