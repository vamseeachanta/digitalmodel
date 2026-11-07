"""Independent SI examples; see the global-model docs/hand-check-examples.md.

All dimensions, densities and stiffnesses below are synthetic inputs, not design
coefficients. NIST's standard acceleration of gravity (exact) is 9.80665 m/s²:
https://physics.nist.gov/cgi-bin/cuu/Value?gn . Archimedes' principle is from
OpenStax, University Physics Volume 1, first edition (2016), section 14.4.
Arithmetic comparisons allow relative 1e-12 for binary floating-point rounding.
"""

import math

import pytest
from pydantic import ValidationError

from digitalmodel.drilling_riser.global_model import hand_checks as hc
from digitalmodel.drilling_riser.global_model.spec import (
    Contents,
    FlexJoint,
    GlobalEnvironment,
    LineSection,
    RiserGlobalModelSpec,
    Tensioners,
    TensionRing,
)


def section(name="test", **overrides):
    """Synthetic section in m, kg/m, m³/m, N.m² and N; no unit conversion."""
    values = dict(
        name=name,
        length_m=10.0,
        segment_length_m=1.0,
        mass_per_m_kg=1000.0,
        displaced_volume_per_m_m3=1.0,
        bore_id_m=0.0,
        ei_nm2=1e12,
        ea_n=1e10,
        drag_diameter_m=1.0,
        cd_normal=0.0,
        ca_normal=0.0,
    )
    values.update(overrides)
    return LineSection(**values)


def hand_spec():
    """Uniform 40 m vibrating span: 10 m inner barrel plus three 10 m risers.

    The stack is outside the vibrating span. Water is 1000 kg/m³, the riser
    displaces 1 m³/m and weighs 1000 kg/m: each riser has zero submerged weight.
    Empty bore, no added mass, massless ring, nearly pinned flex joints.
    """
    return RiserGlobalModelSpec(
        name="hand-example",
        environment=GlobalEnvironment(water_depth_m=40, water_density_kg_m3=1000),
        contents=Contents(density_kg_m3=0, pressure_ref_z_m=0),
        upper_flex_joint=FlexJoint(pivot_z_m=10, rotational_stiffness_nm_per_rad=1e-8),
        lower_flex_joint=FlexJoint(pivot_z_m=-30, rotational_stiffness_nm_per_rad=1e-8),
        tension_ring=TensionRing(
            mass_kg=0,
            volume_m3=0,
            moments_of_inertia_kgm2=(0, 0, 0),
            z_static_m=0,
        ),
        tensioners=Tensioners(
            count=4,
            sheave_radius_m=1,
            ring_attach_radius_m=1,
            sheave_z_m=100,
            total_vertical_tension_n=100000,
        ),
        inner_barrel=[section("inner")],
        riser=[section(f"riser-{i}") for i in range(3)],
        stack=[section("stack", length_m=1, mass_per_m_kg=2000)],
        wellhead_datum_z_m=-31,
    )


@pytest.mark.parametrize(
    "top, length, wet",
    [
        (4, 10, 6),
        (0, 10, 10),
        (-4, 10, 10),
        (10, 10, 0),
        (11, 10, 0),
        (0, 0, 0),
        (4, 0, 0),
    ],
)
def test_submerged_length_waterline_boundaries(top, length, wet):
    """Intersect [top-length, top] with z <= 0; lengths are exact metres."""
    assert hc.submerged_length_m(top, length) == wet


def test_partial_wet_contents_weight_and_two_section_chain():
    """Independent volume ledger (OpenStax 2016, section 14.4).

    A: L=10 m, structure=1000 kg, bore diameter=2 m, contents density=10/pi
    kg/m³: area=pi m², contents=10 kg/m * 10 m=100 kg; total mass=1100 kg.
    With top=4 m only 6 m displaces 4 m³/m of water at 25 kg/m³: 600 kg.
    Net=500 kg; weight=4903.325 N. B: L=5 m, structure=1000 kg,
    displaced water=500 kg, no contents; weight=4903.325 N.
    Top tension 20000 N -> 15096.675 N -> 10193.350 N.
    """
    a = section("a", mass_per_m_kg=100, displaced_volume_per_m_m3=4, bore_id_m=2)
    b = section("b", length_m=5, mass_per_m_kg=200, displaced_volume_per_m_m3=4)
    assert hc.submerged_weight_n(
        a, top_z_m=4, rho_water=25, rho_contents=10 / math.pi
    ) == pytest.approx(4903.325, rel=1e-12)
    rows = hc.effective_tension_chain(
        [a, b],
        top_z_m=4,
        top_tension_n=20000,
        rho_water=25,
        rho_contents=10 / math.pi,
    )
    assert [r["name"] for r in rows] == ["a", "b"]
    assert [r["top_z_m"] for r in rows] == [4, -6]
    assert [r["bottom_z_m"] for r in rows] == [-6, -11]
    assert [r["te_top_n"] for r in rows] == pytest.approx([20000, 15096.675], rel=1e-12)
    assert [r["te_bottom_n"] for r in rows] == pytest.approx(
        [15096.675, 10193.350], rel=1e-12
    )
    assert [r["submerged_weight_n"] for r in rows] == pytest.approx(
        [4903.325] * 2, rel=1e-12
    )


@pytest.mark.parametrize("z, expected", [(1, 19613.3), (0, 19613.3), (-1, 9806.65)])
def test_ring_weight_at_waterline(z, expected):
    """2000 kg ring, 1 m³, 1000 kg/m³ water; wet ring weighs 1000*g N.

    The lumped ring convention in ring_weight_n applies buoyancy only at z<0;
    this records that boundary rather than modelling partial ring immersion.
    """
    spec = hand_spec()
    spec.tension_ring = TensionRing(
        mass_kg=2000,
        volume_m3=1,
        moments_of_inertia_kgm2=(0, 0, 0),
        z_static_m=z,
    )
    assert hc.ring_weight_n(spec) == pytest.approx(expected, rel=1e-12)


def test_tension_references_with_ring_and_nonzero_joint_steps():
    """Ring at z=0: 1000*g=9806.65 N; net top=90193.35 N.

    Three wet sections each have (1100-1000)*10=1000 kg net mass.
    Joint tensions: 90193.35, 80386.70, 70580.05, 60773.40 N.
    Stack net mass=(2000-1000)*1=1000 kg; bottom=50966.75 N.
    Total string weight=4*9806.65=39226.60 N, excluding the ring.
    """
    spec = hand_spec()
    spec.tension_ring.mass_kg = 1000
    spec.riser = [section(f"riser-{i}", mass_per_m_kg=1100) for i in range(3)]
    ref = hc.tension_references(spec)
    assert {
        k: ref[k]
        for k in (
            "riser_top_n",
            "riser_bottom_n",
            "stack_bottom_n",
            "submerged_weight_n",
            "ring_weight_n",
        )
    } == pytest.approx(
        dict(
            riser_top_n=90193.35,
            riser_bottom_n=60773.40,
            stack_bottom_n=50966.75,
            submerged_weight_n=39226.60,
            ring_weight_n=9806.65,
        ),
        rel=1e-12,
    )
    assert [r["te_bottom_n"] for r in ref["riser_chain"]] == pytest.approx(
        [80386.70, 70580.05, 60773.40],
        rel=1e-12,
    )


def test_tensioner_stiffness_from_force_direction_derivative():
    """Spec.Tensioners documents constant-tension lines and pendulum T/L.

    Independent linearisation: F_x=T*(a-x)/sqrt((a-x)^2+b^2+dz^2).
    -dF_x/dx=T/l*(1-a^2/l^2). Four sheaves at cardinal azimuths,
    radial separation=3 m, dz=4 m, l=5 m. Vertical total=4000 N,
    so each T=1250 N. The two x lines give 160 N/m each and the two
    y lines 250 N/m each: k_x=820 N/m. No axial wire stiffness is used.
    """
    spec = hand_spec()
    spec.tensioners = Tensioners(
        count=4,
        sheave_radius_m=4,
        ring_attach_radius_m=1,
        sheave_z_m=4,
        total_vertical_tension_n=4000,
    )
    assert hc.tensioner_lateral_stiffness_n_m(spec) == pytest.approx(820, rel=1e-12)


@pytest.mark.parametrize("elements", [20, 40])
def test_string_and_beam_limits_on_two_meshes(elements):
    """OpenStax University Physics Vol. 1, first ed. (2016), §§16.3, 16.6:
    v=sqrt(T/m), f_n=n*v/(2L). Synthetic L=100 m, T=10000 N, m=100 kg/m
    gives v=10 m/s and periods [20, 10] s; WKB has travel time=10 s.

    Beam PDE: university teaching note 'Vibrations of Beams' (1999), eq. (3),
    University of Nebraska–Lincoln, 'Vibrations of Beams', 23 April 1999.
    For zero tension, sin(n*pi*x/L) satisfies pinned w=w''=0 at both ends.
    EI*w''''+m*w_tt=0 gives T_n=2L²/(n²*pi*sqrt(EI/m)).
    L=100 m, EI=1e6 N.m², m=100 kg/m gives [200/pi, 50/pi] s.

    The second mode spans 10 elements per half-wave on the coarser mesh,
    and 20 on the finer mesh. Relative 1e-3 is a synthetic numerical budget,
    checked against the closed forms at both resolutions, not a design limit.
    It includes rotational regularisation and the string's small positive
    EI=1e-4 (ratio EI*(2*pi/L)^2/T <4e-11). The separate refinement test
    requires the zero-tension beam error to decrease on doubling the mesh.
    """
    string = [
        hc.BeamSegment(100 / elements, 1e-4, 10000, 10000, 100) for _ in range(elements)
    ]
    beam = [hc.BeamSegment(100 / elements, 1e6, 0, 0, 100) for _ in range(elements)]
    assert hc.tensioned_beam_periods(string, 2) == pytest.approx([20, 10], rel=1e-3)
    assert hc.tensioned_beam_periods(beam, 2) == pytest.approx(
        [200 / math.pi, 50 / math.pi],
        rel=1e-3,
    )
    # WKB integration is exact here; 1e-12 allows summation rounding only.
    assert hc.wkb_string_periods(string, 2) == pytest.approx([20, 10], rel=1e-12)


def test_wkb_reference_periods_constant_tension():
    """The three neutral 10 m risers carry 100000 N at 1000 kg/m.

    OpenStax (2016), §§16.3, 16.6: wave speed=10 m/s, travel time=3 s,
    periods [6, 3] s. The inner barrel is excluded by this reference's contract.
    Tolerance is summation rounding only, not a WKB applicability allowance.
    """
    assert hc.wkb_reference_periods(hand_spec(), 2) == pytest.approx([6, 3], rel=1e-12)


@pytest.mark.parametrize("element_m", [1.0, 0.5])
def test_end_to_end_neutral_string_in_beam_limit(element_m):
    """Hand ledger for a three-section riser plus its inner barrel.

    Each riser weighs (1000-1000*1)*10*g=0 N submerged. Joint tensions
    are all 100000 N. The stack weighs 1000*g=9806.65 N, leaving 90193.35 N.
    Four vertical tensioners each carry 25000 N over 100 m; a horizontal
    displacement x gives restoring force 4*25000*x/100, hence k=1000 N/m.

    The entire vibrating span is 40 m with EI=1e12 N.m² and m=1000 kg/m.
    From the 1999 beam PDE cited above, the pinned zero-tension periods are
    3200/(pi*sqrt(1e9)) and 800/(pi*sqrt(1e9)) seconds.
    The largest tension correction is bounded by T_max*L²/(pi²*EI)<1.7e-5
    in squared frequency. The point-spring correction is <1.4e-6 using
    k/(m*L/2) divided by the first beam squared frequency; negligible end
    springs contribute less. On 40 and 80 elements the second mode spans
    20 and 40 elements per half-wave. The same 1e-3 numerical budget as the
    uniform-beam tests includes these effects and the mass regularisation.
    This is a bending-dominated limit, not a string approximation of a beam.
    """
    spec = hand_spec()
    refs = hc.tension_references(spec)
    assert refs["riser_top_n"] == pytest.approx(100000, rel=1e-12)
    assert [r["te_bottom_n"] for r in refs["riser_chain"]] == pytest.approx(
        [100000, 100000, 100000],
        rel=1e-12,
    )
    assert refs["stack_bottom_n"] == pytest.approx(90193.35, rel=1e-12)
    assert hc.tensioner_lateral_stiffness_n_m(spec) == pytest.approx(1000, rel=1e-12)
    expected = [3200 / (math.pi * math.sqrt(1e9)), 800 / (math.pi * math.sqrt(1e9))]
    assert hc.reference_periods(spec, 2, max_element_m=element_m) == pytest.approx(
        expected,
        rel=1e-3,
    )


@pytest.mark.parametrize("length", [-1.0, 0.0, float("nan")])
def test_section_rejects_nonpositive_or_nan_length(length):
    """LineSection.length_m has gt=0; NaN cannot satisfy that bound."""
    with pytest.raises(ValidationError, match="greater than 0"):
        section(length_m=length)


def test_si_field_rejects_a_unit_bearing_string():
    """A numeric SI field accepts metres, not a string annotated in feet.

    Plain numbers have no unit metadata; a wrong but numeric unit cannot be
    detected by this API and is NOT_EVALUATED rather than claimed as refused.
    """
    with pytest.raises(ValidationError, match="valid number"):
        section(length_m="10 ft")


@pytest.mark.parametrize("field", ["riser", "stack", "inner_barrel"])
def test_spec_rejects_empty_section_lists(field):
    values = hand_spec().model_dump()
    values[field] = []
    with pytest.raises(ValidationError, match="at least 1 item"):
        RiserGlobalModelSpec.model_validate(values)


@pytest.mark.parametrize("segments", [[], [hc.BeamSegment(1, 1, 1, 1, 1)]])
def test_beam_requires_two_elements(segments):
    with pytest.raises(ValueError, match="at least two elements"):
        hc.tensioned_beam_periods(segments, 1)


@pytest.mark.parametrize("top, bottom", [(0, 1), (1, 0), (-1, 1), (1, -1)])
def test_wkb_requires_strictly_positive_end_tensions(top, bottom):
    with pytest.raises(ValueError, match="positive tension throughout"):
        hc.wkb_string_periods([hc.BeamSegment(1, 1, top, bottom, 1)], 1)


def test_reference_periods_rejects_vertical_force_model():
    spec = hand_spec()
    spec.tensioners.representation = "vertical_force"
    with pytest.raises(ValueError, match="'lines' tensioner representation only"):
        hc.reference_periods(spec, 2)


def test_beam_mesh_refinement_reduces_closed_form_error():
    """Same 100 m zero-tension beam as above; each oracle is analytical.

    This checks the element-count justification for the numerical budget;
    neither computed mesh supplies expected periods to the other mesh.
    """
    expected = [200 / math.pi, 50 / math.pi]
    errors = []
    for elements in (20, 40):
        segments = [
            hc.BeamSegment(100 / elements, 1e6, 0, 0, 100) for _ in range(elements)
        ]
        periods = hc.tensioned_beam_periods(segments, 2)
        errors.append([abs(t - exact) / exact for t, exact in zip(periods, expected)])
    assert all(fine < coarse for coarse, fine in zip(errors[0], errors[1]))
