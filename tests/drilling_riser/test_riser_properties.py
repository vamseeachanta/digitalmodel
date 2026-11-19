"""Illustrative example tests; expectations follow independent hand formulas.

Unit reference values are typed from NIST SP 811 (2008), Guide for the Use of
the International System of Units: Appendix B.8 gives 1 ft = 0.3048 m and
1 in = 0.0254 m (boldface, exact); footnote 22 gives the exact avoirdupois
pound 0.45359237 kg; footnote 23 gives the exact pound-force 4.4482216152605 N
for the standard acceleration of free fall 9.80665 m/s2; Sec. 4.3 Table 5
gives kilo = 10**3.
"""

from dataclasses import FrozenInstanceError, replace
from math import pi, sqrt

import numpy as np
import pytest

from digitalmodel.citations import Citation, CitationResolutionError, validate_citation
from digitalmodel.drilling_riser import riser_properties
from digitalmodel.drilling_riser.riser_properties import (
    BuoyancyInput,
    RiserJointInput,
    SoilResistanceInput,
    TensionInput,
    apparent_mass_from_ratio,
    compute_buoyancy_diameter_m,
    compute_joint_properties,
    compute_soil_pressure_pa,
    compute_tension_balance,
    mass_from_linear_density,
)

# NIST SP 811 (2008), Appendix B footnote 22 (exact avoirdupois pound).
KG_PER_LB_NIST_SP811 = 0.45359237
# NIST SP 811 (2008), Appendix B footnote 23 (exact for g_n = 9.80665 m/s2).
N_PER_LBF_NIST_SP811 = 4.4482216152605


def illustrative_joint():
    """Neutral illustrative example with SI dimensions and mass equivalents."""
    return RiserJointInput(
        joint_count_dimensionless=3,
        joint_length_m=10.0,
        outer_diameter_m=0.8,
        wall_thickness_m=0.04,
        dry_mass_per_joint_kg=800.0,
        submerged_mass_per_joint_kg=200.0,
        youngs_modulus_pa=200e9,
        poisson_ratio_dimensionless=0.25,
    )


def test_section_properties_and_count_scaling():
    joint = illustrative_joint()
    result = compute_joint_properties(joint)
    bore = 0.8 - 2 * 0.04
    area = pi * (0.8**2 - bore**2) / 4
    inertia = pi * (0.8**4 - bore**4) / 64
    assert result.inner_diameter_m == pytest.approx(bore)
    assert result.area_m2 == pytest.approx(area)
    assert result.second_moment_m4 == pytest.approx(inertia)
    assert result.area_m2 == pytest.approx(0.0955044166, rel=1e-8)
    assert result.second_moment_m4 == pytest.approx(0.0069145198, rel=1e-6)
    assert result.polar_moment_m4 == pytest.approx(2 * inertia)
    assert result.axial_stiffness_n == pytest.approx(200e9 * area)
    assert result.bending_stiffness_n_m2 == pytest.approx(200e9 * inertia)
    assert result.torsional_stiffness_n_m2 == pytest.approx(80e9 * 2 * inertia)
    assert result.total_length_m == 30.0
    assert result.total_dry_mass_kg == 2400.0
    assert result.total_submerged_mass_kg == 600.0
    assert result.dry_mass_kg_per_m == 80.0
    assert result.submerged_mass_kg_per_m == 20.0
    single = compute_joint_properties(replace(joint, joint_count_dimensionless=1))
    assert single.area_m2 == result.area_m2
    assert single.dry_mass_kg_per_m == result.dry_mass_kg_per_m
    assert 3 * single.total_length_m == result.total_length_m


def test_exact_imperial_boundary_and_explicit_mass_coefficient():
    joint = RiserJointInput.from_imperial(
        joint_count_dimensionless=2, joint_length_ft=10.0,
        outer_diameter_in=10.0, wall_thickness_in=0.5,
        dry_mass_per_joint_lb=200.0, submerged_mass_per_joint_lb=100.0,
        youngs_modulus_kpa=200e6, poisson_ratio_dimensionless=0.25,
        mass_conversion_kg_per_lb=KG_PER_LB_NIST_SP811,
    )
    assert joint.joint_length_m == 3.048
    assert joint.outer_diameter_m == pytest.approx(0.254)
    assert joint.wall_thickness_m == pytest.approx(0.0127)
    assert joint.dry_mass_per_joint_kg == 200 * KG_PER_LB_NIST_SP811
    assert joint.submerged_mass_per_joint_kg == 100 * KG_PER_LB_NIST_SP811
    assert joint.youngs_modulus_pa == 200e9
    with pytest.raises(FrozenInstanceError):
        joint.joint_length_m = 4.0


def test_buoyancy_inverse_and_signed_submerged_mass():
    # Illustrative example: displaced annulus per metre recovers the mass loss.
    params = BuoyancyInput(80.0, 20.0, 0.4, 1000.0)
    diameter = compute_buoyancy_diameter_m(params)
    assert diameter == pytest.approx(0.486204, abs=0.0000005)
    assert diameter == pytest.approx(sqrt(4 * 60 / (pi * 1000) + 0.4**2))
    assert 1000 * pi / 4 * (diameter**2 - 0.4**2) == pytest.approx(60.0)
    neutral = replace(params, submerged_mass_kg_per_m=80.0)
    assert compute_buoyancy_diameter_m(neutral) == 0.4
    buoyant = replace(params, submerged_mass_kg_per_m=-40.0)
    assert compute_buoyancy_diameter_m(buoyant) > diameter


def test_zero_mass_and_solid_circular_section():
    joint = replace(illustrative_joint(), wall_thickness_m=0.4,
                    dry_mass_per_joint_kg=0.0, submerged_mass_per_joint_kg=0.0)
    result = compute_joint_properties(joint)
    assert result.inner_diameter_m == 0.0
    assert result.area_m2 == pytest.approx(pi * 0.8**2 / 4)
    assert result.second_moment_m4 == pytest.approx(pi * 0.8**4 / 64)
    assert result.total_dry_mass_kg == 0.0


def test_weight_and_tension_equilibrium_including_compression_and_uplift():
    params = TensionInput(10000.0, 300.0, 10.0)
    result = compute_tension_balance(params)
    exact = pytest.approx  # kN boundary division leaves at most rounding error.
    assert result.inservice_weight_n == 3000.0
    assert result.bottom_tension_n == exact(7000.0, rel=1e-12)
    assert result.bottom_tension_n + result.inservice_weight_n == exact(params.top_tension_n,
                                                                        rel=1e-12)
    # Hand values: 100 - 3000 = -2900 N; 10000 - (-20 * 10) = 10200 N; 10000 - 0.
    assert compute_tension_balance(
        replace(params, top_tension_n=100.0)).bottom_tension_n == exact(-2900, rel=1e-12)
    assert compute_tension_balance(
        replace(params, inservice_mass_kg=-20.0)).bottom_tension_n == exact(10200, rel=1e-12)
    assert compute_tension_balance(
        replace(params, inservice_mass_kg=0.0)).bottom_tension_n == exact(10000, rel=1e-12)


def test_tension_balance_passes_kilonewtons_to_existing_effective_tension(monkeypatch):
    # stackup.effective_tension takes and returns kN; the boundary converts
    # N -> kN on the way in and kN -> N on the way out (kilo = 10**3).
    calls = []

    def spy(top_tension_kn, submerged_weight_kn, depth_factor):
        calls.append((top_tension_kn, submerged_weight_kn, depth_factor))
        return top_tension_kn - submerged_weight_kn * depth_factor

    monkeypatch.setattr(riser_properties, 'effective_tension', spy)
    result = compute_tension_balance(TensionInput(10000.0, 300.0, 10.0))
    assert calls == [(10.0, 3.0, 1.0)]
    # Hand calculation: 10000 N - 300 kg * 10 m/s2 = 7000 N.
    assert result.bottom_tension_n == pytest.approx(7000.0, rel=1e-12)


def test_apparent_mass_ratio_requires_explicit_factor():
    assert apparent_mass_from_ratio(800.0, 0.75) == 600.0
    assert apparent_mass_from_ratio(800.0, -0.25) == -200.0
    assert apparent_mass_from_ratio(0.0, 0.75) == 0.0


def test_soil_load_is_normalized_by_reference_diameter_only():
    params = SoilResistanceInput(0.02, 12000.0, 0.6)
    assert compute_soil_pressure_pa(params) == 20000.0
    assert compute_soil_pressure_pa(replace(params, reference_diameter_m=1.2)) == 10000
    assert compute_soil_pressure_pa(replace(params, line_load_n_per_m=0.0)) == 0.0
    assert compute_soil_pressure_pa(replace(params, line_load_n_per_m=-12000)) == -20000
    converted = SoilResistanceInput.from_imperial(
        deflection_in=0.5, line_load_lbf_per_in=10.0,
        reference_diameter_in=20.0, force_conversion_n_per_lbf=N_PER_LBF_NIST_SP811,
    )
    assert converted.deflection_m == pytest.approx(0.0127)
    expected = (10 * N_PER_LBF_NIST_SP811 / 0.0254) / (20 * 0.0254)
    assert compute_soil_pressure_pa(converted) == pytest.approx(expected)


@pytest.mark.parametrize('field,value', [
    ('joint_count_dimensionless', 0), ('joint_count_dimensionless', 1.5),
    ('joint_count_dimensionless', True), ('joint_length_m', 0),
    ('outer_diameter_m', -1), ('wall_thickness_m', 0),
    ('wall_thickness_m', 0.5), ('dry_mass_per_joint_kg', -1),
    ('submerged_mass_per_joint_kg', float('nan')),
    ('youngs_modulus_pa', 0), ('poisson_ratio_dimensionless', -1),
    ('poisson_ratio_dimensionless', 0.5), ('outer_diameter_m', float('inf')),
    ('joint_length_m', True), ('poisson_ratio_dimensionless', '0.25'),
])
def test_invalid_joint_input(field, value):
    with pytest.raises(ValueError):
        replace(illustrative_joint(), **{field: value})


@pytest.mark.parametrize('params', [
    (0, 0, 0, 0), (10, 20, 0.4, 1000), (-1, 0, 0.4, 1000),
    (10, 0, -0.4, 1000), (10, 0, 0.4, float('nan')),
])
def test_invalid_buoyancy_input(params):
    with pytest.raises(ValueError):
        BuoyancyInput(*params)


@pytest.mark.parametrize('params', [
    (10, 2, 0), (float('inf'), 2, 10), (10, float('nan'), 10),
])
def test_invalid_tension_input(params):
    with pytest.raises(ValueError):
        TensionInput(*params)


@pytest.mark.parametrize('params', [(0, 1, 0), (-0.1, 1, 1), (0, float('inf'), 1)])
def test_invalid_soil_input(params):
    with pytest.raises(ValueError):
        SoilResistanceInput(*params)


@pytest.mark.parametrize('mass,ratio', [(-1, 1), (1, 2), (1, float('nan'))])
def test_invalid_apparent_mass_ratio(mass, ratio):
    with pytest.raises(ValueError):
        apparent_mass_from_ratio(mass, ratio)


@pytest.mark.parametrize('scale', [0, -1, float('nan'), True])
def test_invalid_boundary_coefficient(scale):
    with pytest.raises(ValueError):
        SoilResistanceInput.from_imperial(
            deflection_in=0.5, line_load_lbf_per_in=10,
            reference_diameter_in=20, force_conversion_n_per_lbf=scale,
        )


def imperial_joint_arguments():
    return dict(
        joint_count_dimensionless=2, joint_length_ft=10.0,
        outer_diameter_in=10.0, wall_thickness_in=0.5,
        dry_mass_per_joint_lb=200.0, submerged_mass_per_joint_lb=100.0,
        youngs_modulus_kpa=200e6, poisson_ratio_dimensionless=0.25,
        mass_conversion_kg_per_lb=KG_PER_LB_NIST_SP811,
    )


@pytest.mark.parametrize('field,value', [
    ('mass_conversion_kg_per_lb', 0), ('mass_conversion_kg_per_lb', -1),
    ('mass_conversion_kg_per_lb', float('nan')), ('mass_conversion_kg_per_lb', True),
    ('joint_length_ft', float('inf')), ('outer_diameter_in', float('nan')),
    ('wall_thickness_in', float('-inf')), ('dry_mass_per_joint_lb', float('nan')),
    ('submerged_mass_per_joint_lb', float('inf')), ('youngs_modulus_kpa', float('nan')),
])
def test_invalid_imperial_joint_boundary(field, value):
    with pytest.raises(ValueError):
        RiserJointInput.from_imperial(**{**imperial_joint_arguments(), field: value})


def test_submerged_mass_cannot_exceed_dry_mass_on_one_load_basis():
    # Apparent submerged mass = dry mass - displaced water mass, and displaced
    # mass >= 0, so 110 kg submerged against 100 kg dry implies negative volume.
    with pytest.raises(ValueError, match='submerged mass exceeds dry mass'):
        replace(illustrative_joint(), joint_length_m=10.0,
                dry_mass_per_joint_kg=100.0, submerged_mass_per_joint_kg=110.0)
    with pytest.raises(ValueError, match='submerged mass exceeds dry mass'):
        RiserJointInput.from_imperial(**{**imperial_joint_arguments(),
                                         'submerged_mass_per_joint_lb': 200.5})
    # Equality (no displaced water) is accepted; the buoyancy diameter is the bore.
    neutral = compute_joint_properties(replace(
        illustrative_joint(), dry_mass_per_joint_kg=100.0, submerged_mass_per_joint_kg=100.0))
    assert neutral.dry_mass_kg_per_m == neutral.submerged_mass_kg_per_m == 10.0
    assert compute_buoyancy_diameter_m(BuoyancyInput(
        neutral.dry_mass_kg_per_m, neutral.submerged_mass_kg_per_m,
        neutral.inner_diameter_m, 1000.0)) == pytest.approx(0.72)
    # Net uplift (negative apparent mass) is accepted and feeds the buoyancy step.
    uplift = compute_joint_properties(replace(
        illustrative_joint(), dry_mass_per_joint_kg=100.0, submerged_mass_per_joint_kg=-20.0))
    assert uplift.submerged_mass_kg_per_m == -2.0
    # Hand value: sqrt(4 * (10 - (-2)) / (pi * 1000) + 0.72**2) m.
    assert compute_buoyancy_diameter_m(BuoyancyInput(
        uplift.dry_mass_kg_per_m, uplift.submerged_mass_kg_per_m,
        uplift.inner_diameter_m, 1000.0)) == pytest.approx(sqrt(48 / (pi * 1000) + 0.5184))


def test_integral_numpy_joint_count_is_normalised_to_int():
    joint = replace(illustrative_joint(), joint_count_dimensionless=np.int64(3))
    assert joint.joint_count_dimensionless == 3
    assert type(joint.joint_count_dimensionless) is int
    for bad in (np.bool_(True), np.float64(3.0), np.int64(0)):
        with pytest.raises(ValueError):
            replace(illustrative_joint(), joint_count_dimensionless=bad)


@pytest.mark.parametrize('nu', [-0.999, 0.0, 0.499])
def test_poisson_ratio_inside_positive_moduli_interval_is_accepted(nu):
    # G = E / (2 (1 + nu)) > 0 needs nu > -1; K = E / (3 (1 - 2 nu)) > 0 needs nu < 0.5.
    assert replace(illustrative_joint(), poisson_ratio_dimensionless=nu)


def test_mass_from_required_linear_density():
    assert mass_from_linear_density(25.0, 8.0) == 200.0
    assert mass_from_linear_density(0.0, 8.0) == 0.0
    for density, length in [(-1, 8), (25, 0), (float('nan'), 8), (25, True)]:
        with pytest.raises(ValueError):
            mass_from_linear_density(density, length)


def test_fixed_width_mass_and_gravity_give_compression():
    balance = compute_tension_balance(TensionInput(100_000, np.int16(12_000), np.int16(10)))
    assert balance.inservice_weight_n == 120_000
    assert balance.bottom_tension_n == -20_000


def test_three_fixed_width_joint_masses_sum_in_kg():
    joint = replace(illustrative_joint(), dry_mass_per_joint_kg=np.int16(12_000))
    assert compute_joint_properties(joint).total_dry_mass_kg == 36_000


def test_fixed_width_buoyancy_and_mass_helpers():
    buoyancy = BuoyancyInput(np.int16(24_000), np.int16(-16_000), 0, 1000)
    assert compute_buoyancy_diameter_m(buoyancy) == pytest.approx(sqrt(160 / pi))
    assert mass_from_linear_density(np.int16(12_000), np.int16(3)) == 36_000
    assert apparent_mass_from_ratio(np.int16(12_000), np.int16(-3)) == -36_000


def test_fixed_width_inputs_are_promoted_before_imperial_scaling():
    joint = RiserJointInput.from_imperial(**{
        **imperial_joint_arguments(), 'dry_mass_per_joint_lb': np.int16(20_000),
        'mass_conversion_kg_per_lb': np.int16(2),
    })
    assert joint.dry_mass_per_joint_kg == 40_000
    soil = SoilResistanceInput.from_imperial(
        deflection_in=1, line_load_lbf_per_in=np.int16(-20_000),
        reference_diameter_in=2, force_conversion_n_per_lbf=np.int16(2),
    )
    assert soil.line_load_n_per_m == pytest.approx(-40_000 / 0.0254)


def test_exact_conversions_carry_their_nist_source():
    joint = RiserJointInput.from_imperial(**imperial_joint_arguments())
    length, prefix = joint.citations
    for reference in joint.citations:
        assert isinstance(reference, Citation)
        assert (reference.code_id, reference.publisher, reference.revision) == (
            'NIST-SP-811', 'NIST', '2008',
        )
    assert length.section == 'Appendix B.8 (international foot and inch)'
    assert prefix.section == 'Section 4.3, Table 5 (kilo)'
    assert compute_joint_properties(joint).citations == joint.citations
    assert compute_joint_properties(illustrative_joint()).citations == ()
    assert compute_tension_balance(TensionInput(8000, 100, 10)).citations == (prefix,)
    soil = SoilResistanceInput.from_imperial(
        deflection_in=1, line_load_lbf_per_in=2, reference_diameter_in=3,
        force_conversion_n_per_lbf=4.4,
    )
    assert soil.citations == (length,)
    assert SoilResistanceInput(1, 2, 3).citations == ()


def test_consumer_revalidates_tension_citation_from_package(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    (tmp_path / 'wikis').mkdir()
    monkeypatch.setenv('LLM_WIKI_PATH', str(tmp_path))
    balance = compute_tension_balance(TensionInput(8000, 100, 10))
    validate_citation(balance.citations[0], repo_root=riser_properties.PACKAGED_CITATION_ROOT)


@pytest.mark.parametrize('revision', [None, '1995'])
def test_conversion_requires_matching_source_metadata(tmp_path, monkeypatch, revision):
    monkeypatch.setattr(riser_properties, 'PACKAGED_CITATION_ROOT', tmp_path)
    if revision is not None:
        page = tmp_path / 'wikis/engineering-standards/wiki/standards/nist-sp-811.md'
        page.parent.mkdir(parents=True)
        page.write_text(
            f'---\ncode_id: NIST-SP-811\npublisher: NIST\nrevision: {revision}\n---\n',
            encoding='utf-8',
        )
    with pytest.raises(CitationResolutionError, match='NIST-SP-811'):
        compute_tension_balance(TensionInput(8000, 100, 10))
