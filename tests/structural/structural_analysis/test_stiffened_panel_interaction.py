"""Independent formulas for an illustrative example; no source case data."""

from dataclasses import FrozenInstanceError, replace
import math

import pytest

from digitalmodel.structural.structural_analysis.stiffened_panel_interaction import (
    FlexuralBucklingInput,
    PanelActions,
    PanelResistanceInput,
    PanelResistances,
    ShearBucklingInput,
    characteristic_flexural_strength,
    combined_panel_utilizations,
    design_panel_resistances,
    fixed_end_moments,
    validate_panel_method_basis,
    panel_shear_resistance,
)


def resistances():
    """Illustrative example: independently specified design capacities in SI."""
    return PanelResistances(
        axial_yield_n=900000.0, axial_plate_n=650000.0,
        axial_stiffener_n=550000.0, moment_plate_nm=85000.0,
        moment_stiffener_1_nm=70000.0, moment_stiffener_2_nm=60000.0,
        moment_tension_nm=95000.0, euler_n=2400000.0, shear_pa=65000000.0,
    )


def test_all_eight_interactions_with_nonzero_eccentricity(basis):
    """Eight hand expressions exercise both faces and their capacity choices."""
    r = resistances()
    a = PanelActions(120000.0, 22000.0, 11000.0, 0.045, 13000000.0)
    amplification = 1 - 120000 / 2400000
    shear = (13000000 / 65000000) ** 2
    eccentric_moment = 120000 * 0.045
    expected_plate = (
        120000 / 550000 + (22000 - eccentric_moment) / (70000 * amplification) + shear,
        120000 / 650000 - 2 * 120000 / 900000 + (22000 - eccentric_moment) / (85000 * amplification) + shear,
        120000 / 550000 - 2 * 120000 / 900000 + (11000 + eccentric_moment) / (95000 * amplification) + shear,
        120000 / 650000 + (11000 + eccentric_moment) / (85000 * amplification) + shear,
    )
    expected_stiffener = (
        120000 / 550000 - 2 * 120000 / 900000 + (22000 + eccentric_moment) / (95000 * amplification) + shear,
        120000 / 650000 + (22000 + eccentric_moment) / (85000 * amplification) + shear,
        120000 / 550000 + (11000 - eccentric_moment) / (60000 * amplification) + shear,
        120000 / 650000 - 2 * 120000 / 900000 + (11000 - eccentric_moment) / (85000 * amplification) + shear,
    )
    result = combined_panel_utilizations(a, r, basis=basis)
    assert result.plate_pressure_utilizations == pytest.approx(expected_plate)
    assert result.stiffener_pressure_utilizations == pytest.approx(expected_stiffener)
    assert result.plate_pressure_maximum == max(result.plate_pressure_utilizations)
    assert result.stiffener_pressure_maximum == max(result.stiffener_pressure_utilizations)


@pytest.mark.parametrize('plate_n, stiffener_n', [(650000.0, 550000.0), (450000.0, 750000.0)])
def test_each_interaction_uses_its_axial_capacity(plate_n, stiffener_n, basis):
    """Illustrative unequal capacities isolate the eight axial terms."""
    r = replace(resistances(), axial_plate_n=plate_n, axial_stiffener_n=stiffener_n)
    assert r.axial_plate_n != r.axial_stiffener_n
    result = combined_panel_utilizations(PanelActions(120000, 0, 0, 0, 0), r, basis=basis)
    plate = 120000 / plate_n
    stiffener = 120000 / stiffener_n
    yield_term = 2 * 120000 / 900000
    assert result.plate_pressure_utilizations == pytest.approx(
        (stiffener, plate - yield_term, stiffener - yield_term, plate)
    )
    assert result.stiffener_pressure_utilizations == pytest.approx(
        (stiffener - yield_term, plate, stiffener, plate - yield_term)
    )


def test_pure_shear_and_zero_loading_identities(basis):
    r = resistances()
    assert combined_panel_utilizations(PanelActions(0, 0, 0, 0, 0), r, basis=basis).maximum == 0
    shear = combined_panel_utilizations(PanelActions(0, 0, 0, 0, -r.shear_pa), r, basis=basis)
    assert shear.plate_pressure_utilizations == (1, 1, 1, 1)
    assert shear.stiffener_pressure_utilizations == (1, 1, 1, 1)
    assert shear.maximum == 1


def test_pure_bending_uses_different_compression_capacities(basis):
    r = resistances()
    result = combined_panel_utilizations(PanelActions(0, 42000, 18000, 0, 0), r, basis=basis)
    assert result.plate_pressure_utilizations == pytest.approx((42000/70000, 42000/85000, 18000/95000, 18000/85000))
    assert result.stiffener_pressure_utilizations == pytest.approx((42000/95000, 42000/85000, 18000/60000, 18000/85000))


def test_signed_eccentricity_changes_moment_direction(basis):
    a = PanelActions(120000, 22000, 11000, -0.045, 0)
    result = combined_panel_utilizations(a, resistances(), basis=basis)
    assert result.plate_pressure_utilizations[0] == pytest.approx(120000/550000 + (22000 + 5400)/(70000*(1-120000/2400000)))


@pytest.mark.parametrize('fraction', [1, 1.5])
def test_euler_limit_is_rejected(fraction, basis):
    r = resistances()
    with pytest.raises(ValueError, match='Euler'):
        combined_panel_utilizations(PanelActions(fraction*r.euler_n, 0, 0, 0, 0), r, basis=basis)


def test_euler_limit_approach_increases_bending(basis):
    r = resistances()
    low = combined_panel_utilizations(PanelActions(0.9*r.euler_n, 22000, 0, 0, 0), r, basis=basis)
    high = combined_panel_utilizations(PanelActions(0.99*r.euler_n, 22000, 0, 0, 0), r, basis=basis)
    assert high.maximum > low.maximum


def test_characteristic_flexural_strength_is_quadratic_root(basis):
    p = FlexuralBucklingInput(285e6, 190e6, 0.21, 0.16)
    result = characteristic_flexural_strength(p, basis=basis)
    lam = math.sqrt(285/190)
    mu = (0.34 + 0.08*0.21/0.16)*(lam - 0.2)
    phi = (1 + mu + lam**2)/2
    chi = 1/(phi + math.sqrt(phi**2-lam**2))
    assert result.slenderness == pytest.approx(lam)
    assert result.imperfection == pytest.approx(mu)
    assert result.characteristic_strength_pa == pytest.approx(285e6*chi)


@pytest.mark.parametrize('slenderness', [0.05, 0.2, math.nextafter(0.2, 0.0)])
def test_stocky_flexural_strength_equals_reference(slenderness, basis):
    result = characteristic_flexural_strength(FlexuralBucklingInput(285e6, 285e6/slenderness**2, 0.21, 0.16), basis=basis)
    assert result.characteristic_strength_pa == pytest.approx(285e6, rel=1e-14)


def test_fixed_end_moments_from_uniform_load(basis):
    moments = fixed_end_moments(3200.0, 2.8, basis=basis)
    end, middle = moments.end_moment_nm, moments.midspan_moment_nm
    # Uniform load on a fixed-fixed beam: end moment q L^2/12;
    # centre moment q L^2/8 minus the end moment.
    assert end == pytest.approx(3200*2.8**2/12)
    assert middle == pytest.approx(3200*2.8**2/8-end)
    zero = fixed_end_moments(0, 2.8, basis=basis)
    assert (zero.end_moment_nm, zero.midspan_moment_nm) == (0, 0)


def shear_input():
    return ShearBucklingInput(190e9, 285e6, 0.85, 0.016, 2.8, 2.5e-7, 4.8e-4, 6.1, 0.8, 35.0, 1.2)


def test_shear_resistance_reports_each_candidate_and_minimum(basis):
    p = shear_input()
    result = panel_shear_resistance(p, basis=basis)
    local = 6.1*0.8*190e9*(0.016/0.85)**2
    global_elastic = 35*190e9/(0.85*0.016*2.8**2)*(2.5e-7*(4.8e-4)**3)**0.25
    assert result.yield_design_pa == pytest.approx(285e6/(1.2*math.sqrt(3)))
    assert result.local_elastic_pa == pytest.approx(local)
    assert result.global_elastic_pa == pytest.approx(global_elastic)
    assert result.local_design_pa == pytest.approx(local/1.2)
    assert result.global_design_pa == pytest.approx(global_elastic/1.2)
    assert result.design_pa == min(result.yield_design_pa, result.local_design_pa, result.global_design_pa)


def test_shear_coefficient_remains_a_required_input(basis):
    p = shear_input()
    low = panel_shear_resistance(replace(p, local_elastic_factor=1e-4), basis=basis)
    assert low.design_pa == low.local_design_pa
    high = panel_shear_resistance(replace(p, local_elastic_factor=1e6, global_elastic_factor=1e6), basis=basis)
    assert high.design_pa == high.yield_design_pa
    global_control = panel_shear_resistance(replace(p, global_elastic_factor=1e-4), basis=basis)
    assert global_control.design_pa == global_control.global_design_pa


def test_design_resistances_are_dimensional_products(basis):
    p = PanelResistanceInput(0.018, 0.0025, 0.0018, 285e6, 210e6, 175e6, 165e6, 155e6, 1.2, 2400000, 65e6)
    r = design_panel_resistances(p, basis=basis)
    assert r.axial_yield_n == pytest.approx(0.018*285e6/1.2)
    assert r.axial_plate_n == pytest.approx(0.018*210e6/1.2)
    assert r.axial_stiffener_n == pytest.approx(0.018*175e6/1.2)
    assert r.moment_plate_nm == pytest.approx(0.0025*285e6/1.2)
    assert r.moment_stiffener_1_nm == pytest.approx(0.0018*165e6/1.2)
    assert r.moment_stiffener_2_nm == pytest.approx(0.0018*155e6/1.2)
    assert r.moment_tension_nm == pytest.approx(0.0018*285e6/1.2)
    assert r.euler_n == 2400000
    assert r.shear_pa == 65e6


@pytest.mark.parametrize('invalid', [float('nan'), float('inf'), -float('inf'), True])
def test_all_input_families_reject_nonfinite_or_boolean_values(invalid, basis):
    with pytest.raises(ValueError):
        replace(resistances(), axial_yield_n=invalid)
    with pytest.raises(ValueError):
        PanelActions(0, 0, 0, invalid, 0)
    with pytest.raises(ValueError):
        replace(shear_input(), local_elastic_factor=invalid)
    with pytest.raises(ValueError):
        FlexuralBucklingInput(invalid, 190e6, 0.21, 0.16)
    with pytest.raises(ValueError):
        fixed_end_moments(invalid, 2.8, basis=basis)


@pytest.mark.parametrize('field', ['axial_force_n', 'end_moment_nm', 'midspan_moment_nm'])
def test_negative_compression_or_moment_magnitudes_rejected(field):
    with pytest.raises(ValueError):
        replace(PanelActions(0, 0, 0, 0, 0), **{field: -1})


def test_nonpositive_geometries_and_capacities_rejected(basis):
    for value in [0, -1]:
        with pytest.raises(ValueError):
            replace(resistances(), euler_n=value)
        with pytest.raises(ValueError):
            replace(shear_input(), thickness_m=value)
        with pytest.raises(ValueError):
            FlexuralBucklingInput(285e6, 190e6, 0.21, value)
        with pytest.raises(ValueError):
            fixed_end_moments(3200, value, basis=basis)


def test_inputs_and_results_are_frozen():
    with pytest.raises(FrozenInstanceError):
        resistances().euler_n = 1


@pytest.fixture
def basis(tmp_path):
    """Synthetic citation metadata qualifies test provenance only."""
    from digitalmodel.citations.schema import Citation
    page = tmp_path / "wikis" / "illustrative-method.md"
    page.parent.mkdir()
    page.write_text("---\ncode_id: illustrative-method\npublisher: illustrative example\nrevision: illustrative example\n---\nSynthetic metadata for arithmetic tests only.\n")
    citation = Citation("illustrative-method", "illustrative example", "illustrative example", "illustrative example", "wikis/illustrative-method.md", source_sibling="generic")
    return validate_panel_method_basis(citation, wiki_root=tmp_path)


def test_citation_boundary_fails_closed_on_missing_page(tmp_path):
    from digitalmodel.citations.schema import Citation, CitationResolutionError
    citation = Citation("illustrative-method", "illustrative example", "illustrative example", "illustrative example", "wikis/missing.md", source_sibling="generic")
    with pytest.raises(CitationResolutionError, match="illustrative-method"):
        validate_panel_method_basis(citation, wiki_root=tmp_path)


def test_result_retains_citation_sidecar(basis):
    result = combined_panel_utilizations(PanelActions(0, 0, 0, 0, 0), resistances(), basis=basis)
    assert result.citations == basis.citations


def test_citation_boundary_rejects_metadata_mismatch(tmp_path):
    from digitalmodel.citations.schema import Citation, CitationResolutionError
    page = tmp_path / "wikis" / "illustrative-method.md"
    page.parent.mkdir()
    page.write_text("---\ncode_id: illustrative-method\npublisher: illustrative example\nrevision: illustrative example\n---\n")
    citation = Citation("illustrative-method", "illustrative example", "different example", "illustrative example", "wikis/illustrative-method.md", source_sibling="generic")
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch"):
        validate_panel_method_basis(citation, wiki_root=tmp_path)


def test_flexural_curve_is_continuous_across_short_column_boundary(basis):
    below = characteristic_flexural_strength(FlexuralBucklingInput(285e6, 285e6/(0.2-1e-8)**2, 0.21, 0.16), basis=basis)
    above = characteristic_flexural_strength(FlexuralBucklingInput(285e6, 285e6/(0.2+1e-8)**2, 0.21, 0.16), basis=basis)
    assert below.characteristic_strength_pa == 285e6
    assert above.characteristic_strength_pa <= 285e6
    assert above.characteristic_strength_pa == pytest.approx(below.characteristic_strength_pa, rel=1e-7)


@pytest.mark.parametrize('bad_basis', [None, object()])
def test_numerical_blocks_require_a_validated_basis(bad_basis):
    with pytest.raises(ValueError, match="validated PanelMethodBasis"):
        combined_panel_utilizations(PanelActions(0, 0, 0, 0, 0), resistances(), basis=bad_basis)


def test_upstream_resistance_citations_are_preserved(basis):
    upstream = replace(basis.citations[0], section="illustrative capacity inputs")
    r = replace(resistances(), citations=(upstream,))
    result = combined_panel_utilizations(PanelActions(0, 0, 0, 0, 0), r, basis=basis)
    assert result.citations == basis.citations + (upstream,)


@pytest.mark.parametrize('operation', [
    lambda basis: characteristic_flexural_strength(FlexuralBucklingInput(285e6, 190e6, 0.21, 0.16), basis=basis),
    lambda basis: panel_shear_resistance(shear_input(), basis=basis),
    lambda basis: design_panel_resistances(PanelResistanceInput(0.018, 0.0025, 0.0018, 285e6, 210e6, 175e6, 165e6, 155e6, 1.2, 2400000, 65e6), basis=basis),
    lambda basis: fixed_end_moments(3200, 2.8, basis=basis),
    lambda basis: combined_panel_utilizations(PanelActions(0, 0, 0, 0, 0), resistances(), basis=basis),
])
@pytest.mark.parametrize('bad_basis', [None, object()])
def test_each_entry_point_rejects_missing_basis(operation, bad_basis):
    with pytest.raises(ValueError, match="validated PanelMethodBasis"):
        operation(bad_basis)


@pytest.mark.parametrize('load', [-1, True])
def test_fixed_end_moments_reject_negative_or_boolean_load(load, basis):
    with pytest.raises(ValueError):
        fixed_end_moments(load, 2.8, basis=basis)
