# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic checks, with no design acceptance criteria.

Equations: OpenStax Prealgebra 2e (2020), §9.5; University Physics
Volume 1 (2016), §§12.3, 14.1, 14.4. Second moments: Engineering Statics:
Open and Interactive, §10.2.4, Eq. 10.2.10 (undated public edition,
accessed 2026-10-22), https://engineeringstatics.org/MOI-common-shapes.html.
Steel density: Engineering ToolBox (2004), Metals and Alloys - Densities,
steel row, https://www.engineeringtoolbox.com/metal-alloys-densities-d_50.html.
Other test properties are synthetic arithmetic inputs, not material defaults.
The relative tolerance 1e-12 is an arithmetic rounding allowance for these
hand calculations, not a physical acceptance limit.
"""

import math

import pytest

from digitalmodel.subsea.cross_sections import calculations as calc
from digitalmodel.subsea.cross_sections.schema import Provenance, RadialLayer, UnitValue


def layer(inner, outer, density=7850):
    """Use the cited steel density only when explicitly constructing a test."""
    return RadialLayer(
        name="arithmetic layer", role="test", material="unspecified",
        inner_diameter=UnitValue(value=inner, unit="m"),
        outer_diameter=UnitValue(value=outer, unit="m"),
        density=UnitValue(value=density, unit="kg/m^3"),
        provenance=Provenance(source_id="hand-arithmetic", source_type="project_assumption"),
    )


def stack():
    return [layer(0.2, 0.3), layer(0.3, 0.4, 1000), layer(0.4, 0.5, 100)]


def test_pipe_example_area_and_mass():
    """OD=.2731, ID=.2461 m: (OD-ID)(OD+ID)/4=.0035046 m².

    A=.0035046*pi m²; rho=7850 kg/m³ gives m/L=27.51111*pi kg/m.
    Geometry is an assumed example; density is the cited steel-table input.
    """
    result = calc.calculate_annular_layer(layer(0.2461, 0.2731))
    assert result.area_m2 == pytest.approx(0.0035046 * math.pi, rel=1e-12)
    assert result.mass_per_length_kg_m == pytest.approx(27.51111 * math.pi, rel=1e-12)


def test_thin_wall_approximation_bound():
    """OD=1, t=.001, ID=.998 m: A=.000999*pi, A_thin=.001*pi.

    Using OD in pi*OD*t overestimates by t/(OD-t)=1/999 < .001002.
    This bound is derived algebraically here, not a design-code criterion.
    Using the mean diameter gives exactly pi*(OD-t)*t.
    """
    area = calc.calculate_annular_layer(layer(0.998, 1)).area_m2
    assert area == pytest.approx(0.000999 * math.pi, rel=1e-12)
    assert 0 < (0.001 * math.pi - area) / area < 0.001002


@pytest.mark.parametrize("inner,outer,density", [
    (1, 1, 7850), (2, 1, 7850), (-1, 1, 7850), (0, -1, 7850),
    (0, 1, -1), (0, 1, 0), (math.nan, 1, 7850),
    (0, math.inf, 7850), (0, 1, math.inf), (0, 1, math.nan),
])
def test_invalid_layer_inputs(inner, outer, density):
    with pytest.raises(ValueError):
        calc.calculate_annular_layer(layer(inner, outer, density))


@pytest.mark.parametrize("bore,weight_coefficient", [("sealed", 553.75), ("flooded", 653.75)])
def test_layer_stack_mass_buoyancy_and_build_up(bore, weight_coefficient):
    """D pairs (.2,.3),(.3,.4),(.4,.5): A/pi=.0125,.0175,.0225.

    rho=7850,1000,100 gives m/(L*pi)=98.125,17.5,2.25; sum=117.875.
    With synthetic water rho=1000 and g=10, sealed displacement/pi=.0625:
    W/(L*pi)=(117.875-62.5)*10=553.75 N/m. Flooded displacement/pi
    =.0625-.01=.0525 gives 653.75 N/m. OD=.2+2*(.05+.05+.05)=.5 m.
    """
    result = calc.calculate_radial_section(
        stack(), water_density_kg_m3=1000, gravity_m_s2=10, bore_condition=bore,
    )
    for actual, coefficient in zip(result.layers, [0.0125, 0.0175, 0.0225], strict=True):
        assert actual.area_m2 == pytest.approx(coefficient * math.pi, rel=1e-12)
    assert result.mass_per_length_kg_m == pytest.approx(117.875 * math.pi, rel=1e-12)
    assert sum(item.mass_per_length_kg_m for item in result.layers) == result.mass_per_length_kg_m
    assert result.submerged_weight_n_m == pytest.approx(weight_coefficient * math.pi, rel=1e-12)
    assert result.outer_diameter_m == 0.5


def test_buoyant_stack_can_have_negative_weight():
    """Solid OD=2, rho=1, water rho=2, g=10: W/L=(1-2)*pi*10."""
    result = calc.calculate_radial_section(
        [layer(0, 2, 1)], water_density_kg_m3=2, gravity_m_s2=10, bore_condition="sealed",
    )
    assert result.submerged_weight_n_m == pytest.approx(-10 * math.pi, rel=1e-12)


@pytest.mark.parametrize("kwargs", [
    {"water_density_kg_m3": 0}, {"water_density_kg_m3": -1},
    {"water_density_kg_m3": math.inf}, {"water_density_kg_m3": math.nan},
    {"gravity_m_s2": 0}, {"gravity_m_s2": -1}, {"gravity_m_s2": math.inf},
    {"bore_condition": "unspecified"},
])
def test_invalid_environment(kwargs):
    inputs = dict(water_density_kg_m3=1000, gravity_m_s2=10, bore_condition="sealed")
    inputs.update(kwargs)
    with pytest.raises(ValueError):
        calc.calculate_radial_section(stack(), **inputs)


@pytest.mark.parametrize("layers", [[], [layer(0.2, 0.3), layer(0.31, 0.4)],
                                         [layer(0.2, 0.3), layer(0.29, 0.4)]])
def test_empty_or_noncontiguous_stack(layers):
    with pytest.raises(ValueError):
        calc.calculate_radial_section(
            layers, water_density_kg_m3=1000, gravity_m_s2=10, bore_condition="sealed",
        )


def test_elastic_composite_closed_forms():
    """I/pi=(OD^4-ID^4)/64; Z=I/(OD/2); EA=sum(E*A); EI=sum(E*I).

    E=(2e11,1e9,1e6) Pa: steel uses OpenStax (2016), §12.3, Table 12.1;
    the other two are synthetic. Layer I/pi=.0001015625,.0002734375,.0005765625.
    Total I/pi=.0009515625; Z/pi=.00380625; A/pi=.0525.
    EA/pi=2500000000+17500000+22500=2517522500 N.
    EI/pi=20312500+273437.5+576.5625=20586514.0625 N m².
    Perfect bonding and common curvature/strain are assumed.
    """
    result = calc.calculate_elastic_section(stack(), youngs_moduli_pa=[2e11, 1e9, 1e6])
    assert result.area_m2 == pytest.approx(0.0525 * math.pi, rel=1e-12)
    assert result.second_moment_m4 == pytest.approx(0.0009515625 * math.pi, rel=1e-12)
    assert result.section_modulus_m3 == pytest.approx(0.00380625 * math.pi, rel=1e-12)
    assert result.axial_stiffness_n == pytest.approx(2517522500 * math.pi, rel=1e-12)
    assert result.bending_stiffness_n_m2 == pytest.approx(20586514.0625 * math.pi, rel=1e-12)


def test_solid_circle_elastic_properties():
    """OD=2 m: A=pi, I=pi/4, Z=pi/4; synthetic E=4 Pa gives EA=4pi, EI=pi."""
    result = calc.calculate_elastic_section([layer(0, 2)], youngs_moduli_pa=[4])
    assert result.second_moment_m4 == pytest.approx(math.pi / 4, rel=1e-12)
    assert result.section_modulus_m3 == pytest.approx(math.pi / 4, rel=1e-12)
    assert result.axial_stiffness_n == pytest.approx(4 * math.pi, rel=1e-12)
    assert result.bending_stiffness_n_m2 == pytest.approx(math.pi, rel=1e-12)


@pytest.mark.parametrize("moduli", [[], [1], [1, 2, 0], [1, 2, -1],
                                     [1, 2, math.inf], [1, 2, math.nan]])
def test_modulus_required_for_every_layer(moduli):
    with pytest.raises(ValueError):
        calc.calculate_elastic_section(stack(), youngs_moduli_pa=moduli)


def test_si_contract_and_explicit_round_trip():
    """BIPM SI Brochure, ninth ed. (2019), Table 7: milli=10^-3 exactly.

    No implicit conversion: serialize .2/.3 m as 200/300 mm, then convert
    back explicitly; both area and mass must survive the round trip.
    """
    original = layer(0.2, 0.3)
    data = original.model_dump()
    for name in ("inner_diameter", "outer_diameter"):
        data[name] = dict(value=data[name]["value"] * 1000, unit="mm")
    with pytest.raises(ValueError, match="metres"):
        calc.calculate_annular_layer(RadialLayer.model_validate(data))
    for name in ("inner_diameter", "outer_diameter"):
        data[name] = dict(value=data[name]["value"] / 1000, unit="m")
    restored = calc.calculate_annular_layer(RadialLayer.model_validate(data))
    assert restored == calc.calculate_annular_layer(original)


@pytest.mark.parametrize("scale", [1e200, 1e-200])
def test_elastic_unrepresentable_geometry(scale):
    """Synthetic scales put area or I outside binary64 range, not near a design limit."""
    with pytest.raises(ValueError, match="finite and positive"):
        calc.calculate_elastic_section([layer(scale, 2 * scale)], youngs_moduli_pa=[1])


def test_stiffness_does_not_need_density():
    """OD=2 m, E=4 Pa: EI=pi even when no mass property is available."""
    item = layer(0, 2)
    item.density = None
    assert calc.calculate_elastic_section([item], youngs_moduli_pa=[4]).bending_stiffness_n_m2 == pytest.approx(math.pi)
    with pytest.raises(ValueError, match="density is required"):
        calc.calculate_radial_section([item], water_density_kg_m3=1, gravity_m_s2=1, bore_condition="sealed")


def test_calculation_rejects_mutated_geometry():
    item = layer(0, 2)
    item.outer_diameter.value = math.nan
    with pytest.raises(ValueError, match="finite"):
        calc.calculate_elastic_section([item], youngs_moduli_pa=[1])


def test_buoyancy_rejects_unrepresentable_environment_result():
    """Finite synthetic inputs can still overflow displaced mass; no infinity is a result."""
    with pytest.raises(ValueError, match="submerged weight must be finite"):
        calc.calculate_radial_section(
            [layer(0, 2)], water_density_kg_m3=1e308, gravity_m_s2=1e308, bore_condition="sealed",
        )
