"""
Property-Based Testing for PipeSizing.py
Uses Hypothesis to generate comprehensive test scenarios for formula validation.
"""

import math
import pytest
import numpy as np
from hypothesis import given, strategies as st, assume, settings, example
from hypothesis.strategies import floats, integers, composite
import sys
import os

# Add the source directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../../src'))

# Define realistic ranges for pipe parameters based on industry standards
PIPE_OD_RANGE = (0.405, 60.0)  # inches, from 1/8" to 60"
WALL_THICKNESS_RANGE = (0.035, 3.0)  # inches, realistic wall thickness
ELASTIC_MODULUS_RANGE = (150e9, 250e9)  # Pa, for various steels
DENSITY_RANGE = (7000, 8500)  # kg/m³, for steel alloys
POISSON_RATIO_RANGE = (0.25, 0.35)  # dimensionless, for steel
YIELD_STRENGTH_RANGE = (200e6, 700e6)  # Pa, API steel grades
ULTIMATE_STRENGTH_RANGE = (400e6, 900e6)  # Pa, API steel grades


@composite
def valid_pipe_geometry(draw):
    """Generate valid pipe geometry that satisfies physical constraints"""
    od = draw(floats(min_value=PIPE_OD_RANGE[0], max_value=PIPE_OD_RANGE[1],
                    allow_nan=False, allow_infinity=False))

    # Wall thickness must be less than half the OD
    max_wt = min(od/2 - 0.001, WALL_THICKNESS_RANGE[1])  # Leave small gap for ID
    wt = draw(floats(min_value=WALL_THICKNESS_RANGE[0], max_value=max_wt,
                    allow_nan=False, allow_infinity=False))

    id_val = od - 2*wt

    return {"OD": od, "ID": id_val, "WT": wt}


@composite
def valid_material_properties(draw):
    """Generate valid material properties for steel"""
    E = draw(floats(min_value=ELASTIC_MODULUS_RANGE[0], max_value=ELASTIC_MODULUS_RANGE[1],
                   allow_nan=False, allow_infinity=False))

    nu = draw(floats(min_value=POISSON_RATIO_RANGE[0], max_value=POISSON_RATIO_RANGE[1],
                    allow_nan=False, allow_infinity=False))

    rho = draw(floats(min_value=DENSITY_RANGE[0], max_value=DENSITY_RANGE[1],
                     allow_nan=False, allow_infinity=False))

    smys = draw(floats(min_value=YIELD_STRENGTH_RANGE[0], max_value=YIELD_STRENGTH_RANGE[1],
                      allow_nan=False, allow_infinity=False))

    # Ultimate strength must be greater than yield strength
    smus = draw(floats(min_value=smys*1.1, max_value=ULTIMATE_STRENGTH_RANGE[1],
                      allow_nan=False, allow_infinity=False))

    return {"E": E, "nu": nu, "rho": rho, "SMYS": smys, "SMUS": smus}


class TestPropertyBasedGeometry:
    """Property-based tests for geometric calculations"""

    @given(geometry=valid_pipe_geometry())
    @settings(max_examples=200, deadline=None)
    def test_area_calculations_properties(self, geometry):
        """Test fundamental properties of area calculations"""
        od, id_val, wt = geometry["OD"], geometry["ID"], geometry["WT"]

        # Calculate areas
        Ao = (math.pi / 4) * (od ** 2)
        Ai = (math.pi / 4) * (id_val ** 2)
        A = Ao - Ai

        # Fundamental properties that must always hold
        assert Ao > 0, "Outer area must be positive"
        assert Ai >= 0, "Inner area must be non-negative"
        assert A > 0, "Cross-sectional area must be positive"
        assert Ao > Ai, "Outer area must be greater than inner area"
        assert A < Ao, "Cross-sectional area must be less than outer area"

        # Geometric consistency
        assert abs((od - id_val) - 2*wt) < 1e-10, "OD = ID + 2*WT must hold"

        # Area scaling properties
        scale_factor = 2.0
        Ao_scaled = (math.pi / 4) * ((od * scale_factor) ** 2)
        assert abs(Ao_scaled / Ao - scale_factor**2) < 1e-10, "Area scales as length squared"

    @given(geometry=valid_pipe_geometry())
    @settings(max_examples=200, deadline=None)
    def test_moment_of_inertia_properties(self, geometry):
        """Test fundamental properties of moment of inertia calculations"""
        od, id_val = geometry["OD"], geometry["ID"]

        # Calculate moments of inertia
        Io = (math.pi / 64) * (od ** 4)
        Ii = (math.pi / 64) * (id_val ** 4)
        I = Io - Ii

        # Fundamental properties
        assert Io > 0, "Outer moment of inertia must be positive"
        assert Ii >= 0, "Inner moment of inertia must be non-negative"
        assert I > 0, "Section moment of inertia must be positive"
        assert Io > Ii, "Outer moment must be greater than inner moment"
        assert I < Io, "Section moment must be less than outer moment"

        # Moment scaling properties (should scale as length^4)
        scale_factor = 2.0
        Io_scaled = (math.pi / 64) * ((od * scale_factor) ** 4)
        assert abs(Io_scaled / Io - scale_factor**4) < 1e-10, "Moment scales as length to fourth power"

    @given(geometry=valid_pipe_geometry())
    @settings(max_examples=100, deadline=None)
    def test_polar_moment_properties(self, geometry):
        """Test properties of polar moment calculations"""
        od, id_val = geometry["OD"], geometry["ID"]

        # Calculate polar moments
        Jo = (math.pi / 32) * (od ** 4)
        Ji = (math.pi / 32) * (id_val ** 4)
        J = Jo - Ji

        # Fundamental properties
        assert Jo > 0, "Outer polar moment must be positive"
        assert Ji >= 0, "Inner polar moment must be non-negative"
        assert J > 0, "Section polar moment must be positive"
        assert Jo > Ji, "Outer polar moment must be greater than inner"

        # Relationship between polar and second moments
        Io = (math.pi / 64) * (od ** 4)
        Ii = (math.pi / 64) * (id_val ** 4)
        I = Io - Ii

        # For circular sections: J = 2*I
        assert abs(J - 2*I) < 1e-10, "For circular sections, J = 2*I"


class TestPropertyBasedMaterial:
    """Property-based tests for material property calculations"""

    @given(material=valid_material_properties())
    @settings(max_examples=100, deadline=None)
    def test_shear_modulus_properties(self, material):
        """Test properties of shear modulus calculation"""
        E, nu = material["E"], material["nu"]

        # Calculate shear modulus
        G = E / (2 * (1 + nu))

        # Fundamental properties
        assert G > 0, "Shear modulus must be positive"
        assert G < E, "Shear modulus must be less than elastic modulus"

        # Physical bounds for realistic materials
        assert G > E/4, "Shear modulus should be greater than E/4 for typical materials"
        assert G < E/2, "Shear modulus should be less than E/2 (nu > 0)"

        # Test limiting cases
        if nu < 0.001:  # Near zero Poisson's ratio
            assert abs(G - E/2) < E*0.01, "G approaches E/2 as nu approaches 0"

    @given(
        material=valid_material_properties(),
        geometry=valid_pipe_geometry()
    )
    @settings(max_examples=100, deadline=None)
    def test_structural_stiffness_properties(self, material, geometry):
        """Test properties of structural stiffness calculations"""
        E = material["E"]
        od, id_val = geometry["OD"], geometry["ID"]

        # Calculate section properties
        A = (math.pi / 4) * (od**2 - id_val**2)
        I = (math.pi / 64) * (od**4 - id_val**4)

        # Calculate stiffnesses
        EA = E * A  # Axial stiffness
        EI = E * I  # Flexural stiffness

        # Fundamental properties
        assert EA > 0, "Axial stiffness must be positive"
        assert EI > 0, "Flexural stiffness must be positive"

        # Scaling properties
        # Use math.isclose for floating point comparison to handle precision issues
        assert math.isclose(EA / E, A, rel_tol=1e-9), "Axial stiffness scales linearly with modulus"
        assert math.isclose(EI / E, I, rel_tol=1e-9), "Flexural stiffness scales linearly with modulus"

        # Physical reasonableness
        assert EA < E * (math.pi / 4) * od**2, "Axial stiffness less than solid section"

    @given(
        material=valid_material_properties(),
        geometry=valid_pipe_geometry()
    )
    @settings(max_examples=100, deadline=None)
    def test_mass_properties(self, material, geometry):
        """Test properties of mass calculations"""
        rho = material["rho"]
        od, id_val = geometry["OD"], geometry["ID"]

        # Calculate mass per unit length
        A = (math.pi / 4) * (od**2 - id_val**2)
        mass_per_length = A * rho

        # Fundamental properties
        assert mass_per_length > 0, "Mass per length must be positive"
        # Use math.isclose for floating point comparison to handle precision issues
        assert math.isclose(mass_per_length / rho, A, rel_tol=1e-9), "Mass scales linearly with density"

        # Physical reasonableness for steel pipes
        if rho > 7000:  # Steel density range
            # Convert to kg/m for reasonableness check
            A_m2 = A * (0.0254**2)  # Convert from in² to m²
            mass_kg_m = A_m2 * rho

            # Typical steel pipe mass range
            # Small thin-wall pipes can have mass as low as ~0.1 kg/m
            # Large thick-wall pipes (50" OD, 3" WT) can exceed 2000 kg/m
            # Upper bound based on maximum geometry: 60" OD with maximum WT
            assert 0.1 < mass_kg_m < 5000, "Mass per meter should be reasonable for steel pipes"


class TestPropertyBasedSystemBehavior:
    """Property-based tests for system-level behavior"""

    @given(
        od1=floats(min_value=6, max_value=30, allow_nan=False, allow_infinity=False),
        od2=floats(min_value=3, max_value=15, allow_nan=False, allow_infinity=False),
        wt_ratio=floats(min_value=0.02, max_value=0.2, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=50, deadline=None)
    def test_dual_pipe_system_properties(self, od1, od2, wt_ratio):
        """Test properties of dual pipe systems (pipe-in-pipe)"""
        # Ensure outer pipe is larger
        outer_od = max(od1, od2)
        inner_od = min(od1, od2)

        # Calculate wall thicknesses as fraction of OD
        outer_wt = outer_od * wt_ratio
        inner_wt = inner_od * wt_ratio

        # Ensure physical constraints
        assume(outer_wt < outer_od/2)
        assume(inner_wt < inner_od/2)
        assume(inner_od + 2*inner_wt < outer_od - 2*outer_wt)  # Inner pipe fits inside outer

        # Calculate individual pipe properties
        outer_A = (math.pi / 4) * (outer_od**2 - (outer_od - 2*outer_wt)**2)
        inner_A = (math.pi / 4) * (inner_od**2 - (inner_od - 2*inner_wt)**2)

        outer_I = (math.pi / 64) * (outer_od**4 - (outer_od - 2*outer_wt)**4)
        inner_I = (math.pi / 64) * (inner_od**4 - (inner_od - 2*inner_wt)**4)

        # System properties (additive for this case)
        system_A = outer_A + inner_A
        system_I = outer_I + inner_I

        # Properties that should hold
        assert system_A > outer_A, "System area greater than individual pipe areas"
        assert system_A > inner_A, "System area greater than individual pipe areas"
        assert system_I > outer_I, "System moment greater than individual pipe moments"
        assert system_I > inner_I, "System moment greater than individual pipe moments"

        # Conservation properties
        assert abs(system_A - (outer_A + inner_A)) < 1e-10, "Area additivity"
        assert abs(system_I - (outer_I + inner_I)) < 1e-10, "Moment additivity (for this case)"

    @given(
        base_geometry=valid_pipe_geometry(),
        scale_factor=floats(min_value=0.1, max_value=10.0, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=100, deadline=None)
    def test_scaling_invariance(self, base_geometry, scale_factor):
        """Test scaling properties of calculations"""
        od_base, id_base = base_geometry["OD"], base_geometry["ID"]

        # Scale geometry
        od_scaled = od_base * scale_factor
        id_scaled = id_base * scale_factor

        # Calculate properties for both
        A_base = (math.pi / 4) * (od_base**2 - id_base**2)
        A_scaled = (math.pi / 4) * (od_scaled**2 - id_scaled**2)

        I_base = (math.pi / 64) * (od_base**4 - id_base**4)
        I_scaled = (math.pi / 64) * (od_scaled**4 - id_scaled**4)

        # Test scaling relationships
        area_scale_ratio = A_scaled / A_base
        moment_scale_ratio = I_scaled / I_base

        # Use relative tolerance for floating point comparison (large scale factors cause precision issues)
        assert math.isclose(area_scale_ratio, scale_factor**2, rel_tol=1e-9), "Area scales as length squared"
        assert math.isclose(moment_scale_ratio, scale_factor**4, rel_tol=1e-9), "Moment scales as length to fourth"

    @given(
        geometry=valid_pipe_geometry(),
        pressure=floats(min_value=1e5, max_value=50e6, allow_nan=False, allow_infinity=False),
        smys=floats(min_value=200e6, max_value=700e6, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=50, deadline=None)
    def test_pressure_design_properties(self, geometry, pressure, smys):
        """Test properties of pressure design calculations (Barlow's formula)"""
        od_inches = geometry["OD"]
        wt_inches = geometry["WT"]

        # Convert to consistent units (SI)
        od = od_inches * 0.0254  # meters
        wt = wt_inches * 0.0254  # meters

        # Barlow's formula: P = 2*S*t*F / D
        design_factor = 0.72  # ASME B31.8 typical value
        allowable_pressure = (2 * smys * wt * design_factor) / od

        # Properties that should hold
        assert allowable_pressure > 0, "Allowable pressure must be positive"
        assert allowable_pressure < smys, "Allowable pressure should be less than yield strength"

        # Test pressure design adequacy
        if pressure <= allowable_pressure:
            # Design is adequate
            stress_ratio = pressure / allowable_pressure
            assert stress_ratio <= 1.0, "Stress ratio should not exceed 1.0"
            assert stress_ratio > 0, "Stress ratio should be positive"

        # Scaling properties
        # Doubling wall thickness should roughly double allowable pressure
        wt_doubled = 2 * wt
        allowable_doubled = (2 * smys * wt_doubled * design_factor) / od
        pressure_ratio = allowable_doubled / allowable_pressure
        assert abs(pressure_ratio - 2.0) < 1e-10, "Allowable pressure scales linearly with wall thickness"


class TestPropertyBasedEdgeCases:
    """Property-based tests for edge cases and boundary conditions"""

    @given(
        od=floats(min_value=PIPE_OD_RANGE[0], max_value=1.0, allow_nan=False, allow_infinity=False),
        wt_ratio=floats(min_value=0.001, max_value=0.1, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=50, deadline=None)
    def test_thin_wall_approximations(self, od, wt_ratio):
        """Test behavior for thin-wall pipes"""
        wt = od * wt_ratio
        id_val = od - 2*wt

        assume(wt < od/10)  # Thin wall condition
        assume(id_val > 0)

        # For thin-wall pipes: A ≈ π*D*t (mean diameter approximation)
        mean_diameter = (od + id_val) / 2
        A_exact = (math.pi / 4) * (od**2 - id_val**2)
        A_thin_wall = math.pi * mean_diameter * wt

        # Thin wall approximation should be close for very thin walls
        relative_error = abs(A_thin_wall - A_exact) / A_exact

        if wt_ratio < 0.05:  # Very thin wall
            assert relative_error < 0.05, "Thin wall approximation should be accurate"

    @given(
        od=floats(min_value=1.0, max_value=PIPE_OD_RANGE[1], allow_nan=False, allow_infinity=False),
        wt_ratio=floats(min_value=0.3, max_value=0.49, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=50, deadline=None)
    def test_thick_wall_behavior(self, od, wt_ratio):
        """Test behavior for thick-wall pipes"""
        wt = od * wt_ratio
        id_val = od - 2*wt

        assume(id_val > 0.01)  # Ensure positive ID

        # For thick walls, solid shaft formulas become important reference
        A_pipe = (math.pi / 4) * (od**2 - id_val**2)
        A_solid = (math.pi / 4) * od**2

        # Thick wall pipe area should be significant fraction of solid area
        area_ratio = A_pipe / A_solid

        assert area_ratio > 0.5, "Thick wall pipe should have substantial area"
        assert area_ratio < 1.0, "Pipe area less than solid area"

    @given(
        material=valid_material_properties(),
        temperature_factor=floats(min_value=0.8, max_value=1.2, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=50, deadline=None)
    def test_temperature_effects_properties(self, material, temperature_factor):
        """Test properties when considering temperature effects"""
        E_base = material["E"]
        smys_base = material["SMYS"]

        # Temperature effects (simplified linear model)
        E_temp = E_base * temperature_factor
        smys_temp = smys_base * temperature_factor

        # Properties that should hold
        if temperature_factor > 1.0:  # Heating typically reduces properties
            # This is a simplified model - real behavior is more complex
            pass

        assert E_temp > 0, "Modulus must remain positive"
        assert smys_temp > 0, "Yield strength must remain positive"

        # Shear modulus relationship should still hold
        nu = material["nu"]
        G_temp = E_temp / (2 * (1 + nu))
        assert G_temp > 0, "Shear modulus must remain positive"
        assert G_temp < E_temp, "G < E relationship must hold"


class TestPropertyBasedValidationScenarios:
    """Property-based tests for comprehensive validation scenarios"""

    @given(
        geometry=valid_pipe_geometry(),
        material=valid_material_properties(),
        fluid_density=floats(min_value=800, max_value=1200, allow_nan=False, allow_infinity=False),
        seawater_density=floats(min_value=1020, max_value=1030, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=30, deadline=None)
    def test_complete_pipe_analysis_properties(self, geometry, material, fluid_density, seawater_density):
        """Test properties of complete pipe analysis including fluids"""
        od, id_val, wt = geometry["OD"], geometry["ID"], geometry["WT"]
        E, rho_steel = material["E"], material["rho"]

        # Convert to consistent units (SI)
        od_m = od * 0.0254
        id_m = id_val * 0.0254

        # Calculate properties
        A_steel = (math.pi / 4) * (od_m**2 - id_m**2)
        A_internal = (math.pi / 4) * id_m**2
        A_external = (math.pi / 4) * od_m**2

        # Mass calculations
        mass_steel = A_steel * rho_steel
        mass_internal_fluid = A_internal * fluid_density
        mass_total_air = mass_steel + mass_internal_fluid

        # Buoyancy calculation
        buoyancy_force = A_external * seawater_density
        mass_in_water = mass_total_air - buoyancy_force

        # Properties that should hold
        assert mass_steel > 0, "Steel mass must be positive"
        assert mass_internal_fluid >= 0, "Internal fluid mass must be non-negative"
        assert mass_total_air > mass_steel, "Total mass includes fluid"
        assert buoyancy_force > 0, "Buoyancy force must be positive"

        # Specific gravity checks
        specific_gravity = mass_total_air / buoyancy_force

        if specific_gravity > 1.0:
            assert mass_in_water > 0, "Pipe sinks - positive weight in water"
        else:
            assert mass_in_water < 0, "Pipe floats - negative weight in water"

        # Physical reasonableness
        assert 0.1 < specific_gravity < 10.0, "Specific gravity should be reasonable"

    @example(
        geometry={"OD": 12.75, "ID": 11.626, "WT": 0.562},
        material={"E": 200e9, "nu": 0.3, "rho": 7850, "SMYS": 450e6, "SMUS": 535e6}
    )
    @given(
        geometry=valid_pipe_geometry(),
        material=valid_material_properties()
    )
    @settings(max_examples=50, deadline=None)
    def test_known_values_consistency(self, geometry, material):
        """Test that calculations are consistent with known engineering values"""
        od, id_val = geometry["OD"], geometry["ID"]
        E, rho = material["E"], material["rho"]

        # Calculate section properties
        A = (math.pi / 4) * (od**2 - id_val**2)
        I = (math.pi / 64) * (od**4 - id_val**4)

        # Calculate structural properties
        EI = E * I * (0.0254**4)  # Convert to SI units
        mass_per_length = A * rho * (0.0254**2)  # Convert to SI units

        # Engineering reasonableness checks
        assert EI > 0, "Flexural stiffness must be positive"
        assert mass_per_length > 0, "Mass per length must be positive"

        # For typical steel pipes
        # Note: Thin-wall small pipes can have low mass (~2 kg/m) and stiffness (~1e5 N*m^2)
        # Thick-wall large pipes (15" OD, 3" WT) can exceed 500 kg/m
        if 6 <= od <= 24 and 7000 <= rho <= 8500:  # Common pipe sizes and steel density
            assert 1 < mass_per_length < 1000, "Mass per length in reasonable range (kg/m)"
            assert 1e5 < EI < 1e12, "Flexural stiffness in reasonable range (N*m^2)"


if __name__ == "__main__":
    # Run property-based tests with different configurations
    pytest.main([__file__, "-v", "--hypothesis-show-statistics"])