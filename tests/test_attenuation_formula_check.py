"""
Test script to verify DNV RP-F103 attenuation length formula.
This helps debug the magnitude issue in the main test suite.
"""

import math

def calculate_attenuation_length(
    outer_diameter_m,
    wall_thickness_m,
    coating_resistance_ohm_m2,
    steel_resistivity_ohm_m
):
    """
    Calculate attenuation length using DNV RP-F103 formula.

    Formula: La = sqrt((D × t × ρ_coating) / (4 × ρ_steel × ln(D/d)))
    """
    # Calculate inner diameter
    inner_diameter_m = outer_diameter_m - 2.0 * wall_thickness_m

    # Calculate logarithmic term
    diameter_ratio = outer_diameter_m / inner_diameter_m
    ln_diameter_ratio = math.log(diameter_ratio)

    # Calculate attenuation length
    numerator = outer_diameter_m * wall_thickness_m * coating_resistance_ohm_m2
    denominator = 4.0 * steel_resistivity_ohm_m * ln_diameter_ratio

    attenuation_length_m = math.sqrt(numerator / denominator)

    return attenuation_length_m, numerator, denominator, ln_diameter_ratio


# Test values from dnv_base_config fixture
D = 0.610  # outer diameter (m)
t = 0.025  # wall thickness (m)
steel_res = 1.7e-7  # steel resistivity (Ω·m)

print("="*70)
print("DNV RP-F103 Attenuation Length Formula Analysis")
print("="*70)

# Test 1: Current test value (50,000 Ω·m²)
print("\n--- Test 1: Coating Resistance = 50,000 Ω·m² (current test value) ---")
coating_res_1 = 50000.0
La_1, num_1, den_1, ln_1 = calculate_attenuation_length(D, t, coating_res_1, steel_res)
print(f"Outer diameter:      {D} m")
print(f"Wall thickness:      {t} m")
print(f"Inner diameter:      {D - 2*t} m")
print(f"Coating resistance:  {coating_res_1:,.0f} Ω·m²")
print(f"Steel resistivity:   {steel_res:.2e} Ω·m")
print(f"ln(D/d):            {ln_1:.6f}")
print(f"Numerator:          {num_1:.6f}")
print(f"Denominator:        {den_1:.6e}")
print(f"Attenuation length: {La_1:,.2f} m ({La_1/1000:.2f} km)")
print(f"Expected range:     50-1000 m")
print(f"Test result:        {'FAIL' if La_1 >= 1000 else 'PASS'}")

# Test 2: Skill example value (10,000 Ω·m²)
print("\n--- Test 2: Coating Resistance = 10,000 Ω·m² (skill example value) ---")
coating_res_2 = 10000.0
La_2, num_2, den_2, ln_2 = calculate_attenuation_length(D, t, coating_res_2, steel_res)
print(f"Coating resistance:  {coating_res_2:,.0f} Ω·m²")
print(f"Attenuation length: {La_2:,.2f} m ({La_2/1000:.2f} km)")
print(f"Reduction factor:   {La_1/La_2:.2f}x (should be sqrt(5) ≈ 2.24)")
print(f"Test result:        {'FAIL' if La_2 >= 1000 else 'PASS'}")

# Test 3: What coating resistance would give ~500m?
print("\n--- Test 3: Find coating resistance for La = 500m ---")
target_La = 500.0  # meters
# La = sqrt(numerator / denominator)
# La² = numerator / denominator
# numerator = La² × denominator
# D × t × coating = La² × denominator
# coating = (La² × denominator) / (D × t)
ln_ratio = math.log(D / (D - 2*t))
denominator_fixed = 4.0 * steel_res * ln_ratio
coating_res_target = (target_La**2 * denominator_fixed) / (D * t)
La_3, _, _, _ = calculate_attenuation_length(D, t, coating_res_target, steel_res)
print(f"Required coating resistance: {coating_res_target:,.2f} Ω·m²")
print(f"Resulting attenuation length: {La_3:,.2f} m")
print(f"Ratio to test value (50,000): {coating_res_target/50000:.4f}x")

# Test 4: Dimensional analysis
print("\n--- Test 4: Dimensional Analysis ---")
print("Numerator units: m × m × Ω·m² = Ω·m⁴")
print("Denominator units: Ω·m × (dimensionless) = Ω·m")
print("Result units: sqrt(Ω·m⁴ / Ω·m) = sqrt(m³) = m^(3/2)")
print("⚠️  WARNING: Units don't simplify to meters!")
print("This suggests potential formula issue beyond just parameter values.")

print("\n" + "="*70)
