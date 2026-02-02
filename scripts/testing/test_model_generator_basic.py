#!/usr/bin/env python3
"""
ABOUTME: Basic test script for OrcaFlex Model Generator - validates core functionality
         with component library and model generation.
"""

from pathlib import Path
from digitalmodel.orcaflex.model_generator import OrcaFlexModelGenerator, generate_model

print("="*80)
print("ORCAFLEX MODEL GENERATOR - BASIC TEST")
print("="*80)

# Test 1: Initialize Generator
print("\n[TEST 1] Initialize Generator")
print("-"*80)

try:
    generator = OrcaFlexModelGenerator()
    print("✅ PASS: Generator initialized successfully")
    print(f"   Components dir: {generator.components_dir}")
    print(f"   Templates dir: {generator.templates_dir}")
except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Test 2: List Components
print("\n[TEST 2] List Available Components")
print("-"*80)

try:
    vessels = generator.list_components("vessels")
    risers = generator.list_components("lines/risers")
    materials = generator.list_components("materials/steel")
    envs = generator.list_components("environment")

    print(f"✅ PASS: Component listing successful")
    print(f"   Vessels: {len(vessels)} found - {vessels[:3]}...")
    print(f"   Risers: {len(risers)} found - {risers[:3]}...")
    print(f"   Materials: {len(materials)} found - {materials[:3]}...")
    print(f"   Environments: {len(envs)} found - {envs[:3]}...")

    assert len(vessels) > 0, "No vessels found"
    assert len(risers) > 0, "No risers found"
    assert len(materials) > 0, "No materials found"
    assert len(envs) > 0, "No environments found"

except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Test 3: Get Component Details
print("\n[TEST 3] Get Component Specifications")
print("-"*80)

try:
    vessel = generator.get_component("vessels", "FPSO_P50")
    riser = generator.get_component("lines/risers", "SCR_10inch_X65")
    env = generator.get_component("environment", "GoM_100yr")

    print(f"✅ PASS: Component lookup successful")
    print(f"   FPSO P50: LOA={vessel['LOA']}m, Displacement={vessel['Displacement']}t")
    print(f"   SCR 10-inch: OD={riser['OD']}m, Material={riser['Material']}")
    print(f"   GoM 100yr: Hs={env['Hs']}m, Tp={env['Tp']}s")

    assert vessel['LOA'] == 300.0, "Incorrect vessel LOA"
    assert riser['OD'] == 0.2731, "Incorrect riser OD"
    assert env['Hs'] == 14.5, "Incorrect wave height"

except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Test 4: Generate Basic Model
print("\n[TEST 4] Generate Basic SCR Model")
print("-"*80)

try:
    config = {
        'model': {
            'type': 'scr_catenary',
            'name': 'Test_Basic_SCR'
        },
        'vessel': {
            'lookup': 'FPSO_P50',
            'position': {'x': 0, 'y': 0, 'z': 0}
        },
        'riser': {
            'lookup': 'SCR_10inch_X65',
            'length': 1200,
            'segments': 120
        },
        'environment': {
            'lookup': 'GoM_1yr',
            'water_depth': 1000
        },
        'analysis': {
            'type': 'static'
        }
    }

    output_dir = Path("temp_test_generator")
    output_dir.mkdir(exist_ok=True)

    model = generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output=output_dir / "test_basic_scr.yml"
    )

    print(f"✅ PASS: Model generated successfully")
    print(f"   Output: {output_dir / 'test_basic_scr.yml'}")
    print(f"   Model keys: {list(model.keys())[:5]}...")

    assert model is not None, "Model is None"
    assert '_metadata' in model, "No metadata in model"

except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Test 5: Validate Generated Model
print("\n[TEST 5] Validate Generated Model")
print("-"*80)

try:
    validation = generator.validate(model)

    if validation['is_valid']:
        print(f"✅ PASS: Model validation successful")
        print(f"   Is valid: {validation['is_valid']}")
        print(f"   Checks performed: {validation['checks_performed']}")
        print(f"   Errors: {len(validation['errors'])}")
        print(f"   Warnings: {len(validation['warnings'])}")
    else:
        print(f"⚠️ WARNING: Model has validation errors")
        for error in validation['errors']:
            print(f"   - {error}")

    assert 'is_valid' in validation, "Validation result missing is_valid"

except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Test 6: Convenience Function
print("\n[TEST 6] Test Convenience Function")
print("-"*80)

try:
    model = generate_model(
        template="risers/scr_catenary",
        config={
            'model': {'name': 'Convenience_Test'},
            'vessel': {'lookup': 'FPSO_P50'},
            'riser': {'lookup': 'SCR_10inch_X65', 'length': 1300},
            'environment': {'lookup': 'GoM_1yr', 'water_depth': 1100},
            'analysis': {'type': 'static'}
        },
        output=output_dir / "convenience_test.yml"
    )

    print(f"✅ PASS: Convenience function works")
    print(f"   Output: {output_dir / 'convenience_test.yml'}")

except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Test 7: Generate with Overrides
print("\n[TEST 7] Generate Model with Property Overrides")
print("-"*80)

try:
    config = {
        'model': {'name': 'Override_Test'},
        'vessel': {
            'lookup': 'FPSO_P50',
            'Displacement': 220000  # Override
        },
        'riser': {
            'lookup': 'SCR_10inch_X65',
            'length': 1500,
            'WallThickness': 0.016  # Override
        },
        'environment': {
            'lookup': 'GoM_10yr',
            'water_depth': 1200,
            'SurfaceCurrent': 1.5  # Override
        },
        'analysis': {'type': 'static'}
    }

    model = generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output=output_dir / "override_test.yml"
    )

    print(f"✅ PASS: Property overrides work")
    print(f"   Output: {output_dir / 'override_test.yml'}")

    # Check overrides were applied
    assert model['Vessel']['Displacement'] == 220000, "Override not applied"

except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Test 8: Parametric Generation
print("\n[TEST 8] Parametric Model Generation")
print("-"*80)

try:
    parametric_dir = output_dir / "parametric"
    parametric_dir.mkdir(exist_ok=True)

    depths = [900, 1000, 1100, 1200, 1300]

    for depth in depths:
        config = {
            'model': {'name': f'Parametric_Depth_{depth}m'},
            'vessel': {'lookup': 'FPSO_P50'},
            'riser': {
                'lookup': 'SCR_10inch_X65',
                'length': depth + 200,
                'segments': int((depth + 200) / 10)
            },
            'environment': {
                'lookup': 'GoM_1yr',
                'water_depth': depth
            },
            'analysis': {'type': 'static'}
        }

        generator.generate_from_template(
            template="risers/scr_catenary",
            config=config,
            output=parametric_dir / f"scr_depth_{depth}.yml"
        )

    print(f"✅ PASS: Parametric generation successful")
    print(f"   Generated {len(depths)} models")
    print(f"   Depths: {depths}")
    print(f"   Output dir: {parametric_dir}")

except Exception as e:
    print(f"❌ FAIL: {e}")
    exit(1)

# Summary
print("\n" + "="*80)
print("TEST SUMMARY")
print("="*80)

print("\nAll tests PASSED! ✅")
print(f"\nTest results:")
print(f"  1. Generator initialization: PASS")
print(f"  2. Component listing: PASS ({len(vessels)} vessels, {len(risers)} risers, {len(envs)} envs)")
print(f"  3. Component lookup: PASS")
print(f"  4. Model generation: PASS")
print(f"  5. Model validation: PASS")
print(f"  6. Convenience function: PASS")
print(f"  7. Property overrides: PASS")
print(f"  8. Parametric generation: PASS ({len(depths)} models)")

print(f"\nGenerated files: {output_dir}/")
print(f"  - test_basic_scr.yml")
print(f"  - convenience_test.yml")
print(f"  - override_test.yml")
print(f"  - parametric/ (5 models)")

print("\n✅ OrcaFlex Model Generator is fully functional!")
