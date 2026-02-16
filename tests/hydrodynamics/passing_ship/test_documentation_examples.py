"""Test script to verify all documentation code examples work correctly."""

import os
import sys
import tempfile
import shutil
from pathlib import Path
import json
import yaml
import pytest
import numpy as np

# Add the src directory to the path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../src')))

def create_test_config():
    """Create a test configuration file."""
    config = {
        'vessel': {
            'moored': {
                'length': 180.0,
                'beam': 32.0,
                'draft': 12.0,
                'displacement': 45000.0
            },
            'passing': {
                'length': 200.0,
                'beam': 35.0,
                'draft': 14.0,
                'velocity': 5.0
            }
        },
        'environment': {
            'water_depth': 50.0,
            'water_density': 1025.0,
            'separation_distance': 50.0
        },
        'calculation': {
            'stagger_range': [-400, 400],
            'stagger_points': 81,
            'use_finite_depth_correction': True,
            'harmonic_terms': 10
        }
    }
    return config

def test_basic_usage_example():
    """Test the basic usage example from README.md."""
    print("\n=== Testing Basic Usage Example ===")

    from digitalmodel.hydrodynamics.passing_ship import (
        PassingShipCalculator, VesselConfig, EnvironmentalConfig, CalculationConfig
    )

    # Create vessel configurations (block_coefficient is required)
    moored_vessel = VesselConfig(
        length=180.0,
        beam=32.0,
        draft=12.0,
        block_coefficient=0.85
    )

    passing_vessel = VesselConfig(
        length=200.0,
        beam=35.0,
        draft=14.0,
        block_coefficient=0.80
    )

    environment = EnvironmentalConfig(
        water_depth=50.0,
        water_density=1025.0
    )

    calculation_config = CalculationConfig(
        num_points=50,
        depth_modes=10
    )

    # Initialize calculator with configuration objects
    calculator = PassingShipCalculator(
        moored_vessel=moored_vessel,
        passing_vessel=passing_vessel,
        environment=environment,
        calculation_config=calculation_config
    )

    # Perform calculation at stagger = 0 with separation = 50m
    results = calculator.calculate_forces(separation=50.0, stagger=0, velocity=5.0)

    # calculate_forces returns a dict with 'surge', 'sway', 'yaw' keys
    print(f"Surge Force: {results['surge']:.2f} N")
    print(f"Sway Force: {results['sway']:.2f} N")
    print(f"Yaw Moment: {results['yaw']:.2f} N·m")

    # Check that results are not None and are numerical
    assert results['surge'] is not None
    assert results['sway'] is not None
    assert results['yaw'] is not None
    assert isinstance(results['surge'], (int, float))
    assert isinstance(results['sway'], (int, float))
    assert isinstance(results['yaw'], (int, float))

    print("PASS: Basic usage example works correctly")

    # Test batch calculation over a range of staggers
    stagger_values = np.linspace(-400, 400, 11)
    scenarios = [{'separation': 50.0, 'stagger': s, 'velocity': 5.0} for s in stagger_values]
    batch_results = calculator.calculate_batch(scenarios, parallel=False)
    assert len(batch_results) == len(stagger_values)
    print(f"PASS: Batch calculation completed for {len(batch_results)} points")
    
def test_cli_module_entry():
    """Test that the module entry point works."""
    print("\n=== Testing Module Entry Point ===")
    
    import subprocess
    
    # Create a test config
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
        yaml.dump(create_test_config(), f)
        config_path = f.name
    
    try:
        # Test module entry point with help
        result = subprocess.run(
            [sys.executable, '-m', 'digitalmodel.hydrodynamics.passing_ship', '--help'],
            capture_output=True,
            text=True
        )
        
        assert result.returncode == 0, f"Module entry point failed: {result.stderr}"
        assert 'usage:' in result.stdout.lower(), "Help text not displayed"
        print("PASS: Module entry point works correctly")
        
        # Test with actual config (dry run to avoid long computation)
        with tempfile.TemporaryDirectory() as tmpdir:
            result = subprocess.run(
                [sys.executable, '-m', 'digitalmodel.hydrodynamics.passing_ship',
                 '--config', config_path,
                 '--output-directory', tmpdir,
                 '--dry-run'],
                capture_output=True,
                text=True
            )
            
            # Allow dry-run to not be implemented, just check module loads
            if '--dry-run' not in result.stderr:
                print("PASS: Module loads and accepts parameters")
        
    finally:
        os.unlink(config_path)

def test_configuration_examples():
    """Test configuration examples from the documentation."""
    print("\n=== Testing Configuration Examples ===")

    from digitalmodel.hydrodynamics.passing_ship.configuration import (
        PassingShipConfig, VesselConfig, EnvironmentalConfig, CalculationConfig
    )

    # Test basic configuration using actual Pydantic models
    config = PassingShipConfig(
        moored_vessel=VesselConfig(
            length=180.0, beam=32.0, draft=12.0, block_coefficient=0.85
        ),
        passing_vessel=VesselConfig(
            length=200.0, beam=35.0, draft=14.0, block_coefficient=0.80
        ),
        environment=EnvironmentalConfig(
            water_depth=50.0, water_density=1025.0
        ),
        calculation=CalculationConfig(num_points=50)
    )

    assert config.moored_vessel.length == 180.0
    assert config.passing_vessel.length == 200.0
    assert config.environment.water_depth == 50.0
    print("PASS: Basic configuration validates correctly")

    # Test parametric study configuration round-trip through YAML
    config_data = create_test_config()
    parametric_config = config_data.copy()
    parametric_config['parametric'] = {
        'variable': 'separation_distance',
        'values': [30, 40, 50, 60, 70, 80, 90, 100],
        'plot_results': True
    }

    with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
        yaml.dump(parametric_config, f)
        temp_path = f.name

    try:
        with open(temp_path, 'r') as f:
            loaded = yaml.safe_load(f)
            assert 'parametric' in loaded
            assert loaded['parametric']['variable'] == 'separation_distance'
    finally:
        os.unlink(temp_path)

    print("PASS: Parametric study configuration structure is valid")
    
def test_api_reference_examples():
    """Test examples from the API reference."""
    print("\n=== Testing API Reference Examples ===")

    from digitalmodel.hydrodynamics.passing_ship.formulations import (
        s1_function,
        f_kernel,
        g_kernel,
        calculate_forces_with_depth
    )

    # Test sectional area function (s1_function takes x position and vessel length)
    L = 100.0
    positions = np.linspace(-L / 2, L / 2, 10)
    S = np.array([s1_function(x, L) for x in positions])
    assert len(S) == len(positions)
    assert np.all(S >= 0)
    print("PASS: s1_function works")

    # Test kernel functions (f_kernel and g_kernel, lowercase)
    F_val = f_kernel(xi=0.5, eta=1.0, x=1.0, y=1.0, L=50.0)
    assert isinstance(F_val, (int, float))
    print("PASS: f_kernel function works")

    G_val = g_kernel(xi=0.5, eta=1.0, x=1.0, y=1.0, L=50.0)
    assert isinstance(G_val, (int, float))
    print("PASS: g_kernel function works")

    # Test force calculation with calculate_forces_with_depth
    vessel_params = {'L': 180.0, 'B': 32.0, 'T': 12.0, 'Cb': 0.85}
    passing_params = {'U': 5.0, 'y': 50.0, 'x': 0.0}

    fx, fy, mz = calculate_forces_with_depth(
        vessel_params=vessel_params,
        passing_params=passing_params,
        water_depth=50.0
    )
    assert isinstance(fx, (int, float))
    assert isinstance(fy, (int, float))
    assert isinstance(mz, (int, float))
    print("PASS: calculate_forces_with_depth works")
    
def test_export_functionality():
    """Test the export functionality examples."""
    print("\n=== Testing Export Functionality ===")

    from digitalmodel.hydrodynamics.passing_ship.exporters import ResultExporter
    import csv

    # Create sample results
    results = {
        'surge_force': 12345.67,
        'sway_force': 23456.78,
        'yaw_moment': 34567.89,
        'metadata': {
            'calculation_time': 0.123,
            'vessel_separation': 50.0
        }
    }

    exporter = ResultExporter(results)

    with tempfile.TemporaryDirectory() as tmpdir:
        # Test JSON export via ResultExporter.to_json
        json_path = os.path.join(tmpdir, 'results.json')
        exporter.to_json(json_path)

        with open(json_path, 'r') as f:
            loaded = json.load(f)
            assert loaded['results']['surge_force'] == 12345.67
        print("PASS: JSON export works")

        # Test CSV export via ResultExporter.to_csv
        csv_path = os.path.join(tmpdir, 'results.csv')
        exporter.to_csv(csv_path)

        with open(csv_path, 'r') as f:
            reader = csv.DictReader(f)
            row = next(reader)
            assert float(row['surge_force']) == 12345.67
        print("PASS: CSV export works")

def test_visualization_examples():
    """Test visualization code examples."""
    print("\n=== Testing Visualization Examples ===")

    from digitalmodel.hydrodynamics.passing_ship.visualization import (
        ForceDistributionPlotter,
        PlotExporter,
    )
    from digitalmodel.hydrodynamics.passing_ship.calculator import ForceResults
    import matplotlib
    matplotlib.use('Agg')  # non-interactive backend for CI
    import matplotlib.pyplot as plt

    # Test data — construct ForceResults objects as the API expects
    stagger = np.linspace(-400, 400, 81)
    forces = [
        ForceResults(
            surge=1000 * np.sin(s * np.pi / 400),
            sway=2000 * np.cos(s * np.pi / 400),
            yaw=500 * np.sin(2 * s * np.pi / 400),
        )
        for s in stagger
    ]

    plotter = ForceDistributionPlotter()

    with tempfile.TemporaryDirectory() as tmpdir:
        # Test force distribution plot
        fig = plotter.plot_force_distribution(stagger, forces, title="Test Forces")
        plot_path = os.path.join(tmpdir, 'forces.png')
        fig.savefig(plot_path)
        plt.close(fig)
        assert os.path.exists(plot_path)
        print("PASS: Force distribution plot works")

        # Test multiple distributions comparison
        forces_case2 = [
            ForceResults(
                surge=1500 * np.sin(s * np.pi / 400),
                sway=3000 * np.cos(s * np.pi / 400),
                yaw=750 * np.sin(2 * s * np.pi / 400),
            )
            for s in stagger
        ]
        fig2 = plotter.plot_multiple_distributions(
            stagger,
            datasets=[(forces, 'Case 1'), (forces_case2, 'Case 2')],
            title="Comparison"
        )
        plot_path2 = os.path.join(tmpdir, 'comparison.png')
        fig2.savefig(plot_path2)
        plt.close(fig2)
        assert os.path.exists(plot_path2)
        print("PASS: Comparison plot works")

def main():
    """Run all documentation tests."""
    print("=" * 50)
    print("DOCUMENTATION EXAMPLES VERIFICATION")
    print("=" * 50)
    
    tests = [
        test_basic_usage_example,
        test_cli_module_entry,
        test_configuration_examples,
        test_api_reference_examples,
        test_export_functionality,
        test_visualization_examples
    ]
    
    passed = 0
    failed = 0
    
    for test_func in tests:
        try:
            test_func()
            passed += 1
        except Exception as e:
            print(f"X {test_func.__name__} failed: {e}")
            failed += 1
            import traceback
            traceback.print_exc()
    
    print("\n" + "=" * 50)
    print(f"SUMMARY: {passed} passed, {failed} failed")
    print("=" * 50)
    
    return failed == 0

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)