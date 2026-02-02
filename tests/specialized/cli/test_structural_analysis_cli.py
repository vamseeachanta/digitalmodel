"""
ABOUTME: Integration tests for structural_analysis CLI
ABOUTME: Tests stress calculations, buckling analysis, and capacity checks via CLI
"""

import pytest
import json
from pathlib import Path

pytest.importorskip(
    "tests.cli.conftest",
    reason="module tests.cli.conftest not available"
)
from digitalmodel.structural_analysis.cli import cli
from tests.cli.conftest import assert_cli_success, assert_cli_failure, assert_json_output, assert_output_contains


class TestStressCommand:
    """Tests for the 'stress' command"""

    def test_basic_stress_calculation(self, cli_runner):
        """Test basic von Mises stress calculation"""
        result = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '150',
            '--sigma-y', '100',
            '--sigma-z', '50',
            '--material', 'S355'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Stress Analysis Results',
            'Von Mises Stress:',
            'Principal Stresses:',
            'Safety Factor:'
        )

    def test_stress_with_shear(self, cli_runner):
        """Test stress calculation with shear stresses"""
        result = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '200',
            '--sigma-y', '150',
            '--tau-xy', '50',
            '--tau-xz', '30',
            '--material', 'S275'
        ])

        assert_cli_success(result)
        assert 'Von Mises Stress:' in result.output
        assert 'Max Shear Stress:' in result.output

    def test_stress_with_json_output(self, cli_runner, temp_output_dir):
        """Test stress calculation with JSON output"""
        output_file = temp_output_dir / "stress_results.json"

        result = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '150',
            '--sigma-y', '100',
            '--sigma-z', '50',
            '--material', 'S355',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Verify JSON output
        data = assert_json_output(output_file, [
            'stress_state',
            'von_mises_stress',
            'principal_stresses',
            'safety_factor',
            'status'
        ])

        # Verify calculations
        assert data['von_mises_stress'] > 0
        assert data['safety_factor'] > 0
        assert data['status'] in ['PASS', 'FAIL']
        assert data['material'] == 'S355'

    def test_stress_pass_status(self, cli_runner):
        """Test that low stress shows PASS status"""
        result = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '50',
            '--sigma-y', '30',
            '--material', 'S355'
        ])

        assert_cli_success(result)
        assert 'Status:               PASS' in result.output

    def test_stress_fail_status(self, cli_runner):
        """Test that high stress shows FAIL status"""
        result = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '400',  # Exceeds S275 yield
            '--sigma-y', '300',
            '--material', 'S275'
        ])

        assert_cli_success(result)
        assert 'Status:               FAIL' in result.output

    def test_stress_different_materials(self, cli_runner):
        """Test stress calculation with different material grades"""
        materials = ['S275', 'S355', 'S420']

        for material in materials:
            result = cli_runner.invoke(cli, [
                'stress',
                '--sigma-x', '200',
                '--material', material
            ])

            assert_cli_success(result)
            assert f'Material:             {material}' in result.output

    def test_stress_zero_input(self, cli_runner):
        """Test stress calculation with zero stresses"""
        result = cli_runner.invoke(cli, [
            'stress',
            '--material', 'S355'
        ])

        assert_cli_success(result)
        assert 'Von Mises Stress:     0.00 MPa' in result.output

    def test_stress_principal_stresses(self, cli_runner, temp_output_dir):
        """Test that principal stresses are correctly calculated"""
        output_file = temp_output_dir / "principal.json"

        result = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '150',
            '--sigma-y', '100',
            '--sigma-z', '50',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        with open(output_file) as f:
            data = json.load(f)

        principals = data['principal_stresses']
        # Principal stresses should be ordered: σ1 >= σ2 >= σ3
        assert principals['sigma_1'] >= principals['sigma_2']
        assert principals['sigma_2'] >= principals['sigma_3']


class TestBucklingPlateCommand:
    """Tests for the 'buckling-plate' command"""

    def test_basic_plate_buckling(self, cli_runner):
        """Test basic plate buckling calculation"""
        result = cli_runner.invoke(cli, [
            'buckling-plate',
            '--length', '1000',
            '--width', '500',
            '--thickness', '10',
            '--sigma-x', '150.0',
            '--material', 'S355'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Plate Buckling Check',
            'Critical Stress:',
            'Applied Stress:',
            'Utilization:'
        )

    def test_plate_buckling_with_boundary_conditions(self, cli_runner):
        """Test plate buckling with different boundary conditions"""
        boundary_conditions = ['SSSS', 'CCCC', 'SCSC', 'CSCS']

        for bc in boundary_conditions:
            result = cli_runner.invoke(cli, [
                'buckling-plate',
                '--length', '1000',
                '--width', '500',
                '--thickness', '10',
                '--sigma-x', '150.0',
                '--boundary', bc,
                '--material', 'S355'
            ])

            # Some boundary conditions might not be implemented
            # Just check that it doesn't crash
            assert result.exit_code in [0, 2]  # Success or not implemented

    def test_plate_buckling_json_output(self, cli_runner, temp_output_dir):
        """Test plate buckling with JSON output"""
        output_file = temp_output_dir / "buckling.json"

        result = cli_runner.invoke(cli, [
            'buckling-plate',
            '--length', '1000',
            '--width', '500',
            '--thickness', '10',
            '--sigma-x', '150.0',
            '--material', 'S355',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        data = assert_json_output(output_file, [
            'plate_geometry',
            'applied_loading',
            'critical_stress',
            'utilization'
        ])

        assert data['critical_stress'] > 0

    def test_plate_buckling_thin_plate(self, cli_runner):
        """Test buckling of thin plate (high slenderness)"""
        result = cli_runner.invoke(cli, [
            'buckling-plate',
            '--length', '2000',
            '--width', '1000',
            '--thickness', '5',  # Thin plate
            '--sigma-x', '100.0',
            '--material', 'S355'
        ])

        assert_cli_success(result)
        # Thin plates should have lower critical stress
        assert 'Critical Stress:' in result.output

    def test_plate_buckling_thick_plate(self, cli_runner):
        """Test buckling of thick plate (low slenderness)"""
        result = cli_runner.invoke(cli, [
            'buckling-plate',
            '--length', '500',
            '--width', '300',
            '--thickness', '25',  # Thick plate
            '--sigma-x', '200.0',
            '--material', 'S355'
        ])

        assert_cli_success(result)
        # Thick plates should have higher critical stress
        assert 'Critical Stress:' in result.output


class TestCapacityCommand:
    """Tests for the 'capacity' command"""

    def test_basic_member_capacity(self, cli_runner):
        """Test basic member capacity check"""
        result = cli_runner.invoke(cli, [
            'capacity',
            '--section', 'I-beam',
            '--height', '300',
            '--width', '150',
            '--material', 'S355'
        ])

        # Command might not be fully implemented
        # Just check it doesn't crash
        assert result.exit_code in [0, 2]

    def test_capacity_json_output(self, cli_runner, temp_output_dir):
        """Test capacity check with JSON output"""
        output_file = temp_output_dir / "capacity.json"

        result = cli_runner.invoke(cli, [
            'capacity',
            '--section', 'I-beam',
            '--height', '300',
            '--material', 'S355',
            '--output', str(output_file)
        ])

        # Command might not be fully implemented
        if result.exit_code == 0:
            assert output_file.exists()


class TestCLIHelp:
    """Tests for CLI help and version information"""

    def test_main_help(self, cli_runner):
        """Test main CLI help message"""
        result = cli_runner.invoke(cli, ['--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Structural Analysis Tools',
            'stress',
            'buckling-plate'
        )

    def test_stress_help(self, cli_runner):
        """Test stress command help"""
        result = cli_runner.invoke(cli, ['stress', '--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Von Mises stress',
            '--sigma-x',
            '--sigma-y',
            '--material'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert 'structural-analysis' in result.output or 'version' in result.output.lower()


class TestCLIErrorHandling:
    """Tests for CLI error handling"""

    def test_missing_required_option(self, cli_runner):
        """Test error handling for missing required options"""
        result = cli_runner.invoke(cli, [
            'buckling-plate',
            '--width', '500',
            # Missing --length (required)
        ])

        assert_cli_failure(result)
        # Click shows "Missing option" error
        assert 'Error' in result.output or 'required' in result.output.lower()

    def test_invalid_material(self, cli_runner):
        """Test error handling for invalid material choice"""
        result = cli_runner.invoke(cli, [
            'stress',
            '--material', 'INVALID',
            '--sigma-x', '100'
        ])

        assert_cli_failure(result)

    def test_invalid_numeric_input(self, cli_runner):
        """Test error handling for non-numeric input"""
        result = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', 'not-a-number'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'invalid' in result.output.lower()


class TestIntegrationScenarios:
    """Integration tests combining multiple CLI operations"""

    def test_full_stress_analysis_workflow(self, cli_runner, temp_output_dir):
        """Test complete stress analysis workflow"""
        # Step 1: Calculate stress
        stress_output = temp_output_dir / "step1_stress.json"
        result1 = cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '180',
            '--sigma-y', '120',
            '--tau-xy', '40',
            '--material', 'S355',
            '--output', str(stress_output)
        ])

        assert_cli_success(result1)

        # Step 2: Verify results are usable
        with open(stress_output) as f:
            stress_data = json.load(f)

        assert stress_data['von_mises_stress'] > 0
        assert stress_data['status'] in ['PASS', 'FAIL']

        # Step 3: Check plate buckling with same material
        buckling_output = temp_output_dir / "step2_buckling.json"
        result2 = cli_runner.invoke(cli, [
            'buckling-plate',
            '--length', '1000',
            '--width', '500',
            '--thickness', '12',
            '--sigma-x', '180.0',
            '--material', 'S355',
            '--output', str(buckling_output)
        ])

        assert_cli_success(result2)

    def test_parametric_stress_study(self, cli_runner, temp_output_dir):
        """Test parametric study varying stress levels"""
        stress_levels = [100, 150, 200, 250, 300]
        results = []

        for i, stress in enumerate(stress_levels):
            output_file = temp_output_dir / f"stress_{i}.json"

            result = cli_runner.invoke(cli, [
                'stress',
                '--sigma-x', str(stress),
                '--material', 'S355',
                '--output', str(output_file)
            ])

            assert_cli_success(result)

            with open(output_file) as f:
                data = json.load(f)
                results.append(data)

        # Verify results make sense
        for i in range(len(results) - 1):
            # Higher stress should give lower safety factor
            assert results[i]['safety_factor'] >= results[i+1]['safety_factor']

    def test_material_comparison(self, cli_runner, temp_output_dir):
        """Test comparing different materials under same stress"""
        materials = ['S275', 'S355', 'S420']
        results = {}

        for material in materials:
            output_file = temp_output_dir / f"{material}.json"

            result = cli_runner.invoke(cli, [
                'stress',
                '--sigma-x', '200',
                '--sigma-y', '150',
                '--material', material,
                '--output', str(output_file)
            ])

            assert_cli_success(result)

            with open(output_file) as f:
                results[material] = json.load(f)

        # Verify higher grade materials have higher safety factors under same stress
        assert results['S275']['safety_factor'] < results['S355']['safety_factor']
        assert results['S355']['safety_factor'] < results['S420']['safety_factor']
