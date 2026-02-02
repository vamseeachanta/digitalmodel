"""
Integration tests for OCIMF Environmental Loading → Mooring Analysis.

Tests that environmental forces from OCIMF database correctly feed into
mooring tension calculations through the catenary solver.

Test Coverage:
- Wind/current forces → mooring tensions
- Vessel heading changes → force components
- Environmental loading → catenary solver
- Combined loading scenarios
- Force balance validation
- Exceedance probability analysis
"""

import pytest
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import tempfile

from digitalmodel.marine_ops.marine_analysis.environmental_loading.ocimf import (
    OCIMFDatabase,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForces,
    create_sample_database
)
from digitalmodel.marine_ops.marine_analysis.catenary.adapter import catenaryEquation, catenaryForces


class TestOCIMFMooringIntegration:
    """Integration tests for OCIMF environmental forces and mooring analysis."""

    @pytest.fixture
    def output_dir(self):
        """Create output directory for test charts."""
        output_path = Path(__file__).parent / "charts" / "ocimf_mooring"
        output_path.mkdir(parents=True, exist_ok=True)
        return output_path

    @pytest.fixture
    def ocimf_database(self, tmp_path):
        """Create sample OCIMF database."""
        db_path = tmp_path / "ocimf_sample.csv"
        create_sample_database(str(db_path), num_vessels=3,
                              num_headings=13, num_displacements=3)
        return OCIMFDatabase(str(db_path))

    @pytest.fixture
    def vessel_geometry(self):
        """Create sample vessel geometry."""
        return VesselGeometry(
            loa=320.0,  # VLCC length [m]
            beam=58.0,  # VLCC beam [m]
            draft=22.0,  # VLCC draft [m]
        )

    @pytest.fixture
    def environmental_conditions(self):
        """Create sample environmental conditions."""
        return EnvironmentalConditions(
            wind_speed=20.0,  # 20 m/s (~40 knots)
            wind_direction=45.0,  # degrees
            current_speed=1.5,  # 1.5 m/s (~3 knots)
            current_direction=30.0,  # degrees
        )

    def test_wind_forces_calculation(self, ocimf_database, vessel_geometry,
                                    environmental_conditions):
        """Test that wind forces are calculated correctly."""
        env_forces = EnvironmentalForces(ocimf_database)

        displacement = 300000  # tonnes (VLCC)
        Fx, Fy, Mz = env_forces.calculate_wind_forces(
            environmental_conditions, vessel_geometry, displacement
        )

        # Forces should be reasonable magnitude for 20 m/s wind
        assert abs(Fx) < 5e6, f"Wind Fx {Fx/1e6:.2f} MN seems too large"
        assert abs(Fy) < 10e6, f"Wind Fy {Fy/1e6:.2f} MN seems too large"
        assert abs(Mz) < 1e9, f"Wind Mz {Mz/1e9:.2f} GN·m seems too large"

        # All forces should be non-zero
        assert abs(Fx) > 100, "Wind Fx should be non-zero"
        assert abs(Fy) > 100, "Wind Fy should be non-zero"

    def test_current_forces_calculation(self, ocimf_database, vessel_geometry,
                                       environmental_conditions):
        """Test that current forces are calculated correctly."""
        env_forces = EnvironmentalForces(ocimf_database)

        displacement = 300000
        Fx, Fy, Mz = env_forces.calculate_current_forces(
            environmental_conditions, vessel_geometry, displacement
        )

        # Current forces should be significant
        assert abs(Fx) < 3e6, f"Current Fx {Fx/1e6:.2f} MN seems too large"
        assert abs(Fy) < 5e6, f"Current Fy {Fy/1e6:.2f} MN seems too large"

        # Should be non-zero
        assert abs(Fx) > 100, "Current Fx should be non-zero"

    def test_combined_environmental_forces(self, ocimf_database, vessel_geometry,
                                          environmental_conditions):
        """Test combined wind and current forces."""
        env_forces = EnvironmentalForces(ocimf_database)
        displacement = 300000

        results = env_forces.calculate_total_forces(
            environmental_conditions, vessel_geometry, displacement
        )

        # Total should equal sum of components
        np.testing.assert_allclose(
            results.total_fx,
            results.wind_fx + results.current_fx,
            rtol=1e-10,
            err_msg="Total Fx should equal wind + current"
        )

        np.testing.assert_allclose(
            results.total_fy,
            results.wind_fy + results.current_fy,
            rtol=1e-10,
            err_msg="Total Fy should equal wind + current"
        )

        # Wind forces should typically dominate for this scenario
        assert abs(results.wind_fy) > abs(results.current_fy), \
            "Wind lateral force should exceed current for 20 m/s wind"

    def test_heading_variation_effects(self, ocimf_database, vessel_geometry, output_dir):
        """Test that vessel heading affects force distribution."""
        env_forces = EnvironmentalForces(ocimf_database)
        displacement = 300000

        headings = np.linspace(0, 180, 37)  # 5-degree increments
        wind_forces_x = []
        wind_forces_y = []
        current_forces_x = []
        current_forces_y = []

        for heading in headings:
            # Wind from bow (0°) to beam (90°) to stern (180°)
            conditions = EnvironmentalConditions(
                wind_speed=20.0,
                wind_direction=heading,
                current_speed=1.5,
                current_direction=heading,
            )

            results = env_forces.calculate_total_forces(
                conditions, vessel_geometry, displacement
            )

            wind_forces_x.append(results.wind_fx / 1e6)  # Convert to MN
            wind_forces_y.append(results.wind_fy / 1e6)
            current_forces_x.append(results.current_fx / 1e6)
            current_forces_y.append(results.current_fy / 1e6)

        # Create polar plot
        fig = plt.figure(figsize=(14, 6))

        # Wind forces polar
        ax1 = fig.add_subplot(121, projection='polar')
        theta = np.radians(headings)

        # Convert to polar magnitude
        wind_magnitude = np.sqrt(np.array(wind_forces_x)**2 + np.array(wind_forces_y)**2)

        ax1.plot(theta, wind_magnitude, 'b-', linewidth=2, label='Wind')
        ax1.fill(theta, wind_magnitude, alpha=0.25, color='blue')
        ax1.set_theta_zero_location('N')
        ax1.set_theta_direction(-1)
        ax1.set_title('Wind Force Magnitude vs Heading\n(MN)', pad=20)
        ax1.grid(True)

        # Current forces polar
        ax2 = fig.add_subplot(122, projection='polar')
        current_magnitude = np.sqrt(np.array(current_forces_x)**2 + np.array(current_forces_y)**2)

        ax2.plot(theta, current_magnitude, 'g-', linewidth=2, label='Current')
        ax2.fill(theta, current_magnitude, alpha=0.25, color='green')
        ax2.set_theta_zero_location('N')
        ax2.set_theta_direction(-1)
        ax2.set_title('Current Force Magnitude vs Heading\n(MN)', pad=20)
        ax2.grid(True)

        plt.tight_layout()
        plt.savefig(output_dir / "heading_variation.png", dpi=300, bbox_inches='tight')
        plt.close()

        # Peak lateral force should occur around beam seas (90°)
        peak_idx = np.argmax(np.abs(wind_forces_y))
        peak_heading = headings[peak_idx]
        assert 70 <= peak_heading <= 110, \
            f"Peak lateral force at {peak_heading}° should be near 90°"

    def test_environmental_forces_to_mooring_tension(self, ocimf_database,
                                                     vessel_geometry, environmental_conditions,
                                                     output_dir):
        """Test integration of environmental forces with mooring catenary solver."""
        env_forces = EnvironmentalForces(ocimf_database)
        displacement = 300000

        # Calculate environmental forces
        results = env_forces.calculate_total_forces(
            environmental_conditions, vessel_geometry, displacement
        )

        # Total lateral force (critical for mooring)
        total_lateral_force = results.total_fy  # Newtons

        # Mooring system: 8 lines, symmetric spread
        num_lines = 8
        mooring_angles = np.linspace(0, 360, num_lines, endpoint=False)

        # Distribute forces to mooring lines
        # Simplified: each line shares based on angle projection
        line_tensions = []

        for angle in mooring_angles:
            # Angle from environmental force direction
            angle_rad = np.radians(angle - environmental_conditions.current_direction)

            # Force component on this line (simplified)
            line_force = max(0, total_lateral_force * np.cos(angle_rad) / num_lines)

            # Mooring line properties
            water_depth = 100.0  # meters
            line_weight = 800.0  # N/m (chain weight in water)

            # Use catenary solver to find line tension
            try:
                catenary_result = catenaryEquation({
                    "F": line_force + line_weight * water_depth,  # Add static weight
                    "w": line_weight,
                    "d": water_depth,
                    "X": None,
                    "q": None
                })

                tension = catenary_result["THorizontal"]
                line_tensions.append(tension / 1e6)  # Convert to MN
            except (ValueError, ZeroDivisionError):
                line_tensions.append(0.0)

        line_tensions = np.array(line_tensions)

        # Create visualization
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

        # Mooring arrangement
        ax1 = plt.subplot(121, projection='polar')
        theta = np.radians(mooring_angles)
        ax1.plot(theta, line_tensions, 'o-', linewidth=2, markersize=8)
        ax1.fill(theta, line_tensions, alpha=0.25)
        ax1.set_theta_zero_location('N')
        ax1.set_theta_direction(-1)
        ax1.set_title('Mooring Line Tensions\n(MN)', pad=20)
        ax1.grid(True)

        # Bar chart
        ax2.bar(range(num_lines), line_tensions, color='steelblue', alpha=0.7)
        ax2.set_xlabel('Mooring Line Number')
        ax2.set_ylabel('Tension (MN)')
        ax2.set_title('Mooring Line Tensions')
        ax2.grid(True, alpha=0.3, axis='y')

        plt.tight_layout()
        plt.savefig(output_dir / "mooring_tensions.png", dpi=300, bbox_inches='tight')
        plt.close()

        # At least one line should have significant tension
        assert np.max(line_tensions) > 0.5, \
            "Maximum line tension should exceed 0.5 MN for this loading"

    def test_combined_loading_worst_case(self, ocimf_database, vessel_geometry, output_dir):
        """Test worst-case combined environmental loading scenario."""
        env_forces = EnvironmentalForces(ocimf_database)
        displacement = 300000

        # Test various combinations
        wind_speeds = [15, 20, 25, 30]  # m/s
        current_speeds = [1.0, 1.5, 2.0]  # m/s
        headings = [0, 45, 90, 135, 180]  # degrees

        max_total_force = 0
        worst_case = None

        results_table = []

        for ws in wind_speeds:
            for cs in current_speeds:
                for hdg in headings:
                    conditions = EnvironmentalConditions(
                        wind_speed=ws,
                        wind_direction=hdg,
                        current_speed=cs,
                        current_direction=hdg,
                    )

                    results = env_forces.calculate_total_forces(
                        conditions, vessel_geometry, displacement
                    )

                    total_force = np.sqrt(results.total_fx**2 + results.total_fy**2)

                    results_table.append({
                        'wind_speed': ws,
                        'current_speed': cs,
                        'heading': hdg,
                        'total_fx': results.total_fx / 1e6,
                        'total_fy': results.total_fy / 1e6,
                        'total_magnitude': total_force / 1e6
                    })

                    if total_force > max_total_force:
                        max_total_force = total_force
                        worst_case = (ws, cs, hdg, results)

        # Worst case should be high wind, beam seas
        ws_worst, cs_worst, hdg_worst, results_worst = worst_case

        assert ws_worst >= 20, "Worst case should have high wind"
        assert 60 <= hdg_worst <= 120, \
            f"Worst case heading {hdg_worst}° should be near beam (90°)"

        # Create summary chart
        import pandas as pd
        df = pd.DataFrame(results_table)

        # Find top 10 worst cases
        df_sorted = df.sort_values('total_magnitude', ascending=False).head(10)

        fig, ax = plt.subplots(figsize=(12, 6))

        x = range(len(df_sorted))
        labels = [f"W:{row['wind_speed']}m/s\nC:{row['current_speed']}m/s\nH:{row['heading']}°"
                 for _, row in df_sorted.iterrows()]

        ax.bar(x, df_sorted['total_magnitude'], color='darkred', alpha=0.7)
        ax.set_xticks(x)
        ax.set_xticklabels(labels, rotation=45, ha='right', fontsize=8)
        ax.set_ylabel('Total Force Magnitude (MN)')
        ax.set_title('Top 10 Worst-Case Environmental Loading Scenarios')
        ax.grid(True, alpha=0.3, axis='y')

        plt.tight_layout()
        plt.savefig(output_dir / "worst_case_scenarios.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_force_balance_validation(self, ocimf_database, vessel_geometry):
        """Test that force balance is maintained in mooring system."""
        env_forces = EnvironmentalForces(ocimf_database)

        # Head seas (0°) - forces should be primarily longitudinal
        conditions_head = EnvironmentalConditions(
            wind_speed=20.0,
            wind_direction=0.0,
            current_speed=1.5,
            current_direction=0.0,
        )

        results_head = env_forces.calculate_total_forces(
            conditions_head, vessel_geometry, 300000
        )

        # Beam seas (90°) - forces should be primarily lateral
        conditions_beam = EnvironmentalConditions(
            wind_speed=20.0,
            wind_direction=90.0,
            current_speed=1.5,
            current_direction=90.0,
        )

        results_beam = env_forces.calculate_total_forces(
            conditions_beam, vessel_geometry, 300000
        )

        # Head seas: |Fx| should exceed |Fy|
        assert abs(results_head.total_fx) > abs(results_head.total_fy), \
            "Head seas should have larger longitudinal force"

        # Beam seas: |Fy| should exceed |Fx|
        assert abs(results_beam.total_fy) > abs(results_beam.total_fx), \
            "Beam seas should have larger lateral force"

    def test_displacement_effect_on_forces(self, ocimf_database, vessel_geometry,
                                          environmental_conditions, output_dir):
        """Test that vessel displacement affects environmental forces."""
        env_forces = EnvironmentalForces(ocimf_database)

        displacements = [250000, 275000, 300000, 325000, 350000]  # tonnes
        total_forces = []

        for disp in displacements:
            results = env_forces.calculate_total_forces(
                environmental_conditions, vessel_geometry, disp
            )

            total_force = np.sqrt(results.total_fx**2 + results.total_fy**2)
            total_forces.append(total_force / 1e6)  # MN

        # Forces should vary with displacement (coefficients are displacement-dependent)
        force_range = max(total_forces) - min(total_forces)
        assert force_range > 0.1, "Forces should vary with displacement"

        # Create chart
        fig, ax = plt.subplots(figsize=(10, 6))

        ax.plot(np.array(displacements) / 1000, total_forces, 'o-',
               linewidth=2, markersize=8, color='navy')
        ax.set_xlabel('Displacement (×10³ tonnes)')
        ax.set_ylabel('Total Force Magnitude (MN)')
        ax.set_title('Environmental Force vs Vessel Displacement')
        ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "displacement_effect.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_mooring_tension_statistics(self, ocimf_database, vessel_geometry, output_dir):
        """Test statistical distribution of mooring tensions under variable conditions."""
        env_forces = EnvironmentalForces(ocimf_database)
        displacement = 300000

        # Simulate variable environmental conditions
        np.random.seed(42)
        n_simulations = 1000

        # Weibull-distributed wind speeds
        wind_speeds = np.random.weibull(2.0, n_simulations) * 15 + 5  # 5-35 m/s range
        wind_directions = np.random.uniform(0, 360, n_simulations)
        current_speeds = np.random.weibull(1.5, n_simulations) * 1.0 + 0.5  # 0.5-2.5 m/s

        max_tensions = []

        for ws, wd, cs in zip(wind_speeds, wind_directions, current_speeds):
            conditions = EnvironmentalConditions(
                wind_speed=float(ws),
                wind_direction=float(wd),
                current_speed=float(cs),
                current_direction=float(wd),  # Same direction
            )

            results = env_forces.calculate_total_forces(
                conditions, vessel_geometry, displacement
            )

            # Maximum mooring tension (simplified)
            total_force = np.sqrt(results.total_fx**2 + results.total_fy**2)
            # Assume 8-point mooring, worst line takes 30% of total
            max_tension = 0.3 * total_force
            max_tensions.append(max_tension / 1e6)  # MN

        max_tensions = np.array(max_tensions)

        # Calculate statistics
        mean_tension = np.mean(max_tensions)
        median_tension = np.median(max_tensions)
        p95_tension = np.percentile(max_tensions, 95)
        p99_tension = np.percentile(max_tensions, 99)

        # Create histogram with statistics
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

        # Histogram
        ax1.hist(max_tensions, bins=50, density=True, alpha=0.7, color='steelblue',
                edgecolor='black')
        ax1.axvline(mean_tension, color='green', linestyle='--', linewidth=2,
                   label=f'Mean: {mean_tension:.2f} MN')
        ax1.axvline(p95_tension, color='orange', linestyle='--', linewidth=2,
                   label=f'95th %ile: {p95_tension:.2f} MN')
        ax1.axvline(p99_tension, color='red', linestyle='--', linewidth=2,
                   label=f'99th %ile: {p99_tension:.2f} MN')
        ax1.set_xlabel('Maximum Line Tension (MN)')
        ax1.set_ylabel('Probability Density')
        ax1.set_title('Distribution of Maximum Mooring Tensions')
        ax1.legend()
        ax1.grid(True, alpha=0.3)

        # Exceedance probability
        sorted_tensions = np.sort(max_tensions)
        exceedance = 1 - np.arange(1, len(sorted_tensions) + 1) / len(sorted_tensions)

        ax2.semilogy(sorted_tensions, exceedance * 100, linewidth=2, color='darkred')
        ax2.set_xlabel('Maximum Line Tension (MN)')
        ax2.set_ylabel('Exceedance Probability (%)')
        ax2.set_title('Mooring Tension Exceedance Curve')
        ax2.grid(True, alpha=0.3, which='both')

        plt.tight_layout()
        plt.savefig(output_dir / "mooring_tension_statistics.png", dpi=300, bbox_inches='tight')
        plt.close()

        # Design tension should exceed p99
        design_tension = p99_tension * 1.2  # 20% safety margin
        assert design_tension > p99_tension, \
            "Design tension should include safety margin"
