"""
End-to-end integration tests for complete marine engineering workflows.

Tests complete workflows from environmental definition through
mooring analysis and OrcaFlex export.

Test Scenarios:
1. Complete mooring analysis (waves + vessel + mooring)
2. Multi-directional environmental analysis
3. Time-domain simulation preparation
4. Design optimization workflow
5. Validation against industry standards
"""

import pytest
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import json
import tempfile

from digitalmodel.modules.marine_analysis.wave_spectra.spectra import (
    JONSWAPSpectrum,
    WaveSpectrumParameters
)
from digitalmodel.modules.marine_analysis.hydrodynamic_coefficients.coefficients import (
    CoefficientDatabase,
    DOF_NAMES
)
from digitalmodel.modules.marine_analysis.environmental_loading.ocimf import (
    OCIMFDatabase,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForces,
    create_sample_database
)
from digitalmodel.modules.marine_analysis.catenary.adapter import catenaryEquation, catenaryForces
import pandas as pd


class VesselDefinition:
    """Complete vessel definition for end-to-end workflow."""

    def __init__(self, name: str, vessel_type: str):
        self.name = name
        self.vessel_type = vessel_type

        # Geometry (VLCC example)
        self.geometry = VesselGeometry(
            loa=320.0,
            beam=58.0,
            draft=22.0
        )

        # Mass properties
        self.displacement = 300000  # tonnes
        self.mass = self.displacement * 1000  # kg

        # Natural periods (from hydro analysis)
        self.natural_periods = {
            'Surge': 120.0,
            'Sway': 100.0,
            'Heave': 12.0,
            'Roll': 25.0,
            'Pitch': 15.0,
            'Yaw': 90.0
        }

        # Damping ratios
        self.damping_ratios = {
            'Surge': 0.05,
            'Sway': 0.05,
            'Heave': 0.15,
            'Roll': 0.08,
            'Pitch': 0.08,
            'Yaw': 0.05
        }


class MooringSystem:
    """Complete mooring system definition."""

    def __init__(self, num_lines: int = 8):
        self.num_lines = num_lines

        # Line properties
        self.line_length = 500.0  # meters
        self.line_diameter = 0.12  # meters (120mm chain)
        self.line_weight_in_water = 800.0  # N/m

        # Fairlead positions (relative to vessel center)
        self.fairlead_radius = 150.0  # meters from center
        self.fairlead_depth = -15.0  # meters below waterline

        # Anchor positions
        self.anchor_radius = 400.0  # meters from center
        self.water_depth = 100.0  # meters

        # Line angles
        self.line_angles = np.linspace(0, 360, num_lines, endpoint=False)

    def calculate_line_tension(self, env_force_x: float, env_force_y: float):
        """Calculate tension in all mooring lines."""
        tensions = []

        for angle in self.line_angles:
            angle_rad = np.radians(angle)

            # Project environmental forces onto line direction
            force_on_line = (
                env_force_x * np.cos(angle_rad) +
                env_force_y * np.sin(angle_rad)
            )

            # Only tension (no compression)
            force_on_line = max(0, force_on_line / self.num_lines)

            # Add pretension and static weight
            pretension = 500e3  # 500 kN
            total_force = pretension + force_on_line

            tensions.append(total_force)

        return np.array(tensions)


class TestEndToEndWorkflow:
    """End-to-end integration workflow tests."""

    @pytest.fixture
    def output_dir(self):
        """Create output directory."""
        output_path = Path(__file__).parent / "charts" / "end_to_end"
        output_path.mkdir(parents=True, exist_ok=True)
        return output_path

    @pytest.fixture
    def vessel(self):
        """Create vessel definition."""
        return VesselDefinition("Test VLCC", "VLCC")

    @pytest.fixture
    def mooring_system(self):
        """Create mooring system."""
        return MooringSystem(num_lines=8)

    @pytest.fixture
    def environmental_database(self, tmp_path):
        """Create environmental database."""
        db_path = tmp_path / "ocimf_full.csv"
        create_sample_database(str(db_path), num_vessels=5,
                              num_headings=37, num_displacements=5)
        return OCIMFDatabase(str(db_path))

    def test_complete_mooring_analysis_workflow(self, vessel, mooring_system,
                                                environmental_database, output_dir):
        """Test complete mooring analysis from environment to tensions."""
        # Step 1: Define environmental conditions
        wave_params = WaveSpectrumParameters(
            Hs=5.0,  # 5m significant wave height
            Tp=10.0,  # 10s peak period
            gamma=3.3,
            freq_range=(0.01, 1.0),
            n_frequencies=100
        )
        wave_spectrum = JONSWAPSpectrum(wave_params)

        env_conditions = EnvironmentalConditions(
            wind_speed=20.0,
            wind_direction=45.0,
            current_speed=1.5,
            current_direction=30.0
        )

        # Step 2: Calculate wave statistics
        wave_stats = wave_spectrum.get_spectral_statistics()

        # Step 3: Calculate environmental forces
        env_forces = EnvironmentalForces(environmental_database)
        force_results = env_forces.calculate_total_forces(
            env_conditions, vessel.geometry, vessel.displacement
        )

        # Step 4: Calculate mooring tensions
        line_tensions = mooring_system.calculate_line_tension(
            force_results.total_fx,
            force_results.total_fy
        )

        # Step 5: Validate results
        assert np.all(line_tensions > 0), "All line tensions should be positive"
        max_tension = np.max(line_tensions)
        min_tension = np.min(line_tensions)

        # Variation should be reasonable (not all lines equally loaded)
        tension_variation = (max_tension - min_tension) / np.mean(line_tensions)
        assert tension_variation > 0.1, "Should have tension variation between lines"

        # Step 6: Create comprehensive summary report
        workflow_results = {
            'vessel': {
                'name': vessel.name,
                'type': vessel.vessel_type,
                'displacement': vessel.displacement,
                'loa': vessel.geometry.loa,
                'beam': vessel.geometry.beam,
                'draft': vessel.geometry.draft
            },
            'environment': {
                'Hs': wave_stats['Hs'],
                'Tp': wave_params.Tp,
                'Tz': wave_stats['Tz'],
                'wind_speed': env_conditions.wind_speed,
                'current_speed': env_conditions.current_speed
            },
            'forces': {
                'wind_fx_MN': force_results.wind_fx / 1e6,
                'wind_fy_MN': force_results.wind_fy / 1e6,
                'current_fx_MN': force_results.current_fx / 1e6,
                'current_fy_MN': force_results.current_fy / 1e6,
                'total_fx_MN': force_results.total_fx / 1e6,
                'total_fy_MN': force_results.total_fy / 1e6
            },
            'mooring': {
                'num_lines': mooring_system.num_lines,
                'max_tension_MN': max_tension / 1e6,
                'min_tension_MN': min_tension / 1e6,
                'mean_tension_MN': np.mean(line_tensions) / 1e6,
                'line_tensions_MN': (line_tensions / 1e6).tolist()
            }
        }

        # Save JSON report
        with open(output_dir / "workflow_results.json", 'w') as f:
            json.dump(workflow_results, f, indent=2)

        # Create visualization
        self._create_workflow_summary_chart(
            wave_spectrum, force_results, line_tensions,
            mooring_system, output_dir
        )

    def test_multi_directional_environmental_analysis(self, vessel, mooring_system,
                                                      environmental_database, output_dir):
        """Test analysis across multiple environmental directions."""
        directions = [0, 45, 90, 135, 180, 225, 270, 315]  # 8 directions

        results_by_direction = []

        for direction in directions:
            # Environment for this direction
            env_conditions = EnvironmentalConditions(
                wind_speed=20.0,
                wind_direction=direction,
                current_speed=1.5,
                current_direction=direction
            )

            # Calculate forces
            env_forces = EnvironmentalForces(environmental_database)
            force_results = env_forces.calculate_total_forces(
                env_conditions, vessel.geometry, vessel.displacement
            )

            # Calculate mooring tensions
            line_tensions = mooring_system.calculate_line_tension(
                force_results.total_fx,
                force_results.total_fy
            )

            results_by_direction.append({
                'direction': direction,
                'total_force': np.sqrt(force_results.total_fx**2 +
                                      force_results.total_fy**2) / 1e6,
                'max_tension': np.max(line_tensions) / 1e6,
                'mean_tension': np.mean(line_tensions) / 1e6
            })

        # Create polar plot of results
        fig = plt.figure(figsize=(14, 6))

        # Total force polar
        ax1 = fig.add_subplot(121, projection='polar')
        theta = np.radians([r['direction'] for r in results_by_direction])
        forces = [r['total_force'] for r in results_by_direction]

        ax1.plot(theta, forces, 'o-', linewidth=2, markersize=8)
        ax1.fill(theta, forces, alpha=0.25)
        ax1.set_theta_zero_location('N')
        ax1.set_theta_direction(-1)
        ax1.set_title('Total Environmental Force (MN)', pad=20)
        ax1.grid(True)

        # Max tension polar
        ax2 = fig.add_subplot(122, projection='polar')
        tensions = [r['max_tension'] for r in results_by_direction]

        ax2.plot(theta, tensions, 'o-', linewidth=2, markersize=8, color='red')
        ax2.fill(theta, tensions, alpha=0.25, color='red')
        ax2.set_theta_zero_location('N')
        ax2.set_theta_direction(-1)
        ax2.set_title('Maximum Line Tension (MN)', pad=20)
        ax2.grid(True)

        plt.tight_layout()
        plt.savefig(output_dir / "multi_directional_analysis.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_design_condition_envelope(self, vessel, mooring_system,
                                      environmental_database, output_dir):
        """Test design envelope across range of environmental conditions."""
        # Design condition matrix
        Hs_range = [3, 4, 5, 6, 7]  # meters
        wind_range = [15, 20, 25, 30]  # m/s
        heading_range = [0, 45, 90, 135, 180]  # degrees

        max_tensions = []
        condition_labels = []

        for Hs in Hs_range:
            for wind in wind_range:
                for heading in heading_range:
                    # Wave spectrum
                    Tp = 3.0 * np.sqrt(Hs)  # Empirical relationship
                    wave_params = WaveSpectrumParameters(
                        Hs=Hs, Tp=Tp, gamma=3.3,
                        freq_range=(0.01, 1.0), n_frequencies=50
                    )
                    wave_spectrum = JONSWAPSpectrum(wave_params)

                    # Environmental forces
                    env_conditions = EnvironmentalConditions(
                        wind_speed=wind,
                        wind_direction=heading,
                        current_speed=wind * 0.025,  # ~2.5% of wind
                        current_direction=heading
                    )

                    env_forces = EnvironmentalForces(environmental_database)
                    force_results = env_forces.calculate_total_forces(
                        env_conditions, vessel.geometry, vessel.displacement
                    )

                    # Mooring tensions
                    line_tensions = mooring_system.calculate_line_tension(
                        force_results.total_fx,
                        force_results.total_fy
                    )

                    max_tension = np.max(line_tensions) / 1e6  # MN
                    max_tensions.append(max_tension)
                    condition_labels.append(f"Hs{Hs}m_W{wind}ms_H{heading}°")

        # Find design condition (maximum)
        design_idx = np.argmax(max_tensions)
        design_tension = max_tensions[design_idx]
        design_label = condition_labels[design_idx]

        # Create summary
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(14, 10))

        # Top 20 worst cases
        sorted_indices = np.argsort(max_tensions)[::-1][:20]
        top_tensions = [max_tensions[i] for i in sorted_indices]
        top_labels = [condition_labels[i] for i in sorted_indices]

        ax1.barh(range(len(top_tensions)), top_tensions, color='darkred', alpha=0.7)
        ax1.set_yticks(range(len(top_tensions)))
        ax1.set_yticklabels(top_labels, fontsize=7)
        ax1.set_xlabel('Maximum Line Tension (MN)')
        ax1.set_title(f'Top 20 Design Conditions (Worst: {design_label})')
        ax1.grid(True, alpha=0.3, axis='x')

        # Histogram of all cases
        ax2.hist(max_tensions, bins=30, color='steelblue', alpha=0.7, edgecolor='black')
        ax2.axvline(design_tension, color='red', linestyle='--', linewidth=2,
                   label=f'Design: {design_tension:.2f} MN')
        ax2.set_xlabel('Maximum Line Tension (MN)')
        ax2.set_ylabel('Frequency')
        ax2.set_title('Distribution of Maximum Tensions Across Design Space')
        ax2.legend()
        ax2.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "design_envelope.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_orcaflex_export_preparation(self, vessel, mooring_system, output_dir):
        """Test preparation of data for OrcaFlex export."""
        # Prepare OrcaFlex-compatible data structure
        orcaflex_data = {
            'General': {
                'Density': 1025.0,  # kg/m³
                'KinematicViscosity': 1.35e-6,  # m²/s
                'WaterDepth': mooring_system.water_depth
            },
            'Vessel': {
                'Name': vessel.name,
                'Mass': vessel.mass,
                'Length': vessel.geometry.loa,
                'Beam': vessel.geometry.beam,
                'Draft': vessel.geometry.draft,
                'CentreOfMass': [0, 0, -10],  # m (approximate)
            },
            'Lines': []
        }

        # Add mooring lines
        for i, angle in enumerate(mooring_system.line_angles):
            angle_rad = np.radians(angle)

            # Fairlead position
            fairlead_x = mooring_system.fairlead_radius * np.cos(angle_rad)
            fairlead_y = mooring_system.fairlead_radius * np.sin(angle_rad)

            # Anchor position
            anchor_x = mooring_system.anchor_radius * np.cos(angle_rad)
            anchor_y = mooring_system.anchor_radius * np.sin(angle_rad)

            line_data = {
                'Name': f'Line{i+1}',
                'Length': mooring_system.line_length,
                'TargetSegmentLength': 10.0,  # m
                'Fairlead': [fairlead_x, fairlead_y, mooring_system.fairlead_depth],
                'Anchor': [anchor_x, anchor_y, -mooring_system.water_depth],
                'LineType': {
                    'Diameter': mooring_system.line_diameter,
                    'MassPerUnitLength': mooring_system.line_weight_in_water / 9.81,
                    'EA': 1e9,  # N (axial stiffness)
                }
            }

            orcaflex_data['Lines'].append(line_data)

        # Save as JSON (can be converted to OrcaFlex format)
        output_file = output_dir / "orcaflex_model_data.json"
        with open(output_file, 'w') as f:
            json.dump(orcaflex_data, f, indent=2)

        # Validate export
        assert len(orcaflex_data['Lines']) == mooring_system.num_lines, \
            "Should export all mooring lines"

        # Create visualization of mooring layout
        fig, ax = plt.subplots(figsize=(10, 10))

        # Draw vessel
        vessel_circle = plt.Circle((0, 0), vessel.geometry.beam/2,
                                  color='blue', alpha=0.3, label='Vessel')
        ax.add_patch(vessel_circle)

        # Draw mooring lines
        for line in orcaflex_data['Lines']:
            fairlead = line['Fairlead']
            anchor = line['Anchor']

            ax.plot([fairlead[0], anchor[0]], [fairlead[1], anchor[1]],
                   'k-', linewidth=2, alpha=0.7)
            ax.plot(fairlead[0], fairlead[1], 'go', markersize=8)
            ax.plot(anchor[0], anchor[1], 'r^', markersize=10)

        ax.set_xlim(-mooring_system.anchor_radius * 1.2,
                   mooring_system.anchor_radius * 1.2)
        ax.set_ylim(-mooring_system.anchor_radius * 1.2,
                   mooring_system.anchor_radius * 1.2)
        ax.set_aspect('equal')
        ax.set_xlabel('X [m]')
        ax.set_ylabel('Y [m]')
        ax.set_title('Mooring System Layout for OrcaFlex')
        ax.legend(['Mooring Lines', 'Fairleads', 'Anchors', 'Vessel'])
        ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "mooring_layout.png", dpi=300, bbox_inches='tight')
        plt.close()

    def _create_workflow_summary_chart(self, wave_spectrum, force_results,
                                      line_tensions, mooring_system, output_dir):
        """Create comprehensive workflow summary chart."""
        fig = plt.figure(figsize=(16, 12))
        gs = fig.add_gridspec(3, 3, hspace=0.3, wspace=0.3)

        # 1. Wave spectrum
        ax1 = fig.add_subplot(gs[0, :2])
        S = wave_spectrum.compute_spectrum()
        frequencies = wave_spectrum.frequencies
        ax1.plot(frequencies, S, linewidth=2)
        ax1.fill_between(frequencies, 0, S, alpha=0.3)
        ax1.set_xlabel('Frequency [Hz]')
        ax1.set_ylabel('S(f) [m²·s]')
        ax1.set_title('Wave Spectrum')
        ax1.grid(True, alpha=0.3)

        # 2. Wave statistics
        ax2 = fig.add_subplot(gs[0, 2])
        stats = wave_spectrum.get_spectral_statistics()
        stats_text = f"Hs: {stats['Hs']:.2f} m\n"
        stats_text += f"Tp: {wave_spectrum.params.Tp:.2f} s\n"
        stats_text += f"Tz: {stats['Tz']:.2f} s\n"
        stats_text += f"BW: {stats['bandwidth']:.3f}"
        ax2.text(0.5, 0.5, stats_text, ha='center', va='center',
                fontsize=14, family='monospace',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        ax2.axis('off')
        ax2.set_title('Wave Statistics')

        # 3. Environmental forces
        ax3 = fig.add_subplot(gs[1, 0])
        force_components = ['Wind\nFx', 'Wind\nFy', 'Curr\nFx', 'Curr\nFy']
        force_values = [
            force_results.wind_fx / 1e6,
            force_results.wind_fy / 1e6,
            force_results.current_fx / 1e6,
            force_results.current_fy / 1e6
        ]
        ax3.bar(range(len(force_components)), force_values,
               color=['blue', 'lightblue', 'green', 'lightgreen'], alpha=0.7)
        ax3.set_xticks(range(len(force_components)))
        ax3.set_xticklabels(force_components, fontsize=9)
        ax3.set_ylabel('Force (MN)')
        ax3.set_title('Environmental Forces')
        ax3.grid(True, alpha=0.3, axis='y')

        # 4. Force vectors
        ax4 = fig.add_subplot(gs[1, 1])
        ax4.arrow(0, 0, force_results.wind_fx/1e6, force_results.wind_fy/1e6,
                 head_width=0.3, head_length=0.4, fc='blue', ec='blue',
                 alpha=0.7, width=0.1, label='Wind')
        ax4.arrow(0, 0, force_results.current_fx/1e6, force_results.current_fy/1e6,
                 head_width=0.3, head_length=0.4, fc='green', ec='green',
                 alpha=0.7, width=0.1, label='Current')
        ax4.arrow(0, 0, force_results.total_fx/1e6, force_results.total_fy/1e6,
                 head_width=0.4, head_length=0.5, fc='red', ec='red',
                 alpha=0.9, width=0.15, label='Total')
        ax4.set_xlabel('Fx (MN)')
        ax4.set_ylabel('Fy (MN)')
        ax4.set_title('Force Vectors')
        ax4.legend(fontsize=8)
        ax4.grid(True, alpha=0.3)
        ax4.axis('equal')

        # 5. Mooring tensions (bar)
        ax5 = fig.add_subplot(gs[1, 2])
        ax5.bar(range(len(line_tensions)), line_tensions / 1e6,
               color='darkred', alpha=0.7)
        ax5.set_xlabel('Line Number')
        ax5.set_ylabel('Tension (MN)')
        ax5.set_title('Mooring Line Tensions')
        ax5.grid(True, alpha=0.3, axis='y')

        # 6. Mooring tensions (polar)
        ax6 = fig.add_subplot(gs[2, :2], projection='polar')
        theta = np.radians(mooring_system.line_angles)
        tensions_MN = line_tensions / 1e6
        ax6.plot(theta, tensions_MN, 'o-', linewidth=2, markersize=8, color='darkred')
        ax6.fill(theta, tensions_MN, alpha=0.25, color='red')
        ax6.set_theta_zero_location('N')
        ax6.set_theta_direction(-1)
        ax6.set_title('Mooring Tension Distribution', pad=20)
        ax6.grid(True)

        # 7. Tension statistics
        ax7 = fig.add_subplot(gs[2, 2])
        tension_stats = f"Max: {np.max(tensions_MN):.2f} MN\n"
        tension_stats += f"Min: {np.min(tensions_MN):.2f} MN\n"
        tension_stats += f"Mean: {np.mean(tensions_MN):.2f} MN\n"
        tension_stats += f"Std: {np.std(tensions_MN):.2f} MN"
        ax7.text(0.5, 0.5, tension_stats, ha='center', va='center',
                fontsize=14, family='monospace',
                bbox=dict(boxstyle='round', facecolor='lightcoral', alpha=0.5))
        ax7.axis('off')
        ax7.set_title('Tension Statistics')

        plt.suptitle('End-to-End Workflow Summary', fontsize=16, y=0.995)
        plt.savefig(output_dir / "workflow_summary.png", dpi=300, bbox_inches='tight')
        plt.close()
