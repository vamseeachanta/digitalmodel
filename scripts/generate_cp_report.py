#!/usr/bin/env python3
"""
ABOUTME: Generate interactive HTML comparison reports for cathodic protection analysis
ABOUTME: Compares DNV RP-F103:2010 vs 2016 methodologies with Plotly visualizations
"""

import sys
from pathlib import Path
import json
import copy

# Add src to path
src_path = Path(__file__).parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.common.cathodic_protection import CathodicProtection
from digitalmodel.reporting.cp_html_report import CPHTMLReportGenerator


def create_saipem_config():
    """
    Create Saipem test configuration for CP analysis.

    Returns:
        Configuration dictionary with pipeline, coating, environment, and anode parameters
    """
    return {
        "inputs": {
            "pipeline": {
                "outer_diameter_m": 0.610,
                "wall_thickness_m": 0.025,
                "length_m": 10000.0,
                "coating_initial_breakdown_pct": 0.005,
                "coating_yearly_breakdown_pct": 0.002,
                "coating_quality": "good",
                "wet_storage_years": 2.0,
                "resistivity_ohm_m": 2e-7,
            },
            "coating": {
                "resistance_ohm_m2": 1.0,  # Good quality coating resistivity (Ω·m)
            },
            "environment": {
                "seawater_resistivity_ohm_cm": 25.0,
                "seawater_temperature_C": 10.0,
                "free_corrosion_potential_V": -0.630,
                "anode_potential_V": -0.950,
            },
            "design_data": {
                "design_life": 25.0,
            },
            "design": {
                "design_margin": 1.15,
            },
            "anode": {
                "material": "aluminium",
                "individual_anode_mass_kg": 400.0,
                "utilization_factor": 0.85,
                "contingency_factor": 1.10,
            },
        }
    }


def create_deepwater_36inch_config():
    """
    Create deepwater 36-inch pipeline configuration for CP analysis.

    Deepwater characteristics:
    - Larger diameter: 36" (0.914m) vs 24" (0.610m)
    - Longer pipeline: 25 km vs 10 km
    - Colder temperature: 4°C (deepwater environment)
    - Higher design margin: 1.20 for deepwater risk
    - Longer design life: 30 years
    - Larger anodes: 500 kg vs 400 kg
    - Longer wet storage: 3 years (typical for deepwater projects)

    Returns:
        Configuration dictionary for deepwater 36-inch pipeline
    """
    return {
        "inputs": {
            "pipeline": {
                "outer_diameter_m": 0.914,  # 36 inches
                "wall_thickness_m": 0.032,  # Thicker for deepwater pressure
                "length_m": 25000.0,  # 25 km deepwater pipeline
                "coating_initial_breakdown_pct": 0.003,  # Better installation control
                "coating_yearly_breakdown_pct": 0.0015,  # Lower degradation in deepwater
                "coating_quality": "good",  # Essential for deepwater
                "wet_storage_years": 3.0,  # Longer wet storage for deepwater projects
                "resistivity_ohm_m": 2e-7,
            },
            "coating": {
                "resistance_ohm_m2": 1.0,  # Good quality coating resistivity (Ω·m)
            },
            "environment": {
                "seawater_resistivity_ohm_cm": 20.0,  # Slightly lower in deepwater
                "seawater_temperature_C": 4.0,  # Cold deepwater temperature
                "free_corrosion_potential_V": -0.630,
                "anode_potential_V": -0.950,
            },
            "design_data": {
                "design_life": 30.0,  # Longer design life for deepwater investment
            },
            "design": {
                "design_margin": 1.20,  # Higher margin for deepwater risk
            },
            "anode": {
                "material": "aluminium",
                "individual_anode_mass_kg": 500.0,  # Larger anodes for deepwater
                "utilization_factor": 0.80,  # More conservative for deepwater
                "contingency_factor": 1.10,
            },
        }
    }


def main():
    """
    Generate CP comparison report: DNV 2010 vs 2016.

    Workflow:
        1. Initialize CP calculator
        2. Run DNV 2010 calculation (standard)
        3. Run DNV 2016 calculation (enhanced Saipem with wet storage)
        4. Generate interactive HTML comparison report
        5. Export CSV summary data
    """
    print("=" * 80)
    print("Cathodic Protection Analysis Report Generator")
    print("DNV RP-F103: 2010 vs 2016 - Deepwater 36-inch Pipeline Comparison")
    print("=" * 80)
    print()

    # Initialize CP calculator
    cp = CathodicProtection()

    # Create base configuration
    config = create_deepwater_36inch_config()

    # Run DNV 2010 calculation (without wet storage enhancement)
    print("Running DNV RP-F103:2010 calculation...")
    config_2010 = copy.deepcopy(config)  # Deep copy to avoid mutating original config
    config_2010['inputs']['pipeline']['wet_storage_years'] = 0.0  # No wet storage in 2010
    results_standard = cp.DNV_RP_F103_2010(config_2010)
    print("✓ DNV 2010 calculation complete")

    # Run DNV 2016 calculation (with wet storage enhancement - Phase 1 feature)
    print("Running DNV RP-F103:2016 (Deepwater 36-inch) calculation...")
    results_enhanced = cp.DNV_RP_F103_2010(config)  # Uses wet_storage_years=3.0
    print("✓ DNV 2016 calculation complete")

    # Generate HTML comparison report
    print("\nGenerating interactive HTML report...")
    generator = CPHTMLReportGenerator(output_dir="src/reports/cp")

    report_path = generator.generate_comparison_report(
        results_2010=results_standard,
        results_2016=results_enhanced,
        title="Cathodic Protection Analysis: DNV RP-F103 2010 vs 2016 - Deepwater 36-inch Pipeline"
    )

    print(f"✓ Report generated: {report_path}")
    print(f"  CSV data exported to: {generator.data_dir}/")
    print()

    # Print key comparison metrics
    print("Key Metrics Comparison:")
    print("-" * 80)

    # Access nested results structure - FIXED to handle DNV method's nested format
    attn_std = results_standard.get('results', {}).get('anode_spacing_m', {}).get('spacing_m', 0)
    attn_enh = results_enhanced.get('results', {}).get('anode_spacing_m', {}).get('spacing_m', 0)

    print(f"Attenuation Length (approx from anode spacing):")
    print(f"  DNV 2010:           {attn_std:.2f} m")
    print(f"  DNV 2016 (Enhanced): {attn_enh:.2f} m")
    if attn_std > 0:
        print(f"  Difference:         {attn_enh - attn_std:.2f} m ({(attn_enh/attn_std - 1)*100:.1f}%)")
    else:
        print(f"  Difference:         {attn_enh - attn_std:.2f} m (N/A)")

    # Access nested coating breakdown factors - FIXED
    coating_std = results_standard.get('results', {}).get('coating_breakdown_factors', {}).get('final_factor', 0)
    coating_enh = results_enhanced.get('results', {}).get('coating_breakdown_factors', {}).get('final_factor', 0)

    print(f"\nCoating Breakdown Factor:")
    print(f"  Without wet storage: {coating_std:.6f}")
    print(f"  With wet storage:    {coating_enh:.6f}")
    if coating_std > 0:
        print(f"  Increase:            {(coating_enh - coating_std):.6f} ({(coating_enh/coating_std - 1)*100:.4f}%)")
    else:
        print(f"  Increase:            {(coating_enh - coating_std):.6f} (N/A)")

    # Print current demand metrics - NEWLY ADDED
    current_std = results_standard.get('results', {}).get('current_demand_A', {})
    current_enh = results_enhanced.get('results', {}).get('current_demand_A', {})

    print(f"\nCurrent Demand:")
    print(f"  DNV 2010 Initial:   {current_std.get('initial_current_demand_A', 0):.2f} A")
    print(f"  DNV 2010 Mean:      {current_std.get('mean_current_demand_A', 0):.2f} A")
    print(f"  DNV 2010 Final:     {current_std.get('final_current_demand_A', 0):.2f} A")
    print()
    print(f"  DNV 2016 Initial:   {current_enh.get('initial_current_demand_A', 0):.2f} A")
    print(f"  DNV 2016 Mean:      {current_enh.get('mean_current_demand_A', 0):.2f} A")
    print(f"  DNV 2016 Final:     {current_enh.get('final_current_demand_A', 0):.2f} A")

    if current_std.get('mean_current_demand_A', 0) > 0:
        mean_diff = current_enh.get('mean_current_demand_A', 0) - current_std.get('mean_current_demand_A', 0)
        mean_pct = (current_enh.get('mean_current_demand_A', 0) / current_std.get('mean_current_demand_A', 0) - 1) * 100
        print(f"\n  Mean Difference:    {mean_diff:.2f} A ({mean_pct:.2f}%)")

    # Print anode requirements - NEWLY ADDED
    anodes_std = results_standard.get('results', {}).get('anode_requirements', {})
    anodes_enh = results_enhanced.get('results', {}).get('anode_requirements', {})

    print(f"\nAnode Requirements:")
    print(f"  DNV 2010:")
    print(f"    Number of anodes:  {anodes_std.get('anode_count', 0)}")
    print(f"    Total mass:        {anodes_std.get('total_anode_mass_kg', 0):.0f} kg")
    print()
    print(f"  DNV 2016 (Enhanced):")
    print(f"    Number of anodes:  {anodes_enh.get('anode_count', 0)}")
    print(f"    Total mass:        {anodes_enh.get('total_anode_mass_kg', 0):.0f} kg")

    if anodes_std.get('anode_count', 0) > 0:
        count_diff = anodes_enh.get('anode_count', 0) - anodes_std.get('anode_count', 0)
        count_pct = (anodes_enh.get('anode_count', 0) / anodes_std.get('anode_count', 0) - 1) * 100
        print(f"\n  Anode Count Difference: {count_diff} anodes ({count_pct:.2f}%)")

    print()
    print("=" * 80)
    print("Report generation complete!")
    print(f"Open in browser: file://{report_path.absolute()}")
    print("=" * 80)


if __name__ == "__main__":
    main()
