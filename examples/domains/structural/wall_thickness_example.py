#!/usr/bin/env python3
# ABOUTME: Example demonstrating pipeline wall thickness design analysis
# ABOUTME: Shows single check, parametric sweep, phase analysis, and HTML reports
"""
Wall Thickness Design Analysis Example
=======================================

Demonstrates the pipeline wall thickness analysis modules:

1. Single design check   -- verify a specific pipe geometry against a design code
2. Parametric sweep      -- evaluate multiple wall thicknesses to find optimal design
3. Phase analysis        -- assess different lifecycle phases (installation, operation, etc.)
4. HTML report generation -- produce formatted reports for documentation
"""

from pathlib import Path

import numpy as np

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_parametric import (
    API_5L_GRADES,
    ParametricSweep,
    SweepConfig,
    generate_report,
)
from digitalmodel.structural.analysis.wall_thickness_phases import (
    PhaseAnalysisRunner,
    PipeDefinition,
    create_standard_phases,
    generate_phase_report,
)


def run_single_design_check():
    """Perform a single wall thickness design check.

    Pipe: 10.75" OD (0.27305 m), API 5L X65 grade
    Conditions: 20 MPa internal pressure, 5 MPa external pressure
    Code: DNV-ST-F101, Safety Class Medium
    """
    print("=" * 70)
    print("1. SINGLE DESIGN CHECK")
    print("=" * 70)

    grade_data = API_5L_GRADES["X65"]

    geom = PipeGeometry(
        outer_diameter=0.27305,
        wall_thickness=0.0214,
        corrosion_allowance=0.001,
    )
    mat = PipeMaterial(
        grade="X65",
        smys=grade_data["smys"],
        smts=grade_data["smts"],
    )
    loads = DesignLoads(
        internal_pressure=20e6,
        external_pressure=5e6,
    )
    factors = DesignFactors(safety_class=SafetyClass.MEDIUM)

    analyzer = WallThicknessAnalyzer(
        geom, mat, loads, factors, DesignCode.DNV_ST_F101
    )
    result = analyzer.perform_analysis()

    print(f"  Pipe OD:              {geom.outer_diameter * 1000:.1f} mm")
    print(f"  Wall thickness:       {geom.wall_thickness * 1000:.1f} mm")
    print(f"  Grade:                {mat.grade}")
    print(f"  Internal pressure:    {loads.internal_pressure / 1e6:.1f} MPa")
    print(f"  External pressure:    {loads.external_pressure / 1e6:.1f} MPa")
    print(f"  Design code:          DNV-ST-F101")
    print()
    print(f"  Safe:                 {result.is_safe}")
    print(f"  Governing check:     {result.governing_check}")
    print(f"  Governing util.:     {result.max_utilisation:.3f}")
    print()

    print("  Individual checks:")
    for name, util in result.checks.items():
        status = "PASS" if util <= 1.0 else "FAIL"
        print(f"    {name:<30s}  util={util:.3f}  [{status}]")

    print()
    return result


def run_parametric_sweep():
    """Sweep wall thickness from 10 mm to 40 mm in 7 steps.

    Identifies the minimum wall thickness that satisfies DNV-ST-F101
    for the given pipe and loading conditions.
    """
    print("=" * 70)
    print("2. PARAMETRIC SWEEP")
    print("=" * 70)

    wt_values = np.linspace(0.010, 0.040, 7).tolist()

    config = SweepConfig(
        wall_thicknesses=wt_values,
        outer_diameters=[0.27305],
        grades=["X65"],
        internal_pressures=[20e6],
        external_pressures=[5e6],
        code=DesignCode.DNV_ST_F101,
        safety_class=SafetyClass.MEDIUM,
    )

    sweep = ParametricSweep(config)
    df = sweep.run()

    print(f"  Sweep range:  {wt_values[0]*1000:.1f} mm  to  {wt_values[-1]*1000:.1f} mm")
    print(f"  Steps:        {len(wt_values)}")
    print()
    print(df.to_string(index=False))
    print()

    return df


def run_phase_analysis():
    """Analyse wall thickness across lifecycle phases.

    Phases are generated for a 500 m water depth pipeline with 20 MPa
    design pressure.  Both DNV-ST-F101 and API-RP-1111 codes are checked.
    """
    print("=" * 70)
    print("3. PHASE ANALYSIS")
    print("=" * 70)

    pipe_def = PipeDefinition(
        outer_diameter=0.27305,
        wall_thickness=0.0214,
        grade="X65",
        smys=API_5L_GRADES["X65"]["smys"],
        smts=API_5L_GRADES["X65"]["smts"],
        corrosion_allowance=0.001,
    )

    phases = create_standard_phases(
        water_depth=500.0,
        design_pressure=20e6,
    )

    runner = PhaseAnalysisRunner(
        pipe_def,
        phases,
        codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111],
    )
    result = runner.run()

    summary_df = result.summary_dataframe()

    print(f"  Pipe OD:         {pipe_def.outer_diameter * 1000:.1f} mm")
    print(f"  Wall thickness:  {pipe_def.wall_thickness * 1000:.1f} mm")
    print(f"  Water depth:     500.0 m")
    print(f"  Design pressure: 20.0 MPa")
    print(f"  Phases:          {len(phases)}")
    print()
    print(summary_df.to_string(index=False))
    print()

    return result


def generate_html_reports(sweep_df, phase_result):
    """Generate HTML reports to the output directory."""
    print("=" * 70)
    print("4. HTML REPORT GENERATION")
    print("=" * 70)

    output_dir = Path(__file__).parent / "output" / "wall_thickness"
    output_dir.mkdir(parents=True, exist_ok=True)

    sweep_path = str(output_dir / "parametric_sweep.html")
    generate_report(sweep_df, sweep_path)
    print(f"  Parametric sweep report -> {sweep_path}")

    phase_path = str(output_dir / "phase_analysis.html")
    generate_phase_report(phase_result, phase_path)
    print(f"  Phase analysis report   -> {phase_path}")

    print()
    print(f"  Reports written to: {output_dir.resolve()}")
    print()


if __name__ == "__main__":
    print()
    print("Pipeline Wall Thickness Design Analysis")
    print("========================================")
    print()

    single_result = run_single_design_check()
    sweep_df = run_parametric_sweep()
    phase_result = run_phase_analysis()
    generate_html_reports(sweep_df, phase_result)

    print("Done.")
