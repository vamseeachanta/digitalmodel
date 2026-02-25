# ABOUTME: CLI entry point for pipeline wall thickness design checks
# ABOUTME: Commands for single check, parametric sweep, phase analysis, grade listing

import json
from pathlib import Path

import click
import numpy as np

from .wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)
from .wall_thickness_parametric import (
    API_5L_GRADES,
    ParametricSweep,
    SweepConfig,
    generate_report,
)
from .wall_thickness_interactive_report import InteractiveReportBuilder
from .wall_thickness_lookup import LookupConfig, MTLookupGenerator
from .wall_thickness_phases import (
    PhaseAnalysisRunner,
    PipeDefinition,
    create_standard_phases,
    generate_phase_report,
)

CODE_MAP = {
    "DNV": DesignCode.DNV_ST_F101,
    "API": DesignCode.API_RP_1111,
    "PD8010": DesignCode.PD_8010_2,
    "ASME": DesignCode.ASME_B31_8,
    "ISO": DesignCode.ISO_13623,
}

SAFETY_CLASS_MAP = {
    "low": SafetyClass.LOW,
    "medium": SafetyClass.MEDIUM,
    "high": SafetyClass.HIGH,
}


@click.group()
@click.version_option(version="1.0.0", prog_name="wall-thickness")
def cli():
    """Pipeline Wall Thickness Design Checks -- Multi-code analysis"""
    pass


@cli.command("check")
@click.option("--od", type=float, required=True, help="Outer diameter (m)")
@click.option("--wt", type=float, required=True, help="Wall thickness (m)")
@click.option("--grade", type=str, default="X65", help="API 5L grade")
@click.option("--pi", type=float, default=0.0, help="Internal pressure (Pa)")
@click.option("--pe", type=float, default=0.0, help="External pressure (Pa)")
@click.option("--moment", type=float, default=0.0, help="Bending moment (N-m)")
@click.option("--tension", type=float, default=0.0, help="Axial tension (N)")
@click.option(
    "--code",
    type=click.Choice(["DNV", "API", "PD8010", "ASME", "ISO"], case_sensitive=False),
    default="DNV",
    help="Design code",
)
@click.option(
    "--safety-class",
    type=click.Choice(["low", "medium", "high"], case_sensitive=False),
    default="medium",
    help="Safety class",
)
@click.option(
    "--corrosion", type=float, default=0.001, help="Corrosion allowance (m)"
)
@click.option("--output", "-o", type=click.Path(), default=None, help="JSON output file")
def check(od, wt, grade, pi, pe, moment, tension, code, safety_class, corrosion, output):
    """Single-point wall thickness design check."""
    grade_upper = grade.upper()
    if grade_upper not in API_5L_GRADES:
        click.echo(f"Error: Unknown grade '{grade}'. Use 'list-grades' to see available grades.")
        raise SystemExit(1)

    grade_data = API_5L_GRADES[grade_upper]
    design_code = CODE_MAP[code.upper()]
    sc = SAFETY_CLASS_MAP[safety_class.lower()]

    geometry = PipeGeometry(
        outer_diameter=od, wall_thickness=wt, corrosion_allowance=corrosion,
    )
    material = PipeMaterial(
        grade=grade_upper, smys=grade_data["smys"], smts=grade_data["smts"],
    )
    loads = DesignLoads(
        internal_pressure=pi,
        external_pressure=pe,
        bending_moment=moment,
        effective_tension=tension,
    )
    factors = DesignFactors(safety_class=sc)

    analyzer = WallThicknessAnalyzer(geometry, material, loads, factors, design_code)
    result = analyzer.perform_analysis()

    click.echo("")
    click.echo("=" * 70)
    click.echo("Wall Thickness Design Check")
    click.echo("=" * 70)
    click.echo(f"  Pipe:           OD={od*1000:.1f} mm, WT={wt*1000:.1f} mm")
    click.echo(f"  Grade:          {grade_upper} (SMYS={grade_data['smys']/1e6:.0f} MPa, SMTS={grade_data['smts']/1e6:.0f} MPa)")
    click.echo(f"  Code:           {code.upper()}")
    click.echo(f"  Safety Class:   {safety_class}")
    click.echo(f"  Corrosion:      {corrosion*1000:.1f} mm")
    click.echo("-" * 70)
    click.echo(f"  Internal Pressure:  {pi/1e6:.2f} MPa")
    click.echo(f"  External Pressure:  {pe/1e6:.2f} MPa")
    click.echo(f"  Bending Moment:     {moment:.0f} N-m")
    click.echo(f"  Axial Tension:      {tension:.0f} N")
    click.echo("-" * 70)

    click.echo("  Individual Checks:")
    for check_name, util in result.checks.items():
        status_str = "PASS" if util <= 1.0 else "FAIL"
        click.echo(f"    {check_name:<30s}  utilisation={util:.3f}  [{status_str}]")

    click.echo("-" * 70)
    status_label = "PASS" if result.is_safe else "FAIL"
    click.echo(f"  Governing Check:    {result.governing_check}")
    click.echo(f"  Max Utilisation:    {result.max_utilisation:.3f}")
    click.echo(f"  Overall Status:     {status_label}")
    click.echo("=" * 70)
    click.echo("")

    if output:
        output_data = {
            "pipe": {"od_m": od, "wt_m": wt, "grade": grade_upper},
            "code": code.upper(),
            "safety_class": safety_class,
            "corrosion_allowance_m": corrosion,
            "loads": {
                "internal_pressure_Pa": pi,
                "external_pressure_Pa": pe,
                "bending_moment_Nm": moment,
                "axial_tension_N": tension,
            },
            "is_safe": result.is_safe,
            "governing_check": result.governing_check,
            "max_utilisation": result.max_utilisation,
            "checks": result.checks,
        }
        output_path = Path(output)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, "w") as f:
            json.dump(output_data, f, indent=2)
        click.echo(f"Results saved to: {output}\n")


@cli.command("sweep")
@click.option("--wt-min", type=float, default=0.010, help="Minimum wall thickness (m)")
@click.option("--wt-max", type=float, default=0.040, help="Maximum wall thickness (m)")
@click.option("--wt-steps", type=int, default=7, help="Number of wall thickness steps")
@click.option(
    "--od",
    type=float,
    multiple=True,
    default=[0.27305],
    help="Outer diameter(s) (m), repeatable",
)
@click.option(
    "--grade",
    type=str,
    multiple=True,
    default=["X65"],
    help="API 5L grade(s), repeatable",
)
@click.option("--pi", type=float, default=20e6, help="Internal pressure (Pa)")
@click.option("--pe", type=float, default=0.0, help="External pressure (Pa)")
@click.option(
    "--code",
    type=click.Choice(["DNV", "API", "PD8010", "ASME", "ISO"], case_sensitive=False),
    default="DNV",
    help="Design code",
)
@click.option(
    "--safety-class",
    type=click.Choice(["low", "medium", "high"], case_sensitive=False),
    default="medium",
    help="Safety class",
)
@click.option(
    "--output", "-o", type=click.Path(), default=None, help="Output directory for HTML report"
)
def sweep(wt_min, wt_max, wt_steps, od, grade, pi, pe, code, safety_class, output):
    """Parametric wall thickness sweep across multiple ODs and grades."""
    design_code = CODE_MAP[code.upper()]
    sc = SAFETY_CLASS_MAP[safety_class.lower()]

    wall_thicknesses = np.linspace(wt_min, wt_max, wt_steps)
    outer_diameters = list(od)
    grades = [g.upper() for g in grade]

    for g in grades:
        if g not in API_5L_GRADES:
            click.echo(f"Error: Unknown grade '{g}'. Use 'list-grades' to see available grades.")
            raise SystemExit(1)

    config = SweepConfig(
        wall_thicknesses=list(wall_thicknesses),
        outer_diameters=outer_diameters,
        grades=grades,
        internal_pressures=[pi],
        external_pressures=[pe],
        code=design_code,
        safety_class=sc,
    )

    click.echo("")
    click.echo("=" * 70)
    click.echo("Parametric Wall Thickness Sweep")
    click.echo("=" * 70)
    click.echo(f"  WT range:   {wt_min*1000:.1f} - {wt_max*1000:.1f} mm ({wt_steps} steps)")
    click.echo(f"  ODs:        {', '.join(f'{d*1000:.1f} mm' for d in outer_diameters)}")
    click.echo(f"  Grades:     {', '.join(grades)}")
    click.echo(f"  Code:       {code.upper()}")
    click.echo(f"  Pi:         {pi/1e6:.2f} MPa")
    click.echo(f"  Pe:         {pe/1e6:.2f} MPa")
    click.echo("-" * 70)

    sweep_runner = ParametricSweep(config)
    df = sweep_runner.run()

    click.echo(f"\n  Results ({len(df)} combinations):\n")
    click.echo(df.head(20).to_string(index=False))
    if len(df) > 20:
        click.echo(f"\n  ... ({len(df) - 20} more rows)")
    click.echo("")

    if output:
        output_dir = Path(output)
        output_dir.mkdir(parents=True, exist_ok=True)
        report_path = str(output_dir / "parametric_sweep.html")
        generate_report(df, report_path)
        click.echo(f"HTML report saved to: {report_path}\n")


@cli.command("phases")
@click.option("--od", type=float, required=True, help="Outer diameter (m)")
@click.option("--wt", type=float, required=True, help="Wall thickness (m)")
@click.option("--grade", type=str, default="X65", help="API 5L grade")
@click.option("--depth", type=float, required=True, help="Water depth (m)")
@click.option(
    "--design-pressure", type=float, required=True, help="Maximum allowable operating pressure (Pa)"
)
@click.option(
    "--corrosion", type=float, default=0.001, help="Corrosion allowance (m)"
)
@click.option(
    "--codes",
    type=str,
    multiple=True,
    default=["DNV", "API"],
    help="Design codes to compare, repeatable",
)
@click.option(
    "--output", "-o", type=click.Path(), default=None, help="Output directory for HTML report"
)
def phases(od, wt, grade, depth, design_pressure, corrosion, codes, output):
    """Five-phase pipeline lifecycle analysis."""
    grade_upper = grade.upper()
    if grade_upper not in API_5L_GRADES:
        click.echo(f"Error: Unknown grade '{grade}'. Use 'list-grades' to see available grades.")
        raise SystemExit(1)

    design_codes = []
    for c in codes:
        c_upper = c.upper()
        if c_upper not in CODE_MAP:
            click.echo(f"Error: Unknown code '{c}'. Choose from: {', '.join(CODE_MAP.keys())}")
            raise SystemExit(1)
        design_codes.append(CODE_MAP[c_upper])

    grade_data = API_5L_GRADES[grade_upper]
    pipe_def = PipeDefinition(
        outer_diameter=od,
        wall_thickness=wt,
        grade=grade_upper,
        smys=grade_data["smys"],
        smts=grade_data["smts"],
        corrosion_allowance=corrosion,
    )
    phase_list = create_standard_phases(depth, design_pressure)

    click.echo("")
    click.echo("=" * 70)
    click.echo("Pipeline Phase Analysis")
    click.echo("=" * 70)
    click.echo(f"  Pipe:           OD={od*1000:.1f} mm, WT={wt*1000:.1f} mm")
    click.echo(f"  Grade:          {grade_upper}")
    click.echo(f"  Water Depth:    {depth:.1f} m")
    click.echo(f"  Design Press:   {design_pressure/1e6:.2f} MPa")
    click.echo(f"  Corrosion:      {corrosion*1000:.1f} mm")
    click.echo(f"  Codes:          {', '.join(c.upper() for c in codes)}")
    click.echo(f"  Phases:         {len(phase_list)}")
    click.echo("-" * 70)

    runner = PhaseAnalysisRunner(pipe_def, phase_list, codes=design_codes)
    comparison_result = runner.run()

    summary_df = comparison_result.summary_dataframe()
    click.echo(f"\n  Phase Summary:\n")
    click.echo(summary_df.to_string(index=False))
    click.echo("")

    if output:
        output_dir = Path(output)
        output_dir.mkdir(parents=True, exist_ok=True)
        report_path = str(output_dir / "phase_analysis.html")
        generate_phase_report(comparison_result, report_path)
        click.echo(f"HTML report saved to: {report_path}\n")


@cli.command("list-grades")
def list_grades():
    """List available API 5L steel grades."""
    click.echo("")
    click.echo("=" * 50)
    click.echo("API 5L Steel Grades")
    click.echo("=" * 50)
    click.echo(f"  {'Grade':<10s} {'SMYS (MPa)':>12s} {'SMTS (MPa)':>12s}")
    click.echo("  " + "-" * 36)
    for name, data in sorted(API_5L_GRADES.items()):
        smys_mpa = data["smys"] / 1e6
        smts_mpa = data["smts"] / 1e6
        click.echo(f"  {name:<10s} {smys_mpa:>12.0f} {smts_mpa:>12.0f}")
    click.echo("=" * 50)
    click.echo("")


@cli.command("lookup")
@click.option("--od", type=float, required=True, help="Outer diameter (m)")
@click.option("--wt", type=float, required=True, help="Wall thickness (m)")
@click.option("--grade", type=str, default="X65", help="API 5L grade")
@click.option("--pi", type=float, default=20e6, help="Internal pressure (Pa)")
@click.option("--pe", type=float, default=5e6, help="External pressure (Pa)")
@click.option(
    "--safety-class",
    type=click.Choice(["low", "medium", "high"], case_sensitive=False),
    default="medium",
    help="Safety class",
)
@click.option(
    "--codes",
    type=str,
    multiple=True,
    default=["DNV", "API", "PD8010", "ASME", "ISO"],
    help="Design codes, repeatable",
)
@click.option("--steps", type=int, default=50, help="Grid steps per axis")
@click.option("--output", "-o", type=click.Path(), required=True, help="Output CSV file path")
def lookup(od, wt, grade, pi, pe, safety_class, codes, steps, output):
    """Generate 50x50 moment-tension allowable lookup table as CSV."""
    grade_upper = grade.upper()
    if grade_upper not in API_5L_GRADES:
        click.echo(f"Error: Unknown grade '{grade}'. Use 'list-grades' to see available grades.")
        raise SystemExit(1)

    grade_data = API_5L_GRADES[grade_upper]
    sc = SAFETY_CLASS_MAP[safety_class.lower()]

    design_codes = []
    for c in codes:
        c_upper = c.upper()
        if c_upper not in CODE_MAP:
            click.echo(f"Error: Unknown code '{c}'. Choose from: {', '.join(CODE_MAP.keys())}")
            raise SystemExit(1)
        design_codes.append(CODE_MAP[c_upper])

    geometry = PipeGeometry(outer_diameter=od, wall_thickness=wt)
    material = PipeMaterial(grade=grade_upper, smys=grade_data["smys"], smts=grade_data["smts"])

    config = LookupConfig(
        geometry=geometry,
        material=material,
        internal_pressure=pi,
        external_pressure=pe,
        safety_class=sc,
        codes=design_codes,
        n_moment_steps=steps,
        n_tension_steps=steps,
    )

    click.echo("")
    click.echo("=" * 70)
    click.echo("M-T Allowable Lookup Table Generation")
    click.echo("=" * 70)
    click.echo(f"  Pipe:     OD={od*1000:.1f} mm, WT={wt*1000:.1f} mm, {grade_upper}")
    click.echo(f"  Pi:       {pi/1e6:.2f} MPa")
    click.echo(f"  Pe:       {pe/1e6:.2f} MPa")
    click.echo(f"  Grid:     {steps}x{steps}")
    click.echo(f"  Codes:    {', '.join(c.upper() for c in codes)}")
    click.echo("-" * 70)

    gen = MTLookupGenerator(config)
    gen.to_csv(output)
    click.echo(f"\n  CSV written to: {output}\n")


@cli.command("report")
@click.option("--od", type=float, required=True, help="Outer diameter (m)")
@click.option("--wt", type=float, required=True, help="Wall thickness (m)")
@click.option("--grade", type=str, default="X65", help="API 5L grade")
@click.option("--pi", type=float, default=20e6, help="Internal pressure (Pa)")
@click.option("--pe", type=float, default=5e6, help="External pressure (Pa)")
@click.option("--depth", type=float, default=500, help="Water depth (m) for phase analysis")
@click.option(
    "--codes",
    type=str,
    multiple=True,
    default=["DNV", "API", "PD8010", "ASME", "ISO"],
    help="Design codes, repeatable",
)
@click.option(
    "--safety-class",
    type=click.Choice(["low", "medium", "high"], case_sensitive=False),
    default="medium",
    help="Safety class",
)
@click.option("--output", "-o", type=click.Path(), required=True, help="Output HTML file path")
def report(od, wt, grade, pi, pe, depth, codes, safety_class, output):
    """Generate interactive multi-code HTML report with code toggling."""
    grade_upper = grade.upper()
    if grade_upper not in API_5L_GRADES:
        click.echo(f"Error: Unknown grade '{grade}'. Use 'list-grades' to see available grades.")
        raise SystemExit(1)

    grade_data = API_5L_GRADES[grade_upper]
    sc = SAFETY_CLASS_MAP[safety_class.lower()]

    design_codes = []
    for c in codes:
        c_upper = c.upper()
        if c_upper not in CODE_MAP:
            click.echo(f"Error: Unknown code '{c}'. Choose from: {', '.join(CODE_MAP.keys())}")
            raise SystemExit(1)
        design_codes.append(CODE_MAP[c_upper])

    geometry = PipeGeometry(
        outer_diameter=od, wall_thickness=wt, corrosion_allowance=0.001,
    )
    material = PipeMaterial(
        grade=grade_upper, smys=grade_data["smys"], smts=grade_data["smts"],
    )

    click.echo("")
    click.echo("=" * 70)
    click.echo("Interactive Multi-Code Report Generation")
    click.echo("=" * 70)
    click.echo(f"  Pipe:     OD={od*1000:.1f} mm, WT={wt*1000:.1f} mm, {grade_upper}")
    click.echo(f"  Codes:    {', '.join(c.upper() for c in codes)}")
    click.echo("-" * 70)

    builder = InteractiveReportBuilder()

    # Add utilisation vs WT chart
    builder.add_utilisation_vs_wt_chart(
        geometry=geometry,
        material=material,
        internal_pressure=pi,
        external_pressure=pe,
        codes=design_codes,
        safety_class=sc,
    )

    # Add phase comparison chart
    pipe_def = PipeDefinition(
        outer_diameter=od, wall_thickness=wt,
        grade=grade_upper, smys=grade_data["smys"], smts=grade_data["smts"],
        corrosion_allowance=0.001,
    )
    phase_list = create_standard_phases(depth, pi)
    runner = PhaseAnalysisRunner(pipe_def, phase_list, codes=design_codes, safety_class=sc)
    comparison = runner.run()
    builder.add_phase_comparison_chart(comparison)

    builder.build(output)
    click.echo(f"\n  Report written to: {output}\n")


def main():
    """Main CLI entry point."""
    cli()


if __name__ == "__main__":
    main()
