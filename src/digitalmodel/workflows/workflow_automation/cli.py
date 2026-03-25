#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for workflow automation providing end-to-end
engineering analysis workflows.
"""

import click
import json
import sys
from pathlib import Path

from . import __version__
from .orchestrator import WorkflowOrchestrator
from .workflows import (
    CompleteRiserAnalysisWorkflow,
    MooringSystemDesignWorkflow,
    PlatformStructuralWorkflow,
    VesselResponseWorkflow,
)
from .models import WorkflowConfig
from .reporter import generate_workflow_report


@click.group()
@click.version_option(version=__version__, prog_name="workflow-automation")
def cli():
    """Workflow Automation - End-to-end engineering analysis workflows"""
    pass


@cli.command('list')
def list_workflows():
    """List available pre-built workflows"""
    workflows = {
        'riser-analysis': {
            'name': 'Complete Riser Analysis',
            'description': 'Catenary -> VIV -> Fatigue -> Report',
            'modules': ['catenary_riser', 'viv_analysis', 'fatigue_analysis'],
            'duration': '~2-5 minutes',
        },
        'mooring-design': {
            'name': 'Mooring System Design',
            'description': 'Environmental loads -> Catenary -> Safety verification',
            'modules': ['hydrodynamics', 'mooring_analysis'],
            'duration': '~1-3 minutes',
        },
        'structural-check': {
            'name': 'Platform Structural Check',
            'description': 'Stress -> Buckling -> Capacity verification',
            'modules': ['structural_analysis'],
            'duration': '~30-60 seconds',
        },
        'vessel-response': {
            'name': 'Vessel Response Analysis',
            'description': 'Wave spectrum -> RAO -> Vessel motions',
            'modules': ['hydrodynamics'],
            'duration': '~1-2 minutes',
        },
    }

    click.echo("\n=== Available Workflows ===\n")

    for workflow_id, info in workflows.items():
        click.echo(f"ID: {workflow_id}")
        click.echo(f"  Name:        {info['name']}")
        click.echo(f"  Description: {info['description']}")
        click.echo(f"  Modules:     {', '.join(info['modules'])}")
        click.echo(f"  Duration:    {info['duration']}")
        click.echo()

    click.echo("Usage: workflow-automation run <workflow-id> [options]\n")


@cli.command('riser-analysis')
@click.option('--diameter', type=float, required=True, help='Riser outer diameter (m)')
@click.option('--thickness', type=float, required=True, help='Wall thickness (m)')
@click.option('--length', type=float, required=True, help='Total riser length (m)')
@click.option('--water-depth', type=float, required=True, help='Water depth (m)')
@click.option('--offset', type=float, required=True, help='Horizontal offset at top (m)')
@click.option('--current-speed', type=float, required=True, help='Current speed (m/s)')
@click.option('--material', type=str, default='x65', help='Material grade')
@click.option('--internal-fluid', type=str, default='oil', help='Internal fluid type')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def riser_analysis_cmd(diameter, thickness, length, water_depth, offset, current_speed,
                       material, internal_fluid, output):
    """Execute complete riser analysis workflow"""

    try:
        click.echo("\n=== Complete Riser Analysis Workflow ===\n")
        click.echo(f"Riser:  OD={diameter}m, WT={thickness}m, L={length}m")
        click.echo(f"Config: Depth={water_depth}m, Offset={offset}m, Current={current_speed}m/s")
        click.echo()

        # Create workflow
        workflow = CompleteRiserAnalysisWorkflow.create(
            diameter=diameter,
            thickness=thickness,
            length=length,
            water_depth=water_depth,
            offset=offset,
            current_speed=current_speed,
            material=material,
            internal_fluid=internal_fluid,
        )

        # Execute
        orchestrator = WorkflowOrchestrator()
        result = orchestrator.execute_workflow(workflow)

        # Display results
        click.echo(f"\n=== Workflow Complete ===\n")
        click.echo(f"Status:         {result.status.upper()}")
        click.echo(f"Duration:       {result.duration_seconds:.1f}s")
        click.echo(f"Success Rate:   {result.success_rate:.1f}%")
        click.echo(f"Tasks:          {len(result.task_statuses)}")

        if result.get_failed_tasks():
            click.echo(f"\nFailed Tasks:   {', '.join(result.get_failed_tasks())}")

        # Key outputs
        if 'riser_top_tension' in result.outputs:
            click.echo(f"\nTop Tension:    {result.outputs['riser_top_tension']/1e3:.1f} kN")

        if 'viv_risk_status' in result.outputs:
            click.echo(f"VIV Status:     {result.outputs['viv_risk_status']}")

        if 'riser_fatigue_life_years' in result.outputs:
            click.echo(f"Fatigue Life:   {result.outputs['riser_fatigue_life_years']:.1f} years")

        # Save results
        if output:
            orchestrator.save_results(result, output)
            click.echo(f"\nResults saved to: {output}")

        # Generate HTML report
        report_path = generate_workflow_report(result)
        click.echo(f"HTML report: {report_path}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('mooring-design')
@click.option('--vessel-type', type=click.Choice(['fpso', 'semisubmersible', 'tanker']),
              required=True, help='Vessel type')
@click.option('--water-depth', type=float, required=True, help='Water depth (m)')
@click.option('--line-length', type=float, required=True, help='Mooring line length (m)')
@click.option('--line-diameter', type=float, default=0.120, help='Chain diameter (m)')
@click.option('--wind-speed', type=float, default=25.0, help='Wind speed (m/s)')
@click.option('--current-speed', type=float, default=1.5, help='Current speed (m/s)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def mooring_design_cmd(vessel_type, water_depth, line_length, line_diameter,
                       wind_speed, current_speed, output):
    """Execute mooring system design workflow"""

    try:
        click.echo("\n=== Mooring System Design Workflow ===\n")
        click.echo(f"Vessel:   {vessel_type.upper()}")
        click.echo(f"Depth:    {water_depth}m")
        click.echo(f"Line:     L={line_length}m, D={line_diameter}m")
        click.echo(f"Loading:  Wind={wind_speed}m/s, Current={current_speed}m/s")
        click.echo()

        # Create workflow
        workflow = MooringSystemDesignWorkflow.create(
            vessel_type=vessel_type,
            water_depth=water_depth,
            line_length=line_length,
            line_diameter=line_diameter,
            wind_speed=wind_speed,
            current_speed=current_speed,
        )

        # Execute
        orchestrator = WorkflowOrchestrator()
        result = orchestrator.execute_workflow(workflow)

        # Display results
        click.echo(f"\n=== Workflow Complete ===\n")
        click.echo(f"Status:         {result.status.upper()}")
        click.echo(f"Duration:       {result.duration_seconds:.1f}s")

        if 'mooring_total_load' in result.outputs:
            click.echo(f"\nTotal Load:     {result.outputs['mooring_total_load']/1e3:.1f} kN")

        if 'mooring_line_tension' in result.outputs:
            click.echo(f"Line Tension:   {result.outputs['mooring_line_tension']/1e3:.1f} kN")

        if 'mooring_safety_factor' in result.outputs:
            click.echo(f"Safety Factor:  {result.outputs['mooring_safety_factor']:.2f}")

        if output:
            orchestrator.save_results(result, output)
            click.echo(f"\nResults saved to: {output}")

        # Generate HTML report
        report_path = generate_workflow_report(result)
        click.echo(f"HTML report: {report_path}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('structural-check')
@click.option('--load-type', type=str, default='combined', help='Load type')
@click.option('--plate-length', type=float, required=True, help='Plate length (m)')
@click.option('--plate-width', type=float, required=True, help='Plate width (m)')
@click.option('--plate-thickness', type=float, required=True, help='Plate thickness (m)')
@click.option('--material', type=str, default='s355', help='Material grade')
@click.option('--applied-stress', type=float, default=200.0, help='Applied stress (MPa)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def structural_check_cmd(load_type, plate_length, plate_width, plate_thickness,
                        material, applied_stress, output):
    """Execute platform structural check workflow"""

    try:
        click.echo("\n=== Platform Structural Check Workflow ===\n")
        click.echo(f"Plate:   {plate_length}m × {plate_width}m × {plate_thickness}m")
        click.echo(f"Material: {material.upper()}")
        click.echo(f"Applied Stress: {applied_stress} MPa")
        click.echo()

        # Create workflow
        workflow = PlatformStructuralWorkflow.create(
            load_type=load_type,
            plate_length=plate_length,
            plate_width=plate_width,
            plate_thickness=plate_thickness,
            material=material,
            applied_stress=applied_stress,
        )

        # Execute
        orchestrator = WorkflowOrchestrator()
        result = orchestrator.execute_workflow(workflow)

        # Display results
        click.echo(f"\n=== Workflow Complete ===\n")
        click.echo(f"Status:         {result.status.upper()}")
        click.echo(f"Duration:       {result.duration_seconds:.1f}s")

        if 'plate_von_mises_stress' in result.outputs:
            click.echo(f"\nVon Mises:      {result.outputs['plate_von_mises_stress']:.1f} MPa")

        if 'plate_stress_utilization' in result.outputs:
            click.echo(f"Utilization:    {result.outputs['plate_stress_utilization']:.1%}")

        if 'structural_status' in result.outputs:
            click.echo(f"Status:         {result.outputs['structural_status']}")

        if output:
            orchestrator.save_results(result, output)
            click.echo(f"\nResults saved to: {output}")

        # Generate HTML report
        report_path = generate_workflow_report(result)
        click.echo(f"HTML report: {report_path}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


def main():
    """Main entry point"""
    cli()


if __name__ == '__main__':
    main()
