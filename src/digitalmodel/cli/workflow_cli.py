"""ABOUTME: CLI commands for workflow template execution and management.
Provides commands for listing, executing, and monitoring workflow templates."""

import json
import sys
from pathlib import Path

import click
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from digitalmodel.modules.automation.workflow_executor import WorkflowExecutor

console = Console()


@click.group()
def workflow():
    """Enhanced workflow template management."""
    pass


@workflow.command()
@click.option(
    "--config",
    default="config/workflow-templates-enhanced.yaml",
    help="Path to workflow templates configuration",
)
def list(config):
    """List all available workflow templates."""
    try:
        executor = WorkflowExecutor(config)
        templates = executor.list_templates()

        table = Table(title="Available Workflow Templates", show_header=True, header_style="bold magenta")
        table.add_column("Template", style="cyan", no_wrap=True)
        table.add_column("Description", style="white")
        table.add_column("Category", style="yellow")
        table.add_column("Complexity", style="green")
        table.add_column("Duration", style="blue")
        table.add_column("Phases", style="magenta")

        for template in templates:
            table.add_row(
                template["name"],
                template["description"][:50] + "..." if len(template["description"]) > 50 else template["description"],
                template["category"],
                template["complexity"],
                f"{template['estimated_duration_minutes']}m",
                str(template["phase_count"]),
            )

        console.print(table)

        # Show recommended use cases
        console.print("\n[bold]Template Details:[/bold]")
        for template in templates:
            console.print(f"\n[cyan]{template['name']}[/cyan]:")
            console.print(f"  {template['description']}")
            if template['recommended_for']:
                console.print(f"  [dim]Recommended for: {', '.join(template['recommended_for'])}[/dim]")

    except Exception as e:
        console.print(f"[red]Error: {e}[/red]")
        sys.exit(1)


@workflow.command()
@click.argument("template_name")
@click.option(
    "--config",
    default="config/workflow-templates-enhanced.yaml",
    help="Path to workflow templates configuration",
)
@click.option(
    "--workflow-id",
    help="Custom workflow ID (default: auto-generated)",
)
@click.option(
    "--background",
    is_flag=True,
    help="Run workflow in background mode",
)
@click.option(
    "--json-output",
    is_flag=True,
    help="Output status in JSON format",
)
@click.option(
    "--checkpoint-id",
    help="Resume from specific checkpoint",
)
@click.option(
    "--report-path",
    default="reports/workflow_report.html",
    help="Path for HTML report output",
)
def execute(template_name, config, workflow_id, background, json_output, checkpoint_id, report_path):
    """Execute a workflow template.

    TEMPLATE_NAME: Name of the workflow template to execute
    """
    try:
        executor = WorkflowExecutor(config)

        # Check if resuming from checkpoint
        if checkpoint_id:
            console.print(f"[yellow]Resuming workflow from checkpoint: {checkpoint_id}[/yellow]")
            execution = executor.load_checkpoint(checkpoint_id)
            resume = True
        else:
            # Create new workflow execution
            execution = executor.create_workflow_execution(template_name, workflow_id)
            resume = False

        # Display workflow info
        if not json_output:
            info_panel = Panel(
                f"[bold]Template:[/bold] {execution.template_name}\n"
                f"[bold]Workflow ID:[/bold] {execution.workflow_id}\n"
                f"[bold]Phases:[/bold] {len(execution.phases)}\n"
                f"[bold]Checkpoint Dir:[/bold] {execution.checkpoint_dir}",
                title="Workflow Execution",
                border_style="green",
            )
            console.print(info_panel)

        if background:
            # Background mode - just print status updates
            console.print("[yellow]Running in background mode...[/yellow]")
            success = executor.execute_workflow(execution, resume_from_checkpoint=resume)

            if json_output:
                output = {
                    "workflow_id": execution.workflow_id,
                    "status": execution.status,
                    "success": success,
                }
                print(json.dumps(output))
            else:
                if success:
                    console.print(f"[green]✓ Workflow completed successfully[/green]")
                else:
                    console.print(f"[red]✗ Workflow failed[/red]")
        else:
            # Foreground mode with live progress
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=console,
            ) as progress:
                task = progress.add_task(f"Executing workflow: {template_name}", total=None)

                success = executor.execute_workflow(execution, resume_from_checkpoint=resume)

                progress.update(task, completed=True)

            if success:
                console.print(f"\n[green]✓ Workflow completed successfully![/green]")
            else:
                console.print(f"\n[red]✗ Workflow failed[/red]")

        # Generate HTML report
        if success:
            executor.generate_html_report(execution, report_path)

        # Output JSON if requested
        if json_output:
            output = {
                "workflow_id": execution.workflow_id,
                "template_name": execution.template_name,
                "status": execution.status,
                "success": success,
                "phases": len(execution.phases),
                "checkpoint_dir": str(execution.checkpoint_dir),
                "report_path": report_path if success else None,
            }
            print(json.dumps(output, indent=2))

        sys.exit(0 if success else 1)

    except Exception as e:
        console.print(f"[red]Error: {e}[/red]")
        if json_output:
            print(json.dumps({"error": str(e)}))
        sys.exit(1)


@workflow.command()
@click.argument("workflow_id")
@click.option(
    "--config",
    default="config/workflow-templates-enhanced.yaml",
    help="Path to workflow templates configuration",
)
@click.option(
    "--json-output",
    is_flag=True,
    help="Output status in JSON format",
)
def status(workflow_id, config, json_output):
    """Get status of a workflow execution.

    WORKFLOW_ID: ID of the workflow to check
    """
    try:
        executor = WorkflowExecutor(config)
        status_data = executor.get_workflow_status(workflow_id)

        if not status_data:
            console.print(f"[red]Workflow not found: {workflow_id}[/red]")
            sys.exit(1)

        if json_output:
            print(json.dumps(status_data, indent=2))
        else:
            # Display formatted status
            console.print(Panel(
                f"[bold]Workflow ID:[/bold] {status_data['workflow_id']}\n"
                f"[bold]Template:[/bold] {status_data['template_name']}\n"
                f"[bold]Status:[/bold] {status_data['status']}\n"
                f"[bold]Current Phase:[/bold] {status_data.get('current_phase', 'N/A')}\n"
                f"[bold]Start Time:[/bold] {status_data.get('start_time', 'N/A')}\n"
                f"[bold]End Time:[/bold] {status_data.get('end_time', 'N/A')}",
                title="Workflow Status",
                border_style="blue",
            ))

            # Phase summary table
            table = Table(title="Phase Status", show_header=True, header_style="bold magenta")
            table.add_column("Phase ID", style="cyan")
            table.add_column("Name", style="white")
            table.add_column("Status", style="yellow")
            table.add_column("Agents", style="green")

            for phase in status_data.get('phases', []):
                completed_agents = sum(1 for a in phase['agents'] if a['status'] == 'completed')
                total_agents = len(phase['agents'])

                table.add_row(
                    phase['phase_id'],
                    phase['name'],
                    phase['status'],
                    f"{completed_agents}/{total_agents}",
                )

            console.print(table)

    except Exception as e:
        console.print(f"[red]Error: {e}[/red]")
        if json_output:
            print(json.dumps({"error": str(e)}))
        sys.exit(1)


@workflow.command()
@click.option(
    "--config",
    default="config/workflow-templates-enhanced.yaml",
    help="Path to workflow templates configuration",
)
def checkpoints(config):
    """List all available workflow checkpoints."""
    try:
        executor = WorkflowExecutor(config)
        checkpoint_dir = executor.checkpoint_base_dir

        if not checkpoint_dir.exists():
            console.print(f"[yellow]No checkpoints found at: {checkpoint_dir}[/yellow]")
            return

        table = Table(title="Available Checkpoints", show_header=True, header_style="bold magenta")
        table.add_column("Workflow ID", style="cyan")
        table.add_column("Checkpoint", style="white")
        table.add_column("Timestamp", style="yellow")
        table.add_column("Path", style="green")

        for workflow_dir in checkpoint_dir.iterdir():
            if workflow_dir.is_dir():
                for checkpoint_file in workflow_dir.glob("checkpoint_*.json"):
                    table.add_row(
                        workflow_dir.name,
                        checkpoint_file.stem,
                        checkpoint_file.stat().st_mtime.__str__(),
                        str(checkpoint_file),
                    )

        console.print(table)

    except Exception as e:
        console.print(f"[red]Error: {e}[/red]")
        sys.exit(1)


if __name__ == "__main__":
    workflow()
