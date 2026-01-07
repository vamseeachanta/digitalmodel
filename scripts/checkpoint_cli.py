#!/usr/bin/env python3
# ABOUTME: CLI wrapper for workflow checkpoint management
# Provides command-line interface for creating, restoring, listing, and cleaning checkpoints

import asyncio
import json
import sys
from datetime import datetime
from pathlib import Path
from typing import Optional

import click
from loguru import logger
from tabulate import tabulate

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.modules.automation.workflow_checkpoints import (
    WorkflowCheckpointManager,
)


@click.group()
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose logging")
@click.pass_context
def cli(ctx, verbose):
    """Workflow checkpoint management CLI."""
    ctx.ensure_object(dict)

    # Configure logging
    logger.remove()
    if verbose:
        logger.add(sys.stderr, level="DEBUG")
    else:
        logger.add(sys.stderr, level="INFO")

    # Initialize manager
    ctx.obj["manager"] = WorkflowCheckpointManager()


@cli.command()
@click.argument("workflow_id")
@click.argument("phase")
@click.option("--description", "-d", default="", help="Checkpoint description")
@click.option("--tags", "-t", multiple=True, help="Checkpoint tags")
@click.option("--json", "output_json", is_flag=True, help="Output as JSON")
@click.pass_context
def create(ctx, workflow_id, phase, description, tags, output_json):
    """Create a new checkpoint."""
    manager: WorkflowCheckpointManager = ctx.obj["manager"]

    async def _create():
        checkpoint_id = await manager.create_checkpoint(
            workflow_id=workflow_id,
            phase=phase,
            description=description,
            tags=list(tags) if tags else None,
        )

        if output_json:
            click.echo(json.dumps({"checkpoint_id": checkpoint_id}))
        else:
            click.echo(f"✓ Checkpoint created: {checkpoint_id}")

        return checkpoint_id

    asyncio.run(_create())


@cli.command()
@click.argument("checkpoint_id")
@click.option("--force", "-f", is_flag=True, help="Skip confirmation")
@click.option("--no-backup", is_flag=True, help="Don't create backup before restore")
@click.option("--json", "output_json", is_flag=True, help="Output as JSON")
@click.pass_context
def restore(ctx, checkpoint_id, force, no_backup, output_json):
    """Restore a checkpoint."""
    manager: WorkflowCheckpointManager = ctx.obj["manager"]

    if not force and not output_json:
        click.confirm(
            f"Restore checkpoint {checkpoint_id}? This will modify your working directory.",
            abort=True,
        )

    async def _restore():
        success = await manager.restore_checkpoint(
            checkpoint_id=checkpoint_id,
            force=force,
            create_backup=not no_backup,
        )

        if output_json:
            click.echo(json.dumps({"success": success}))
        else:
            if success:
                click.echo(f"✓ Checkpoint restored: {checkpoint_id}")
            else:
                click.echo(f"✗ Failed to restore checkpoint: {checkpoint_id}", err=True)
                sys.exit(1)

    asyncio.run(_restore())


@cli.command()
@click.option("--workflow-id", "-w", help="Filter by workflow ID")
@click.option("--phase", "-p", help="Filter by phase")
@click.option("--tags", "-t", multiple=True, help="Filter by tags")
@click.option("--date-from", help="Filter by start date (YYYY-MM-DD)")
@click.option("--date-to", help="Filter by end date (YYYY-MM-DD)")
@click.option("--json", "output_json", is_flag=True, help="Output as JSON")
@click.pass_context
def list(ctx, workflow_id, phase, tags, date_from, date_to, output_json):
    """List available checkpoints."""
    manager: WorkflowCheckpointManager = ctx.obj["manager"]

    # Parse dates
    date_from_dt = None
    date_to_dt = None

    if date_from:
        try:
            date_from_dt = datetime.strptime(date_from, "%Y-%m-%d")
        except ValueError:
            click.echo(f"Invalid date format: {date_from}", err=True)
            sys.exit(1)

    if date_to:
        try:
            date_to_dt = datetime.strptime(date_to, "%Y-%m-%d")
        except ValueError:
            click.echo(f"Invalid date format: {date_to}", err=True)
            sys.exit(1)

    checkpoints = manager.list_checkpoints(
        workflow_id=workflow_id,
        phase=phase,
        tags=list(tags) if tags else None,
        date_from=date_from_dt,
        date_to=date_to_dt,
    )

    if output_json:
        # Output as JSON
        output = []
        for cp in checkpoints:
            output.append({
                "checkpoint_id": cp.checkpoint_id,
                "workflow_id": cp.workflow_id,
                "phase": cp.phase,
                "description": cp.description,
                "timestamp": cp.timestamp,
                "tags": cp.tags,
                "file_count": cp.file_count,
                "size_mb": cp.total_size_bytes / 1024 / 1024,
                "git_commit": cp.git_state.commit_hash[:8],
                "git_branch": cp.git_state.branch,
            })
        click.echo(json.dumps(output, indent=2))
    else:
        # Output as table
        if not checkpoints:
            click.echo("No checkpoints found.")
            return

        table_data = []
        for cp in checkpoints:
            table_data.append([
                cp.checkpoint_id,
                cp.workflow_id,
                cp.phase,
                datetime.strptime(cp.timestamp, "%Y%m%d_%H%M%S").strftime("%Y-%m-%d %H:%M:%S"),
                f"{cp.total_size_bytes / 1024 / 1024:.2f} MB",
                cp.file_count,
                ", ".join(cp.tags) if cp.tags else "",
                cp.git_state.commit_hash[:8],
            ])

        headers = ["Checkpoint ID", "Workflow", "Phase", "Timestamp", "Size", "Files", "Tags", "Git"]
        click.echo(tabulate(table_data, headers=headers, tablefmt="grid"))
        click.echo(f"\nTotal: {len(checkpoints)} checkpoint(s)")


@cli.command()
@click.option("--dry-run", is_flag=True, help="Show what would be deleted without deleting")
@click.option("--json", "output_json", is_flag=True, help="Output as JSON")
@click.pass_context
def clean(ctx, dry_run, output_json):
    """Clean up old checkpoints based on retention policies."""
    manager: WorkflowCheckpointManager = ctx.obj["manager"]

    async def _clean():
        if dry_run:
            # Just list what would be deleted
            checkpoints = manager.list_checkpoints()
            retention_days = manager.config["retention"]["time_based_days"]
            max_count = manager.config["retention"]["count_based"]

            click.echo(f"Retention policy: {retention_days} days, {max_count} checkpoints per workflow")
            click.echo(f"Total checkpoints: {len(checkpoints)}")

            # TODO: Implement dry-run logic
            click.echo("(Dry-run mode: no deletions performed)")
        else:
            stats = await manager.cleanup_checkpoints()

            if output_json:
                click.echo(json.dumps(stats, indent=2))
            else:
                click.echo(f"✓ Cleanup complete:")
                click.echo(f"  Total checkpoints: {stats['total_checkpoints']}")
                click.echo(f"  Deleted: {stats['deleted_checkpoints']}")
                click.echo(f"  Kept by tag: {stats['kept_by_tag']}")
                click.echo(f"  Kept by time: {stats['kept_by_time']}")
                click.echo(f"  Kept by count: {stats['kept_by_count']}")

    asyncio.run(_clean())


@cli.command()
@click.argument("checkpoint_id")
@click.option("--json", "output_json", is_flag=True, help="Output as JSON")
@click.pass_context
def info(ctx, checkpoint_id, output_json):
    """Show detailed information about a checkpoint."""
    manager: WorkflowCheckpointManager = ctx.obj["manager"]

    checkpoint = manager.get_checkpoint_info(checkpoint_id)

    if not checkpoint:
        click.echo(f"Checkpoint not found: {checkpoint_id}", err=True)
        sys.exit(1)

    if output_json:
        output = {
            "checkpoint_id": checkpoint.checkpoint_id,
            "workflow_id": checkpoint.workflow_id,
            "phase": checkpoint.phase,
            "description": checkpoint.description,
            "timestamp": checkpoint.timestamp,
            "tags": checkpoint.tags,
            "file_count": checkpoint.file_count,
            "total_size_bytes": checkpoint.total_size_bytes,
            "compressed": checkpoint.compressed,
            "git_state": {
                "commit_hash": checkpoint.git_state.commit_hash,
                "branch": checkpoint.git_state.branch,
                "author": checkpoint.git_state.author,
                "message": checkpoint.git_state.message,
                "dirty": checkpoint.git_state.dirty,
                "staged_files": checkpoint.git_state.staged_files,
                "modified_files": checkpoint.git_state.modified_files,
                "untracked_files": checkpoint.git_state.untracked_files,
            },
            "agent_states": [
                {
                    "agent_id": a.agent_id,
                    "agent_type": a.agent_type,
                    "status": a.status,
                    "task_id": a.task_id,
                }
                for a in checkpoint.agent_states
            ],
            "memory_keys": checkpoint.memory_keys,
        }
        click.echo(json.dumps(output, indent=2))
    else:
        click.echo(f"Checkpoint: {checkpoint.checkpoint_id}")
        click.echo(f"Workflow: {checkpoint.workflow_id}")
        click.echo(f"Phase: {checkpoint.phase}")
        click.echo(f"Description: {checkpoint.description}")
        click.echo(f"Timestamp: {datetime.strptime(checkpoint.timestamp, '%Y%m%d_%H%M%S').strftime('%Y-%m-%d %H:%M:%S')}")
        click.echo(f"Tags: {', '.join(checkpoint.tags) if checkpoint.tags else 'None'}")
        click.echo(f"Files: {checkpoint.file_count}")
        click.echo(f"Size: {checkpoint.total_size_bytes / 1024 / 1024:.2f} MB")
        click.echo(f"Compressed: {'Yes' if checkpoint.compressed else 'No'}")
        click.echo(f"\nGit State:")
        click.echo(f"  Commit: {checkpoint.git_state.commit_hash}")
        click.echo(f"  Branch: {checkpoint.git_state.branch}")
        click.echo(f"  Author: {checkpoint.git_state.author}")
        click.echo(f"  Message: {checkpoint.git_state.message}")
        click.echo(f"  Dirty: {'Yes' if checkpoint.git_state.dirty else 'No'}")

        if checkpoint.agent_states:
            click.echo(f"\nAgent States ({len(checkpoint.agent_states)}):")
            for agent in checkpoint.agent_states:
                click.echo(f"  - {agent.agent_id} ({agent.agent_type}): {agent.status}")

        if checkpoint.memory_keys:
            click.echo(f"\nMemory Keys ({len(checkpoint.memory_keys)}):")
            for key in checkpoint.memory_keys[:10]:  # Show first 10
                click.echo(f"  - {key}")
            if len(checkpoint.memory_keys) > 10:
                click.echo(f"  ... and {len(checkpoint.memory_keys) - 10} more")


if __name__ == "__main__":
    cli(obj={})
