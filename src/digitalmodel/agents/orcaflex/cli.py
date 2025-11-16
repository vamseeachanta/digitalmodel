#!/usr/bin/env python3
"""
OrcaFlex Agent CLI - Main command-line interface
"""

import click
from pathlib import Path
from .commands import generate, validate, workflow

@click.group()
@click.version_option(version='0.1.0')
@click.option('--verbose', '-v', is_flag=True, help='Enable verbose output')
@click.pass_context
def cli(ctx, verbose):
    """
    OrcaFlex Agent - Automate OrcaFlex project creation and validation

    Generate modular base files, environmental conditions, analysis models,
    and post-processing configurations following proven patterns.
    """
    ctx.ensure_object(dict)
    ctx.obj['VERBOSE'] = verbose

# Register command groups
cli.add_command(generate.generate)
cli.add_command(validate.validate)
cli.add_command(workflow.workflow)

def main():
    """Entry point for console_scripts"""
    cli(obj={})

if __name__ == '__main__':
    main()
