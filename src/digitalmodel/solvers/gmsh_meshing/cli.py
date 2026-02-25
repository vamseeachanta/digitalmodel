#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for GMSH mesh generation providing box and cylinder
meshing with quality assessment and VTK export.
"""

import click
import json
import sys
import numpy as np
from pathlib import Path

from . import __version__
from .mesh_generator import GMSHMeshGenerator, GMSH_AVAILABLE
from .quality_analyzer import MeshQualityAnalyzer
from .models import MeshAlgorithm


@click.group()
@click.version_option(version=__version__, prog_name="gmsh-meshing")
def cli():
    """GMSH Mesh Generation Tools - Finite element meshing and quality analysis"""
    if not GMSH_AVAILABLE:
        click.echo("Warning: GMSH not available. Install: pip install gmsh", err=True)


@cli.command('box')
@click.option('--length', type=float, required=True, help='Box length (m)')
@click.option('--width', type=float, required=True, help='Box width (m)')
@click.option('--height', type=float, required=True, help='Box height (m)')
@click.option('--element-size', type=float, default=1.0, help='Target element size (m)')
@click.option('--output', '-o', type=click.Path(), required=True, help='Output mesh file (.msh or .vtk)')
@click.option('--analyze', is_flag=True, help='Analyze mesh quality')
def box_cmd(length, width, height, element_size, output, analyze):
    """Generate tetrahedral mesh for box geometry"""

    if not GMSH_AVAILABLE:
        click.echo("Error: GMSH not installed. Install: pip install gmsh", err=True)
        sys.exit(1)

    try:
        with GMSHMeshGenerator() as generator:
            # Generate mesh
            mesh_data = generator.generate_simple_box_mesh(
                dimensions=(length, width, height),
                element_size=element_size
            )

            # Save mesh
            if output.endswith('.vtk'):
                generator.save_mesh_vtk(output)
            else:
                generator.save_mesh_msh(output)

            # Get statistics
            stats = generator.get_mesh_statistics()

            # Output results
            click.echo("\n=== Box Mesh Generated ===\n")
            click.echo(f"Dimensions:    {length} × {width} × {height} m")
            click.echo(f"Element Size:  {element_size} m")
            click.echo(f"Nodes:         {stats['n_nodes']}")
            click.echo(f"Elements:      {stats['n_elements']}")
            click.echo(f"Element Types: {', '.join(stats['element_types'])}")
            click.echo(f"\nMesh saved to: {output}")

            # Quality analysis
            if analyze:
                analyzer = MeshQualityAnalyzer()
                nodes = mesh_data['nodes']
                elements = mesh_data['elements']

                # Prefer tetrahedral elements; fall back to triangles
                _TET_KEY = 'Tetrahedron 4'
                _TRI_KEY = 'Triangle 3'
                elem_conn = None
                if _TET_KEY in elements:
                    elem_conn = elements[_TET_KEY]['connectivity']
                elif _TRI_KEY in elements:
                    elem_conn = elements[_TRI_KEY]['connectivity']
                elif elements:
                    elem_conn = next(iter(elements.values()))['connectivity']

                # Only analyse 4-node tetrahedral elements; surface meshes
                # produce Triangle 3 elements which require a different metric.
                has_tets = (
                    elem_conn is not None
                    and len(elem_conn) > 0
                    and elem_conn.shape[1] == 4
                )

                if has_tets:
                    quality = analyzer.analyze_tetrahedral_mesh(nodes, elem_conn)

                    click.echo("\n=== Mesh Quality ===\n")
                    click.echo(
                        f"Jacobian:      min={quality.min_jacobian:.3f},"
                        f" mean={quality.mean_jacobian:.3f}"
                    )
                    click.echo(
                        f"Aspect Ratio:  max={quality.max_aspect_ratio:.2f},"
                        f" mean={quality.mean_aspect_ratio:.2f}"
                    )
                    click.echo(
                        f"Skewness:      max={quality.max_skewness:.3f},"
                        f" mean={quality.mean_skewness:.3f}"
                    )
                    click.echo(f"\nQuality Score: {quality.quality_score:.1f}/100")
                    click.echo(
                        f"Status:        "
                        f"{'GOOD' if quality.is_good else 'ACCEPTABLE' if quality.is_acceptable else 'POOR'}"
                    )
                else:
                    click.echo("\n=== Mesh Quality ===\n")
                    click.echo("No 3D tetrahedral elements found (surface mesh only).")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('cylinder')
@click.option('--radius', type=float, required=True, help='Cylinder radius (m)')
@click.option('--height', type=float, required=True, help='Cylinder height (m)')
@click.option('--element-size', type=float, default=1.0, help='Target element size (m)')
@click.option('--output', '-o', type=click.Path(), required=True, help='Output mesh file (.msh or .vtk)')
@click.option('--analyze', is_flag=True, help='Analyze mesh quality')
def cylinder_cmd(radius, height, element_size, output, analyze):
    """Generate tetrahedral mesh for cylinder geometry"""

    if not GMSH_AVAILABLE:
        click.echo("Error: GMSH not installed", err=True)
        sys.exit(1)

    try:
        with GMSHMeshGenerator() as generator:
            mesh_data = generator.generate_cylinder_mesh(
                radius=radius,
                height=height,
                element_size=element_size
            )

            # Save mesh
            if output.endswith('.vtk'):
                generator.save_mesh_vtk(output)
            else:
                generator.save_mesh_msh(output)

            stats = generator.get_mesh_statistics()

            click.echo("\n=== Cylinder Mesh Generated ===\n")
            click.echo(f"Radius:        {radius} m")
            click.echo(f"Height:        {height} m")
            click.echo(f"Element Size:  {element_size} m")
            click.echo(f"Nodes:         {stats['n_nodes']}")
            click.echo(f"Elements:      {stats['n_elements']}")
            click.echo(f"\nMesh saved to: {output}")

            if analyze:
                analyzer = MeshQualityAnalyzer()
                nodes = mesh_data['nodes']
                elements = mesh_data['elements']

                if 'Tetrahedron 4' in elements:
                    elem_conn = elements['Tetrahedron 4']['connectivity']
                    quality = analyzer.analyze_tetrahedral_mesh(nodes, elem_conn)

                    click.echo("\n=== Mesh Quality ===\n")
                    click.echo(f"Quality Score: {quality.quality_score:.1f}/100")
                    click.echo(f"Status:        {'GOOD' if quality.is_good else 'ACCEPTABLE' if quality.is_acceptable else 'POOR'}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


def main():
    """Main entry point"""
    cli()


if __name__ == '__main__':
    main()
