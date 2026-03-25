#!/usr/bin/env python3
"""
ABOUTME: Visualize generated operability load cases as polar plot
ABOUTME: Shows load case distribution and metocean conditions for each heading
"""

import sys
import argparse
from pathlib import Path
import yaml
import io

# Configure stdout for UTF-8
if sys.platform == 'win32':
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

# Import visualization
try:
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False
    print("❌ Plotly not available. Install with: pip install plotly")
    sys.exit(1)


def load_project_config(config_path: Path) -> dict:
    """Load project configuration YAML."""
    with open(config_path, 'r', encoding='utf-8') as f:
        return yaml.safe_load(f)


def extract_load_cases(config: dict) -> list:
    """Extract load cases from configuration."""
    human_input = config.get('human_input', {})
    analysis = human_input.get('analysis', {})
    return analysis.get('load_cases', [])


def visualize_load_cases(load_cases: list, output_path: Path, project_name: str = "CALM Buoy"):
    """
    Create polar plot visualization of load cases.

    Args:
        load_cases: List of load case dictionaries
        output_path: Path to save HTML file
        project_name: Project name for title
    """
    if not load_cases:
        print("❌ No load cases found in configuration")
        return

    # Extract data
    headings = []
    conditions = []
    wave_dirs = []
    current_dirs = []
    wind_dirs = []

    for lc in load_cases:
        headings.append(lc.get('wave_direction', 0))
        conditions.append(lc.get('condition', 'unknown'))
        wave_dirs.append(lc.get('wave_direction', 0))
        current_dirs.append(lc.get('current_direction', 0))
        wind_dirs.append(lc.get('wind_direction', 0))

    # Create subplots - one for load case distribution, one for wind/current alignment
    fig = make_subplots(
        rows=1, cols=2,
        subplot_titles=('Load Case Distribution', 'Wind/Current/Wave Alignment'),
        specs=[[{'type': 'polar'}, {'type': 'polar'}]]
    )

    # Plot 1: Load case distribution with conditions
    # Group by condition
    condition_groups = {}
    for i, condition in enumerate(conditions):
        if condition not in condition_groups:
            condition_groups[condition] = {'headings': [], 'names': []}
        condition_groups[condition]['headings'].append(headings[i])
        condition_groups[condition]['names'].append(load_cases[i].get('name', f'LC{i}'))

    # Color map for conditions
    color_map = {
        'operating': 'rgb(44, 160, 44)',
        'design_1yr': 'rgb(31, 119, 180)',
        'design_10yr': 'rgb(255, 127, 14)',
        'extreme': 'rgb(214, 39, 40)'
    }

    # Add traces for each condition
    for condition, data in condition_groups.items():
        fig.add_trace(
            go.Scatterpolar(
                r=[1] * len(data['headings']),  # All at same radius
                theta=data['headings'],
                mode='markers',
                name=condition.replace('_', ' ').title(),
                marker=dict(
                    size=15,
                    color=color_map.get(condition, 'gray'),
                    symbol='circle'
                ),
                text=data['names'],
                hovertemplate='<b>%{text}</b><br>Heading: %{theta}°<br>Condition: ' + condition + '<extra></extra>'
            ),
            row=1, col=1
        )

    # Plot 2: Wind/Current/Wave alignment
    fig.add_trace(
        go.Scatterpolar(
            r=[1] * len(wave_dirs),
            theta=wave_dirs,
            mode='markers',
            name='Wave Direction',
            marker=dict(size=12, color='rgb(31, 119, 180)', symbol='circle'),
            hovertemplate='Wave: %{theta}°<extra></extra>'
        ),
        row=1, col=2
    )

    fig.add_trace(
        go.Scatterpolar(
            r=[0.85] * len(current_dirs),
            theta=current_dirs,
            mode='markers',
            name='Current Direction',
            marker=dict(size=10, color='rgb(44, 160, 44)', symbol='diamond'),
            hovertemplate='Current: %{theta}°<extra></extra>'
        ),
        row=1, col=2
    )

    fig.add_trace(
        go.Scatterpolar(
            r=[0.7] * len(wind_dirs),
            theta=wind_dirs,
            mode='markers',
            name='Wind Direction',
            marker=dict(size=8, color='rgb(255, 127, 14)', symbol='square'),
            hovertemplate='Wind: %{theta}°<extra></extra>'
        ),
        row=1, col=2
    )

    # Update layout
    fig.update_layout(
        title=dict(
            text=f'{project_name}<br><sub>Operability Analysis Load Case Configuration</sub>',
            font=dict(size=20),
            x=0.5,
            xanchor='center'
        ),
        showlegend=True,
        legend=dict(
            x=1.05,
            y=0.5,
            orientation='v'
        ),
        width=1400,
        height=600,
        template='plotly_white'
    )

    # Update polar plots
    fig.update_polars(
        radialaxis=dict(
            visible=False,
            range=[0, 1.2]
        ),
        angularaxis=dict(
            direction='clockwise',
            rotation=90,
            tickmode='linear',
            tick0=0,
            dtick=30
        ),
        row=1, col=1
    )

    fig.update_polars(
        radialaxis=dict(
            visible=False,
            range=[0, 1.2]
        ),
        angularaxis=dict(
            direction='clockwise',
            rotation=90,
            tickmode='linear',
            tick0=0,
            dtick=30
        ),
        row=1, col=2
    )

    # Save to HTML
    fig.write_html(
        str(output_path),
        include_plotlyjs='cdn',
        config={'displayModeBar': True, 'displaylogo': False}
    )

    print(f"✅ Load case visualization saved: {output_path}")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Visualize CALM Buoy operability load cases"
    )
    parser.add_argument(
        '--project',
        type=Path,
        required=True,
        help='Path to project directory'
    )
    parser.add_argument(
        '--output',
        type=Path,
        help='Output HTML file path (default: project/reports/load_cases_visualization.html)'
    )

    args = parser.parse_args()

    print("=" * 80)
    print("LOAD CASE VISUALIZATION")
    print("=" * 80)

    # Load configuration
    config_path = args.project / "project_config.yml"
    if not config_path.exists():
        print(f"❌ Configuration not found: {config_path}")
        return 1

    print(f"\n📖 Loading configuration: {config_path}")
    config = load_project_config(config_path)

    # Extract load cases
    load_cases = extract_load_cases(config)
    print(f"✅ Found {len(load_cases)} load cases")

    if not load_cases:
        print("❌ No load cases found in configuration")
        return 1

    # Print summary
    conditions = [lc.get('condition', 'unknown') for lc in load_cases]
    from collections import Counter
    condition_counts = Counter(conditions)

    print(f"\n📊 Load Case Summary:")
    for condition, count in condition_counts.items():
        print(f"   {condition}: {count} cases")

    # Determine output path
    if args.output:
        output_path = args.output
    else:
        reports_dir = args.project / "reports"
        reports_dir.mkdir(exist_ok=True)
        output_path = reports_dir / "load_cases_visualization.html"

    # Generate visualization
    project_name = config.get('human_input', {}).get('project', {}).get('name', 'CALM Buoy')
    print(f"\n📈 Generating visualization...")
    visualize_load_cases(load_cases, output_path, project_name)

    print("\n" + "=" * 80)
    print("✅ VISUALIZATION COMPLETE")
    print("=" * 80)
    print(f"\nVisualization: {output_path}")
    print(f"\nNext steps:")
    print(f"  1. Open in browser: {output_path}")
    print(f"  2. Review load case distribution")
    print(f"  3. Verify wind/current/wave alignment")

    return 0


if __name__ == "__main__":
    sys.exit(main())
