"""
ABOUTME: Interactive RAO comparison plots for AQWA vs OrcaWave
         Creates Plotly visualizations with heading selection and CSV data export
"""

import sys
from pathlib import Path
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from loguru import logger

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent / "src"))

from digitalmodel.modules.diffraction.aqwa_lis_parser import AQWALISParser
import OrcFxAPI as ofx


def extract_aqwa_raos(lis_file: Path) -> pd.DataFrame:
    """Extract AQWA RAO data and convert to DataFrame."""
    logger.info(f"Extracting AQWA RAO data from {lis_file.name}")

    parser = AQWALISParser(str(lis_file))
    rao_dict = parser.parse_rao_table()

    # Convert to DataFrame
    rows = []
    for (freq, heading), raos in rao_dict.items():
        # Convert angular frequency (rad/s) to period (s)
        period = 2 * np.pi / freq

        row = {
            'frequency_rad_s': freq,
            'period_s': period,
            'heading_deg': heading,
            'surge_amp': raos['surge'][0],
            'surge_phase': raos['surge'][1],
            'sway_amp': raos['sway'][0],
            'sway_phase': raos['sway'][1],
            'heave_amp': raos['heave'][0],
            'heave_phase': raos['heave'][1],
            'roll_amp': np.degrees(raos['roll'][0]),  # Convert rad/m to deg/m
            'roll_phase': raos['roll'][1],
            'pitch_amp': np.degrees(raos['pitch'][0]),  # Convert rad/m to deg/m
            'pitch_phase': raos['pitch'][1],
            'yaw_amp': np.degrees(raos['yaw'][0]),  # Convert rad/m to deg/m
            'yaw_phase': raos['yaw'][1],
        }
        rows.append(row)

    df = pd.DataFrame(rows)
    logger.info(f"Extracted {len(df)} AQWA RAO data points")
    logger.info(f"  Headings: {sorted(df['heading_deg'].unique())}")
    logger.info(f"  Periods: {df['period_s'].min():.2f}s - {df['period_s'].max():.2f}s")

    return df


def extract_orcawave_raos(owr_file: Path) -> pd.DataFrame:
    """Extract OrcaWave RAO data and convert to DataFrame."""
    logger.info(f"Extracting OrcaWave RAO data from {owr_file.name}")

    # Load diffraction results
    diffraction = ofx.Diffraction()
    diffraction.LoadResults(str(owr_file.absolute()))

    # Extract arrays
    frequencies = diffraction.frequencies
    periods = diffraction.periods
    headings = diffraction.headings

    # Get displacement RAOs: shape (n_headings, n_periods, 6 DOFs)
    # Each DOF has complex values
    motion_raos = diffraction.displacementRAOs
    logger.info(f"✓ Extracted displacement RAOs")
    logger.info(f"  Shape: {motion_raos.shape}")

    # Convert complex to amplitude and phase
    rows = []
    for i, heading in enumerate(headings):
        for j, period in enumerate(periods):
            # Extract complex RAOs for this heading and period
            # motion_raos shape: (n_headings, n_periods, 6 DOFs)
            surge_complex = motion_raos[i, j, 0]
            sway_complex = motion_raos[i, j, 1]
            heave_complex = motion_raos[i, j, 2]
            roll_complex = motion_raos[i, j, 3]
            pitch_complex = motion_raos[i, j, 4]
            yaw_complex = motion_raos[i, j, 5]

            # Calculate amplitude and phase
            row = {
                'period_s': period,
                'frequency_rad_s': 2 * np.pi / period,
                'heading_deg': heading,
                'surge_amp': abs(surge_complex),
                'surge_phase': np.degrees(np.angle(surge_complex)),
                'sway_amp': abs(sway_complex),
                'sway_phase': np.degrees(np.angle(sway_complex)),
                'heave_amp': abs(heave_complex),
                'heave_phase': np.degrees(np.angle(heave_complex)),
                'roll_amp': np.degrees(abs(roll_complex)),  # Convert rad/m to deg/m
                'roll_phase': np.degrees(np.angle(roll_complex)),
                'pitch_amp': np.degrees(abs(pitch_complex)),  # Convert rad/m to deg/m
                'pitch_phase': np.degrees(np.angle(pitch_complex)),
                'yaw_amp': np.degrees(abs(yaw_complex)),  # Convert rad/m to deg/m
                'yaw_phase': np.degrees(np.angle(yaw_complex)),
            }
            rows.append(row)

    df = pd.DataFrame(rows)
    logger.info(f"Extracted {len(df)} OrcaWave RAO data points")
    logger.info(f"  Headings: {sorted(df['heading_deg'].unique())}")
    logger.info(f"  Periods: {df['period_s'].min():.2f}s - {df['period_s'].max():.2f}s")

    return df


def create_interactive_comparison_plot(
    aqwa_df: pd.DataFrame,
    orcawave_df: pd.DataFrame,
    output_html: Path,
    output_csv_dir: Path
):
    """Create interactive Plotly comparison plots."""
    logger.info("Creating interactive comparison plots")

    # Save data to CSV
    output_csv_dir.mkdir(exist_ok=True)
    aqwa_csv = output_csv_dir / "aqwa_rao_data.csv"
    orcawave_csv = output_csv_dir / "orcawave_rao_data.csv"

    aqwa_df.to_csv(aqwa_csv, index=False)
    orcawave_df.to_csv(orcawave_csv, index=False)
    logger.info(f"Saved CSV data: {aqwa_csv.name}, {orcawave_csv.name}")

    # Get common headings
    aqwa_headings = sorted(aqwa_df['heading_deg'].unique())
    orcawave_headings = sorted(orcawave_df['heading_deg'].unique())

    # Find closest OrcaWave headings to AQWA headings
    heading_pairs = []
    for aqwa_h in aqwa_headings:
        # Find closest OrcaWave heading
        closest_ow = min(orcawave_headings, key=lambda x: abs(x - aqwa_h))
        if abs(closest_ow - aqwa_h) <= 5.0:  # Within 5 degrees
            heading_pairs.append((aqwa_h, closest_ow))

    logger.info(f"Found {len(heading_pairs)} matching heading pairs")

    # DOF names and units
    dofs = [
        ('surge', 'Surge', 'm/m'),
        ('sway', 'Sway', 'm/m'),
        ('heave', 'Heave', 'm/m'),
        ('roll', 'Roll', 'deg/m'),
        ('pitch', 'Pitch', 'deg/m'),
        ('yaw', 'Yaw', 'deg/m'),
    ]

    # Calculate y-axis ranges for each DOF (minimum 0.2 range to avoid noise)
    y_ranges = {}
    min_range = 0.2
    for dof_name, dof_label, dof_unit in dofs:
        # Get all amplitude values for this DOF
        aqwa_vals = aqwa_df[f'{dof_name}_amp'].values
        orcawave_vals = orcawave_df[f'{dof_name}_amp'].values
        all_vals = np.concatenate([aqwa_vals, orcawave_vals])

        max_val = np.max(all_vals)
        # Set range to at least min_range, or higher if data requires
        y_max = max(min_range, max_val * 1.1)  # Add 10% margin
        y_ranges[dof_name] = [0, y_max]
        logger.info(f"  {dof_label}: y-axis range [0, {y_max:.3f}]")

    # Create subplots: 3 rows x 2 columns
    fig = make_subplots(
        rows=3, cols=2,
        subplot_titles=[dof[1] for dof in dofs],  # Plain text, will bold with annotations
        vertical_spacing=0.12,
        horizontal_spacing=0.10,
    )

    # Define colors for different headings
    colors = [
        '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
        '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22'
    ]

    # Create dropdown menu for heading selection
    buttons = []

    # Add "All Headings" option
    all_visible = []

    for heading_idx, (aqwa_h, orcawave_h) in enumerate(heading_pairs):
        # Filter data for this heading
        aqwa_data = aqwa_df[aqwa_df['heading_deg'] == aqwa_h].sort_values('period_s')
        orcawave_data = orcawave_df[orcawave_df['heading_deg'] == orcawave_h].sort_values('period_s')

        color = colors[heading_idx % len(colors)]

        # Add traces for each DOF
        for dof_idx, (dof_name, dof_label, dof_unit) in enumerate(dofs):
            row = (dof_idx // 2) + 1
            col = (dof_idx % 2) + 1

            # AQWA trace
            fig.add_trace(
                go.Scatter(
                    x=aqwa_data['period_s'],
                    y=aqwa_data[f'{dof_name}_amp'],
                    mode='lines+markers',
                    name=f'AQWA {aqwa_h}°',
                    line=dict(color=color, width=2),
                    marker=dict(size=6, symbol='circle'),
                    legendgroup=f'heading_{heading_idx}',
                    showlegend=(dof_idx == 0),  # Only show in legend once
                    visible=(heading_idx == 0),  # Initially show first heading only
                    hovertemplate=(
                        f'<b>AQWA {aqwa_h}°</b><br>'
                        'Period: %{x:.2f}s<br>'
                        f'{dof_label}: %{{y:.4f}} {dof_unit}<br>'
                        '<extra></extra>'
                    ),
                ),
                row=row, col=col
            )

            # OrcaWave trace
            fig.add_trace(
                go.Scatter(
                    x=orcawave_data['period_s'],
                    y=orcawave_data[f'{dof_name}_amp'],
                    mode='lines',
                    name=f'OrcaWave {orcawave_h}°',
                    line=dict(color=color, width=2, dash='dash'),
                    legendgroup=f'heading_{heading_idx}',
                    showlegend=(dof_idx == 0),
                    visible=(heading_idx == 0),
                    hovertemplate=(
                        f'<b>OrcaWave {orcawave_h}°</b><br>'
                        'Period: %{x:.2f}s<br>'
                        f'{dof_label}: %{{y:.4f}} {dof_unit}<br>'
                        '<extra></extra>'
                    ),
                ),
                row=row, col=col
            )

        # Create visibility array for this heading (12 traces = 6 DOFs × 2 sources)
        visible = [False] * len(heading_pairs) * 12
        for i in range(12):
            visible[heading_idx * 12 + i] = True

        buttons.append(
            dict(
                label=f"{aqwa_h}° / {orcawave_h}°",
                method="update",
                args=[{"visible": visible}]
            )
        )

        # Track for "All Headings" option
        all_visible.extend([True] * 12)

    # Add "All Headings" button at the beginning
    buttons.insert(0, dict(
        label="All Headings",
        method="update",
        args=[{"visible": all_visible}]
    ))

    # Update layout
    fig.update_layout(
        title={
            'text': 'AQWA vs OrcaWave Displacement RAO Comparison<br><sub>Interactive plot with heading selection</sub>',
            'x': 0.5,
            'xanchor': 'center',
            'font': {'size': 20}
        },
        height=1000,
        width=1400,
        showlegend=True,
        legend=dict(
            orientation="v",
            yanchor="top",
            y=1.0,
            xanchor="left",
            x=1.02,
            font=dict(size=10)
        ),
        hovermode='closest',
        updatemenus=[
            dict(
                buttons=buttons,
                direction="down",
                showactive=True,
                x=0.02,
                xanchor="left",
                y=1.15,
                yanchor="top",
                bgcolor="white",
                bordercolor="gray",
                borderwidth=1,
            )
        ],
        annotations=[
            dict(
                text="Select Heading:",
                x=0.02,
                xanchor="left",
                y=1.18,
                yanchor="top",
                showarrow=False,
                font=dict(size=12, color="black"),
            )
        ]
    )

    # Update axes labels and ranges with clear DOF labels
    for dof_idx, (dof_name, dof_label, dof_unit) in enumerate(dofs):
        row = (dof_idx // 2) + 1
        col = (dof_idx % 2) + 1

        # Update x-axis with bold title
        fig.update_xaxes(
            title_text="Period (s)",
            title_font=dict(size=14, family="Arial, sans-serif"),
            title_standoff=10,
            row=row, col=col,
            gridcolor='lightgray',
            showgrid=True
        )

        # Update y-axis with DOF-specific range and bold label
        fig.update_yaxes(
            title_text=f"{dof_label} ({dof_unit})",
            title_font=dict(size=14, family="Arial, sans-serif"),
            title_standoff=10,
            row=row, col=col,
            gridcolor='lightgray',
            showgrid=True,
            range=y_ranges[dof_name]  # Set consistent y-axis range
        )

    # Update subplot titles to be bold and larger
    # First 6 annotations are the subplot titles
    for i, annotation in enumerate(fig['layout']['annotations'][:6]):
        annotation['font'] = dict(
            size=18,
            family="Arial, sans-serif",
            color="black"
        )
        # Make text bold by wrapping in HTML
        annotation['text'] = f"<b>{annotation['text']}</b>"

    # Save HTML
    fig.write_html(
        str(output_html),
        include_plotlyjs='cdn',
        config={
            'displayModeBar': True,
            'displaylogo': False,
            'toImageButtonOptions': {
                'format': 'png',
                'filename': 'rao_comparison',
                'height': 1000,
                'width': 1400,
                'scale': 2
            }
        }
    )

    logger.success(f"Interactive plot saved: {output_html}")
    logger.info(f"  - {len(heading_pairs)} heading pairs")
    logger.info(f"  - 6 DOFs per heading")
    logger.info(f"  - Dropdown menu for heading selection")


def main():
    """Main execution function."""
    benchmark_dir = Path(__file__).parent

    # Input files
    lis_file = benchmark_dir / "001_SHIP_RAOS_REV2.LIS"
    owr_file = benchmark_dir / "orcawave_001_ship_raos_rev2_matched.owr"

    # Output files
    output_html = benchmark_dir / "interactive_rao_comparison.html"
    output_csv_dir = benchmark_dir / "data"

    logger.info("="*80)
    logger.info("INTERACTIVE RAO COMPARISON PLOT")
    logger.info("="*80)

    # Extract data
    aqwa_df = extract_aqwa_raos(lis_file)
    orcawave_df = extract_orcawave_raos(owr_file)

    # Create plots
    create_interactive_comparison_plot(aqwa_df, orcawave_df, output_html, output_csv_dir)

    logger.success("="*80)
    logger.success("INTERACTIVE PLOT COMPLETE!")
    logger.success(f"HTML Report: {output_html}")
    logger.success(f"CSV Data: {output_csv_dir}")
    logger.success("="*80)


if __name__ == "__main__":
    main()
