#!/usr/bin/env python3
"""Legacy Migration Utilities for Reservoir Domain.

This module provides utilities to migrate code and data from the legacy
reservoir domain structure to the new modernized reservoir analysis framework.

The original stratigraphic.py file contained valuable plotting functionality
that can be enhanced and integrated into the new StratigraphicAnalysis class.
"""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import numpy as np
from typing import Dict, List, Any, Optional, Tuple
import warnings

from .analysis import StratigraphicAnalysis, LogAnalysis, LogCurve
from .modeling import WellData


def migrate_legacy_stratigraphic_plot(
    wells_list: List[str],
    df_logs: pd.DataFrame,
    statdata: pd.DataFrame,
    output_path: Optional[str] = None
) -> plt.Figure:
    """Migrate and enhance legacy stratigraphic plotting functionality.
    
    This function takes the original plotting approach from stratigraphic.py
    and integrates it with the new StratigraphicAnalysis framework.
    
    Args:
        wells_list: List of well UWIs in desired cross-section order
        df_logs: DataFrame with well log data (UWI, Depth, GR, RT, RHOB, NPHI, etc.)
        statdata: DataFrame with formation tops (UWI, TOP, BASE)
        output_path: Optional path to save the plot
    
    Returns:
        Matplotlib figure object
    """
    print("Migrating legacy stratigraphic plotting to modern framework...")
    
    # Validate input data
    required_log_columns = ['UWI', 'Depth', 'GR', 'RT', 'RHOB', 'NPHI']
    missing_log_cols = [col for col in required_log_columns if col not in df_logs.columns]
    if missing_log_cols:
        raise ValueError(f"Missing required log columns: {missing_log_cols}")
    
    required_stat_columns = ['UWI', 'TOP', 'BASE']
    missing_stat_cols = [col for col in required_stat_columns if col not in statdata.columns]
    if missing_stat_cols:
        raise ValueError(f"Missing required formation top columns: {missing_stat_cols}")
    
    # Filter data for selected wells
    well_data_df = df_logs[df_logs['UWI'].isin(wells_list)].copy()
    tops_df = statdata[statdata['UWI'].isin(wells_list)].copy()
    
    if well_data_df.empty:
        raise ValueError("No log data found for specified wells")
    
    if tops_df.empty:
        warnings.warn("No formation top data found for specified wells")
    
    # Convert 'UWI' column to categorical with specified order
    well_data_df['UWI'] = pd.Categorical(well_data_df['UWI'], categories=wells_list, ordered=True)
    well_data_df = well_data_df.sort_values(by=['UWI', 'Depth']).reset_index(drop=True)
    
    # Create modern StratigraphicAnalysis object
    strat_analysis = StratigraphicAnalysis()
    
    # Process each well
    for well_id in wells_list:
        well_logs = well_data_df[well_data_df['UWI'] == well_id]
        
        if well_logs.empty:
            warnings.warn(f"No log data for well {well_id}")
            continue
        
        # Create log analysis for this well
        log_analysis = LogAnalysis(well_id)
        
        # Add log curves
        if 'GR' in well_logs.columns:
            gr_curve = LogCurve('GR', well_logs['Depth'].values, well_logs['GR'].values, 'API')
            log_analysis.add_curve(gr_curve)
        
        if 'RT' in well_logs.columns:
            rt_curve = LogCurve('RT', well_logs['Depth'].values, well_logs['RT'].values, 'ohm-m')
            log_analysis.add_curve(rt_curve)
        
        if 'RHOB' in well_logs.columns:
            rhob_curve = LogCurve('RHOB', well_logs['Depth'].values, well_logs['RHOB'].values, 'g/cm3')
            log_analysis.add_curve(rhob_curve)
        
        if 'NPHI' in well_logs.columns:
            nphi_curve = LogCurve('NPHI', well_logs['Depth'].values, well_logs['NPHI'].values, 'fraction')
            log_analysis.add_curve(nphi_curve)
        
        # Add facies if available
        if 'KMeans' in well_logs.columns:
            # Store facies data in the log analysis
            depths = well_logs['Depth'].values
            facies = well_logs['KMeans'].values
            
            # Create synthetic facies model to store the data
            log_analysis.facies_model = {
                'depths': depths,
                'facies': facies,
                'log_names': ['GR', 'RT', 'RHOB', 'NPHI'],
                'kmeans': None,  # Not available from legacy data
                'scaler': None,  # Not available from legacy data
                'cluster_centers': None  # Not available from legacy data
            }
        
        # Add to stratigraphic analysis
        strat_analysis.add_well_analysis(well_id, log_analysis)
        
        # Add formation tops if available
        well_tops = tops_df[tops_df['UWI'] == well_id]
        for _, top_row in well_tops.iterrows():
            if 'Formation' in top_row:
                formation_name = top_row['Formation']
            else:
                formation_name = f"Formation_{len(strat_analysis.correlation_markers.get(well_id, []))}"
            
            from .analysis import StratigraphicLayer
            marker = StratigraphicLayer(
                name=formation_name,
                top_depth=top_row['TOP'],
                base_depth=top_row['BASE']
            )
            
            strat_analysis.add_correlation_marker(well_id, marker)
    
    # Set well order
    strat_analysis.set_well_order(wells_list)
    
    # Create enhanced stratigraphic plot using new framework
    try:
        fig = strat_analysis.plot_stratigraphic_section(
            figsize=(18, 10),
            track_names=['GR', 'RT', 'RHOB_NPHI'],
            show_facies=True,
            flatten=False  # Keep original depths for legacy compatibility
        )
        
        # Add title
        fig.suptitle('Migrated Stratigraphic Cross-Section', fontsize=16, y=0.95)
        
        # Save if requested
        if output_path:
            fig.savefig(output_path, dpi=150, bbox_inches='tight')
            print(f"Enhanced stratigraphic plot saved to: {output_path}")
        
        return fig
        
    except Exception as e:
        print(f"Error creating enhanced plot: {e}")
        print("Falling back to legacy plotting method...")
        
        # Fallback to legacy-style plotting
        return _create_legacy_style_plot(wells_list, well_data_df, tops_df)


def _create_legacy_style_plot(
    wells_list: List[str],
    well_data_df: pd.DataFrame,
    tops_df: pd.DataFrame
) -> plt.Figure:
    """Create plot using legacy-style approach as fallback.
    
    This preserves the original plotting logic from stratigraphic.py
    but with improved error handling and documentation.
    """
    # Calculate thickness
    if not tops_df.empty:
        tops_df = tops_df.copy()
        tops_df['Thick'] = tops_df['BASE'] - tops_df['TOP']
        well_tops = tops_df.set_index("UWI")[["TOP", "BASE"]].to_dict(orient='index')
        well_thick = tops_df.groupby('UWI')["Thick"].apply(list).to_dict()
        
        # Determine depth limits based on tops data
        depth_min = tops_df["TOP"].min()
        depth_max = tops_df["BASE"].max()
    else:
        well_tops = {}
        well_thick = {}
        depth_min = well_data_df['Depth'].min()
        depth_max = well_data_df['Depth'].max()
    
    # Group the well data by UWI
    wells = well_data_df.groupby("UWI")
    
    # Define colors for FACIES values (if available)
    facies_colors = {
        0: 'magenta',
        1: 'blue',
        2: 'red',
        3: 'green',
        4: 'yellow',
        5: 'black'
    }
    color_list = [facies_colors[i] for i in sorted(facies_colors.keys())]
    cmap_facies = colors.ListedColormap(color_list)
    
    # Determine number of tracks
    n_wells = len(wells_list)
    tracks_per_well = 4  # GR, RT, RHOB/NPHI, FACIES
    
    # Create figure with tracks for each well
    width_ratios = [1, 0.8, 0.8, 0.6] * n_wells
    
    fig, axes = plt.subplots(
        nrows=1, 
        ncols=tracks_per_well * n_wells, 
        figsize=(18, 10),
        gridspec_kw={
            'hspace': 0, 
            'wspace': 0,
            'width_ratios': width_ratios
        }, 
        sharey=True
    )
    
    # Ensure axes is always iterable
    if n_wells == 1:
        axes = [axes] if tracks_per_well == 1 else axes
    
    tops_and_bases = []
    
    # Loop through each well group to plot logs
    for i, (well_name, df) in enumerate(wells):
        if well_name not in wells_list:
            continue
        
        # Get the top and base depths for alignment
        if well_name in well_tops:
            well_info = well_tops[well_name]
            top_depth = well_info['TOP']
            base_depth = well_info['BASE']
        else:
            top_depth = df['Depth'].min()
            base_depth = df['Depth'].max()
        
        # Store top and base for connecting lines
        tops_and_bases.append((well_name, top_depth, base_depth, i))
        
        # Adjust depths to align at the tops (if flattening was requested)
        df = df.copy()
        df['Adj_Depth'] = df['Depth'] - top_depth
        
        # Track indexes for each log type
        well_index = wells_list.index(well_name)
        track_gr = well_index * 4
        track_rt = well_index * 4 + 1
        track_rhob_nphi = well_index * 4 + 2
        track_facies = well_index * 4 + 3
        
        # Plot GR in the first track
        if 'GR' in df.columns:
            axes[track_gr].plot(df['GR'], df['Adj_Depth'], color='green', label='GR')
            # Shade the GR track
            axes[track_gr].fill_betweenx(
                df['Adj_Depth'], df['GR'], 30, 
                where=(df['GR'] < 30), color='yellow', alpha=0.5
            )
            axes[track_gr].fill_betweenx(
                df['Adj_Depth'], 30, df['GR'], 
                where=(df['GR'] > 30), color='grey', alpha=0.5
            )
            axes[track_gr].set_xlim(0, 150)
            axes[track_gr].legend(loc="upper right")
        
        axes[track_gr].invert_yaxis()
        axes[track_gr].set_title(f"{well_name}")
        
        # Plot RT in the second track
        if 'RT' in df.columns:
            rt_values = df['RT'].values
            valid_rt = rt_values > 0
            
            if np.any(valid_rt):
                axes[track_rt].plot(rt_values, df['Adj_Depth'], color='black', label='RT')
                
                # Fill with color gradient
                norm = plt.Normalize(rt_values[valid_rt].min(), rt_values[valid_rt].max())
                colors_rt = plt.cm.rainbow(norm(rt_values))
                
                for j in range(len(df) - 1):
                    if valid_rt[j] and valid_rt[j+1]:
                        axes[track_rt].fill_betweenx(
                            df['Adj_Depth'].iloc[j:j + 2],
                            0, rt_values[j:j + 2],
                            color=colors_rt[j], edgecolor='none'
                        )
                
                axes[track_rt].set_xlim(1, 1000)
                axes[track_rt].set_xscale('log')
                axes[track_rt].legend(loc="upper right")
        
        axes[track_rt].invert_yaxis()
        
        # Plot RHOB and NPHI in third track
        if 'RHOB' in df.columns and 'NPHI' in df.columns:
            # Calculate density porosity for shale/hydrocarbon indication
            df['DPHI'] = (2.71 - df['RHOB']) / (2.71 - 1)
            
            # Plot RHOB
            secax_rhob = axes[track_rhob_nphi].twiny()
            secax_rhob.plot(df['RHOB'], df['Adj_Depth'], color='red', label='RHOB')
            secax_rhob.invert_yaxis()
            secax_rhob.set_xlim(1.95, 2.95)
            secax_rhob.legend(loc="upper right")
            
            # Plot NPHI
            secax_nphi = axes[track_rhob_nphi].twiny()
            secax_nphi.plot(df['NPHI'], df['Adj_Depth'], color='blue', label='NPHI')
            secax_nphi.set_xlim(0.45, -0.15)
            secax_nphi.legend(loc="upper left")
            
            # Fill between curves for hydrocarbon/shale indication
            secax_nphi.fill_betweenx(
                df['Adj_Depth'], df['DPHI'], df['NPHI'],
                where=(df['DPHI'] > df['NPHI']), color="yellow", alpha=0.7
            )
            secax_nphi.fill_betweenx(
                df['Adj_Depth'], df['DPHI'], df['NPHI'],
                where=(df['DPHI'] < df['NPHI']), color="grey", alpha=0.7
            )
        
        # Plot facies if available
        if 'KMeans' in df.columns:
            facies_data = df[['KMeans']].values
            axes[track_facies].imshow(
                facies_data, aspect='auto', cmap=cmap_facies,
                extent=[-1, 1, df['Adj_Depth'].max(), df['Adj_Depth'].min()]
            )
            axes[track_facies].set_xlabel("FACIES")
        
        axes[track_facies].invert_yaxis()
        
        # Remove x-axis labels from most tracks
        for ax in [axes[track_gr], axes[track_rhob_nphi], axes[track_rt], axes[track_facies]]:
            ax.set_xticks([])
            ax.set_xlabel("")
        
        # Plot formation markers if available
        if well_name in well_thick:
            for thickness in well_thick[well_name]:
                for ax_idx in [track_gr, track_rt, track_rhob_nphi]:
                    axes[ax_idx].axhline(y=thickness, color='purple', linestyle='--', linewidth=2)
    
    # Connect formation bases between wells
    if len(tops_and_bases) > 1:
        for i in range(len(tops_and_bases) - 1):
            current_well_name, _, current_base_depth, current_idx = tops_and_bases[i]
            next_well_name, _, next_base_depth, next_idx = tops_and_bases[i + 1]
            
            if current_well_name in well_tops and next_well_name in well_tops:
                adjusted_current_base = (current_base_depth - 
                                       well_tops[current_well_name]['TOP'])
                adjusted_next_base = (next_base_depth - 
                                    well_tops[next_well_name]['TOP'])
                
                # Draw connection line
                current_facies_track = current_idx * 4 + 3
                if current_facies_track < len(axes):
                    axes[current_facies_track].plot(
                        [-1, 1], [adjusted_current_base, adjusted_next_base], 
                        color='purple', linestyle='--', linewidth=2
                    )
    
    # Draw flattening line at top
    for ax in axes:
        ax.axhline(y=0, color='black', linewidth=1.5)
    
    plt.tight_layout()
    plt.suptitle('Legacy Stratigraphic Cross-Section (Fallback)', fontsize=14, y=0.98)
    
    return fig


def extract_legacy_data_patterns(
    df_logs: pd.DataFrame,
    statdata: pd.DataFrame
) -> Dict[str, Any]:
    """Extract and analyze patterns from legacy data for migration guidance.
    
    Args:
        df_logs: Legacy log data DataFrame
        statdata: Legacy formation tops DataFrame
    
    Returns:
        Dictionary with analysis of data patterns and migration recommendations
    """
    analysis = {
        'log_data': {},
        'formation_tops': {},
        'recommendations': []
    }
    
    # Analyze log data
    if not df_logs.empty:
        analysis['log_data'] = {
            'total_wells': df_logs['UWI'].nunique() if 'UWI' in df_logs.columns else 0,
            'total_depth_points': len(df_logs),
            'available_curves': [col for col in df_logs.columns if col not in ['UWI', 'Depth']],
            'depth_range': (df_logs['Depth'].min(), df_logs['Depth'].max()) if 'Depth' in df_logs.columns else None,
            'well_list': df_logs['UWI'].unique().tolist() if 'UWI' in df_logs.columns else []
        }
        
        # Check for facies data
        if 'KMeans' in df_logs.columns:
            analysis['log_data']['facies_available'] = True
            analysis['log_data']['facies_classes'] = sorted(df_logs['KMeans'].unique())
        else:
            analysis['log_data']['facies_available'] = False
        
        # Recommendations for log data
        if len(analysis['log_data']['available_curves']) >= 4:
            analysis['recommendations'].append(
                "Sufficient log curves available for full petrophysical analysis"
            )
        else:
            analysis['recommendations'].append(
                "Limited log curves - consider adding synthetic curves for complete analysis"
            )
    
    # Analyze formation tops
    if not statdata.empty:
        analysis['formation_tops'] = {
            'total_wells': statdata['UWI'].nunique() if 'UWI' in statdata.columns else 0,
            'total_formations': len(statdata),
            'formation_names': statdata['Formation'].unique().tolist() if 'Formation' in statdata.columns else [],
            'depth_range': (statdata['TOP'].min(), statdata['BASE'].max()) if 'TOP' in statdata.columns and 'BASE' in statdata.columns else None
        }
        
        if 'TOP' in statdata.columns and 'BASE' in statdata.columns:
            statdata_copy = statdata.copy()
            statdata_copy['Thickness'] = statdata_copy['BASE'] - statdata_copy['TOP']
            analysis['formation_tops']['average_thickness'] = statdata_copy['Thickness'].mean()
            analysis['formation_tops']['thickness_range'] = (statdata_copy['Thickness'].min(), statdata_copy['Thickness'].max())
        
        # Recommendations for formation tops
        if analysis['formation_tops']['total_formations'] > 0:
            analysis['recommendations'].append(
                "Formation top data available - can create stratigraphic correlations"
            )
        else:
            analysis['recommendations'].append(
                "No formation top data - consider adding structural markers"
            )
    
    # Overall recommendations
    if analysis['log_data'].get('total_wells', 0) > 1:
        analysis['recommendations'].append(
            "Multiple wells available - suitable for cross-section analysis"
        )
    
    if analysis['log_data'].get('facies_available', False):
        analysis['recommendations'].append(
            "Facies data present - can enhance stratigraphic interpretation"
        )
    
    return analysis


def create_migration_report(
    wells_list: List[str],
    df_logs: pd.DataFrame,
    statdata: pd.DataFrame,
    output_file: Optional[str] = None
) -> str:
    """Create a detailed migration report for legacy reservoir data.
    
    Args:
        wells_list: List of wells to analyze
        df_logs: Legacy log data
        statdata: Legacy formation tops
        output_file: Optional file path to save report
    
    Returns:
        Report text
    """
    analysis = extract_legacy_data_patterns(df_logs, statdata)
    
    report_lines = [
        "RESERVOIR DOMAIN MIGRATION REPORT",
        "=" * 50,
        "",
        f"Analysis Date: {pd.Timestamp.now().strftime('%Y-%m-%d %H:%M:%S')}",
        f"Target Wells: {', '.join(wells_list)}",
        "",
        "LOG DATA ANALYSIS:",
        "-" * 20
    ]
    
    log_data = analysis['log_data']
    if log_data:
        report_lines.extend([
            f"Total Wells: {log_data.get('total_wells', 0)}",
            f"Total Depth Points: {log_data.get('total_depth_points', 0):,}",
            f"Available Curves: {', '.join(log_data.get('available_curves', []))}",
            f"Depth Range: {log_data.get('depth_range', 'Unknown')}",
            f"Facies Available: {log_data.get('facies_available', False)}"
        ])
        
        if log_data.get('facies_available', False):
            report_lines.append(f"Facies Classes: {log_data.get('facies_classes', [])}")
    else:
        report_lines.append("No log data found")
    
    report_lines.extend([
        "",
        "FORMATION TOPS ANALYSIS:",
        "-" * 25
    ])
    
    tops_data = analysis['formation_tops']
    if tops_data:
        report_lines.extend([
            f"Total Wells with Tops: {tops_data.get('total_wells', 0)}",
            f"Total Formations: {tops_data.get('total_formations', 0)}",
            f"Formation Names: {', '.join(tops_data.get('formation_names', []))}",
            f"Depth Range: {tops_data.get('depth_range', 'Unknown')}"
        ])
        
        if 'average_thickness' in tops_data:
            report_lines.extend([
                f"Average Thickness: {tops_data['average_thickness']:.1f} ft",
                f"Thickness Range: {tops_data['thickness_range']}"
            ])
    else:
        report_lines.append("No formation top data found")
    
    report_lines.extend([
        "",
        "MIGRATION RECOMMENDATIONS:",
        "-" * 28
    ])
    
    for i, recommendation in enumerate(analysis['recommendations'], 1):
        report_lines.append(f"{i}. {recommendation}")
    
    report_lines.extend([
        "",
        "MIGRATION STEPS:",
        "-" * 16,
        "1. Use migrate_legacy_stratigraphic_plot() to create enhanced visualizations",
        "2. Import data into LogAnalysis objects for petrophysical calculations", 
        "3. Use StratigraphicAnalysis for correlation and mapping",
        "4. Apply ReservoirModel for integrated analysis",
        "5. Validate results against original legacy plots",
        "",
        "EXAMPLE USAGE:",
        "-" * 14,
        "from digitalmodel.reservoir.legacy_migration import migrate_legacy_stratigraphic_plot",
        "fig = migrate_legacy_stratigraphic_plot(wells_list, df_logs, statdata)",
        "plt.show()"
    ])
    
    report_text = "\n".join(report_lines)
    
    if output_file:
        with open(output_file, 'w') as f:
            f.write(report_text)
        print(f"Migration report saved to: {output_file}")
    
    return report_text


if __name__ == "__main__":
    print("Legacy Reservoir Domain Migration Utilities")
    print("This module provides tools to migrate from legacy stratigraphic.py")
    print("to the modernized reservoir analysis framework.")
    print("\nExample usage:")
    print("  from digitalmodel.reservoir.legacy_migration import migrate_legacy_stratigraphic_plot")
    print("  fig = migrate_legacy_stratigraphic_plot(wells_list, df_logs, statdata)")
