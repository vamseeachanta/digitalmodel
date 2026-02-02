"""
ABOUTME: Interactive HTML report generator for cathodic protection analysis
ABOUTME: Generates Plotly-based dashboards with DNV RP-F103 comparison visualizations
"""

import plotly.graph_objects as go
from plotly.subplots import make_subplots
import pandas as pd
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, Optional, List
import json


class CPHTMLReportGenerator:
    """Generate interactive HTML reports for cathodic protection analysis."""

    def __init__(self, output_dir: str = "reports/cp"):
        """
        Initialize report generator.

        Args:
            output_dir: Directory for output reports and data files
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

        # Create data subdirectory for CSV exports
        self.data_dir = self.output_dir / "data"
        self.data_dir.mkdir(exist_ok=True)

    def _normalize_results(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """
        Normalize CP results to handle both direct and nested structures.

        This method solves the zero-values bug where DNV_RP_F103_2010 returns
        nested results under 'results' key, but report generator expects flat structure.

        Args:
            results: Raw results from DNV_RP_F103_2010

        Returns:
            Normalized results dictionary with standard keys

        Example nested structure:
            results['results']['current_demand_A']['mean_current_demand_A']

        Example flat structure (output):
            results['current_demand']['mean_A']
        """
        # Check if results are nested under 'results' key
        if 'results' in results and isinstance(results['results'], dict):
            nested = results['results']
            inputs = results.get('inputs', {})
        else:
            nested = results
            inputs = {}

        # Extract with actual key names from DNV method
        return {
            'design_life_years': nested.get('design_life_years', 25.0),
            'geometry': nested.get('pipeline_geometry_m', {}),
            'attenuation': {
                'attenuation_length_m': nested.get('anode_spacing_m', {}).get('spacing_m', 0) * 10,
            },
            'current_demand': {
                'initial_A': nested.get('current_demand_A', {}).get('initial_current_demand_A', 0),
                'mean_A': nested.get('current_demand_A', {}).get('mean_current_demand_A', 0),
                'final_A': nested.get('current_demand_A', {}).get('final_current_demand_A', 0),
            },
            'coating_breakdown': nested.get('coating_breakdown_factors', {}),
            'anode_requirements': nested.get('anode_requirements', {}),
            'inputs': inputs,
        }

    def generate_attenuation_report(
        self,
        results: Dict[str, Any],
        comparison_results: Optional[Dict[str, Any]] = None,
        title: str = "Cathodic Protection Analysis"
    ) -> Path:
        """
        Generate comprehensive CP analysis report with interactive charts.

        Args:
            results: Primary calculation results (e.g., DNV 2010)
            comparison_results: Optional comparison results (e.g., DNV 2016)
            title: Report title

        Returns:
            Path to generated HTML report
        """
        # Normalize results structure
        results = self._normalize_results(results)
        if comparison_results:
            comparison_results = self._normalize_results(comparison_results)

        # Create timestamp for filename
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = self.output_dir / f"cp_analysis_{timestamp}.html"

        # Create 6-panel subplot layout (3 rows x 2 columns)
        fig = make_subplots(
            rows=3, cols=2,
            subplot_titles=(
                'Pipeline Geometry',
                'Attenuation Length',
                'Current Demand Profile',
                'Coating Breakdown Factors',
                'Anode Requirements',
                'Design Parameters'
            ),
            specs=[
                [{"type": "bar"}, {"type": "bar"}],
                [{"type": "scatter"}, {"type": "bar"}],
                [{"type": "bar"}, {"type": "table"}]
            ],
            vertical_spacing=0.12,
            horizontal_spacing=0.15
        )

        # Panel 1: Pipeline Geometry
        geometry = results.get('geometry', {})
        geo_labels = ['Outer Diameter (m)', 'Wall Thickness (m)', 'Length (m)']
        geo_values = [
            geometry.get('outer_diameter_m', 0),
            geometry.get('wall_thickness_m', 0),
            geometry.get('length_m', 0) / 1000  # Convert to km for display
        ]

        fig.add_trace(
            go.Bar(
                x=geo_labels,
                y=geo_values,
                name='Primary',
                marker_color='steelblue',
                text=[f'{v:.3f}' for v in geo_values],
                textposition='outside'
            ),
            row=1, col=1
        )

        # Panel 2: Attenuation Length Comparison
        attn_primary = results.get('attenuation', {}).get('attenuation_length_m', 0)
        attn_labels = ['DNV 2010']
        attn_values = [attn_primary]

        if comparison_results:
            attn_comparison = comparison_results.get('attenuation', {}).get('attenuation_length_m', 0)
            attn_labels.append('DNV 2016')
            attn_values.append(attn_comparison)

        fig.add_trace(
            go.Bar(
                x=attn_labels,
                y=attn_values,
                name='Attenuation',
                marker_color=['steelblue', 'coral'] if comparison_results else ['steelblue'],
                text=[f'{v:.2f} m' for v in attn_values],
                textposition='outside'
            ),
            row=1, col=2
        )

        # Panel 3: Current Demand Profile
        current = results.get('current_demand', {})
        time_points = [0, results.get('design_life_years', 25) / 2, results.get('design_life_years', 25)]
        current_values = [
            current.get('initial_A', 0),
            current.get('mean_A', 0),
            current.get('final_A', 0)
        ]

        fig.add_trace(
            go.Scatter(
                x=time_points,
                y=current_values,
                mode='lines+markers',
                name='Primary',
                line=dict(color='steelblue', width=2),
                marker=dict(size=8)
            ),
            row=2, col=1
        )

        if comparison_results:
            comp_current = comparison_results.get('current_demand', {})
            comp_values = [
                comp_current.get('initial_A', 0),
                comp_current.get('mean_A', 0),
                comp_current.get('final_A', 0)
            ]
            fig.add_trace(
                go.Scatter(
                    x=time_points,
                    y=comp_values,
                    mode='lines+markers',
                    name='Comparison',
                    line=dict(color='coral', width=2, dash='dash'),
                    marker=dict(size=8)
                ),
                row=2, col=1
            )

        # Panel 4: Coating Breakdown Factors
        coating = results.get('coating_breakdown', {})
        coating_labels = ['Initial Factor', 'Final Factor']
        coating_values = [
            coating.get('initial_factor', 0),
            coating.get('final_factor', 0)
        ]

        fig.add_trace(
            go.Bar(
                x=coating_labels,
                y=coating_values,
                name='Primary',
                marker_color='steelblue',
                text=[f'{v:.6f}' for v in coating_values],
                textposition='outside'
            ),
            row=2, col=2
        )

        if comparison_results:
            comp_coating = comparison_results.get('coating_breakdown', {})
            comp_coating_values = [
                comp_coating.get('initial_factor', 0),
                comp_coating.get('final_factor', 0)
            ]
            fig.add_trace(
                go.Bar(
                    x=coating_labels,
                    y=comp_coating_values,
                    name='Comparison',
                    marker_color='coral',
                    text=[f'{v:.6f}' for v in comp_coating_values],
                    textposition='outside'
                ),
                row=2, col=2
            )

        # Panel 5: Anode Requirements
        anodes = results.get('anode_requirements', {})
        anode_labels = ['Total Mass (kg)', 'Number of Anodes']
        anode_values = [
            anodes.get('total_anode_mass_kg', 0),
            anodes.get('anode_count', 0)
        ]

        fig.add_trace(
            go.Bar(
                x=anode_labels,
                y=anode_values,
                name='Primary',
                marker_color='steelblue',
                text=[f'{v:.0f}' for v in anode_values],
                textposition='outside'
            ),
            row=3, col=1
        )

        if comparison_results:
            comp_anodes = comparison_results.get('anode_requirements', {})
            comp_anode_values = [
                comp_anodes.get('total_anode_mass_kg', 0),
                comp_anodes.get('anode_count', 0)
            ]
            fig.add_trace(
                go.Bar(
                    x=anode_labels,
                    y=comp_anode_values,
                    name='Comparison',
                    marker_color='coral',
                    text=[f'{v:.0f}' for v in comp_anode_values],
                    textposition='outside'
                ),
                row=3, col=1
            )

        # Panel 6: Design Parameters Table
        inputs = results.get('inputs', {})
        pipeline = inputs.get('pipeline', {})
        environment = inputs.get('environment', {})

        table_data = [
            ['Design Life', f"{results.get('design_life_years', 25)} years"],
            ['Outer Diameter', f"{geometry.get('outer_diameter_m', 0):.3f} m"],
            ['Length', f"{geometry.get('length_m', 0)/1000:.2f} km"],
            ['Seawater Resistivity', f"{environment.get('seawater_resistivity_ohm_cm', 0)} Ω·cm"],
            ['Temperature', f"{environment.get('seawater_temperature_C', 0)}°C"],
            ['Coating Quality', pipeline.get('coating_quality', 'N/A')],
            ['Wet Storage', f"{pipeline.get('wet_storage_years', 0)} years"]
        ]

        fig.add_trace(
            go.Table(
                header=dict(
                    values=['<b>Parameter</b>', '<b>Value</b>'],
                    fill_color='steelblue',
                    font=dict(color='white', size=12),
                    align='left'
                ),
                cells=dict(
                    values=[[row[0] for row in table_data], [row[1] for row in table_data]],
                    fill_color='lavender',
                    align='left'
                )
            ),
            row=3, col=2
        )

        # Update layout
        fig.update_layout(
            title=dict(
                text=f"<b>{title}</b><br><sub>Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</sub>",
                x=0.5,
                xanchor='center'
            ),
            height=1200,
            showlegend=True,
            template='plotly_white',
            font=dict(size=10)
        )

        # Update axes labels
        fig.update_xaxes(title_text="Years", row=2, col=1)
        fig.update_yaxes(title_text="Current (A)", row=2, col=1)

        # Save HTML report
        fig.write_html(
            str(report_file),
            include_plotlyjs='cdn',
            config={'responsive': True, 'displayModeBar': True, 'toImageButtonOptions': {'format': 'png'}}
        )

        # Export CSV summary
        self._export_csv_summary(results, comparison_results, timestamp)

        return report_file

    def _export_csv_summary(
        self,
        results: Dict[str, Any],
        comparison_results: Optional[Dict[str, Any]],
        timestamp: str
    ) -> Path:
        """
        Export summary data to CSV file.

        Args:
            results: Primary calculation results
            comparison_results: Optional comparison results
            timestamp: Timestamp for filename

        Returns:
            Path to CSV file
        """
        csv_file = self.data_dir / f"cp_summary_{timestamp}.csv"

        # Extract key metrics
        geometry = results.get('geometry', {})
        attenuation = results.get('attenuation', {})
        current = results.get('current_demand', {})
        coating = results.get('coating_breakdown', {})
        inputs = results.get('inputs', {}).get('pipeline', {})
        anodes = results.get('anode_requirements', {})

        data = {
            'timestamp': [datetime.now().strftime('%Y-%m-%d %H:%M:%S')],
            'outer_diameter_m': [geometry.get('outer_diameter_m', 0)],
            'length_m': [geometry.get('length_m', 0)],
            'attenuation_length_m': [attenuation.get('attenuation_length_m', 0)],
            'mean_current_A': [current.get('mean_A', 0)],
            'final_coating_factor': [coating.get('final_factor', 0)],
            'wet_storage_years': [inputs.get('wet_storage_years', 0)],
            'number_of_anodes': [anodes.get('anode_count', 0)]
        }

        df = pd.DataFrame(data)
        df.to_csv(csv_file, index=False)

        return csv_file

    def generate_comparison_report(
        self,
        results_2010: Dict[str, Any],
        results_2016: Dict[str, Any],
        title: str = "DNV RP-F103 Comparison: 2010 vs 2016"
    ) -> Path:
        """
        Generate comparison report between DNV 2010 and 2016 methodologies.

        Args:
            results_2010: DNV RP-F103:2010 results
            results_2016: DNV RP-F103:2016 (Enhanced Saipem) results
            title: Report title

        Returns:
            Path to generated HTML report
        """
        return self.generate_attenuation_report(
            results=results_2010,
            comparison_results=results_2016,
            title=title
        )


def generate_cp_report(
    results: Dict[str, Any],
    output_dir: str = "reports/cp",
    comparison_results: Optional[Dict[str, Any]] = None,
    title: str = "Cathodic Protection Analysis"
) -> Path:
    """
    Convenience function to generate CP HTML report.

    Args:
        results: Calculation results from DNV method
        output_dir: Output directory for reports
        comparison_results: Optional comparison results
        title: Report title

    Returns:
        Path to generated HTML report

    Example:
        >>> from digitalmodel.common.cathodic_protection import CathodicProtection
        >>> cp = CathodicProtection()
        >>> results = cp.DNV_RP_F103_2010(config)
        >>> report_path = generate_cp_report(results)
    """
    generator = CPHTMLReportGenerator(output_dir)
    return generator.generate_attenuation_report(results, comparison_results, title)
