"""
Data Validator Template

Reusable data validation class with quality scoring and interactive reporting.
Based on digitalmodel pattern (commit 47b64945).

Usage:
    validator = DataValidator(config_path="config/validation.yaml")
    results = validator.validate_dataframe(df, required_fields=["id"])
    validator.generate_interactive_report(results, Path("report.html"))
"""

import pandas as pd
import logging
from typing import Dict, List, Optional, Any
from pathlib import Path
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import yaml


class DataValidator:
    """Validates data with quality scoring and interactive reporting."""

    def __init__(self, config_path: Optional[Path] = None):
        """
        Initialize data validator.

        Args:
            config_path: Optional path to YAML configuration file
        """
        self.logger = logging.getLogger(self.__class__.__name__)
        self.config = self._load_config(config_path) if config_path else {}

    def _load_config(self, config_path: Path) -> Dict[str, Any]:
        """
        Load validation configuration from YAML.

        Args:
            config_path: Path to YAML config file

        Returns:
            Configuration dictionary
        """
        try:
            with open(config_path, 'r') as f:
                config = yaml.safe_load(f)
            self.logger.info(f"Loaded validation config from {config_path}")
            return config
        except Exception as e:
            self.logger.error(f"Failed to load config: {e}")
            return {}

    def validate_dataframe(
        self,
        df: pd.DataFrame,
        required_fields: Optional[List[str]] = None,
        unique_field: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Validate a DataFrame and return quality metrics.

        Args:
            df: DataFrame to validate
            required_fields: List of required field names
            unique_field: Field that should be unique (e.g., 'id')

        Returns:
            Dictionary with validation results:
            {
                'valid': bool,
                'total_rows': int,
                'total_columns': int,
                'issues': List[str],
                'quality_score': float,
                'missing_data': Dict[str, float],
                'type_issues': List[str]
            }
        """
        results = {
            'valid': True,
            'total_rows': len(df),
            'total_columns': len(df.columns),
            'issues': [],
            'quality_score': 100.0
        }

        # Check for empty DataFrame
        if df.empty:
            results['valid'] = False
            results['issues'].append('DataFrame is empty')
            results['quality_score'] = 0.0
            return results

        # Check required fields
        if required_fields:
            missing_fields = set(required_fields) - set(df.columns)
            if missing_fields:
                results['valid'] = False
                results['issues'].append(f'Missing required fields: {missing_fields}')
                results['quality_score'] -= 20

        # Check for missing data
        missing_data = self._check_missing_data(df)
        if missing_data:
            results['missing_data'] = missing_data
            avg_missing = sum(missing_data.values()) / len(missing_data)
            if avg_missing > 0.5:  # More than 50% missing
                results['issues'].append(f'High missing data: {avg_missing:.1%} average')
                results['quality_score'] -= 30
            elif avg_missing > 0.2:  # More than 20% missing
                results['issues'].append(f'Moderate missing data: {avg_missing:.1%} average')
                results['quality_score'] -= 15

        # Check for duplicates
        if unique_field and unique_field in df.columns:
            duplicates = df[unique_field].duplicated().sum()
            if duplicates > 0:
                results['issues'].append(f'{duplicates} duplicate {unique_field} values')
                results['quality_score'] -= min(20, duplicates * 2)

        # Check data types
        type_issues = self._check_data_types(df)
        if type_issues:
            results['type_issues'] = type_issues
            results['issues'].extend(type_issues)
            results['quality_score'] -= min(15, len(type_issues) * 5)

        # Ensure quality score doesn't go below 0
        results['quality_score'] = max(0.0, results['quality_score'])

        # Overall validation status
        if results['quality_score'] < 60:
            results['valid'] = False

        return results

    def _check_missing_data(self, df: pd.DataFrame) -> Dict[str, float]:
        """
        Calculate percentage of missing data per column.

        Args:
            df: DataFrame to check

        Returns:
            Dictionary mapping column names to missing data percentage
        """
        missing = {}
        for col in df.columns:
            missing_pct = df[col].isna().sum() / len(df)
            if missing_pct > 0:
                missing[col] = round(missing_pct, 3)

        return missing

    def _check_data_types(self, df: pd.DataFrame) -> List[str]:
        """
        Check for data type issues.

        Args:
            df: DataFrame to check

        Returns:
            List of data type issue descriptions
        """
        issues = []

        # Get numeric fields from config or use defaults
        numeric_fields = self.config.get('validation', {}).get('numeric_fields', [
            'year', 'count', 'amount', 'value', 'quantity'
        ])

        for field in numeric_fields:
            if field in df.columns:
                non_numeric = pd.to_numeric(df[field], errors='coerce').isna().sum()
                if non_numeric > 0 and not df[field].isna().all():
                    issues.append(f'{field}: {non_numeric} non-numeric values')

        return issues

    def generate_report(self, validation_results: Dict[str, Any]) -> str:
        """
        Generate human-readable validation report.

        Args:
            validation_results: Results from validate_dataframe()

        Returns:
            Formatted report string
        """
        report = []
        report.append("=" * 60)
        report.append("DATA VALIDATION REPORT")
        report.append("=" * 60)

        report.append(f"\nOverall Status: {'✓ PASS' if validation_results['valid'] else '✗ FAIL'}")
        report.append(f"Quality Score: {validation_results['quality_score']:.1f}/100.0")

        report.append(f"\nDataset Size:")
        report.append(f"  - Rows: {validation_results['total_rows']}")
        report.append(f"  - Columns: {validation_results['total_columns']}")

        if validation_results.get('missing_data'):
            report.append(f"\nMissing Data:")
            for col, pct in sorted(validation_results['missing_data'].items(),
                                  key=lambda x: x[1], reverse=True):
                report.append(f"  - {col}: {pct:.1%}")

        if validation_results['issues']:
            report.append(f"\nIssues Found ({len(validation_results['issues'])}):")
            for i, issue in enumerate(validation_results['issues'], 1):
                report.append(f"  {i}. {issue}")

        report.append("\n" + "=" * 60)

        return "\n".join(report)

    def generate_interactive_report(
        self,
        validation_results: Dict[str, Any],
        output_path: Path
    ) -> None:
        """
        Generate interactive HTML validation report with Plotly visualizations.

        Args:
            validation_results: Results from validate_dataframe()
            output_path: Path to save HTML report
        """
        # Create subplots
        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=(
                'Quality Score',
                'Missing Data by Column',
                'Data Type Issues',
                'Validation Summary'
            ),
            specs=[
                [{'type': 'indicator'}, {'type': 'bar'}],
                [{'type': 'bar'}, {'type': 'table'}]
            ]
        )

        # 1. Quality Score Gauge
        fig.add_trace(
            go.Indicator(
                mode="gauge+number+delta",
                value=validation_results['quality_score'],
                delta={'reference': 80},
                gauge={
                    'axis': {'range': [0, 100]},
                    'bar': {'color': self._get_quality_color(validation_results['quality_score'])},
                    'steps': [
                        {'range': [0, 60], 'color': 'lightgray'},
                        {'range': [60, 80], 'color': 'gray'},
                        {'range': [80, 100], 'color': 'lightgreen'}
                    ],
                    'threshold': {
                        'line': {'color': 'red', 'width': 4},
                        'thickness': 0.75,
                        'value': 80
                    }
                },
                title={'text': "Quality Score"}
            ),
            row=1, col=1
        )

        # 2. Missing Data Bar Chart
        if validation_results.get('missing_data'):
            missing_data = validation_results['missing_data']
            fig.add_trace(
                go.Bar(
                    x=list(missing_data.keys()),
                    y=[v * 100 for v in missing_data.values()],
                    name='Missing %',
                    marker_color='indianred',
                    hovertemplate='%{x}<br>Missing: %{y:.1f}%<extra></extra>'
                ),
                row=1, col=2
            )

        # 3. Data Type Issues
        if validation_results.get('type_issues'):
            type_issues = validation_results['type_issues']
            issue_counts = {}
            for issue in type_issues:
                field = issue.split(':')[0]
                count = int(issue.split(':')[1].split()[0])
                issue_counts[field] = count

            fig.add_trace(
                go.Bar(
                    x=list(issue_counts.keys()),
                    y=list(issue_counts.values()),
                    name='Type Issues',
                    marker_color='orange',
                    hovertemplate='%{x}<br>Issues: %{y}<extra></extra>'
                ),
                row=2, col=1
            )

        # 4. Summary Table
        summary_data = {
            'Metric': ['Total Rows', 'Total Columns', 'Quality Score', 'Issues Found', 'Status'],
            'Value': [
                str(validation_results['total_rows']),
                str(validation_results['total_columns']),
                f"{validation_results['quality_score']:.1f}/100",
                str(len(validation_results['issues'])),
                '✓ PASS' if validation_results['valid'] else '✗ FAIL'
            ]
        }

        fig.add_trace(
            go.Table(
                header=dict(
                    values=list(summary_data.keys()),
                    fill_color='paleturquoise',
                    align='left',
                    font=dict(size=12, color='black')
                ),
                cells=dict(
                    values=list(summary_data.values()),
                    fill_color='lavender',
                    align='left',
                    font=dict(size=11)
                )
            ),
            row=2, col=2
        )

        # Update layout
        fig.update_layout(
            title_text="Data Validation Report",
            height=800,
            showlegend=False,
            template='plotly_white'
        )

        # Update axes
        fig.update_xaxes(title_text="Column", row=1, col=2)
        fig.update_yaxes(title_text="Missing %", row=1, col=2)
        fig.update_xaxes(title_text="Field", row=2, col=1)
        fig.update_yaxes(title_text="Issue Count", row=2, col=1)

        # Save as HTML
        output_path.parent.mkdir(parents=True, exist_ok=True)
        fig.write_html(output_path, include_plotlyjs='cdn')
        self.logger.info(f"Interactive validation report saved to {output_path}")

    def _get_quality_color(self, score: float) -> str:
        """
        Get color for quality score gauge.

        Args:
            score: Quality score (0-100)

        Returns:
            Color name for gauge
        """
        if score >= 80:
            return 'green'
        elif score >= 60:
            return 'yellow'
        else:
            return 'red'
