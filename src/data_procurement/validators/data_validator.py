"""
Data Validator

Validates scraped data for completeness, accuracy, and quality.
"""

import pandas as pd
import logging
from typing import Dict, List, Optional, Any


class DataValidator:
    """Validates scraped marine engineering data."""

    def __init__(self):
        """Initialize data validator."""
        self.logger = logging.getLogger(self.__class__.__name__)

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
            unique_field: Field that should be unique (e.g., 'vessel_name')

        Returns:
            Dictionary with validation results
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

        # Check numeric fields that should be numeric
        numeric_fields = ['year_built', 'length_m', 'beam_m', 'depth_m',
                         'draft_m', 'displacement_tonnes', 'water_depth_m']

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
