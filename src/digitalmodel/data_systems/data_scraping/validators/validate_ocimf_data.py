#!/usr/bin/env python3
"""
OCIMF Mooring Coefficient Data Validator

Validates OCIMF mooring coefficient datasets for completeness, consistency,
and physical realism. Generates quality reports for production use.
"""

import sys
from pathlib import Path
from typing import Dict, List, Tuple
import pandas as pd
import numpy as np


class OCIMFValidator:
    """Validate OCIMF mooring coefficient datasets."""

    def __init__(self, data_dir: Path = None):
        """
        Initialize OCIMF validator.

        Args:
            data_dir: Directory containing OCIMF CSV files
        """
        if data_dir is None:
            data_dir = Path("data/mooring/raw/ocimf")
        self.data_dir = data_dir

        # Expected coefficient ranges (based on physical realism)
        self.coefficient_ranges = {
            'CXw': (-0.7, 1.0),   # Wind surge
            'CYw': (0.0, 1.0),    # Wind sway
            'CMw': (-0.3, 0.3),   # Wind yaw
            'CXc': (-0.7, 1.0),   # Current surge
            'CYc': (0.0, 1.0),    # Current sway
            'CMc': (-0.3, 0.3),   # Current yaw
        }

        # Expected vessel dimension ranges
        self.dimension_ranges = {
            'loa': (150.0, 450.0),    # Length overall (meters)
            'beam': (20.0, 70.0),     # Beam (meters)
            'draft': (5.0, 30.0),     # Draft (meters)
        }

    def validate_dataset(self, csv_path: Path) -> Dict:
        """
        Validate a single OCIMF dataset.

        Args:
            csv_path: Path to CSV file

        Returns:
            Dictionary with validation results
        """
        results = {
            'file': csv_path.name,
            'valid': True,
            'errors': [],
            'warnings': [],
            'stats': {}
        }

        try:
            df = pd.read_csv(csv_path)
            results['stats']['total_rows'] = len(df)
            results['stats']['total_columns'] = len(df.columns)

            # Check required columns
            self._validate_columns(df, results)

            # Check data completeness
            self._validate_completeness(df, results)

            # Check coefficient ranges
            self._validate_coefficient_ranges(df, results)

            # Check vessel dimensions
            self._validate_dimensions(df, results)

            # Check heading coverage
            self._validate_headings(df, results)

            # Check for duplicates
            self._validate_duplicates(df, results)

            # Calculate quality score
            results['quality_score'] = self._calculate_quality_score(results)

        except Exception as e:
            results['valid'] = False
            results['errors'].append(f"Failed to read file: {str(e)}")

        return results

    def _validate_columns(self, df: pd.DataFrame, results: Dict):
        """Validate presence of required columns."""
        required_cols = ['vessel_type', 'heading']
        coefficient_cols = ['CXw', 'CYw', 'CMw', 'CXc', 'CYc', 'CMc']

        # Check for required columns
        missing_required = [col for col in required_cols if col not in df.columns]
        if missing_required:
            results['errors'].append(f"Missing required columns: {missing_required}")
            results['valid'] = False

        # Check for coefficient columns
        missing_coeffs = [col for col in coefficient_cols if col not in df.columns]
        if missing_coeffs:
            results['warnings'].append(f"Missing coefficient columns: {missing_coeffs}")

        results['stats']['columns_present'] = list(df.columns)

    def _validate_completeness(self, df: pd.DataFrame, results: Dict):
        """Validate data completeness."""
        completeness = {}

        for col in df.columns:
            non_null = df[col].notna().sum()
            total = len(df)
            pct_complete = (non_null / total) * 100
            completeness[col] = {
                'non_null': non_null,
                'total': total,
                'percent_complete': round(pct_complete, 2)
            }

            if pct_complete < 100 and col in ['CXw', 'CYw', 'CMw', 'CXc', 'CYc', 'CMc']:
                results['warnings'].append(
                    f"Column '{col}' is {pct_complete:.1f}% complete "
                    f"({non_null}/{total} rows)"
                )

        results['stats']['completeness'] = completeness

        # Overall completeness
        total_cells = len(df) * len(df.columns)
        filled_cells = df.notna().sum().sum()
        overall_completeness = (filled_cells / total_cells) * 100
        results['stats']['overall_completeness'] = round(overall_completeness, 2)

    def _validate_coefficient_ranges(self, df: pd.DataFrame, results: Dict):
        """Validate that coefficients fall within expected ranges."""
        for coeff, (min_val, max_val) in self.coefficient_ranges.items():
            if coeff not in df.columns:
                continue

            values = df[coeff].dropna()
            if len(values) == 0:
                continue

            out_of_range = values[(values < min_val) | (values > max_val)]

            if len(out_of_range) > 0:
                results['warnings'].append(
                    f"Coefficient '{coeff}' has {len(out_of_range)} values "
                    f"outside expected range [{min_val}, {max_val}]: "
                    f"min={values.min():.3f}, max={values.max():.3f}"
                )

            results['stats'][f'{coeff}_range'] = {
                'min': float(values.min()),
                'max': float(values.max()),
                'mean': float(values.mean()),
                'std': float(values.std())
            }

    def _validate_dimensions(self, df: pd.DataFrame, results: Dict):
        """Validate vessel dimensions."""
        for dim, (min_val, max_val) in self.dimension_ranges.items():
            if dim not in df.columns:
                continue

            values = df[dim].dropna()
            if len(values) == 0:
                continue

            out_of_range = values[(values < min_val) | (values > max_val)]

            if len(out_of_range) > 0:
                results['warnings'].append(
                    f"Dimension '{dim}' has {len(out_of_range)} values "
                    f"outside typical range [{min_val}, {max_val}]: "
                    f"min={values.min():.1f}, max={values.max():.1f}"
                )

    def _validate_headings(self, df: pd.DataFrame, results: Dict):
        """Validate heading angle coverage."""
        if 'heading' not in df.columns:
            return

        headings = sorted(df['heading'].dropna().unique())
        results['stats']['heading_points'] = len(headings)
        results['stats']['heading_range'] = {
            'min': int(headings[0]) if len(headings) > 0 else None,
            'max': int(headings[-1]) if len(headings) > 0 else None,
            'unique_values': [int(h) for h in headings[:10]]  # First 10
        }

        # Check for expected 10-degree intervals
        if len(headings) > 1:
            intervals = np.diff(headings)
            common_interval = np.median(intervals)

            if common_interval != 10:
                results['warnings'].append(
                    f"Heading intervals are not standard 10°: median={common_interval}°"
                )

    def _validate_duplicates(self, df: pd.DataFrame, results: Dict):
        """Check for duplicate rows."""
        key_cols = ['vessel_type', 'heading']

        if 'displacement' in df.columns:
            key_cols.append('displacement')
        elif 'displacement_ratio' in df.columns:
            key_cols.append('displacement_ratio')

        # Check if all key columns exist
        existing_keys = [col for col in key_cols if col in df.columns]

        if len(existing_keys) < 2:
            return

        duplicates = df.duplicated(subset=existing_keys, keep=False)
        num_duplicates = duplicates.sum()

        if num_duplicates > 0:
            results['errors'].append(
                f"Found {num_duplicates} duplicate rows based on {existing_keys}"
            )
            results['valid'] = False

    def _calculate_quality_score(self, results: Dict) -> int:
        """
        Calculate overall quality score (0-100).

        Scoring:
        - Completeness: 40 points
        - No errors: 30 points
        - Few warnings: 30 points
        """
        score = 0

        # Completeness (40 points)
        if 'overall_completeness' in results.get('stats', {}):
            completeness = results['stats']['overall_completeness']
            score += int(completeness * 0.4)

        # Errors (30 points - all or nothing)
        if len(results.get('errors', [])) == 0:
            score += 30

        # Warnings (30 points - deduct 5 per warning, min 0)
        warning_penalty = len(results.get('warnings', [])) * 5
        warning_score = max(0, 30 - warning_penalty)
        score += warning_score

        return min(100, score)

    def validate_all(self) -> Dict[str, Dict]:
        """
        Validate all OCIMF datasets in the data directory.

        Returns:
            Dictionary mapping filenames to validation results
        """
        results = {}

        csv_files = list(self.data_dir.glob("*.csv"))

        if not csv_files:
            print(f"⚠️  No CSV files found in {self.data_dir}")
            return results

        print(f"\n{'='*70}")
        print("OCIMF DATA VALIDATION")
        print(f"{'='*70}\n")
        print(f"Found {len(csv_files)} CSV files in {self.data_dir}\n")

        for csv_file in sorted(csv_files):
            print(f"Validating: {csv_file.name}")
            print("-" * 70)

            result = self.validate_dataset(csv_file)
            results[csv_file.name] = result

            # Print summary
            status = "✅ VALID" if result['valid'] else "❌ INVALID"
            quality = result.get('quality_score', 0)
            print(f"  Status: {status}")
            print(f"  Quality Score: {quality}/100")
            print(f"  Rows: {result['stats'].get('total_rows', 0)}")
            print(f"  Completeness: {result['stats'].get('overall_completeness', 0):.1f}%")

            if result.get('errors'):
                print(f"\n  ❌ Errors ({len(result['errors'])}):")
                for error in result['errors']:
                    print(f"    - {error}")

            if result.get('warnings'):
                print(f"\n  ⚠️  Warnings ({len(result['warnings'])}):")
                for warning in result['warnings'][:5]:  # Show first 5
                    print(f"    - {warning}")
                if len(result['warnings']) > 5:
                    print(f"    ... and {len(result['warnings']) - 5} more warnings")

            print()

        return results

    def generate_report(self, results: Dict[str, Dict], output_path: Path = None):
        """
        Generate a validation report.

        Args:
            results: Validation results from validate_all()
            output_path: Path to save report (optional)
        """
        if output_path is None:
            output_path = self.data_dir / "validation_report.txt"

        with open(output_path, 'w') as f:
            f.write("=" * 70 + "\n")
            f.write("OCIMF MOORING COEFFICIENT DATA VALIDATION REPORT\n")
            f.write("=" * 70 + "\n\n")

            f.write(f"Total Datasets Validated: {len(results)}\n")

            valid_count = sum(1 for r in results.values() if r['valid'])
            f.write(f"Valid Datasets: {valid_count}/{len(results)}\n\n")

            # Summary by dataset
            for filename, result in sorted(results.items()):
                f.write("-" * 70 + "\n")
                f.write(f"Dataset: {filename}\n")
                f.write("-" * 70 + "\n")

                status = "VALID ✓" if result['valid'] else "INVALID ✗"
                f.write(f"Status: {status}\n")
                f.write(f"Quality Score: {result.get('quality_score', 0)}/100\n")

                stats = result.get('stats', {})
                f.write(f"Rows: {stats.get('total_rows', 0)}\n")
                f.write(f"Columns: {stats.get('total_columns', 0)}\n")
                f.write(f"Completeness: {stats.get('overall_completeness', 0):.2f}%\n")

                if 'heading_points' in stats:
                    f.write(f"Heading Points: {stats['heading_points']}\n")

                if result.get('errors'):
                    f.write(f"\nErrors ({len(result['errors'])}):\n")
                    for error in result['errors']:
                        f.write(f"  - {error}\n")

                if result.get('warnings'):
                    f.write(f"\nWarnings ({len(result['warnings'])}):\n")
                    for warning in result['warnings']:
                        f.write(f"  - {warning}\n")

                f.write("\n")

        print(f"\n✅ Validation report saved to: {output_path}")


def main():
    """Run OCIMF data validation."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Validate OCIMF mooring coefficient datasets"
    )
    parser.add_argument(
        '--data-dir',
        type=Path,
        default=Path("data/mooring/raw/ocimf"),
        help="Directory containing OCIMF CSV files"
    )
    parser.add_argument(
        '--report',
        type=Path,
        help="Path to save validation report"
    )

    args = parser.parse_args()

    # Run validation
    validator = OCIMFValidator(data_dir=args.data_dir)
    results = validator.validate_all()

    # Generate report
    validator.generate_report(results, output_path=args.report)

    # Overall summary
    print("\n" + "=" * 70)
    print("VALIDATION SUMMARY")
    print("=" * 70)

    valid_count = sum(1 for r in results.values() if r['valid'])
    total_count = len(results)

    print(f"\nDatasets Validated: {total_count}")
    print(f"Valid: {valid_count}")
    print(f"Invalid: {total_count - valid_count}")

    avg_quality = sum(r.get('quality_score', 0) for r in results.values()) / total_count if total_count > 0 else 0
    print(f"\nAverage Quality Score: {avg_quality:.1f}/100")

    if all(r['valid'] for r in results.values()):
        print("\n✅ All datasets passed validation!")
        return 0
    else:
        print("\n⚠️  Some datasets have validation issues - see report for details")
        return 1


if __name__ == "__main__":
    sys.exit(main())
