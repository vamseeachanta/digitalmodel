"""
Example Usage: Data Validation Reporter

Demonstrates the data validation and reporting workflow.
"""

import pandas as pd
import logging
from pathlib import Path
from validator_template import DataValidator

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)


def example_basic_validation():
    """Example 1: Basic validation workflow."""
    print("\n" + "=" * 60)
    print("EXAMPLE 1: Basic Validation")
    print("=" * 60)

    # Create sample data
    data = {
        'id': [1, 2, 3, 4, 5],
        'name': ['Alice', 'Bob', 'Charlie', None, 'Eve'],
        'value': [100, 200, 300, 400, 500],
        'year': [2020, 2021, 2022, 2023, 2024]
    }
    df = pd.DataFrame(data)

    # Initialize validator
    validator = DataValidator()

    # Validate
    results = validator.validate_dataframe(
        df=df,
        required_fields=['id', 'name', 'value'],
        unique_field='id'
    )

    # Print results
    print(f"\nValidation Status: {'✅ PASS' if results['valid'] else '❌ FAIL'}")
    print(f"Quality Score: {results['quality_score']:.1f}/100")
    print(f"Total Rows: {results['total_rows']}")
    print(f"Issues Found: {len(results['issues'])}")

    if results['issues']:
        print("\nIssues:")
        for issue in results['issues']:
            print(f"  - {issue}")

    # Generate text report
    print("\n" + validator.generate_report(results))


def example_with_config():
    """Example 2: Validation with YAML configuration."""
    print("\n" + "=" * 60)
    print("EXAMPLE 2: Validation with Configuration")
    print("=" * 60)

    # Create sample data with quality issues
    data = {
        'id': [1, 2, 3, 4, 5, 5],  # Duplicate ID
        'vessel_name': ['Ship A', 'Ship B', None, 'Ship D', 'Ship E', 'Ship F'],  # Missing name
        'year_built': [2020, 2021, 'invalid', 2023, 2024, 2025],  # Type issue
        'length_m': [100.0, 200.0, 300.0, None, 500.0, 600.0],  # Missing value
    }
    df = pd.DataFrame(data)

    # Initialize validator with config
    # Note: config_path is optional - will use defaults if not provided
    validator = DataValidator()

    # Validate
    results = validator.validate_dataframe(
        df=df,
        required_fields=['id', 'vessel_name', 'year_built'],
        unique_field='id'
    )

    # Print results
    print(f"\nValidation Status: {'✅ PASS' if results['valid'] else '❌ FAIL'}")
    print(f"Quality Score: {results['quality_score']:.1f}/100")

    if results.get('missing_data'):
        print("\nMissing Data:")
        for col, pct in results['missing_data'].items():
            print(f"  - {col}: {pct:.1%}")

    if results.get('type_issues'):
        print("\nType Issues:")
        for issue in results['type_issues']:
            print(f"  - {issue}")


def example_interactive_report():
    """Example 3: Generate interactive HTML report."""
    print("\n" + "=" * 60)
    print("EXAMPLE 3: Interactive Report Generation")
    print("=" * 60)

    # Create larger sample dataset
    data = {
        'id': list(range(1, 101)),
        'category': ['A'] * 50 + ['B'] * 50,
        'value': list(range(100, 200)),
        'quality': [None] * 10 + list(range(90)) + [None]  # 10% missing
    }
    df = pd.DataFrame(data)

    # Add some duplicates
    df = pd.concat([df, df.iloc[:5]], ignore_index=True)

    # Initialize validator
    validator = DataValidator()

    # Validate
    results = validator.validate_dataframe(
        df=df,
        required_fields=['id', 'category', 'value'],
        unique_field='id'
    )

    # Generate interactive report
    output_path = Path("reports/example_validation_report.html")
    validator.generate_interactive_report(results, output_path)

    print(f"\n✅ Interactive report saved to: {output_path}")
    print(f"   Open in browser to view interactive dashboard")
    print(f"   Quality Score: {results['quality_score']:.1f}/100")


def example_batch_validation():
    """Example 4: Validate multiple files."""
    print("\n" + "=" * 60)
    print("EXAMPLE 4: Batch Validation")
    print("=" * 60)

    # Simulate multiple CSV files
    files = {
        'data1.csv': pd.DataFrame({
            'id': [1, 2, 3],
            'value': [100, 200, 300]
        }),
        'data2.csv': pd.DataFrame({
            'id': [1, 2, 3, 3],  # Duplicate
            'value': [100, None, 300, 400]  # Missing value
        }),
        'data3.csv': pd.DataFrame({
            'id': [1, 2],
            'value': [100, 200]
        })
    }

    validator = DataValidator()
    results_summary = []

    for filename, df in files.items():
        results = validator.validate_dataframe(
            df=df,
            required_fields=['id', 'value'],
            unique_field='id'
        )

        results_summary.append({
            'file': filename,
            'status': '✅ PASS' if results['valid'] else '❌ FAIL',
            'score': results['quality_score'],
            'rows': results['total_rows'],
            'issues': len(results['issues'])
        })

    # Print summary
    print("\nBatch Validation Summary:")
    print(f"{'File':<15} {'Status':<10} {'Score':<10} {'Rows':<8} {'Issues'}")
    print("-" * 60)
    for r in results_summary:
        print(f"{r['file']:<15} {r['status']:<10} {r['score']:<10.1f} {r['rows']:<8} {r['issues']}")


def example_quality_trend():
    """Example 5: Track quality over time."""
    print("\n" + "=" * 60)
    print("EXAMPLE 5: Quality Trend Analysis")
    print("=" * 60)

    validator = DataValidator()
    quality_history = []

    # Simulate data quality over time
    for day in range(1, 8):
        # Quality degrades over time
        missing_pct = day * 0.05  # Increasing missing data
        data = {
            'id': list(range(1, 101)),
            'value': [None if i < missing_pct * 100 else i for i in range(100)]
        }
        df = pd.DataFrame(data)

        results = validator.validate_dataframe(
            df=df,
            required_fields=['id', 'value']
        )

        quality_history.append({
            'day': f'Day {day}',
            'score': results['quality_score'],
            'valid': results['valid']
        })

    # Print trend
    print("\nQuality Trend (7 days):")
    print(f"{'Day':<10} {'Score':<10} {'Status'}")
    print("-" * 40)
    for q in quality_history:
        status = '✅ PASS' if q['valid'] else '❌ FAIL'
        print(f"{q['day']:<10} {q['score']:<10.1f} {status}")

    # Alert if degrading
    scores = [q['score'] for q in quality_history]
    if scores[-1] < scores[0] - 10:
        print("\n⚠️  WARNING: Quality degradation detected!")
        print(f"   Score dropped from {scores[0]:.1f} to {scores[-1]:.1f}")


if __name__ == '__main__':
    print("=" * 60)
    print("DATA VALIDATION REPORTER - EXAMPLES")
    print("=" * 60)

    # Run all examples
    example_basic_validation()
    example_with_config()
    example_interactive_report()
    example_batch_validation()
    example_quality_trend()

    print("\n" + "=" * 60)
    print("All examples completed successfully!")
    print("=" * 60)
