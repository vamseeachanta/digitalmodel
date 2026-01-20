"""
Data Validation Integration Demo

Demonstrates using the new DataValidator with existing data procurement workflow.
"""

import pandas as pd
import logging
import sys
import os
from pathlib import Path

# Set UTF-8 encoding for Windows console
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.validators.data_validator import DataValidator

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

logger = logging.getLogger(__name__)


def demo_vessel_data_validation():
    """
    Demo: Validate vessel data with interactive reporting.

    This integrates with existing data procurement validators.
    """
    print("\n" + "=" * 70)
    print("VESSEL DATA VALIDATION DEMO")
    print("=" * 70)

    # Create sample vessel data
    vessel_data = {
        'vessel_id': [1, 2, 3, 4, 5, 5],  # Duplicate ID
        'vessel_name': ['Ship A', 'Ship B', None, 'Ship D', 'Ship E', 'Ship F'],  # Missing name
        'vessel_type': ['Tanker', 'Container', 'Bulk', 'Tanker', 'Container', 'Bulk'],
        'year_built': [2020, 2021, 'invalid', 2023, 2024, 2025],  # Type issue
        'length_m': [100.0, 200.0, 300.0, None, 500.0, 600.0],  # Missing value
        'beam_m': [20.0, 30.0, 40.0, 50.0, 60.0, 70.0],
        'displacement_tonnes': [5000, 10000, 15000, 20000, 25000, 30000]
    }

    df = pd.DataFrame(vessel_data)

    print(f"\nDataset size: {len(df)} vessels")
    print(f"Columns: {list(df.columns)}")

    # Initialize validator with config
    config_path = Path("config/validation/validation_config.yaml")
    validator = DataValidator(config_path=config_path if config_path.exists() else None)

    # Validate
    print("\nRunning validation...")
    results = validator.validate_dataframe(
        df=df,
        required_fields=['vessel_id', 'vessel_name', 'vessel_type'],
        unique_field='vessel_id'
    )

    # Print summary
    print("\n" + "-" * 70)
    print(f"Validation Status: {'✅ PASS' if results['valid'] else '❌ FAIL'}")
    print(f"Quality Score: {results['quality_score']:.1f}/100")
    print(f"Total Rows: {results['total_rows']}")
    print(f"Issues Found: {len(results['issues'])}")

    if results.get('missing_data'):
        print("\nMissing Data:")
        for col, pct in results['missing_data'].items():
            print(f"  - {col}: {pct:.1%}")

    if results['issues']:
        print("\nIssues:")
        for issue in results['issues']:
            print(f"  - {issue}")

    # Generate text report
    print("\n" + validator.generate_report(results))

    # Generate interactive HTML report
    report_path = Path("reports/validation/vessel_data_validation.html")
    report_path.parent.mkdir(parents=True, exist_ok=True)

    validator.generate_interactive_report(results, report_path)
    print(f"\n✅ Interactive report saved to: {report_path}")
    print("   Open in browser to view dashboard with:")
    print("   - Quality score gauge")
    print("   - Missing data visualizations")
    print("   - Type issue analysis")
    print("   - Summary statistics table")

    return results


def demo_integration_with_procurement():
    """
    Demo: Integration with existing data procurement validators.

    Shows how the new validator complements existing validation logic.
    """
    print("\n" + "=" * 70)
    print("INTEGRATION WITH DATA PROCUREMENT")
    print("=" * 70)

    # Import existing validator
    try:
        import importlib.util
        spec = importlib.util.find_spec("data_procurement.validators.data_validator")
        if spec is not None:
            from digitalmodel.modules.data_procurement.validators.data_validator import DataValidator as ProcurementValidator
        else:
            raise ImportError("data_procurement not in path")

        print("\n✅ Found existing data_procurement validator")
        print("   The new validator in src/digitalmodel/validators/ provides:")
        print("   - Interactive Plotly reports (4-panel dashboard)")
        print("   - YAML configuration support")
        print("   - Quality scoring (0-100 scale)")
        print("   - Missing data visualization")
        print("\n   Both validators can work together:")
        print("   - Use procurement validator for domain-specific checks")
        print("   - Use new validator for reporting and quality scoring")

    except ImportError:
        print("\n⚠️  Existing data_procurement validator not found in import path")
        print("   The new validator is standalone and ready to use!")


def demo_batch_validation():
    """
    Demo: Batch validation of multiple datasets.
    """
    print("\n" + "=" * 70)
    print("BATCH VALIDATION DEMO")
    print("=" * 70)

    validator = DataValidator()

    # Simulate multiple datasets
    datasets = {
        'vessels.csv': pd.DataFrame({
            'id': [1, 2, 3],
            'name': ['Ship A', 'Ship B', 'Ship C'],
            'value': [100, 200, 300]
        }),
        'equipment.csv': pd.DataFrame({
            'id': [1, 2, 3, 3],  # Duplicate
            'name': ['Pump A', None, 'Valve C', 'Valve D'],  # Missing name
            'status': ['Active', 'Active', 'Inactive', 'Active']
        })
    }

    print(f"\nValidating {len(datasets)} datasets...")

    results_summary = []
    for filename, df in datasets.items():
        results = validator.validate_dataframe(
            df=df,
            required_fields=['id', 'name'],
            unique_field='id'
        )

        results_summary.append({
            'file': filename,
            'status': '✅ PASS' if results['valid'] else '❌ FAIL',
            'score': results['quality_score'],
            'rows': results['total_rows'],
            'issues': len(results['issues'])
        })

    # Print summary table
    print("\n" + "-" * 70)
    print(f"{'Dataset':<20} {'Status':<10} {'Score':<10} {'Rows':<8} {'Issues'}")
    print("-" * 70)
    for r in results_summary:
        print(f"{r['file']:<20} {r['status']:<10} {r['score']:<10.1f} {r['rows']:<8} {r['issues']}")
    print("-" * 70)


if __name__ == '__main__':
    print("=" * 70)
    print("DATA VALIDATION REPORTER - INTEGRATION DEMOS")
    print("=" * 70)
    print("\nThis demonstrates the newly installed data-validation-reporter skill")
    print("integrated into digitalmodel repository.")
    print("\nInstalled components:")
    print("  ✅ src/digitalmodel/validators/data_validator.py")
    print("  ✅ src/digitalmodel/validators/__init__.py")
    print("  ✅ config/validation/validation_config.yaml")
    print("  ✅ examples/validation_examples.py")
    print("  ✅ examples/validation_integration_demo.py (this file)")
    print("\nNote: Import using 'from digitalmodel.validators import DataValidator'")

    try:
        # Run demos
        demo_vessel_data_validation()
        demo_integration_with_procurement()
        demo_batch_validation()

        print("\n" + "=" * 70)
        print("✅ All demos completed successfully!")
        print("=" * 70)
        print("\nNext steps:")
        print("  1. Review interactive report: reports/validation/vessel_data_validation.html")
        print("  2. Customize config: config/validation/validation_config.yaml")
        print("  3. Integrate into data pipelines")
        print("  4. See more examples: python examples/validation_examples.py")
        print("=" * 70)

    except Exception as e:
        logger.error(f"Demo failed: {e}", exc_info=True)
        print(f"\n❌ Error: {e}")
