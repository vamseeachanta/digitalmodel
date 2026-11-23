"""Basic usage example for digitalmodel.data_systems.pyintegrity.

This example demonstrates how to run a simple API 579 FFS assessment
for a gas pipeline with general metal loss (GML).
"""

import os
import sys
from pathlib import Path

# Add src to path for imports
src_path = Path(__file__).parents[3] / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.data_systems.pyintegrity.engine import engine


def run_basic_gml_example():
    """Run a basic GML assessment example."""

    # Path to test configuration file
    test_config = Path(__file__).parents[3] / "src" / "digitalmodel" / "data_systems" / "pyintegrity" / "tests" / "test_data" / "API579" / "16in_gas_b318.yml"

    if not test_config.exists():
        print(f"Error: Test configuration file not found at {test_config}")
        return None

    print("=" * 80)
    print("API 579 FFS Assessment - Basic Usage Example")
    print("=" * 80)
    print(f"\nConfiguration file: {test_config}")
    print("\nRunning assessment...")

    try:
        # Run the FFS engine
        result_cfg = engine(str(test_config))

        print("\n" + "=" * 80)
        print("Assessment Complete")
        print("=" * 80)

        # Display key results
        if 'Result' in result_cfg:
            print("\nResults Summary:")
            if 'Circumference' in result_cfg['Result']:
                for idx, circ_result in enumerate(result_cfg['Result']['Circumference']):
                    print(f"\n  Reading Set {idx + 1}:")
                    if isinstance(circ_result, dict):
                        for key, value in circ_result.items():
                            if key in ['trd', 'tam', 'tmm', 'CTP', 'LOSS']:
                                print(f"    {key}: {value}")

            print("\nOutput files saved to:", result_cfg.get('Analysis', {}).get('result_folder', 'N/A'))

        return result_cfg

    except Exception as e:
        print(f"\nError during assessment: {e}")
        import traceback
        traceback.print_exc()
        return None


if __name__ == "__main__":
    run_basic_gml_example()
