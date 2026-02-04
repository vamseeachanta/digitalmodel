#!/usr/bin/env python
"""
Test progress bar functionality
"""

import sys
import json
from pathlib import Path

# Add the src directory to the path
src_path = Path(__file__).parent.parent.parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.structural.fatigue_apps.strut_foundation_processor_with_progress import (
    BatchProcessor
)

def test_progress_bars():
    """Test the progress bar functionality"""
    
    print("Testing Progress Bar Implementation")
    print("=" * 60)
    
    # Create a small batch configuration for testing
    sample_path = Path(__file__).parent / "sample_data"
    
    # Create batch config
    batch_config = {
        "description": "Test batch with progress tracking",
        "fatigue_conditions": [
            {
                "id": 1,
                "wind_speed": 5.0,
                "wind_dir": 0.0,
                "hs": 0.15,
                "tp": 1.93,
                "wave_dir": 0.0,
                "occurrence": 7.76
            },
            {
                "id": 2,
                "wind_speed": 10.0,
                "wind_dir": 45.0,
                "hs": 0.25,
                "tp": 2.70,
                "wave_dir": 45.0,
                "occurrence": 5.50
            },
            {
                "id": 3,
                "wind_speed": 15.0,
                "wind_dir": 90.0,
                "hs": 0.35,
                "tp": 3.47,
                "wave_dir": 120.0,
                "occurrence": 3.25
            }
        ],
        "struts": [1, 2],  # Just process 2 struts for demo
        "output_dir": "output_progress_test"
    }
    
    # Save config file
    config_file = Path(__file__).parent / "batch_config_test.json"
    with open(config_file, 'w') as f:
        json.dump(batch_config, f, indent=2)
    
    print(f"Created test configuration: {config_file}")
    print("\nRunning batch processor with progress tracking...")
    print("-" * 60)
    
    # Run the batch processor
    processor = BatchProcessor(base_path=str(sample_path))
    processor.run_batch_analysis(config_file=str(config_file))
    
    print("\n" + "=" * 60)
    print("Progress Bar Test Complete!")
    print("Check the progress display above for:")
    print("  - Overall progress bar")
    print("  - Configuration updates")
    print("  - Current FC and Strut being processed")
    print("  - Completion percentage")
    
    return True

if __name__ == "__main__":
    success = test_progress_bars()
    sys.exit(0 if success else 1)