"""
Wrapper script to run hydrodynamic coefficient extraction with proper encoding.
"""

import sys
import os

# Set UTF-8 encoding for stdout
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8', errors='replace')

# Add scripts to path
sys.path.insert(0, os.path.dirname(__file__))

# Import and run
from .extract_hydro import HydrodynamicCoefficientExtractor

if __name__ == '__main__':
    print("=" * 80)
    print("HYDRODYNAMIC COEFFICIENT EXTRACTION & VISUALIZATION")
    print("=" * 80)

    # Create extractor with sample data
    extractor = HydrodynamicCoefficientExtractor(
        excel_path='sample_data.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    # Run complete extraction
    extractor.run_complete_extraction()
