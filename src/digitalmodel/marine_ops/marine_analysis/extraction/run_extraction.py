"""
Wrapper script to run hydrodynamic coefficient extraction with proper encoding.
"""

import sys
import os

# Add scripts to path
sys.path.insert(0, os.path.dirname(__file__))

# Import and run
from .extract_hydro import HydrodynamicCoefficientExtractor

if __name__ == '__main__':
    if sys.platform == 'win32':
        for stream in (sys.stdout, sys.stderr):
            if hasattr(stream, 'reconfigure'):
                stream.reconfigure(encoding='utf-8', errors='replace')

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
