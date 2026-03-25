#!/usr/bin/env python
"""
Run full batch conversion of all OrcaFlex examples.

This script converts all downloaded .dat and .sim files to YAML format,
with automatic fallback to mock mode if OrcFxAPI is not available.
"""

import logging
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from batch_converter import OrcaFlexBatchConverter

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

logger = logging.getLogger(__name__)


def main():
    """Run full batch conversion."""
    print("\n" + "="*70)
    print("ORCAFLEX EXAMPLES BATCH CONVERSION")
    print("="*70)
    
    # Set up paths
    base_dir = Path(__file__).parent.parent.parent.parent.parent.parent
    input_dir = base_dir / "docs/domains/orcaflex/examples/raw"
    output_dir = base_dir / "docs/domains/orcaflex/examples/yaml"
    
    # Check if input directory exists
    if not input_dir.exists():
        logger.error(f"Input directory not found: {input_dir}")
        print(f"\n[ERROR] Input directory not found: {input_dir}")
        print("Please run the downloader first to get OrcaFlex examples.")
        return 1
    
    # Create converter (auto-detects OrcFxAPI availability)
    print("\nInitializing converter...")
    converter = OrcaFlexBatchConverter(
        input_dir=input_dir,
        output_dir=output_dir,
        use_mock=False,  # Will auto-fallback to mock if needed
        validate=True,
        max_retries=2
    )
    
    # Display mode
    if converter.api_available:
        print("[INFO] Running in REAL mode with OrcFxAPI")
    else:
        print("[INFO] Running in MOCK mode (OrcFxAPI not available)")
        print("      Mock YAML files will be created for testing purposes")
    
    # Run batch conversion
    print("\nStarting batch conversion...")
    print("-" * 70)
    
    results = converter.convert_batch()
    
    # Display results
    print("\n" + "="*70)
    print("CONVERSION COMPLETE")
    print("="*70)
    
    stats = results['statistics']
    print(f"Total files found:    {stats['total_files']}")
    print(f"  - .dat files:       {stats['files_by_type']['dat']}")
    print(f"  - .sim files:       {stats['files_by_type']['sim']}")
    print("-" * 70)
    print(f"Successful:           {stats['successful']}")
    print(f"Failed:               {stats['failed']}")
    print(f"Skipped (existing):   {stats['skipped']}")
    print(f"Validation failed:    {stats['validation_failed']}")
    print("-" * 70)
    print(f"Processing time:      {stats['processing_time']:.2f} seconds")
    
    # Calculate success rate
    attempted = stats['total_files'] - stats['skipped']
    if attempted > 0:
        success_rate = (stats['successful'] / attempted) * 100
        print(f"Success rate:         {success_rate:.1f}%")
    
    print("="*70)
    
    # Report location
    report_path = output_dir / "conversion_report.md"
    if report_path.exists():
        print(f"\n[INFO] Detailed report saved to:")
        print(f"       {report_path}")
    
    # Display errors if any
    if stats['errors']:
        print(f"\n[WARNING] {len(stats['errors'])} files failed to convert:")
        for error in stats['errors'][:5]:  # Show first 5 errors
            print(f"  - {Path(error['file']).name}: {error['error']}")
        if len(stats['errors']) > 5:
            print(f"  ... and {len(stats['errors']) - 5} more errors")
        print("\nCheck the conversion report for full details.")
    
    # Next steps
    print("\n" + "="*70)
    print("NEXT STEPS")
    print("="*70)
    
    if converter.api_available:
        print("1. Review converted YAML files in:")
        print(f"   {output_dir}")
        print("2. Check conversion report for any issues")
        print("3. Proceed to Task 4: Build feature analyzer")
    else:
        print("1. Mock YAML files created for testing in:")
        print(f"   {output_dir}")
        print("2. To perform real conversion, install OrcFxAPI:")
        print("   - Ensure OrcaFlex is installed")
        print("   - Install Python API: pip install OrcFxAPI")
        print("3. Re-run this script after OrcFxAPI is available")
    
    print("="*70)
    
    return 0 if stats['failed'] == 0 else 1


if __name__ == "__main__":
    sys.exit(main())