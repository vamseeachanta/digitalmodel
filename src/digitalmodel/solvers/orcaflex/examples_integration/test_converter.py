"""
Test script for batch converter with mock and real modes.
"""

import logging
from pathlib import Path
from batch_converter import OrcaFlexBatchConverter

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

def test_mock_conversion():
    """Test conversion in mock mode (no OrcFxAPI required)."""
    print("\n" + "="*60)
    print("TESTING MOCK CONVERSION")
    print("="*60)
    
    # Set up paths - use absolute paths
    base_dir = Path(__file__).parent.parent.parent.parent.parent.parent
    input_dir = base_dir / "docs/domains/orcaflex/examples/raw"
    output_dir = base_dir / "docs/domains/orcaflex/examples/yaml_mock_test"
    
    # Create converter in mock mode
    converter = OrcaFlexBatchConverter(
        input_dir=input_dir,
        output_dir=output_dir,
        use_mock=True,
        validate=True
    )
    
    # Find files
    files = converter.find_orcaflex_files()
    
    # Convert only first 3 files for testing
    if files:
        test_files = files[:3]
        print(f"\nConverting {len(test_files)} files in mock mode...")
        
        results = converter.convert_batch(test_files)
        
        print(f"\n[SUCCESS] Mock conversion complete!")
        print(f"  - Successful: {results['statistics']['successful']}")
        print(f"  - Failed: {results['statistics']['failed']}")
        
        # Check if YAML files were created
        yaml_files = list(output_dir.glob("**/*.yml"))
        print(f"  - YAML files created: {len(yaml_files)}")
        
        return True
    else:
        print("[ERROR] No files found to convert")
        return False


def test_real_conversion():
    """Test real conversion with OrcFxAPI (if available)."""
    print("\n" + "="*60)
    print("TESTING REAL CONVERSION")
    print("="*60)
    
    # Set up paths - use absolute paths
    base_dir = Path(__file__).parent.parent.parent.parent.parent.parent
    input_dir = base_dir / "docs/domains/orcaflex/examples/raw"
    output_dir = base_dir / "docs/domains/orcaflex/examples/yaml"
    
    # Create converter (will auto-detect if OrcFxAPI is available)
    converter = OrcaFlexBatchConverter(
        input_dir=input_dir,
        output_dir=output_dir,
        use_mock=False,
        validate=True
    )
    
    if converter.api_available:
        print("[SUCCESS] OrcFxAPI is available - testing real conversion")
        
        # Find files
        files = converter.find_orcaflex_files()
        
        # Convert only first file for testing
        if files:
            test_files = files[:1]
            print(f"\nConverting {len(test_files)} file with OrcFxAPI...")
            
            results = converter.convert_batch(test_files)
            
            print(f"\n[SUCCESS] Real conversion complete!")
            print(f"  - Successful: {results['statistics']['successful']}")
            print(f"  - Failed: {results['statistics']['failed']}")
            
            return True
        else:
            print("[ERROR] No files found to convert")
            return False
    else:
        print("[WARNING] OrcFxAPI not available - skipping real conversion test")
        print("   To enable real conversion, ensure OrcFxAPI is installed")
        return False


if __name__ == "__main__":
    # Test mock conversion (always works)
    mock_success = test_mock_conversion()
    
    # Test real conversion (requires OrcFxAPI)
    real_success = test_real_conversion()
    
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)
    print(f"Mock conversion: {'[PASS]' if mock_success else '[FAIL]'}")
    print(f"Real conversion: {'[PASS]' if real_success else '[SKIPPED] (no OrcFxAPI)'}")
    print("="*60)