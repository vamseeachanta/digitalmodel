"""Convert OrcaFlex .sim and .dat files to .yml (ASCII) format."""

import sys
from pathlib import Path

try:
    import OrcFxAPI
except ImportError:
    print("ERROR: OrcFxAPI not available. Please install OrcaFlex Python API.")
    sys.exit(1)


def convert_to_yml(input_path: str) -> bool:
    """Convert OrcaFlex file to .yml format.
    
    Args:
        input_path: Path to .sim or .dat file
        
    Returns:
        True if successful, False otherwise
    """
    input_file = Path(input_path)
    
    if not input_file.exists():
        print(f"ERROR: File not found: {input_path}")
        return False
    
    output_file = input_file.with_suffix('.yml')
    
    try:
        print(f"Loading: {input_file.name}")
        model = OrcFxAPI.Model(str(input_file))
        
        print(f"Saving to: {output_file.name}")
        model.SaveData(str(output_file))
        
        print(f"[SUCCESS] Converted: {input_file.name} -> {output_file.name}")
        return True
        
    except Exception as e:
        print(f"[FAILED] Could not convert {input_file.name}: {e}")
        return False


def main():
    """Convert multiple OrcaFlex files to .yml format."""
    if len(sys.argv) < 2:
        print("Usage: python orcaflex_yml_converter.py <file1> [file2] ...")
        sys.exit(1)
    
    files = sys.argv[1:]
    success_count = 0
    fail_count = 0
    
    print(f"Converting {len(files)} file(s) to .yml format...\n")
    
    for file_path in files:
        if convert_to_yml(file_path):
            success_count += 1
        else:
            fail_count += 1
        print()
    
    print(f"Conversion complete: {success_count} succeeded, {fail_count} failed")
    
    return 0 if fail_count == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
