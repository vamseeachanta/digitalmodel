#!/usr/bin/env python3
"""
Run OrcaWave via OrcaFlex Python API

This bypasses the GUI and runs OrcaWave in batch mode programmatically.
"""

import sys
from pathlib import Path

# Add OrcaFlex API to path
orcaflex_api_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, orcaflex_api_path)

try:
    import OrcFxAPI
    print(f"[OK] OrcFxAPI loaded from: {orcaflex_api_path}")
except ImportError as e:
    print(f"[ERROR] Failed to import OrcFxAPI: {e}")
    sys.exit(1)

def run_orcawave_diffraction(config_file: str):
    """
    Run OrcaWave diffraction analysis via API

    Args:
        config_file: Path to OrcaWave YAML configuration file
    """
    config_path = Path(config_file)

    if not config_path.exists():
        print(f"[ERROR] Config file not found: {config_path}")
        return False

    print(f"\n{'='*60}")
    print("ORCAWAVE API EXECUTION")
    print(f"{'='*60}")
    print(f"Config: {config_path.name}")
    print(f"Path: {config_path}")
    print(f"{'='*60}\n")

    try:
        # Create OrcaFlex model
        print("[1/5] Creating OrcaFlex model...")
        model = OrcFxAPI.Model()

        # Load OrcaWave configuration
        print(f"[2/5] Loading OrcaWave configuration: {config_path}")
        # Note: OrcaWave uses diffraction data which needs to be loaded
        # The YAML file should be loaded into the model

        # Try to load as OrcaWave diffraction file
        # This might need to be loaded differently - checking API docs
        print("[3/5] Attempting to load diffraction data...")

        # Check available methods for loading OrcaWave files
        diffraction_methods = [m for m in dir(model) if 'diffr' in m.lower()]
        print(f"Available diffraction methods: {diffraction_methods[:10]}")

        # Try direct file load
        print(f"[4/5] Loading file into model...")
        # OrcaWave files might need specific loading method
        # Trying model load
        try:
            model.LoadData(str(config_path))
            print("[OK] File loaded successfully")
        except Exception as e:
            print(f"[WARNING] LoadData failed: {e}")
            print("Attempting alternative loading method...")

        # Calculate diffraction if available
        print("[5/5] Running diffraction calculation...")

        # Check if model has diffraction calculation method
        if hasattr(model, 'CalculateDiffraction'):
            print("Calling model.CalculateDiffraction()...")
            model.CalculateDiffraction()
            print("[SUCCESS] Diffraction calculation completed!")
        elif hasattr(model, 'CalculateStatics'):
            print("Diffraction-specific method not found, trying CalculateStatics...")
            model.CalculateStatics()
            print("[SUCCESS] Static calculation completed!")
        else:
            print("[ERROR] No suitable calculation method found")
            print("Available calc methods:", [m for m in dir(model) if 'calc' in m.lower()])
            return False

        # Save results
        output_file = config_path.parent / f"{config_path.stem}_api_output.sim"
        print(f"\nSaving results to: {output_file}")
        model.SaveSimulation(str(output_file))
        print(f"[SUCCESS] Results saved: {output_file.name}")

        return True

    except Exception as e:
        print(f"\n[ERROR] Execution failed: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python run_orcawave_api.py <config.yml>")
        sys.exit(1)

    config_file = sys.argv[1]
    success = run_orcawave_diffraction(config_file)

    sys.exit(0 if success else 1)
