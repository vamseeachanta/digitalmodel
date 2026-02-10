"""Generate pre-computed statics.sim for 24in monolithic model.

This script loads the monolithic 24in pipeline YAML, runs CalculateStatics(),
and saves the result as a .sim file for efficient test reuse.

The 24in monolithic model has 6 fine-mesh sections over 4900m and may take
>30 minutes for statics to converge.

Usage:
    uv run python scripts/generate_24in_statics_sim.py
"""

from pathlib import Path
import time

MONOLITHIC_YML = Path(
    "docs/modules/orcaflex/pipeline/installation/floating/24in_pipeline/"
    "monolithic/basefile/D24inL4900mBuoy7kNSpacing1500mm.yml"
)
OUTPUT_SIM = MONOLITHIC_YML.parent / "statics.sim"


def main():
    try:
        import OrcFxAPI
    except ImportError:
        print("ERROR: OrcFxAPI not available")
        return 1

    print(f"Loading monolithic YAML: {MONOLITHIC_YML}")
    model = OrcFxAPI.Model(str(MONOLITHIC_YML))
    print(f"Model loaded with {len(model.objects)} objects")

    print("Running CalculateStatics() — this may take >30 minutes...")
    start = time.time()
    model.CalculateStatics()
    elapsed = time.time() - start
    print(f"Statics converged in {elapsed:.1f} seconds ({elapsed/60:.1f} minutes)")

    print(f"Saving simulation to: {OUTPUT_SIM}")
    model.SaveSimulation(str(OUTPUT_SIM))
    print(f"Done! File size: {OUTPUT_SIM.stat().st_size / 1e6:.1f} MB")
    return 0


if __name__ == "__main__":
    exit(main())
