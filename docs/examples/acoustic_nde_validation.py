# ABOUTME: Example: subsea NDE validation using The Well acoustic scattering datasets.
# ABOUTME: Demonstrates AcousticScatteringLoader for all 3 variants (basic, maze, inclusions).
# ABOUTME: Dataset attribution: The Well — CC BY 4.0, Polymathic AI.

"""
Acoustic NDE Validation Example
================================

Demonstrates how to use :class:`~digitalmodel.nde.well_acoustic.AcousticScatteringLoader`
to stream acoustic scattering simulation frames from The Well dataset for subsea
Non-Destructive Evaluation (NDE) validation.

The Well provides physics-accurate PDE simulation data for acoustic wave propagation
around obstacles — directly applicable to ultrasonic inspection of subsea structures
such as pipelines, risers, and pressure vessels.

Dataset (CC BY 4.0):
    The Well — Polymathic AI
    https://github.com/PolymathicAI/the_well

Install optional dependency before running::

    pip install the_well
    # or
    pip install digitalmodel[nde]

Run::

    python docs/examples/acoustic_nde_validation.py
"""

from __future__ import annotations

import sys

from digitalmodel.nde.well_acoustic import (
    THE_WELL_ATTRIBUTION,
    WELL_AVAILABLE,
    AcousticScatteringLoader,
)

# ---------------------------------------------------------------------------
# Guard: exit gracefully when the_well is not installed
# ---------------------------------------------------------------------------

if not WELL_AVAILABLE:
    print("the_well is not installed. Run: pip install the_well")
    print("  or: pip install digitalmodel[nde]")
    sys.exit(0)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

N_STEPS = 10  # Number of simulation time steps to stream per variant
VARIANTS = ["basic", "maze", "inclusions"]

# ---------------------------------------------------------------------------
# Attribution (required by CC BY 4.0)
# ---------------------------------------------------------------------------

print(f"\nDataset attribution: {THE_WELL_ATTRIBUTION}\n")

# ---------------------------------------------------------------------------
# Stream frames for each variant
# ---------------------------------------------------------------------------

for variant in VARIANTS:
    print(f"--- Variant: {variant} ---")
    loader = AcousticScatteringLoader(variant=variant)
    sample = loader.stream_sample(n_steps=N_STEPS)

    print(f"  dataset_name : {sample['dataset_name']}")
    print(f"  frames       : {sample['n_steps']} steps collected")
    print(f"  frame[0] type: {type(sample['frames'][0]).__name__}")

    # Example: inspect the first frame's shape if it exposes .shape
    frame_0 = sample["frames"][0]
    if hasattr(frame_0, "shape"):
        print(f"  frame[0].shape: {frame_0.shape}")
    print()

# ---------------------------------------------------------------------------
# NDE validation stub
# ---------------------------------------------------------------------------

print("Subsea NDE validation summary")
print("=" * 40)
print("All 3 acoustic scattering variants successfully streamed.")
print("Next step: feed frames into NDE defect-detection pipeline.")
print(f"\n{THE_WELL_ATTRIBUTION}")
