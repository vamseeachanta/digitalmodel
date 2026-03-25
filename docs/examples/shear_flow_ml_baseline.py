#!/usr/bin/env python3
"""
shear_flow_ml_baseline.py
=========================
Demonstrates how to use ShearFlowLoader to stream DNS turbulent shear flow
snapshots from The Well as an ML baseline data source for hydrodynamics models.

Attribution: The Well dataset — CC BY 4.0, Polymathic AI
Install:     pip install the_well  (or: pip install digitalmodel[well])

Overview
--------
The Well's ``shear_flow`` dataset contains 2-D DNS turbulent shear flow
simulations at Reynolds number ~1000.  Each snapshot provides:

* ``velocity`` — np.ndarray shape (2, H, W): (u, v) velocity components
* ``vorticity`` — np.ndarray shape (1, H, W): out-of-plane vorticity omega_z

FNO Benchmark
-------------
The Well ships pre-trained FNO (Fourier Neural Operator) weights for the
shear_flow dataset.  Load them with::

    from the_well.benchmark.models import load_pretrained_fno
    fno_model = load_pretrained_fno("shear_flow")

These weights give an instant published baseline (RMSE, correlation) to
compare any custom surrogate model.  See McCabe et al. (2023) for metrics.

Usage
-----
Run this file directly::

    PYTHONPATH=src python3 docs/examples/shear_flow_ml_baseline.py
"""

from __future__ import annotations

import logging

import numpy as np

from digitalmodel.hydrodynamics.well_datasets import (
    WELL_AVAILABLE,
    THE_WELL_ATTRIBUTION,
    ShearFlowLoader,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def print_snapshot_summary(snapshots: list) -> None:
    """Print shape and statistics for a list of streamed snapshots."""
    for i, snap in enumerate(snapshots):
        vel = snap["velocity"]
        vort = snap["vorticity"]
        print(
            f"  [{i:02d}] velocity {vel.shape}  "
            f"u_rms={float(np.sqrt(np.mean(vel**2))):.4f}  "
            f"vorticity {vort.shape}  "
            f"omega_rms={float(np.sqrt(np.mean(vort**2))):.4f}"
        )


def main() -> None:
    print(f"\n{THE_WELL_ATTRIBUTION}")
    print("=" * 60)

    if not WELL_AVAILABLE:
        print(
            "\nthe_well is not installed.  Install it with:\n"
            "    pip install the_well\n"
            "or:\n"
            "    pip install digitalmodel[well]\n"
        )
        return

    # -----------------------------------------------------------------------
    # Stream 10 training snapshots (default)
    # -----------------------------------------------------------------------
    loader = ShearFlowLoader(split="train")
    print("\nStreaming 10 snapshots (train split) …")
    snapshots = loader.stream_sample(n_steps=10)
    print(f"Received {len(snapshots)} snapshots:")
    print_snapshot_summary(snapshots)

    # -----------------------------------------------------------------------
    # Quick statistics across the batch
    # -----------------------------------------------------------------------
    velocity_stack = np.stack([s["velocity"] for s in snapshots], axis=0)
    vorticity_stack = np.stack([s["vorticity"] for s in snapshots], axis=0)

    print(
        f"\nBatch velocity shape : {velocity_stack.shape}"
        f"  (n_steps, components, H, W)"
    )
    print(
        f"Batch vorticity shape: {vorticity_stack.shape}"
        f"  (n_steps, 1, H, W)"
    )
    print(
        f"Velocity  — mean={velocity_stack.mean():.4f}  "
        f"std={velocity_stack.std():.4f}"
    )
    print(
        f"Vorticity — mean={vorticity_stack.mean():.4f}  "
        f"std={vorticity_stack.std():.4f}"
    )

    # -----------------------------------------------------------------------
    # Demonstrate FNO benchmark (informational — requires the_well extras)
    # -----------------------------------------------------------------------
    print(
        "\nTo load pre-trained FNO weights for the shear_flow benchmark:\n"
        "    from the_well.benchmark.models import load_pretrained_fno\n"
        "    model = load_pretrained_fno('shear_flow')\n"
        "    prediction = model(velocity_stack)  # shape: (n, 2, H, W)\n"
    )

    print("\nDone.  " + THE_WELL_ATTRIBUTION)


if __name__ == "__main__":
    main()
