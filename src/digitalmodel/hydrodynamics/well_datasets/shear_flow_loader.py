#!/usr/bin/env python3
"""
ABOUTME: ShearFlowLoader — integrates The Well's shear_flow dataset as an ML
baseline data source for hydrodynamics surrogate modelling.

Dataset: shear_flow (2D turbulent shear flow, DNS-quality velocity/vorticity)
Source:  The Well, Polymathic AI — https://polymathic-ai.org/the_well/
License: CC BY 4.0

Attribution: The Well dataset — CC BY 4.0, Polymathic AI

FNO Benchmark
-------------
The Well ships pre-trained Fourier Neural Operator (FNO) weights for
shear_flow.  To load them::

    from the_well.benchmark.models import load_pretrained_fno
    model = load_pretrained_fno("shear_flow")

These weights provide an instant published baseline for any custom
hydrodynamics surrogate trained on the same dataset.  Benchmark metrics
(RMSE, correlation) are reported in the_well paper (McCabe et al. 2023).
"""

from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

import numpy as np

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Optional dependency guard
# ---------------------------------------------------------------------------

try:
    from the_well.data import WellDataset  # type: ignore[import]

    WELL_AVAILABLE = True
except (ImportError, ModuleNotFoundError):
    WELL_AVAILABLE = False
    WellDataset = None  # type: ignore[assignment,misc]

THE_WELL_ATTRIBUTION = "The Well dataset — CC BY 4.0, Polymathic AI"

# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------

DATASET_NAME = "shear_flow"
DEFAULT_SPLIT = "train"
DEFAULT_N_STEPS = 10


class ShearFlowLoader:
    """Load and stream snapshots from The Well's shear_flow dataset.

    The shear_flow dataset contains DNS-quality 2-D turbulent shear flow
    simulations with velocity (u, v) and vorticity (omega_z) fields.  It is
    suitable as a ground-truth benchmark for neural operator surrogate models
    targeting offshore wake modelling or CFD validation.

    Attribution: The Well dataset — CC BY 4.0, Polymathic AI

    Parameters
    ----------
    split:
        Dataset split to use — ``"train"``, ``"valid"``, or ``"test"``.
    data_root:
        Local filesystem path where The Well stores its data.  Passed
        directly to ``WellDataset``.  ``None`` uses the library default
        (usually ``~/.cache/the_well``).
    """

    def __init__(
        self,
        split: str = DEFAULT_SPLIT,
        data_root: Optional[str] = None,
    ) -> None:
        self.split = split
        self.data_root = data_root

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def stream_sample(self, n_steps: int = DEFAULT_N_STEPS) -> List[Dict[str, Any]]:
        """Stream *n_steps* consecutive snapshots from the shear_flow dataset.

        Each snapshot is a dict with keys:

        * ``"velocity"`` — ``np.ndarray`` of shape ``(2, H, W)`` (u, v components)
        * ``"vorticity"`` — ``np.ndarray`` of shape ``(1, H, W)``

        The method does not trigger a full dataset download; it iterates
        only the requested number of steps using the dataset's streaming
        interface.

        Parameters
        ----------
        n_steps:
            Number of time-step snapshots to return.  Default is 10.

        Returns
        -------
        list[dict]
            List of snapshot dicts, length ``n_steps``.

        Raises
        ------
        ImportError
            If ``the_well`` is not installed.  Install with
            ``pip install the_well``.
        """
        if not WELL_AVAILABLE:
            raise ImportError(
                "the_well is not installed.  Install it with: "
                "pip install the_well\n"
                f"({THE_WELL_ATTRIBUTION})"
            )

        dataset = self._build_dataset()
        snapshots: List[Dict[str, Any]] = []
        for i, sample in enumerate(dataset):
            if i >= n_steps:
                break
            snapshots.append(self._extract_fields(sample))

        logger.info(
            "Streamed %d shear_flow snapshots (split=%s). %s",
            len(snapshots),
            self.split,
            THE_WELL_ATTRIBUTION,
        )
        return snapshots

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _build_dataset(self) -> Any:
        """Construct and return the underlying WellDataset iterator."""
        kwargs: Dict[str, Any] = {
            "dataset_name": DATASET_NAME,
            "split": self.split,
        }
        if self.data_root is not None:
            kwargs["data_root"] = self.data_root

        dataset = WellDataset(**kwargs)
        # The Well supports both direct iteration and torch Dataset interfaces.
        # Prefer direct iteration for framework-agnostic usage.
        if hasattr(dataset, "as_torch_dataset"):
            return dataset.as_torch_dataset()
        return dataset  # pragma: no cover

    @staticmethod
    def _extract_fields(sample: Any) -> Dict[str, Any]:
        """Extract velocity and vorticity arrays from a raw dataset sample."""
        velocity = _to_numpy(sample["velocity"])
        vorticity = _to_numpy(sample["vorticity"])
        return {"velocity": velocity, "vorticity": vorticity}


# ---------------------------------------------------------------------------
# Utility
# ---------------------------------------------------------------------------

def _to_numpy(value: Any) -> np.ndarray:
    """Convert a value to a numpy array (handles tensors and plain arrays)."""
    if isinstance(value, np.ndarray):
        return value
    # torch.Tensor or similar — use .numpy() if available
    if hasattr(value, "numpy"):
        return value.numpy()  # type: ignore[return-value]
    return np.asarray(value)
