# ABOUTME: AcousticScatteringLoader — streams acoustic scattering simulation frames
# ABOUTME: from The Well datasets for subsea NDE validation benchmarking.
# ABOUTME: Dataset attribution: The Well — CC BY 4.0, Polymathic AI.

"""
Acoustic scattering dataset loader backed by The Well (Polymathic AI).

The Well provides physics-accurate PDE simulation datasets. The acoustic
scattering family models pressure wave propagation around obstacles —
directly applicable to ultrasonic NDE of subsea structures.

Dataset variants
----------------
basic        acoustic_scattering              — single cylinder obstacles
maze         acoustic_scattering_maze         — labyrinthine obstacle geometries
inclusions   acoustic_scattering_inclusions   — multi-scatter inclusions fields

Attribution
-----------
The Well dataset — CC BY 4.0, Polymathic AI
https://github.com/PolymathicAI/the_well

Install
-------
pip install the_well

If the_well is not installed, WELL_AVAILABLE is False and stream_sample raises
RuntimeError with a clear install hint, but all other digitalmodel modules
continue to function normally.
"""

from __future__ import annotations

from typing import Any

# ---------------------------------------------------------------------------
# Optional dependency guard
# ---------------------------------------------------------------------------

try:
    from the_well.data import WellDataset  # type: ignore[import-untyped]

    WELL_AVAILABLE: bool = True
except ImportError:
    WELL_AVAILABLE = False
    WellDataset = None  # type: ignore[assignment,misc]


# ---------------------------------------------------------------------------
# Attribution constant (CC BY 4.0 — must be preserved in all derivative files)
# ---------------------------------------------------------------------------

THE_WELL_ATTRIBUTION: str = (
    "The Well dataset — CC BY 4.0, Polymathic AI"
    " (https://github.com/PolymathicAI/the_well)"
)

# ---------------------------------------------------------------------------
# Variant mapping
# ---------------------------------------------------------------------------

VARIANT_DATASET_MAP: dict[str, str] = {
    "basic": "acoustic_scattering",
    "maze": "acoustic_scattering_maze",
    "inclusions": "acoustic_scattering_inclusions",
}

VALID_VARIANTS = frozenset(VARIANT_DATASET_MAP.keys())


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------


class AcousticScatteringLoader:
    """Stream acoustic scattering simulation frames from The Well.

    Parameters
    ----------
    variant:
        Which acoustic scattering dataset to load.
        One of: ``"basic"``, ``"maze"``, ``"inclusions"``.
        Defaults to ``"basic"``.

    Examples
    --------
    >>> loader = AcousticScatteringLoader(variant="basic")
    >>> sample = loader.stream_sample(n_steps=10)
    >>> len(sample["frames"])
    10
    """

    def __init__(self, variant: str = "basic") -> None:
        if variant not in VALID_VARIANTS:
            raise ValueError(
                f"Unknown variant {variant!r}. "
                f"Valid variants: {sorted(VALID_VARIANTS)}"
            )
        self.variant = variant

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def stream_sample(self, n_steps: int = 10) -> dict[str, Any]:
        """Stream *n_steps* frames from the dataset without downloading the full set.

        Parameters
        ----------
        n_steps:
            Number of time-step frames to collect. Defaults to 10.

        Returns
        -------
        dict with keys:
            frames       list of frames (one per time step)
            variant      the variant name used
            dataset_name the canonical Well dataset name
            n_steps      number of frames collected
            attribution  CC BY 4.0 attribution string

        Raises
        ------
        RuntimeError
            When ``the_well`` package is not installed.
        """
        if not WELL_AVAILABLE:
            raise RuntimeError(
                "the_well package is required for AcousticScatteringLoader. "
                "Install it with: pip install the_well"
            )

        dataset_name = VARIANT_DATASET_MAP[self.variant]
        dataset = WellDataset(dataset_name=dataset_name, split="train")

        frames: list[Any] = []
        for frame in dataset:
            frames.append(frame)
            if len(frames) >= n_steps:
                break

        return {
            "frames": frames,
            "variant": self.variant,
            "dataset_name": dataset_name,
            "n_steps": len(frames),
            "attribution": THE_WELL_ATTRIBUTION,
        }
