"""
ABOUTME: Parametric passing ship force sweep across hull library variants.

Uses the existing passing_ship.PassingShipCalculator (Wang 1975 slender-body
theory) rather than Capytaine multi-body, which cannot handle forward speed.
Sweeps hull variants x separation x speed x water depth.
"""

from __future__ import annotations

import itertools
import logging
from typing import Optional

import numpy as np
import pandas as pd

from .models import (
    PassingShipSweepConfig,
    PassingShipSweepEntry,
    DepthClassification,
    classify_depth,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Hull profile conversion
# ---------------------------------------------------------------------------


def hull_profile_to_vessel_config(profile: object) -> object:
    """Convert a hull_library HullProfile to a passing_ship VesselConfig.

    Parameters
    ----------
    profile : HullProfile
        Hull library hull profile.

    Returns
    -------
    VesselConfig
        Passing ship vessel configuration.
    """
    from digitalmodel.hydrodynamics.passing_ship.configuration import VesselConfig

    return VesselConfig(
        length=profile.length_bp,
        beam=profile.beam,
        draft=profile.draft,
        block_coefficient=profile.block_coefficient or 0.80,
        name=profile.name,
    )


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------


def run_passing_ship_sweep(
    hull_variants: list[tuple[str, object]],
    passing_vessel: object,
    config: PassingShipSweepConfig,
) -> list[PassingShipSweepEntry]:
    """Run passing ship force calculations across hull variants x scenarios.

    For each hull variant and each (separation, speed, water_depth) triple,
    computes peak surge, sway, and yaw forces from a time history sweep.

    Parameters
    ----------
    hull_variants : list of (variation_id, HullProfile) tuples
        Hull library variants (from HullParametricSpace.generate_profiles).
    passing_vessel : VesselConfig
        The passing vessel configuration.
    config : PassingShipSweepConfig
        Sweep configuration (separations, speeds, depths).

    Returns
    -------
    list[PassingShipSweepEntry]
        Flat list of all results.
    """
    from digitalmodel.hydrodynamics.passing_ship.calculator import (
        PassingShipCalculator,
    )
    from digitalmodel.hydrodynamics.passing_ship.configuration import (
        EnvironmentalConfig,
    )
    from digitalmodel.hydrodynamics.passing_ship.force_time_history import (
        generate_time_history,
    )

    results: list[PassingShipSweepEntry] = []
    scenarios = list(itertools.product(
        config.separations_m,
        config.speeds_ms,
        config.water_depths_m,
    ))

    for variation_id, profile in hull_variants:
        moored = hull_profile_to_vessel_config(profile)

        # Extract hull params from variation_id
        hull_params = _parse_variation_id(variation_id)

        logger.info(
            "Passing ship sweep: %s (%d scenarios)",
            variation_id, len(scenarios),
        )

        for separation, speed, depth in scenarios:
            env = EnvironmentalConfig(
                water_depth=depth if np.isfinite(depth) else None,
            )
            calc = PassingShipCalculator(
                moored_vessel=moored,
                passing_vessel=passing_vessel,
                environment=env,
            )

            # Generate time history for peak extraction
            try:
                fth = generate_time_history(
                    calc,
                    speed_ms=speed,
                    separation_m=separation,
                )
                peak_surge = float(fth.peak_surge)
                peak_sway = float(fth.peak_sway)
                peak_yaw = float(fth.peak_yaw)
            except Exception:
                logger.warning(
                    "  Failed: sep=%.0fm speed=%.1fm/s depth=%.0fm",
                    separation, speed, depth,
                    exc_info=True,
                )
                peak_surge = peak_sway = peak_yaw = float("nan")

            depth_class = classify_depth(depth, profile.draft)

            results.append(PassingShipSweepEntry(
                variation_id=variation_id,
                hull_params=hull_params,
                separation_m=separation,
                speed_ms=speed,
                water_depth_m=depth,
                peak_surge_N=peak_surge,
                peak_sway_N=peak_sway,
                peak_yaw_Nm=peak_yaw,
                depth_class=depth_class,
            ))

    logger.info("Passing ship sweep complete: %d results", len(results))
    return results


# ---------------------------------------------------------------------------
# Result processing
# ---------------------------------------------------------------------------


def passing_ship_to_dataframe(
    results: list[PassingShipSweepEntry],
) -> pd.DataFrame:
    """Convert passing ship sweep results to a DataFrame.

    Columns: variation_id, hull params, separation_m, speed_ms,
    water_depth_m, peak_surge_N, peak_sway_N, peak_yaw_Nm, depth_class.
    """
    rows = []
    for entry in results:
        row = {
            "variation_id": entry.variation_id,
            "separation_m": entry.separation_m,
            "speed_ms": entry.speed_ms,
            "water_depth_m": entry.water_depth_m,
            "peak_surge_N": entry.peak_surge_N,
            "peak_sway_N": entry.peak_sway_N,
            "peak_yaw_Nm": entry.peak_yaw_Nm,
            "depth_class": entry.depth_class.value,
        }
        for k, v in entry.hull_params.items():
            row[f"param_{k}"] = v
        rows.append(row)

    return pd.DataFrame(rows)


def peak_force_envelope(
    results: list[PassingShipSweepEntry],
    variation_id: str,
) -> dict[str, pd.DataFrame]:
    """Extract peak force envelopes for a single hull variant.

    Returns dict with keys 'surge', 'sway', 'yaw', each a DataFrame
    pivoted on (separation, speed) with peak force values.
    Suitable for contour plotting.
    """
    filtered = [r for r in results if r.variation_id == variation_id]

    if not filtered:
        return {
            "surge": pd.DataFrame(),
            "sway": pd.DataFrame(),
            "yaw": pd.DataFrame(),
        }

    df = passing_ship_to_dataframe(filtered)

    envelopes = {}
    for component, col in [
        ("surge", "peak_surge_N"),
        ("sway", "peak_sway_N"),
        ("yaw", "peak_yaw_Nm"),
    ]:
        pivot = df.pivot_table(
            values=col,
            index="separation_m",
            columns="speed_ms",
            aggfunc="max",
        )
        envelopes[component] = pivot

    return envelopes


def pianc_operability_check(
    results: list[PassingShipSweepEntry],
    max_sway_N: float,
    max_yaw_Nm: float,
) -> pd.DataFrame:
    """Flag scenarios exceeding operability thresholds.

    Parameters
    ----------
    results : list[PassingShipSweepEntry]
        Passing ship sweep results.
    max_sway_N : float
        Maximum acceptable lateral force [N].
    max_yaw_Nm : float
        Maximum acceptable yaw moment [N·m].

    Returns
    -------
    DataFrame with all result columns plus a boolean 'acceptable' column.
    """
    df = passing_ship_to_dataframe(results)
    df["acceptable"] = (
        (df["peak_sway_N"].abs() <= max_sway_N)
        & (df["peak_yaw_Nm"].abs() <= max_yaw_Nm)
    )
    return df


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _parse_variation_id(variation_id: str) -> dict[str, float]:
    """Parse hull parameters from a variation_id string."""
    params: dict[str, float] = {}
    if "__" in variation_id:
        suffix = variation_id.split("__", 1)[1]
        for part in suffix.split("_"):
            if "=" in part:
                key, val_str = part.split("=", 1)
                try:
                    params[key] = float(val_str)
                except ValueError:
                    pass
    return params


__all__ = [
    "hull_profile_to_vessel_config",
    "run_passing_ship_sweep",
    "passing_ship_to_dataframe",
    "peak_force_envelope",
    "pianc_operability_check",
]
