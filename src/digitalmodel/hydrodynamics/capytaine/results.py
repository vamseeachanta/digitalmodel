"""
ABOUTME: Result export for Capytaine BEM analysis — xarray, plots, summary tables.
ABOUTME: Provides visualization and data export for hydrodynamic coefficients and RAOs.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Union

import numpy as np

from .models import BEMResult, DOF, RAOResult

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# xarray / NetCDF export
# ---------------------------------------------------------------------------


def export_netcdf(bem_result: BEMResult, path: Union[str, Path]) -> Path:
    """Export BEM dataset to NetCDF file.

    Args:
        bem_result: Completed BEM analysis results.
        path: Output file path (.nc extension recommended).

    Returns:
        Path to the written file.
    """
    from capytaine.io.xarray import export_dataset

    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)

    export_dataset(bem_result.dataset, str(path))
    logger.info("Exported dataset to %s", path)
    return path


# ---------------------------------------------------------------------------
# Summary tables
# ---------------------------------------------------------------------------


def added_mass_table(bem_result: BEMResult, dof_pair: Optional[tuple] = None) -> str:
    """Format added mass coefficients as a readable table.

    Args:
        bem_result: BEM results.
        dof_pair: Optional (radiating_dof, influenced_dof) to filter. If None, shows diagonal.

    Returns:
        Formatted string table.
    """
    ds = bem_result.dataset
    dofs = bem_result.dof_names
    periods = bem_result.periods

    lines = ["Added Mass Coefficients"]
    lines.append("=" * 60)

    if dof_pair is not None:
        rdof, idof = dof_pair
        values = ds["added_mass"].sel(radiating_dof=rdof, influenced_dof=idof).values
        lines.append(f"  {rdof} → {idof}")
        lines.append(f"  {'Period (s)':>12s}  {'Added Mass (kg)':>18s}")
        lines.append(f"  {'-'*12}  {'-'*18}")
        for t, v in zip(periods, values):
            lines.append(f"  {t:12.3f}  {v:18.3f}")
    else:
        for dof in dofs:
            values = ds["added_mass"].sel(radiating_dof=dof, influenced_dof=dof).values
            lines.append(f"\n  {dof} (diagonal)")
            lines.append(f"  {'Period (s)':>12s}  {'Added Mass':>18s}")
            lines.append(f"  {'-'*12}  {'-'*18}")
            for t, v in zip(periods, values):
                lines.append(f"  {t:12.3f}  {v:18.3f}")

    return "\n".join(lines)


def excitation_force_table(bem_result: BEMResult, heading_idx: int = 0) -> str:
    """Format excitation forces as a readable table.

    Args:
        bem_result: BEM results.
        heading_idx: Index of the wave heading to display.

    Returns:
        Formatted string table.
    """
    ds = bem_result.dataset
    dofs = bem_result.dof_names
    periods = bem_result.periods
    heading_deg = np.rad2deg(bem_result.headings[heading_idx])

    lines = [f"Excitation Forces — heading = {heading_deg:.1f} deg"]
    lines.append("=" * 80)

    header = f"  {'Period (s)':>10s}"
    for dof in dofs:
        header += f"  {'|F_' + dof + '|':>12s}"
    lines.append(header)
    lines.append("  " + "-" * (10 + 14 * len(dofs)))

    exc = ds["excitation_force"]
    heading_val = bem_result.headings[heading_idx]

    for i, t in enumerate(periods):
        row = f"  {t:10.3f}"
        for dof in dofs:
            val = exc.sel(
                omega=bem_result.omegas[i],
                wave_direction=heading_val,
                influenced_dof=dof,
            ).values
            row += f"  {np.abs(val):12.3f}"
        lines.append(row)

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Matplotlib plots
# ---------------------------------------------------------------------------


def plot_added_mass_damping(
    bem_result: BEMResult,
    dofs: Optional[List[str]] = None,
    save_path: Optional[Union[str, Path]] = None,
    show: bool = False,
):
    """Plot added mass and radiation damping vs period for diagonal DOF terms.

    Args:
        bem_result: BEM results.
        dofs: DOFs to plot (default: all).
        save_path: If provided, save figure to this path.
        show: If True, call plt.show().

    Returns:
        matplotlib Figure object.
    """
    import matplotlib.pyplot as plt

    ds = bem_result.dataset
    dofs = dofs or bem_result.dof_names
    periods = bem_result.periods

    fig, axes = plt.subplots(2, len(dofs), figsize=(5 * len(dofs), 8), squeeze=False)
    fig.suptitle(f"Hydrodynamic Coefficients — {bem_result.body_name}", fontsize=14)

    for j, dof in enumerate(dofs):
        am = ds["added_mass"].sel(radiating_dof=dof, influenced_dof=dof).values
        rd = ds["radiation_damping"].sel(radiating_dof=dof, influenced_dof=dof).values

        axes[0, j].plot(periods, am, "b-o", markersize=3)
        axes[0, j].set_title(f"Added Mass — {dof}")
        axes[0, j].set_xlabel("Period (s)")
        axes[0, j].set_ylabel("Added Mass (kg)")
        axes[0, j].grid(True, alpha=0.3)

        axes[1, j].plot(periods, rd, "r-o", markersize=3)
        axes[1, j].set_title(f"Damping — {dof}")
        axes[1, j].set_xlabel("Period (s)")
        axes[1, j].set_ylabel("Radiation Damping (N·s/m)")
        axes[1, j].grid(True, alpha=0.3)

    fig.tight_layout()

    if save_path:
        save_path = Path(save_path)
        save_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(str(save_path), dpi=150, bbox_inches="tight")
        logger.info("Saved added mass/damping plot to %s", save_path)

    if show:
        plt.show()

    return fig


def plot_rao(
    rao_result: RAOResult,
    dofs: Optional[List[str]] = None,
    heading_idx: int = 0,
    save_path: Optional[Union[str, Path]] = None,
    show: bool = False,
):
    """Plot RAO amplitude and phase vs period.

    Args:
        rao_result: RAO computation results.
        dofs: DOFs to plot (default: all).
        heading_idx: Wave heading index to plot.
        save_path: If provided, save figure to this path.
        show: If True, call plt.show().

    Returns:
        matplotlib Figure object.
    """
    import matplotlib.pyplot as plt

    dofs = dofs or rao_result.dof_names
    periods = rao_result.periods
    heading_deg = np.rad2deg(rao_result.headings[heading_idx])

    n_dof = len(dofs)
    fig, axes = plt.subplots(2, n_dof, figsize=(5 * n_dof, 8), squeeze=False)
    fig.suptitle(f"RAO — heading = {heading_deg:.1f} deg", fontsize=14)

    all_dof_names = rao_result.dof_names

    for j, dof in enumerate(dofs):
        dof_idx = all_dof_names.index(dof)
        amp = rao_result.rao_amplitude[:, heading_idx, dof_idx]
        phase = rao_result.rao_phase[:, heading_idx, dof_idx]

        unit = "m/m" if dof in ("Surge", "Sway", "Heave") else "deg/m"

        axes[0, j].plot(periods, amp, "b-o", markersize=3)
        axes[0, j].set_title(f"|RAO| — {dof}")
        axes[0, j].set_xlabel("Period (s)")
        axes[0, j].set_ylabel(f"RAO ({unit})")
        axes[0, j].grid(True, alpha=0.3)

        axes[1, j].plot(periods, phase, "g-o", markersize=3)
        axes[1, j].set_title(f"Phase — {dof}")
        axes[1, j].set_xlabel("Period (s)")
        axes[1, j].set_ylabel("Phase (deg)")
        axes[1, j].grid(True, alpha=0.3)

    fig.tight_layout()

    if save_path:
        save_path = Path(save_path)
        save_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(str(save_path), dpi=150, bbox_inches="tight")
        logger.info("Saved RAO plot to %s", save_path)

    if show:
        plt.show()

    return fig


def plot_excitation_force(
    bem_result: BEMResult,
    dofs: Optional[List[str]] = None,
    heading_idx: int = 0,
    save_path: Optional[Union[str, Path]] = None,
    show: bool = False,
):
    """Plot excitation force amplitude vs period.

    Args:
        bem_result: BEM results.
        dofs: DOFs to plot (default: all).
        heading_idx: Wave heading index.
        save_path: If provided, save figure to this path.
        show: If True, call plt.show().

    Returns:
        matplotlib Figure object.
    """
    import matplotlib.pyplot as plt

    ds = bem_result.dataset
    dofs = dofs or bem_result.dof_names
    periods = bem_result.periods
    heading_val = bem_result.headings[heading_idx]
    heading_deg = np.rad2deg(heading_val)

    fig, ax = plt.subplots(figsize=(10, 6))
    fig.suptitle(f"Excitation Forces — heading = {heading_deg:.1f} deg", fontsize=14)

    for dof in dofs:
        exc = ds["excitation_force"].sel(
            wave_direction=heading_val,
            influenced_dof=dof,
        ).values
        ax.plot(periods, np.abs(exc), "-o", markersize=3, label=dof)

    ax.set_xlabel("Period (s)")
    ax.set_ylabel("|Excitation Force|")
    ax.legend()
    ax.grid(True, alpha=0.3)
    fig.tight_layout()

    if save_path:
        save_path = Path(save_path)
        save_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(str(save_path), dpi=150, bbox_inches="tight")
        logger.info("Saved excitation force plot to %s", save_path)

    if show:
        plt.show()

    return fig
