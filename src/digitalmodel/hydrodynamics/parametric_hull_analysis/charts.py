"""
ABOUTME: Matplotlib parametric plots for hull analysis results — RAO
comparison grids, passing ship contour plots, depth sensitivity, and
operability charts.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Optional

import numpy as np
import matplotlib

matplotlib.use("Agg")  # Non-interactive backend for batch generation
import matplotlib.pyplot as plt
from matplotlib.figure import Figure

from .models import SweepResultEntry, PassingShipSweepEntry

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# RAO plots
# ---------------------------------------------------------------------------


def rao_comparison_grid(
    results: list[SweepResultEntry],
    dofs: tuple[str, ...] = ("Heave", "Roll", "Pitch"),
    heading_idx: int = 0,
    figsize: tuple[float, float] = (14, 4),
) -> Figure:
    """Grid of RAO plots comparing all hull variations.

    One subplot per DOF, all variations overlaid with legend.
    X-axis: wave period [s], Y-axis: RAO amplitude.

    Parameters
    ----------
    results : list[SweepResultEntry]
        Sweep results with RAO data.
    dofs : tuple of str
        DOF names to plot (must match rao_result.dof_names).
    heading_idx : int
        Index into headings array to plot.
    figsize : tuple
        Figure size (width, height) in inches.

    Returns
    -------
    Figure
    """
    n_dofs = len(dofs)
    fig, axes = plt.subplots(1, n_dofs, figsize=figsize, squeeze=False)

    for col, dof_name in enumerate(dofs):
        ax = axes[0, col]
        ax.set_title(dof_name)
        ax.set_xlabel("Wave period [s]")
        ax.set_ylabel("RAO amplitude")
        ax.grid(True, alpha=0.3)

        for entry in results:
            rao = entry.rao_result
            if rao is None or rao.rao_amplitude is None:
                continue

            periods = rao.periods
            dof_names = rao.dof_names or []

            # Find DOF index
            dof_idx = None
            for i, name in enumerate(dof_names):
                if name.lower() == dof_name.lower():
                    dof_idx = i
                    break
            if dof_idx is None or dof_idx >= rao.rao_amplitude.shape[-1]:
                continue

            h_idx = min(heading_idx, rao.rao_amplitude.shape[1] - 1)
            amp = rao.rao_amplitude[:, h_idx, dof_idx]

            label = _short_label(entry.variation_id)
            ax.plot(periods, amp, label=label, linewidth=1.0)

        if len(results) <= 10:
            ax.legend(fontsize=7)

    fig.tight_layout()
    return fig


def parameter_sensitivity_plot(
    results: list[SweepResultEntry],
    param_name: str,
    dof: str = "Heave",
    metric: str = "peak_rao",
    heading_idx: int = 0,
    figsize: tuple[float, float] = (8, 5),
) -> Figure:
    """Single-parameter sensitivity: swept param vs peak RAO.

    Parameters
    ----------
    results : list[SweepResultEntry]
        Sweep results.
    param_name : str
        Name of the hull parameter to plot on x-axis.
    dof : str
        DOF name for peak RAO extraction.
    metric : str
        Currently only 'peak_rao' is supported.
    heading_idx : int
        Heading index for RAO extraction.
    figsize : tuple
        Figure size.

    Returns
    -------
    Figure
    """
    fig, ax = plt.subplots(figsize=figsize)

    x_vals = []
    y_vals = []

    for entry in results:
        param_val = entry.hull_params.get(param_name)
        if param_val is None:
            continue

        rao = entry.rao_result
        if rao is None or rao.rao_amplitude is None:
            continue

        dof_names = rao.dof_names or []
        dof_idx = None
        for i, name in enumerate(dof_names):
            if name.lower() == dof.lower():
                dof_idx = i
                break
        if dof_idx is None:
            continue

        h_idx = min(heading_idx, rao.rao_amplitude.shape[1] - 1)
        peak = float(np.nanmax(rao.rao_amplitude[:, h_idx, dof_idx]))

        x_vals.append(param_val)
        y_vals.append(peak)

    ax.scatter(x_vals, y_vals, marker="o", s=40)
    ax.plot(x_vals, y_vals, linestyle="--", alpha=0.5)
    ax.set_xlabel(param_name)
    ax.set_ylabel(f"Peak {dof} RAO")
    ax.set_title(f"Sensitivity: {param_name} vs Peak {dof} RAO")
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


def depth_sensitivity_plot(
    results_by_depth: dict[float, list[SweepResultEntry]],
    dof: str = "Heave",
    figsize: tuple[float, float] = (8, 5),
) -> Figure:
    """Added mass ratio vs h/T for validation against DNV-RP-C205.

    Overlays Capytaine finite-depth results with DNV analytical curve.

    Parameters
    ----------
    results_by_depth : dict
        Mapping of water_depth → list of SweepResultEntry.
        Must include an inf-depth entry for normalisation.
    dof : str
        DOF name for added mass extraction.
    figsize : tuple
        Figure size.

    Returns
    -------
    Figure
    """
    from .shallow_water import dnv_shallow_water_factor

    fig, ax = plt.subplots(figsize=figsize)

    # Get deep-water reference
    deep_results = results_by_depth.get(float("inf"), [])
    if not deep_results:
        ax.text(0.5, 0.5, "No deep-water reference available",
                transform=ax.transAxes, ha="center")
        return fig

    # Find DOF index from first result
    dof_idx = 2  # Default heave
    if deep_results[0].rao_result and deep_results[0].rao_result.dof_names:
        for i, name in enumerate(deep_results[0].rao_result.dof_names):
            if name.lower() == dof.lower():
                dof_idx = i
                break

    # Average deep-water added mass across frequencies for first variation
    deep_bem = deep_results[0].bem_result
    if deep_bem is None or deep_bem.added_mass is None:
        return fig
    A_deep_mean = float(np.mean(deep_bem.added_mass[:, dof_idx, dof_idx]))

    # Plot Capytaine results
    h_over_T_vals = []
    ratio_vals = []

    for depth, entries in sorted(results_by_depth.items()):
        if not np.isfinite(depth) or not entries:
            continue
        draft = entries[0].draft
        h_over_T = depth / draft

        bem = entries[0].bem_result
        if bem is None or bem.added_mass is None:
            continue
        A_shallow_mean = float(np.mean(bem.added_mass[:, dof_idx, dof_idx]))

        if abs(A_deep_mean) > 1e-10:
            ratio = A_shallow_mean / A_deep_mean
            h_over_T_vals.append(h_over_T)
            ratio_vals.append(ratio)

    ax.scatter(h_over_T_vals, ratio_vals, marker="s", s=60, zorder=5,
               label="Capytaine BEM", color="C0")

    # Plot DNV analytical curve
    h_T_curve = np.linspace(1.1, 5.0, 50)
    dnv_curve = [dnv_shallow_water_factor(x, dof.lower()) for x in h_T_curve]
    ax.plot(h_T_curve, dnv_curve, "k--", linewidth=1.5,
            label="DNV-RP-C205 Table 7-1")

    ax.set_xlabel("h / T  (water depth / draft)")
    ax.set_ylabel(f"{dof} added mass ratio  A(h) / A(∞)")
    ax.set_title(f"Shallow Water Validation: {dof} Added Mass")
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.set_xlim(1.0, 5.5)

    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Passing ship plots
# ---------------------------------------------------------------------------


def passing_ship_contour(
    results: list[PassingShipSweepEntry],
    variation_id: str,
    force_component: str = "sway",
    water_depth_m: Optional[float] = None,
    figsize: tuple[float, float] = (8, 6),
) -> Figure:
    """Contour plot: (separation x speed) -> peak force.

    Parameters
    ----------
    results : list[PassingShipSweepEntry]
        Passing ship sweep results.
    variation_id : str
        Hull variant to plot.
    force_component : str
        'surge', 'sway', or 'yaw'.
    water_depth_m : float, optional
        Filter to specific water depth.  If None, uses the first depth found.
    figsize : tuple
        Figure size.

    Returns
    -------
    Figure
    """
    filtered = [r for r in results if r.variation_id == variation_id]
    if water_depth_m is not None:
        filtered = [r for r in filtered if abs(r.water_depth_m - water_depth_m) < 0.1]

    if not filtered:
        fig, ax = plt.subplots(figsize=figsize)
        ax.text(0.5, 0.5, f"No data for {variation_id}",
                transform=ax.transAxes, ha="center")
        return fig

    col_map = {
        "surge": "peak_surge_N",
        "sway": "peak_sway_N",
        "yaw": "peak_yaw_Nm",
    }
    col = col_map.get(force_component.lower(), "peak_sway_N")

    from .passing_ship_sweep import passing_ship_to_dataframe
    df = passing_ship_to_dataframe(filtered)

    # Pivot for contour
    pivot = df.pivot_table(
        values=col, index="separation_m", columns="speed_ms", aggfunc="max",
    )

    fig, ax = plt.subplots(figsize=figsize)

    X = pivot.columns.values   # speed
    Y = pivot.index.values     # separation
    Z = pivot.values

    if X.size > 1 and Y.size > 1:
        XX, YY = np.meshgrid(X, Y)
        cs = ax.contourf(XX, YY, np.abs(Z), levels=12, cmap="viridis")
        fig.colorbar(cs, ax=ax, label=f"|{force_component.title()}| [{_force_unit(force_component)}]")
        ax.contour(XX, YY, np.abs(Z), levels=12, colors="k", linewidths=0.3)
    else:
        ax.text(0.5, 0.5, "Insufficient data for contour",
                transform=ax.transAxes, ha="center")

    ax.set_xlabel("Passing speed [m/s]")
    ax.set_ylabel("Separation [m]")
    depth_label = f"h={filtered[0].water_depth_m:.0f}m" if filtered else ""
    ax.set_title(f"Peak {force_component.title()} — {_short_label(variation_id)} {depth_label}")

    fig.tight_layout()
    return fig


def operability_chart(
    results: list[PassingShipSweepEntry],
    threshold_sway_N: float,
    threshold_yaw_Nm: float,
    figsize: tuple[float, float] = (10, 6),
) -> Figure:
    """Binary operability chart: green/red for acceptable/unacceptable.

    Shows separation vs speed faceted by hull variant.

    Parameters
    ----------
    results : list[PassingShipSweepEntry]
        Passing ship sweep results.
    threshold_sway_N : float
        Maximum acceptable lateral force [N].
    threshold_yaw_Nm : float
        Maximum acceptable yaw moment [N·m].
    figsize : tuple
        Figure size.

    Returns
    -------
    Figure
    """
    from .passing_ship_sweep import pianc_operability_check

    df = pianc_operability_check(results, threshold_sway_N, threshold_yaw_Nm)

    variants = df["variation_id"].unique()
    n_variants = len(variants)
    n_cols = min(n_variants, 3)
    n_rows = (n_variants + n_cols - 1) // n_cols

    fig, axes = plt.subplots(n_rows, n_cols, figsize=figsize, squeeze=False)

    for idx, var_id in enumerate(variants):
        row, col = divmod(idx, n_cols)
        ax = axes[row, col]

        sub = df[df["variation_id"] == var_id]
        ok = sub[sub["acceptable"]]
        nok = sub[~sub["acceptable"]]

        ax.scatter(ok["speed_ms"], ok["separation_m"],
                   c="green", marker="o", s=40, alpha=0.7, label="OK")
        ax.scatter(nok["speed_ms"], nok["separation_m"],
                   c="red", marker="x", s=40, alpha=0.7, label="Exceed")

        ax.set_title(_short_label(var_id), fontsize=9)
        ax.set_xlabel("Speed [m/s]")
        ax.set_ylabel("Separation [m]")
        ax.grid(True, alpha=0.3)
        if idx == 0:
            ax.legend(fontsize=7)

    # Hide empty subplots
    for idx in range(n_variants, n_rows * n_cols):
        row, col = divmod(idx, n_cols)
        axes[row, col].set_visible(False)

    fig.suptitle(
        f"Operability: |Sway| ≤ {threshold_sway_N/1e3:.0f} kN, "
        f"|Yaw| ≤ {threshold_yaw_Nm/1e3:.0f} kN·m",
        fontsize=11,
    )
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Export helper
# ---------------------------------------------------------------------------


def save_figure(fig: Figure, path: Path, dpi: int = 150) -> None:
    """Save figure to disk (PNG or PDF based on extension)."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path, dpi=dpi, bbox_inches="tight")
    plt.close(fig)
    logger.info("Saved figure: %s", path)


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _short_label(variation_id: str) -> str:
    """Truncate a variation_id for legend labels."""
    if "__" in variation_id:
        return variation_id.split("__", 1)[1][:30]
    return variation_id[:30]


def _force_unit(component: str) -> str:
    """Return force unit string."""
    return "N·m" if component.lower() == "yaw" else "N"


__all__ = [
    "rao_comparison_grid",
    "parameter_sensitivity_plot",
    "depth_sensitivity_plot",
    "passing_ship_contour",
    "operability_chart",
    "save_figure",
]
