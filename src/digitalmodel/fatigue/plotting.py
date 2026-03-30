"""
Fatigue Plotting Utilities

Simple matplotlib-based plots for S-N curves and damage histograms.
"""

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")  # non-interactive backend
import matplotlib.pyplot as plt
from pylife.materiallaws.woehlercurve import WoehlerCurve

from .sn_curves import get_sn_curve


def plot_sn_curve(curve_name_or_wc, ax=None, **kwargs):
    """
    Plot an S-N curve on a log-log scale.

    Parameters
    ----------
    curve_name_or_wc : str or WoehlerCurve
        Either a DNV curve name (e.g. 'F') or a WoehlerCurve instance.
    ax : matplotlib.axes.Axes, optional
        Axes to plot on. Created if None.
    **kwargs
        Passed to ax.plot().

    Returns
    -------
    matplotlib.axes.Axes
    """
    if isinstance(curve_name_or_wc, str):
        label = kwargs.pop("label", f"DNV {curve_name_or_wc}")
        wc = get_sn_curve(curve_name_or_wc)
    else:
        label = kwargs.pop("label", "S-N Curve")
        wc = curve_name_or_wc

    if ax is None:
        _, ax = plt.subplots(figsize=(8, 5))

    # Generate stress range spanning typical offshore range
    stresses = np.logspace(np.log10(10), np.log10(500), 200)
    cycles = wc.cycles(stresses)

    ax.loglog(cycles, stresses, label=label, **kwargs)
    ax.set_xlabel("Cycles to Failure (N)")
    ax.set_ylabel("Stress Range (MPa)")
    ax.set_title("S-N Curve")
    ax.grid(True, which="both", ls="--", alpha=0.5)
    ax.legend()
    return ax


def plot_damage_histogram(damage_result: pd.DataFrame, ax=None, **kwargs):
    """
    Plot per-bin damage as a bar chart.

    Parameters
    ----------
    damage_result : pd.DataFrame
        Output of miner_damage() with 'stress_range' and 'damage' columns.
    ax : matplotlib.axes.Axes, optional
        Axes to plot on. Created if None.

    Returns
    -------
    matplotlib.axes.Axes
    """
    if ax is None:
        _, ax = plt.subplots(figsize=(8, 5))

    stress_labels = [f"{s:.0f}" for s in damage_result["stress_range"]]
    ax.bar(stress_labels, damage_result["damage"], **kwargs)
    ax.set_xlabel("Stress Range (MPa)")
    ax.set_ylabel("Damage (n/N)")
    ax.set_title(
        f"Damage per Bin — Total D = {damage_result.attrs.get('total_damage', 0):.4f}"
    )
    ax.grid(True, axis="y", ls="--", alpha=0.5)
    return ax
