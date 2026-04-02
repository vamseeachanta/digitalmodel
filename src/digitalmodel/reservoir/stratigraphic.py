# ABOUTME: Stratigraphic cross-section plotting for well log analysis — #1633.
# ABOUTME: Refactored from raw script to importable module with typed functions:
# ABOUTME: create_cross_section(), plot_gr_track(), plot_rt_track(),
# ABOUTME: plot_rhob_nphi_track(), plot_facies_track().
"""
Stratigraphic Cross-Section Plotter

Creates multi-well cross-section plots with gamma-ray, resistivity,
density-neutron, and facies tracks. Aligns wells at formation tops
and connects correlations.

Functions:
    create_cross_section: Main entry — builds the full cross-section figure.
    plot_gr_track: Gamma ray log track with shading.
    plot_rt_track: Resistivity track with rainbow gradient fill.
    plot_rhob_nphi_track: Density-neutron overlay with crossover shading.
    plot_facies_track: Facies classification color track.

Usage:
    from digitalmodel.reservoir.stratigraphic import create_cross_section

    fig = create_cross_section(
        wells_list=["WELL_A", "WELL_B"],
        df_logs=my_logs_df,
        statdata=my_tops_df,
    )
    fig.savefig("cross_section.png")
"""
from __future__ import annotations

from typing import Optional

import matplotlib.colors as mpl_colors
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


# ---------------------------------------------------------------------------
# Default facies color map
# ---------------------------------------------------------------------------

DEFAULT_FACIES_COLORS: dict[int, str] = {
    0: "m",
    1: "blue",
    2: "red",
    3: "green",
    4: "yellow",
    5: "black",
}

REQUIRED_LOG_COLUMNS = {"UWI", "Depth", "GR", "RT", "RHOB", "PHIN", "KMeans"}
REQUIRED_TOPS_COLUMNS = {"UWI", "TOP", "BASE"}


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

def _validate_inputs(
    wells_list: list[str],
    df_logs: pd.DataFrame,
    statdata: pd.DataFrame,
) -> None:
    """Validate inputs for create_cross_section.

    Raises:
        ValueError: If inputs are invalid.
    """
    if not wells_list:
        raise ValueError("wells_list must not be empty")

    missing_log_cols = REQUIRED_LOG_COLUMNS - set(df_logs.columns)
    if missing_log_cols:
        raise ValueError(
            f"df_logs missing required column(s): {sorted(missing_log_cols)}"
        )

    missing_tops_cols = REQUIRED_TOPS_COLUMNS - set(statdata.columns)
    if missing_tops_cols:
        raise ValueError(
            f"statdata missing required column(s): {sorted(missing_tops_cols)}"
        )


# ---------------------------------------------------------------------------
# Individual track plotters
# ---------------------------------------------------------------------------

def plot_gr_track(
    ax: plt.Axes,
    df: pd.DataFrame,
    well_name: str = "",
    gr_cutoff: float = 30.0,
    xlim: tuple[float, float] = (0, 75),
) -> None:
    """Plot gamma ray log track with shading.

    Args:
        ax: Matplotlib axes to plot on.
        df: Well log DataFrame with 'GR' and 'Adj_Depth' columns.
        well_name: Well name for title.
        gr_cutoff: GR threshold for sand/shale shading (default 30).
        xlim: X-axis limits for GR scale.
    """
    ax.plot(df["GR"], df["Adj_Depth"], color="green", label="GR")
    ax.fill_betweenx(
        df["Adj_Depth"], df["GR"], gr_cutoff,
        where=(df["GR"] < gr_cutoff),
        color="yellow", alpha=0.5,
    )
    ax.fill_betweenx(
        df["Adj_Depth"], gr_cutoff, df["GR"],
        where=(df["GR"] > gr_cutoff),
        color="grey", alpha=0.5,
    )
    ax.invert_yaxis()
    if well_name:
        ax.set_title(well_name)
    ax.set_xlim(*xlim)
    ax.legend(loc="upper right")


def plot_rt_track(
    ax: plt.Axes,
    df: pd.DataFrame,
    xlim: tuple[float, float] = (1, 1000),
) -> None:
    """Plot resistivity track with rainbow gradient fill.

    Args:
        ax: Matplotlib axes to plot on.
        df: Well log DataFrame with 'RT' and 'Adj_Depth' columns.
        xlim: X-axis limits (log scale).
    """
    norm = plt.Normalize(df["RT"].min(), df["RT"].max())
    colors_rt = plt.cm.rainbow(norm(df["RT"]))

    ax.plot(df["RT"], df["Adj_Depth"], color="black", label="RT")

    for j in range(len(df) - 1):
        ax.fill_betweenx(
            df["Adj_Depth"].iloc[j : j + 2],
            0,
            df["RT"].iloc[j : j + 2],
            color=colors_rt[j],
            edgecolor="none",
        )

    ax.invert_yaxis()
    ax.set_xlim(*xlim)
    ax.set_xscale("log")
    ax.legend(loc="upper right")


def plot_rhob_nphi_track(
    ax: plt.Axes,
    df: pd.DataFrame,
    rhob_xlim: tuple[float, float] = (1.95, 2.95),
    nphi_xlim: tuple[float, float] = (0.45, -0.15),
    matrix_density: float = 2.71,
    fluid_density: float = 1.0,
) -> None:
    """Plot density-neutron overlay track with crossover shading.

    Args:
        ax: Matplotlib axes to plot on.
        df: Well log DataFrame with 'RHOB', 'PHIN', and 'Adj_Depth' columns.
        rhob_xlim: X-axis limits for RHOB.
        nphi_xlim: X-axis limits for NPHI (reversed).
        matrix_density: Matrix density for DPHI calculation.
        fluid_density: Fluid density for DPHI calculation.
    """
    df = df.copy()
    df["DPHI"] = (matrix_density - df["RHOB"]) / (matrix_density - fluid_density)

    # RHOB on secondary x-axis
    secax_rhob = ax.twiny()
    secax_rhob.plot(df["RHOB"], df["Adj_Depth"], color="red", label="RHOB")
    secax_rhob.invert_yaxis()
    secax_rhob.set_xlim(*rhob_xlim)
    secax_rhob.legend(loc="upper right")

    # NPHI on another secondary x-axis
    secax_nphi = ax.twiny()
    secax_nphi.plot(df["PHIN"], df["Adj_Depth"], color="blue", label="NPHI")
    secax_nphi.set_xlim(*nphi_xlim)
    secax_nphi.legend(loc="upper left")

    # Crossover fill
    secax_nphi.fill_betweenx(
        df["Adj_Depth"], df["DPHI"], df["PHIN"],
        where=(df["DPHI"] > df["PHIN"]),
        color="yellow", alpha=0.7,
    )
    secax_nphi.fill_betweenx(
        df["Adj_Depth"], df["DPHI"], df["PHIN"],
        where=(df["DPHI"] < df["PHIN"]),
        color="grey", alpha=0.7,
    )

    return secax_rhob, secax_nphi


def plot_facies_track(
    ax: plt.Axes,
    df: pd.DataFrame,
    facies_col: str = "KMeans",
    facies_colors: Optional[dict[int, str]] = None,
) -> None:
    """Plot facies classification color track.

    Args:
        ax: Matplotlib axes to plot on.
        df: Well log DataFrame with facies column and 'Adj_Depth'.
        facies_col: Column name containing facies codes.
        facies_colors: Dict mapping facies code -> color string.
    """
    if facies_colors is None:
        facies_colors = DEFAULT_FACIES_COLORS

    color_list = [facies_colors[i] for i in sorted(facies_colors.keys())]
    cmap_facies = mpl_colors.ListedColormap(color_list)

    facies_data = df[[facies_col]].values
    ax.imshow(
        facies_data,
        aspect="auto",
        cmap=cmap_facies,
        extent=[-1, 1, df["Adj_Depth"].max(), df["Adj_Depth"].min()],
    )
    ax.set_xlabel("FACIES")


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

def create_cross_section(
    wells_list: list[str],
    df_logs: pd.DataFrame,
    statdata: pd.DataFrame,
    facies_colors: Optional[dict[int, str]] = None,
    figsize: tuple[int, int] = (18, 10),
) -> plt.Figure:
    """Create a multi-well stratigraphic cross-section plot.

    Args:
        wells_list: Ordered list of well identifiers (UWI).
        df_logs: Well log data with columns: UWI, Depth, GR, RT, RHOB, PHIN, KMeans.
        statdata: Formation tops data with columns: UWI, TOP, BASE.
        facies_colors: Optional dict mapping facies code -> color string.
        figsize: Figure size as (width, height).

    Returns:
        matplotlib Figure with the cross-section plot.

    Raises:
        ValueError: If inputs are invalid (empty wells_list, missing columns).
    """
    _validate_inputs(wells_list, df_logs, statdata)

    if facies_colors is None:
        facies_colors = DEFAULT_FACIES_COLORS

    # Filter and sort data
    well_data_df = df_logs[df_logs["UWI"].isin(wells_list)].copy()
    well_data_df["UWI"] = pd.Categorical(
        well_data_df["UWI"], categories=wells_list, ordered=True
    )
    well_data_df = well_data_df.sort_values(by=["UWI", "Depth"]).reset_index(drop=True)

    # Create tops DataFrame
    tops_df = statdata[statdata["UWI"].isin(wells_list)].copy()
    tops_df["Thick"] = tops_df["BASE"] - tops_df["TOP"]
    well_tops = tops_df.set_index("UWI")[["TOP", "BASE"]].to_dict(orient="index")
    well_thick = tops_df.groupby("UWI")["Thick"].apply(list).to_dict()
    wells = well_data_df.groupby("UWI")

    n_wells = len(wells_list)
    fig, axes = plt.subplots(
        nrows=1,
        ncols=4 * n_wells,
        figsize=figsize,
        gridspec_kw={
            "hspace": 0,
            "wspace": 0,
            "width_ratios": [1, 0.8, 0.8, 0.6] * n_wells,
        },
        sharey=True,
    )

    # Ensure axes is always a list
    if n_wells == 1:
        axes = list(axes) if hasattr(axes, "__iter__") else [axes]

    tops_and_bases: list[tuple] = []

    for i, (well_name, df) in enumerate(wells):
        well_info = well_tops.get(well_name)
        if well_info is None:
            continue
        top_depth = well_info["TOP"]
        base_depth = well_info["BASE"]

        tops_and_bases.append((well_name, top_depth, base_depth, i))

        df = df.copy()
        df["Adj_Depth"] = df["Depth"] - top_depth

        track_gr = i * 4
        track_rt = i * 4 + 1
        track_rhob_nphi = i * 4 + 2
        track_facies = i * 4 + 3

        plot_gr_track(axes[track_gr], df, well_name=str(well_name))
        plot_rt_track(axes[track_rt], df)
        result = plot_rhob_nphi_track(axes[track_rhob_nphi], df)
        plot_facies_track(axes[track_facies], df, facies_colors=facies_colors)

        # Remove tick labels from interior tracks
        secax_rhob, secax_nphi = result if result else (None, None)
        for ax in [axes[track_gr], axes[track_rhob_nphi], axes[track_rt], axes[track_facies]]:
            ax.set_xticks([])
            ax.set_xlabel("")
        if secax_rhob:
            secax_rhob.set_xticks([])
            secax_rhob.set_xlabel("")
        if secax_nphi:
            secax_nphi.set_xticks([])
            secax_nphi.set_xlabel("")

        # Draw formation thickness lines
        if well_name in well_thick:
            for base in well_thick[well_name]:
                for ax in [axes[track_gr], axes[track_rt], axes[track_rhob_nphi]]:
                    ax.axhline(y=base, color="purple", linestyle="--", linewidth=2)

    # Connect bases between adjacent wells
    for i in range(len(tops_and_bases) - 1):
        current_well_name, _, current_base_depth, _ = tops_and_bases[i]
        next_well_name, _, next_base_depth, _ = tops_and_bases[i + 1]

        adjusted_current_base = current_base_depth - \
            tops_df[tops_df["UWI"] == current_well_name]["TOP"].values[0]
        adjusted_next_base = next_base_depth - \
            tops_df[tops_df["UWI"] == next_well_name]["TOP"].values[0]

        axes[i * 4 + 3].plot(
            [-1, 1],
            [adjusted_current_base, adjusted_next_base],
            color="purple", linestyle="--", linewidth=2,
        )

    # Flatten line at top
    for ax in axes:
        ax.axhline(y=0, color="black", linewidth=1.5)

    plt.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    print("Stratigraphic cross-section plotter.")
    print("Usage: from digitalmodel.reservoir.stratigraphic import create_cross_section")
    print("This module requires well log data (df_logs) and formation tops (statdata).")
    print("See docstring for details.")
