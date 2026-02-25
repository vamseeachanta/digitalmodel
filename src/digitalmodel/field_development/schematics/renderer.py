# ABOUTME: Common figure rendering function — saves SVG or PNG from a matplotlib figure
"""
Common render function for field development schematics.

Abstracts format-specific save logic; creates parent directories as needed.
"""

from __future__ import annotations

from pathlib import Path

import matplotlib
import matplotlib.pyplot as plt
from matplotlib.figure import Figure

_SUPPORTED_FORMATS = {"svg", "png"}
_DEFAULT_DPI = 150
_HIGH_DPI = 300


def render_figure(
    fig: Figure,
    output_path: str,
    output_format: str,
    dpi: int | None = None,
    transparent: bool = False,
) -> str:
    """Save a matplotlib Figure to disk as SVG or PNG.

    Parameters
    ----------
    fig : Figure
        The figure to save.
    output_path : str
        Destination file path.  Parent directories are created if missing.
    output_format : str
        ``'svg'`` or ``'png'``.
    dpi : int, optional
        Resolution for PNG export.  Defaults to 300 for PNG, ignored for SVG.
    transparent : bool
        If True, save with a transparent background.

    Returns
    -------
    str
        The output_path that was written.

    Raises
    ------
    ValueError
        If output_format is not in the supported set.
    """
    fmt = output_format.lower().strip()
    if fmt not in _SUPPORTED_FORMATS:
        raise ValueError(
            f"Unsupported format {output_format!r}. "
            f"Supported formats: {sorted(_SUPPORTED_FORMATS)}"
        )

    dest = Path(output_path)
    dest.parent.mkdir(parents=True, exist_ok=True)

    save_kwargs: dict = {
        "format": fmt,
        "bbox_inches": "tight",
        "transparent": transparent,
        "facecolor": fig.get_facecolor(),
    }

    if fmt == "png":
        save_kwargs["dpi"] = dpi if dpi is not None else _HIGH_DPI

    fig.savefig(str(dest), **save_kwargs)
    return str(dest)
