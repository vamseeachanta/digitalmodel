# ABOUTME: Top-level generate_field_schematic() API — routes config to correct schematic
"""
Field Development Schematic Generator

Public API: generate_field_schematic(config) → output_path

Routes a field-parameter dictionary to the appropriate schematic class,
renders the figure, and writes it to disk as SVG or PNG.

Config keys
-----------
name : str
    Field name (required).
water_depth_m : float
    Water depth in metres (must be > 0).
n_templates : int
    Number of subsea templates.  0 → platform stand-alone route.
n_wells : int
    Wells per template (tieback) or total conductor wells (platform).
host_type : str
    'FPSO' | 'TLP' | 'SPAR' | 'fixed'
flowline_length_km : float
    Approximate flowline / tieback length.
output_format : str, optional
    'svg' or 'png' (default 'png').
output_path : str
    Destination file path (required).
development_type : str, optional
    Override schematic type: 'subsea_tieback' | 'platform' | 'fpso_spread'.
    Inferred from other keys when absent.
"""

from __future__ import annotations

import matplotlib
matplotlib.use("Agg")

from .schematics.subsea_tieback import SubseaTiebackSchematic
from .schematics.platform_standalone import PlatformSchematic
from .schematics.fpso_spread import FpsoSpreadSchematic
from .schematics.renderer import render_figure

_VALID_FORMATS = {"svg", "png"}
_VALID_HOST_TYPES = {"FPSO", "TLP", "SPAR", "fixed"}


def generate_field_schematic(config: dict) -> str:
    """Generate a field development schematic SVG or PNG from field parameters.

    Parameters
    ----------
    config : dict
        Field parameter dictionary.  See module docstring for keys.

    Returns
    -------
    str
        Absolute path to the written output file.

    Raises
    ------
    KeyError
        If a required key is missing.
    ValueError
        If a parameter value is out of range.
    """
    # --- Validate required keys ---
    name: str = config["name"]

    water_depth_m = float(config["water_depth_m"])
    if water_depth_m <= 0:
        raise ValueError(
            f"water_depth_m must be positive, got {water_depth_m}"
        )

    n_templates: int = int(config.get("n_templates", 1))
    n_wells: int = int(config.get("n_wells", 3))
    host_type: str = config.get("host_type", "FPSO")
    flowline_length_km: float = float(config.get("flowline_length_km", 5.0))
    output_format: str = config.get("output_format", "png").lower().strip()
    output_path: str = config["output_path"]
    development_type: str | None = config.get("development_type")

    if output_format not in _VALID_FORMATS:
        raise ValueError(
            f"output_format must be one of {sorted(_VALID_FORMATS)}, "
            f"got {output_format!r}"
        )

    # --- Determine schematic type ---
    if development_type is None:
        development_type = _infer_development_type(
            n_templates=n_templates,
            host_type=host_type,
            flowline_length_km=flowline_length_km,
        )

    # --- Build and render schematic ---
    fig = _build_schematic(
        development_type=development_type,
        name=name,
        water_depth_m=water_depth_m,
        n_templates=n_templates,
        n_wells=n_wells,
        host_type=host_type,
        flowline_length_km=flowline_length_km,
        config=config,
    )

    result = render_figure(fig, output_path=output_path, output_format=output_format)

    import matplotlib.pyplot as plt
    plt.close(fig)

    return result


def _infer_development_type(
    n_templates: int,
    host_type: str,
    flowline_length_km: float,
) -> str:
    """Infer the schematic class from config parameters."""
    if n_templates == 0 or host_type.lower() in ("fixed", "jacket"):
        return "platform"
    # FPSO with meaningful flowline length → subsea tieback is primary choice
    return "subsea_tieback"


def _build_schematic(
    development_type: str,
    name: str,
    water_depth_m: float,
    n_templates: int,
    n_wells: int,
    host_type: str,
    flowline_length_km: float,
    config: dict,
):
    """Instantiate the correct schematic class and call render()."""
    if development_type == "platform":
        schematic = PlatformSchematic(
            water_depth_m=water_depth_m,
            n_wells=n_wells,
            host_type=host_type,
            n_satellite_wells=int(config.get("n_satellite_wells", 0)),
            satellite_tieback_km=float(config.get("satellite_tieback_km", 3.0)),
        )
        return schematic.render(field_name=name)

    if development_type == "fpso_spread":
        schematic = FpsoSpreadSchematic(
            water_depth_m=water_depth_m,
            n_mooring_lines=int(config.get("n_mooring_lines", 8)),
            n_subsea_wells=n_wells * max(n_templates, 1),
        )
        return schematic.render(field_name=name)

    # Default: subsea_tieback
    schematic = SubseaTiebackSchematic(
        n_templates=max(n_templates, 1),
        water_depth_m=water_depth_m,
        flowline_length_km=flowline_length_km,
        host_type=host_type,
    )
    return schematic.render(field_name=name, n_wells=n_wells)
