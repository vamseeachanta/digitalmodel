"""AISC shapes database lookup for sectionproperties.

Loads W-shape dimensions from the YAML catalog and returns ready-to-mesh
sectionproperties Geometry objects.

Reference: vamseeachanta/workspace-hub#1497
"""

from pathlib import Path
from typing import Optional

import yaml
from sectionproperties.analysis import Section
from sectionproperties.pre.library import i_section

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_CATALOG_PATH = Path(__file__).resolve().parent.parent.parent / "data" / "aisc_shapes.yaml"
_IN_TO_MM = 25.4
_IN2_TO_MM2 = _IN_TO_MM**2          # 645.16
_IN3_TO_MM3 = _IN_TO_MM**3          # 16387.064
_IN4_TO_MM4 = _IN_TO_MM**4          # 416231.426
_IN6_TO_MM6 = _IN_TO_MM**6          # ~2.685e8

# Cache loaded catalog
_catalog_cache: Optional[dict] = None


def _load_catalog() -> dict:
    """Load and cache the AISC shapes YAML catalog."""
    global _catalog_cache
    if _catalog_cache is None:
        with open(_CATALOG_PATH) as f:
            data = yaml.safe_load(f)
        _catalog_cache = data["shapes"]
    return _catalog_cache


def _normalize_designation(designation: str) -> str:
    """Normalize shape designation to match catalog keys.

    Accepts 'W14x90', 'W14X90', 'w14x90', etc.
    """
    return designation.upper().replace("X", "x")


def list_shapes(series: Optional[str] = None) -> list[str]:
    """List available shapes, optionally filtered by series.

    Args:
        series: Optional series prefix, e.g. "W14", "W12".

    Returns:
        Sorted list of shape designations.
    """
    catalog = _load_catalog()
    names = list(catalog.keys())
    if series:
        prefix = series.upper().replace("X", "x")
        names = [n for n in names if n.startswith(prefix)]
    return sorted(names)


def get_aisc_dimensions(designation: str) -> dict:
    """Look up raw AISC dimensions for a shape (imperial, inches).

    Args:
        designation: AISC shape name, e.g. "W14x90".

    Returns:
        Dict with keys: d, bf, tf, tw, k_des, k_det, A, Ix, Iy, Sx, Zx, J, Cw.

    Raises:
        KeyError: If shape not found in catalog.
    """
    catalog = _load_catalog()
    key = _normalize_designation(designation)
    if key not in catalog:
        available = ", ".join(sorted(catalog.keys()))
        raise KeyError(f"Shape '{designation}' not found. Available: {available}")
    return dict(catalog[key])


def get_aisc_geometry(designation: str, unit: str = "imperial"):
    """Look up AISC shape by designation and return sectionproperties Geometry.

    Args:
        designation: AISC shape name, e.g. "W14x90".
        unit: "imperial" (inches) or "metric" (mm).

    Returns:
        sectionproperties Geometry object ready for meshing.

    Raises:
        KeyError: If shape not found in catalog.
        ValueError: If unit is not "imperial" or "metric".
    """
    if unit not in ("imperial", "metric"):
        raise ValueError(f"unit must be 'imperial' or 'metric', got '{unit}'")

    dims = get_aisc_dimensions(designation)

    # Fillet radius from design k: r = k_des - tf
    r = dims["k_des"] - dims["tf"]

    d = dims["d"]
    bf = dims["bf"]
    tf = dims["tf"]
    tw = dims["tw"]

    if unit == "metric":
        d *= _IN_TO_MM
        bf *= _IN_TO_MM
        tf *= _IN_TO_MM
        tw *= _IN_TO_MM
        r *= _IN_TO_MM

    geom = i_section(d=d, b=bf, t_f=tf, t_w=tw, r=r, n_r=16)
    return geom


def validate_shape(designation: str, mesh_size: Optional[float] = None, verbose: bool = False) -> dict:
    """Compute section properties and compare against AISC reference values.

    Args:
        designation: AISC shape name, e.g. "W14x90".
        mesh_size: FEM mesh element size (inches). Auto-scaled if None.
        verbose: Print comparison table.

    Returns:
        Dict with keys 'computed', 'reference', 'pct_diff' for each property.
    """
    dims = get_aisc_dimensions(designation)

    # Auto-scale mesh size based on smallest dimension (flange or web thickness)
    if mesh_size is None:
        min_dim = min(dims["tf"], dims["tw"])
        mesh_size = min_dim / 3.0

    geom = get_aisc_geometry(designation, unit="imperial")
    geom.create_mesh(mesh_sizes=[mesh_size])
    sec = Section(geom)
    sec.calculate_geometric_properties()
    sec.calculate_warping_properties()
    sec.calculate_plastic_properties()

    ic = sec.get_ic()      # (Ixx_c, Iyy_c, Ixy_c)
    # sectionproperties API naming is opposite to AISC convention:
    #   get_s() -> returns what sectionproperties calls "S" (plastic section modulus)
    #   get_z() -> returns what sectionproperties calls "Z" (elastic section modulus)
    # Mapping to AISC:
    #   AISC Sx (elastic = I/c) <- get_z()
    #   AISC Zx (plastic)       <- get_s()
    sp_z = sec.get_z()  # elastic in sectionproperties convention
    sp_s = sec.get_s()  # plastic in sectionproperties convention

    computed = {
        "A": sec.get_area(),
        "Ix": ic[0],
        "Iy": ic[1],
        "Sx": sp_z[0],    # AISC elastic section modulus
        "Zx": sp_s[0],    # AISC plastic section modulus
        "J": sec.get_j(),
        "Cw": sec.get_gamma(),
    }

    reference = {k: dims[k] for k in ("A", "Ix", "Iy", "Sx", "Zx", "J", "Cw")}

    pct_diff = {}
    for k in computed:
        ref = reference[k]
        if ref == 0:
            pct_diff[k] = float("inf")
        else:
            pct_diff[k] = abs(computed[k] - ref) / ref * 100

    results = {
        "designation": designation,
        "computed": computed,
        "reference": reference,
        "pct_diff": pct_diff,
    }

    if verbose:
        print(f"\n{'=' * 70}")
        print(f"  {designation} — Validation")
        print(f"{'=' * 70}")
        print(f"  {'Prop':>4s}  {'Computed':>12s}  {'AISC Ref':>12s}  {'Diff%':>8s}  Status")
        print(f"  {'-' * 62}")
        for k in ("A", "Ix", "Iy", "Sx", "Zx", "J", "Cw"):
            status = "OK" if pct_diff[k] < 3.0 else ("WARN" if pct_diff[k] < 10.0 else "CHECK")
            print(f"  {k:>4s}  {computed[k]:>12.3f}  {reference[k]:>12.3f}  {pct_diff[k]:>7.2f}%  {status}")

    return results


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import sys

    print("AISC Shapes Database — sectionproperties Lookup")
    print(f"Catalog: {_CATALOG_PATH}")
    print(f"Available shapes: {', '.join(list_shapes())}")

    # Validate first arg or default
    shape = sys.argv[1] if len(sys.argv) > 1 else "W14x90"
    validate_shape(shape, verbose=True)
