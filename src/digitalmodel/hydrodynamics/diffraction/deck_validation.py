"""
ABOUTME: License-free sanity check of a prepared diffraction mesh via Capytaine.

OrcaWave/AQWA need a license to *solve*, but a prepared deck's hull mesh can be
hydrostatically validated for free with the open-source BEM solver Capytaine: a
correct, watertight, outward-normal hull returns a POSITIVE heave hydrostatic
stiffness K33 ≈ ρg·A_wp. A negative K33 means inverted panel normals; a wildly
off magnitude means the mesh doesn't float where expected.

Capytaine is an optional dependency. If it isn't importable, ``validate_mesh``
returns ``{"available": False, ...}`` with an install hint — it never raises on
import, so the diffraction package stays importable without it.

    uv run --with capytaine python -m ...    # quickest way to get the solver
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional

import numpy as np

_INSTALL_HINT = (
    "Capytaine not installed — run with `uv run --with capytaine ...` or add it "
    "as a dev dependency to enable license-free mesh hydrostatic validation."
)


def validate_mesh(
    gdf_path: Path | str,
    *,
    expected_waterplane_m2: Optional[float] = None,
    expected_mass_t: Optional[float] = None,
    rho: float = 1025.0,
    g: float = 9.81,
    tol: float = 0.20,
) -> dict:
    """Hydrostatically validate a hull mesh with Capytaine (if available).

    Returns a dict with ``available``, ``k33``, ``k33_positive``, and (when an
    expectation is supplied) ``k33_ratio`` / ``mass_ratio`` plus a ``checks``
    pass/fail summary. Never raises if Capytaine is absent.
    """
    try:
        from digitalmodel.hydrodynamics.capytaine.models import (
            BodyConfig,
            MeshConfig,
            MeshFormat,
            WaveConditions,
        )
        from digitalmodel.hydrodynamics.capytaine.solver import run_bem_analysis
    except Exception:  # pragma: no cover - exercised only without capytaine
        return {"available": False, "hint": _INSTALL_HINT}

    gdf_path = Path(gdf_path)
    if not gdf_path.exists():
        return {"available": True, "error": f"mesh not found: {gdf_path}"}

    waves = WaveConditions(
        periods=np.array([8.0, 12.0]),
        headings=np.array([0.0]),
        water_depth=np.inf,
        rho=rho,
        g=g,
    )
    body = BodyConfig(
        mesh=MeshConfig(path=gdf_path, format=MeshFormat.AUTO, name=gdf_path.stem)
    )
    res = run_bem_analysis(body, waves, compute_hydrostatics=True)

    K = np.asarray(res.hydrostatic_stiffness)
    k33 = float(K[2, 2])
    out: dict = {
        "available": True,
        "mesh": str(gdf_path),
        "k33": k33,
        "k33_positive": k33 > 0,
        "k44": float(K[3, 3]),
        "k55": float(K[4, 4]),
    }
    vol = getattr(res, "displaced_volume", None)
    if vol is None:
        hs = getattr(res, "hydrostatics", None)
        vol = getattr(hs, "displaced_volume", None) if hs is not None else None
    if vol is not None:
        out["displaced_volume_m3"] = float(vol)
        out["mesh_mass_t"] = float(vol) * rho / 1000.0

    checks = {"heave_stiffness_positive": k33 > 0}
    if expected_waterplane_m2:
        expected = rho * g * expected_waterplane_m2
        out["k33_expected"] = expected
        out["k33_ratio"] = k33 / expected if expected else None
        checks["k33_within_tol"] = (
            abs(out["k33_ratio"] - 1.0) <= tol if k33 > 0 else False
        )
    if expected_mass_t and vol is not None:
        out["mass_ratio"] = (
            out["mesh_mass_t"] / expected_mass_t if expected_mass_t else None
        )
        checks["floats_at_expected_mass"] = abs(out["mass_ratio"] - 1.0) <= tol
    out["checks"] = checks
    out["passed"] = all(checks.values())
    return out
