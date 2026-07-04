"""Build the diffraction sparse atlas library (#801).

Operator step: replace the STUB heave-RAO values below with a licensed
OrcaWave/AQWA diffraction run per canonical case (one run yields the full
frequency x heading response for that vessel/draft). The library structure,
coverage map, and query path are identical either way — only the recorded
values change.

Usage:  uv run python scripts/parametric/build_diffraction_library.py
"""

from __future__ import annotations

import math
from pathlib import Path

from digitalmodel.parametric.atlas import Axis
from digitalmodel.parametric.library import build_library_atlas

REPO_ROOT = Path(__file__).resolve().parents[2]

# Canonical cases = the coverage map (exact-match key). Each is ONE licensed run.
CASES = {
    "fpso-design-draft": {"omega_n": 0.40, "zeta": 0.10},
    "fpso-ballast-draft": {"omega_n": 0.46, "zeta": 0.10},
    "semisub-operating": {"omega_n": 0.26, "zeta": 0.08},
}
FREQUENCIES = [0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 1.2]  # rad/s
HEADINGS = [0.0, 45.0, 90.0, 135.0, 180.0]  # deg


def _stub_heave_rao(omega: float, heading: float, omega_n: float, zeta: float) -> float:
    """Single-DOF heave transmissibility shape x mild heading dependence.
    A transparent placeholder — NOT a real hydrodynamic result."""
    r = omega / omega_n
    transmissibility = 1.0 / math.sqrt((1.0 - r**2) ** 2 + (2.0 * zeta * r) ** 2)
    heading_factor = 1.0 - 0.1 * math.sin(math.radians(heading))
    return transmissibility * heading_factor


def main() -> None:
    records = []
    for case, p in CASES.items():
        for omega in FREQUENCIES:
            for heading in HEADINGS:
                records.append({
                    "case": case,
                    "frequency_rad_s": omega,
                    "heading_deg": heading,
                    "heave_rao_m_per_m": _stub_heave_rao(
                        omega, heading, p["omega_n"], p["zeta"]),
                })

    atlas = build_library_atlas(
        basename="diffraction_library",
        key_axis="case",
        key_values=list(CASES),
        continuous_axes=[
            Axis(name="frequency_rad_s", scale="linear", grid=FREQUENCIES),
            Axis(name="heading_deg", scale="linear", grid=HEADINGS),
        ],
        records=records,
        response="heave_rao_m_per_m",
        solver={
            "name": "OrcaWave/AQWA",
            "version": "STUB",
            "licensed": False,
            "run_date": "STUB",
            "note": "synthetic placeholder heave RAO — replace with a licensed "
                    "diffraction run per case (operator, dev-secondary).",
        },
        coverage_note=(
            "STUB diffraction heave-RAO library, 3 canonical vessel cases. "
            "Queries outside these cases escalate to a licensed run."
        ),
    )
    out = atlas.save(REPO_ROOT / "atlases")
    print(f"atlas_id={atlas.atlas_id}  rows={len(atlas.grid)}  cases={list(CASES)}")
    print(f"written -> {out}")


if __name__ == "__main__":
    main()
