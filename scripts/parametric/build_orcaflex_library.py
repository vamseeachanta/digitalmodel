"""Build the OrcaFlex sparse atlas library (#832).

Operator step: replace the STUB max-tension values below with a licensed
OrcaFlex (OrcFxAPI) time-domain run per canonical load case — one run yields
the response over the heading sweep for that sea state on a reference model.
The library structure, coverage map and query path are identical either way;
only the recorded values change. Mirrors build_diffraction_library.py (#801).

Usage:  uv run python scripts/parametric/build_orcaflex_library.py
"""

from __future__ import annotations

import math
from pathlib import Path

from digitalmodel.parametric.atlas import Axis
from digitalmodel.parametric.library import build_library_atlas

REPO_ROOT = Path(__file__).resolve().parents[2]

# Canonical load cases = the coverage map (exact-match key). Each is ONE
# licensed OrcaFlex run; base_tension_kN is a stub stand-in for the solved
# maximum effective line tension.
CASES = {
    "operating-hs2": {"base_tension_kN": 800.0},
    "storm-hs5": {"base_tension_kN": 1800.0},
    "extreme-hs8": {"base_tension_kN": 3200.0},
}
HEADINGS = [0.0, 45.0, 90.0, 135.0, 180.0]  # deg


def _stub_max_tension_N(base_tension_kN: float, heading: float) -> float:
    """Stub: beam seas (90 deg) are worst. NOT a real dynamic result."""
    heading_factor = 1.0 + 0.30 * math.sin(math.radians(heading)) ** 2
    return base_tension_kN * heading_factor * 1000.0  # kN -> N


def main() -> None:
    records = []
    for case, p in CASES.items():
        for heading in HEADINGS:
            records.append({
                "case": case,
                "heading_deg": heading,
                "max_effective_tension_N": _stub_max_tension_N(
                    p["base_tension_kN"], heading),
            })

    atlas = build_library_atlas(
        basename="orcaflex_library",
        key_axis="case",
        key_values=list(CASES),
        continuous_axes=[Axis(name="heading_deg", scale="linear", grid=HEADINGS)],
        records=records,
        response="max_effective_tension_N",
        solver={
            "name": "OrcaFlex/OrcFxAPI",
            "version": "STUB",
            "licensed": False,
            "run_date": "STUB",
            "note": "synthetic placeholder max line tension — replace with a "
                    "licensed OrcaFlex run per case (operator, dev-secondary).",
        },
        coverage_note=(
            "STUB OrcaFlex max-line-tension library, 3 canonical load cases. "
            "Queries outside these cases escalate to a licensed run."
        ),
    )
    out = atlas.save(REPO_ROOT / "atlases")
    print(f"atlas_id={atlas.atlas_id}  rows={len(atlas.grid)}  cases={list(CASES)}")
    print(f"written -> {out}")


if __name__ == "__main__":
    main()
