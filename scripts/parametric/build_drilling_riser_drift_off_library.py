"""Build the drilling-riser drift-off dynamic sparse atlas library (#1375, twin C).

The solver-backed dynamic tier of the DP drift-off screen (epic #1372 child C).
The analytical ``drilling_riser.drift_off`` screen is a quasi-static
constant-acceleration LOWER BOUND on time-to-limit; a licensed OrcaFlex
(OrcFxAPI) time-domain run per operating mode yields the **drift-off
amplification factor** (added mass, recoil, thruster ramp-down, coupled
vessel-riser dynamics) that scales the quasi-static time-to-limit toward the true
dynamic value.

Operator step: replace the STUB values in ``_stub_drift_amp`` with a real
licensed OrcaFlex run per canonical case, then raise the ``solver_version`` in
``parametric.refresh.LIBRARY_EXPECTATIONS`` off "STUB" — the committed stub
immediately reads stale and the query escalates until a real library is built.
Mirrors build_drilling_riser_envelope_library.py.

Usage:  uv run python scripts/parametric/build_drilling_riser_drift_off_library.py
"""

from __future__ import annotations

from pathlib import Path

from digitalmodel.parametric.atlas import Axis
from digitalmodel.parametric.library import build_library_atlas

REPO_ROOT = Path(__file__).resolve().parents[2]

# Canonical cases = the coverage map (exact-match key) = operating modes.
CASES = ["drilling", "connected", "hang_off"]

# Continuous axes interpolated WITHIN a case.
CURRENT_MPS = [0.0, 0.5, 1.0, 1.5, 2.0]        # surface current speed, m/s
HS_M = [1.0, 3.0, 5.0, 7.0]                     # significant wave height, m

_MODE_BASE = {"drilling": 1.10, "connected": 1.15, "hang_off": 1.20}


def _stub_drift_amp(mode: str, current_mps: float, hs_m: float) -> float:
    """STUB: synthetic drift-off dynamic amplification factor (>= 1.0).

    NOT a real dynamic result — a smooth monotone placeholder so the mechanism is
    testable end to end. Replace with a licensed OrcaFlex time-domain run. Applied
    to the quasi-static time-to-limit: a larger factor = a faster true drift =
    less time (the dynamic tier is more, not less, conservative than the screen).
    """
    return _MODE_BASE[mode] + 0.04 * hs_m + 0.06 * current_mps


def main() -> None:
    records = []
    for mode in CASES:
        for cur in CURRENT_MPS:
            for hs in HS_M:
                records.append({
                    "mode": mode,
                    "current_speed_mps": cur,
                    "hs_m": hs,
                    "drift_amp": _stub_drift_amp(mode, cur, hs),
                })

    atlas = build_library_atlas(
        basename="drilling_riser_drift_off",
        key_axis="mode",
        key_values=CASES,
        continuous_axes=[
            Axis(name="current_speed_mps", scale="linear", grid=CURRENT_MPS),
            Axis(name="hs_m", scale="linear", grid=HS_M),
        ],
        records=records,
        response="drift_amp",
        solver={
            "name": "OrcaFlex/OrcFxAPI",
            "version": "STUB",
            "licensed": False,
            "run_date": "STUB",
            "note": "synthetic placeholder drift-off dynamic amplification — "
                    "replace with a licensed OrcaFlex time-domain run per "
                    "operating mode (operator step, dev-secondary).",
        },
        coverage_note=(
            "STUB drilling-riser drift-off dynamic-amplification library, 3 "
            "operating-mode cases. Queries outside these modes or the gridded "
            "current/Hs ranges escalate to a licensed run."
        ),
    )
    out = atlas.save(REPO_ROOT / "atlases")
    print(f"atlas_id={atlas.atlas_id}  rows={len(atlas.grid)}  cases={CASES}")
    print(f"written -> {out}")


if __name__ == "__main__":
    main()
