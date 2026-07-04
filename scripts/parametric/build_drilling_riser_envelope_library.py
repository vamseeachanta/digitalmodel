"""Build the drilling-riser operating-envelope sparse atlas library (#1346, C3).

The solver-backed dynamic tier of the operating-envelope engine (epic #1279
child C). A licensed OrcaFlex (OrcFxAPI) time-domain run per operating mode
yields the wave-frequency + vessel-motion **von Mises dynamic amplification
factor** (DAF) that the C1 static beam-column tier omits; the analytical tier
(drilling_riser.envelope) multiplies its static von Mises utilisation by this
DAF when the dynamic path is enabled.

Operator step: replace the STUB values in ``_stub_von_mises_daf`` with a real
licensed OrcaFlex run per canonical case (operating mode), then bump the
``solver_version`` in ``parametric.refresh.LIBRARY_EXPECTATIONS`` off "STUB" —
the committed stub immediately reads stale and the query escalates until a real
library is built. The structure, coverage map, and query path are identical
either way; only the recorded values change. Mirrors build_orcaflex_library.py.

Usage:  uv run python scripts/parametric/build_drilling_riser_envelope_library.py
"""

from __future__ import annotations

from pathlib import Path

from digitalmodel.parametric.atlas import Axis
from digitalmodel.parametric.library import build_library_atlas

REPO_ROOT = Path(__file__).resolve().parents[2]

# Canonical cases = the coverage map (exact-match key) = the operating modes.
# Each is ONE licensed OrcaFlex run configuration (connected-drilling,
# connected-non-drilling, and hung-off have different boundary conditions, so a
# dynamic response cannot be interpolated across them).
CASES = ["drilling", "connected", "hang_off"]

# Continuous axes interpolated WITHIN a case.
OFFSET_PCT = [0.0, 2.0, 4.0, 6.0, 8.0]        # vessel offset, % water depth
CURRENT_MPS = [0.0, 0.5, 1.0, 1.5, 2.0]        # surface current speed, m/s
HS_M = [1.0, 3.0, 5.0, 7.0]                     # significant wave height, m
TP_S = [8.0, 12.0, 16.0]                        # peak period, s

_MODE_BASE = {"drilling": 1.05, "connected": 1.10, "hang_off": 1.15}


def _stub_von_mises_daf(
    mode: str, offset_pct: float, current_mps: float, hs_m: float, tp_s: float
) -> float:
    """STUB: synthetic von Mises dynamic amplification factor (>= 1.0).

    NOT a real dynamic result — a smooth monotone placeholder so the mechanism
    is testable end to end. Replace with a licensed OrcaFlex time-domain run.
    """
    return (
        _MODE_BASE[mode]
        + 0.03 * hs_m
        + 0.05 * current_mps
        + 0.001 * offset_pct
        + 0.002 * (tp_s - TP_S[0])
    )


def main() -> None:
    records = []
    for mode in CASES:
        for off in OFFSET_PCT:
            for cur in CURRENT_MPS:
                for hs in HS_M:
                    for tp in TP_S:
                        records.append({
                            "mode": mode,
                            "offset_pct": off,
                            "current_speed_mps": cur,
                            "hs_m": hs,
                            "tp_s": tp,
                            "von_mises_daf": _stub_von_mises_daf(mode, off, cur, hs, tp),
                        })

    atlas = build_library_atlas(
        basename="drilling_riser_envelope",
        key_axis="mode",
        key_values=CASES,
        continuous_axes=[
            Axis(name="offset_pct", scale="linear", grid=OFFSET_PCT),
            Axis(name="current_speed_mps", scale="linear", grid=CURRENT_MPS),
            Axis(name="hs_m", scale="linear", grid=HS_M),
            Axis(name="tp_s", scale="linear", grid=TP_S),
        ],
        records=records,
        response="von_mises_daf",
        solver={
            "name": "OrcaFlex/OrcFxAPI",
            "version": "STUB",
            "licensed": False,
            "run_date": "STUB",
            "note": "synthetic placeholder von-Mises dynamic amplification — "
                    "replace with a licensed OrcaFlex time-domain run per "
                    "operating mode (operator step, dev-secondary).",
        },
        coverage_note=(
            "STUB drilling-riser von-Mises dynamic-amplification library, 3 "
            "operating-mode cases. Queries outside these modes or the gridded "
            "offset/current/Hs/Tp ranges escalate to a licensed run."
        ),
    )
    out = atlas.save(REPO_ROOT / "atlases")
    print(f"atlas_id={atlas.atlas_id}  rows={len(atlas.grid)}  cases={CASES}")
    print(f"written -> {out}")


if __name__ == "__main__":
    main()
