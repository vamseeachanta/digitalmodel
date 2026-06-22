#!/usr/bin/env python3
"""P2 demo: build a FOWT MBR-utilisation atlas from an offline sweep, then query.

    uv run python examples/parametrics/build_fowt_atlas.py

Sweeps watch-circle radius x cable MBR limit (closed-form, no licence), builds a
parametric.Atlas from the results, then serves instant interpolated answers with
the out-of-range rail.
"""

from __future__ import annotations

from digitalmodel.parametrics import ParameterSweep, ParametricStudy
from digitalmodel.parametrics.atlas_bridge import atlas_from_summary
from digitalmodel.parametrics.pilots import orcaflex_fowt_watch_circle_sweep

RADII = (15.0, 25.0, 35.0)
MBR = (4.0, 5.0, 6.0)


def main() -> None:
    summary = orcaflex_fowt_watch_circle_sweep(
        watch_circle_radii_m=RADII, mbr_limits_m=MBR
    )
    study = ParametricStudy(
        name="orcaflex-fowt-watch-circle",
        parameters=[
            ParameterSweep(name="watch_circle_radius", values=list(RADII)),
            ParameterSweep(name="mbr_limit_m", values=list(MBR)),
        ],
    )
    atlas = atlas_from_summary(
        study,
        summary,
        "max_utilisation",
        basename="fowt_mooring",
        atlas_id="fowt-mbr-uc-v0",
    )
    print(f"built atlas '{atlas.atlas_id}' over {len(atlas.grid)} grid points")

    for point in (
        {"watch_circle_radius": 25.0, "mbr_limit_m": 5.0},  # grid node
        {"watch_circle_radius": 20.0, "mbr_limit_m": 4.5},  # interpolated
        {"watch_circle_radius": 80.0, "mbr_limit_m": 5.0},  # out of range
    ):
        pred = atlas.predict(point)
        if pred.in_range:
            print(f"  {point} -> UC={pred.value:.4f}")
        else:
            print(f"  {point} -> out of range ({pred.reason}); escalate to a run")


if __name__ == "__main__":
    main()
