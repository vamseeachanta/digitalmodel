# ABOUTME: Demo of the representative field-development API (#1512, epic #1507):
# ABOUTME: iterate rate variants of a demo config through layout/screen/visualize.
"""Iterate the demo field through the representative API — no file edits.

Runs the bundled offshore demo config through ``api.layout`` once, then a
handful of ``api.screen`` iterations (the target usage: several models per
case, 10-20 cheap iterations each) by overriding one well rate per call, and
finally renders the base case with ``api.visualize``.

Usage (from the repo root)::

    python scripts/field_development/representative_api_demo.py [output_dir]
"""

from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src"))

from digitalmodel.field_development import api  # noqa: E402

CONFIG = (
    REPO_ROOT
    / "src"
    / "digitalmodel"
    / "field_development"
    / "data"
    / "offshore_demo_field.yml"
)
FLUID = {"density_kg_per_m3": 850.0, "viscosity_pa_s": 2.0e-3}


def main(output_dir: str | Path = "outputs/field_development/api_demo") -> None:
    summary = api.layout(CONFIG)
    print(f"# {summary['field']}")
    print(f"assets by kind: {summary['assets_by_kind']}")
    print(f"is_subsea: {summary['surface']['is_subsea']}")
    print()

    print("W-1 rate [m3/d] | FL-1 selected | FL-1 arrival [kPa] | overall")
    print("---|---|---|---")
    for rate in (600.0, 950.0, 1300.0, 1650.0, 2000.0):
        result = api.screen(
            CONFIG,
            inlet_pressure_kpa=25000.0,
            target_arrival_pressure_kpa=5000.0,
            fluid=FLUID,
            assets={"W-1": {"rate_m3_per_day": rate}},
        )
        fl1 = next(fl for fl in result["flowlines"] if fl["id"] == "FL-1")
        sel = fl1["selected"]
        selected = "none pass" if sel is None else f"NPS {sel['nps']:g}"
        arrival = "-" if sel is None else f"{sel['arrival_pressure_kpa']:.0f}"
        overall = "PASS" if result["passes"] else "FAIL"
        print(f"{rate:.0f} | {selected} | {arrival} | {overall}")
    print()

    rendered = api.visualize(CONFIG, output_dir)
    print(f"plot:  {rendered['plot_path']}")
    print(f"scene: {rendered['scene_path']}")

    analogs = api.analogs({"water_depth_m": 1450.0, "host_kind": "fpso"})
    status = "available" if analogs["available"] else "unavailable (adapter)"
    print(f"analogs backend: {status}")


if __name__ == "__main__":
    main(*sys.argv[1:2])
