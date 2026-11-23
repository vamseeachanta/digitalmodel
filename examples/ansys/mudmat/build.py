#!/usr/bin/env python3
"""Generate the mudmat screening APDL (mudmat.inp) for the run_ansys lane case.

    uv run python examples/ansys/mudmat/build.py

Edit the MudmatGeometry below (or import and parameterise) and re-run to refresh
mudmat.inp. Dispatch the solve on a licensed host via input.yml (#940/#952).
"""

from pathlib import Path

from digitalmodel.ansys.mudmat import MudmatGeometry, write_mudmat_inp

HERE = Path(__file__).resolve().parent

GEOM = MudmatGeometry(
    mat_length_mm=4000.0,
    mat_width_mm=3000.0,
    thickness_mm=60.0,
    vertical_load_kn=800.0,
    moment_kNm=400.0,
    subgrade_modulus_n_per_mm3=0.05,
    youngs_modulus_mpa=205_000.0,
    poisson=0.3,
    yield_strength_mpa=355.0,
    design_factor=1.67,
    allowable_bearing_mpa=0.25,
    element_size_mm=100.0,
)

if __name__ == "__main__":
    out = write_mudmat_inp(GEOM, HERE / "mudmat.inp")
    print(f"wrote {out}")
