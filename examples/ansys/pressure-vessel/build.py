#!/usr/bin/env python3
"""Generate the pressure-vessel screening APDL (pv.inp) for the run_ansys case.

    uv run python examples/ansys/pressure-vessel/build.py

Edit the VesselGeometry / DesignConditions below (or import and parameterise)
and re-run to refresh pv.inp. Dispatch the solve on a licensed MAPDL host via
input.yml (#940/#948); off-license the run_ansys lane fails closed.
"""

from pathlib import Path

from digitalmodel.ansys.pressure_vessel import (
    DesignConditions,
    PressureVesselGenerator,
    VesselGeometry,
)

HERE = Path(__file__).resolve().parent

# Representative offshore separator: ID 1500 mm, 30 mm wall, SA-516 Gr 70.
GEOM = VesselGeometry(
    inner_diameter_mm=1500.0,
    shell_length_mm=5000.0,
    wall_thickness_mm=30.0,
    head_type="2:1_ellipsoidal",
    corrosion_allowance_mm=3.0,
)

CONDITIONS = DesignConditions(
    design_pressure_mpa=10.0,
    design_temperature_c=250.0,
    operating_pressure_mpa=8.0,
    operating_temperature_c=200.0,
    ambient_temperature_c=25.0,
    allowable_stress_mpa=138.0,
    yield_strength_mpa=260.0,
    tensile_strength_mpa=485.0,
    joint_efficiency=1.0,
    material_name="SA-516 Gr 70",
)


def build() -> str:
    """Return the generated APDL deck for the representative vessel."""
    return PressureVesselGenerator().generate_pv_apdl(
        GEOM, CONDITIONS, include_thermal=True, include_hydrotest=False
    )


if __name__ == "__main__":
    out = HERE / "pv.inp"
    out.write_text(build())
    print(f"wrote {out}")
