"""Text specification of a drilling-riser global OrcaFlex model.

All quantities are SI (m, kg, N, Pa). Elevations are global z, positive up, MSL = 0.
The model topology is fixed:

* ``inner_barrel``: sections from the upper flex-joint pivot (connected to the vessel
  with the upper flex-joint rotational stiffness) down to the tension ring. Its lower end
  rides in the ring on a constraint free along z (the telescopic joint), so it passes
  shear and moment into the ring but no axial load beyond a small stabilising spring.
* ``riser``: sections from the tension ring down to the lower flex-joint pivot.
* ``stack``: sections from the lower flex-joint pivot down to the wellhead datum,
  where the stack is fixed.

Each :class:`LineSection` gives the structural mass per metre *excluding* the fluid in
its main bore; that fluid is the line contents (``Contents``) over ``bore_id_m``. The
displaced volume per metre sets buoyancy and added mass (OrcaFlex uses the equivalent
diameter), independently of the drag diameter.
"""

from __future__ import annotations

import math
from typing import Any

from pydantic import BaseModel, Field, model_validator

LENGTH_TOL_M = 1.0e-3


def tube_section(*, od_m: float, wall_m: float, youngs_modulus_pa: float) -> dict[str, float]:
    """Area, EA and EI of a plain tube."""
    if od_m <= 0 or wall_m <= 0 or 2 * wall_m >= od_m:
        raise ValueError(f"invalid tube: OD {od_m} m, wall {wall_m} m")
    idm = od_m - 2 * wall_m
    area = math.pi / 4 * (od_m**2 - idm**2)
    inertia = math.pi / 64 * (od_m**4 - idm**4)
    return {"id_m": idm, "area_m2": area, "ea_n": youngs_modulus_pa * area,
            "ei_nm2": youngs_modulus_pa * inertia}


class LineSection(BaseModel):
    """One homogeneous section of a line (becomes one OrcaFlex line type)."""

    name: str = Field(..., min_length=1)
    length_m: float = Field(..., gt=0)
    segment_length_m: float = Field(..., gt=0)
    mass_per_m_kg: float = Field(..., ge=0, description="structural mass, excluding main-bore contents")
    displaced_volume_per_m_m3: float = Field(..., ge=0)
    bore_id_m: float = Field(0.0, ge=0, description="main bore carrying the line contents; 0 = none")
    ei_nm2: float = Field(..., gt=0)
    ea_n: float = Field(..., gt=0)
    gj_nm2: float | None = Field(None, gt=0)
    drag_diameter_m: float = Field(..., ge=0)
    cd_normal: float = Field(..., ge=0)
    cd_axial: float = Field(0.0, ge=0)
    ca_normal: float = Field(..., ge=0)
    ca_axial: float = Field(0.0, ge=0)
    stress_od_m: float | None = Field(None, gt=0)
    stress_id_m: float | None = Field(None, ge=0)
    source: dict[str, Any] = Field(default_factory=dict, description="provenance per field (free form)")

    @model_validator(mode="after")
    def _bore_inside_body(self) -> "LineSection":
        if self.bore_id_m > 0:
            eq = math.sqrt(4 * self.displaced_volume_per_m_m3 / math.pi)
            if self.bore_id_m >= eq:
                raise ValueError(
                    f"section {self.name!r}: bore {self.bore_id_m:.4f} m is not inside the "
                    f"buoyancy-equivalent diameter {eq:.4f} m")
        return self


class FlexJoint(BaseModel):
    pivot_z_m: float
    rotational_stiffness_nm_per_rad: float = Field(..., gt=0)


class TensionRing(BaseModel):
    mass_kg: float = Field(..., ge=0)
    volume_m3: float = Field(0.0, ge=0)
    moments_of_inertia_kgm2: tuple[float, float, float]
    z_static_m: float = Field(..., description="expected static elevation, used for the tensioner-line angle")


class Tensioners(BaseModel):
    count: int = Field(..., ge=1)
    sheave_radius_m: float = Field(..., gt=0)
    sheave_z_m: float
    ring_attach_radius_m: float = Field(..., ge=0)
    total_vertical_tension_n: float = Field(..., gt=0)
    first_azimuth_deg: float = 0.0
    wire_stiffness_n: float = Field(1.0e9, gt=0, description="winch wire EA (OrcaFlex 'Stiffness')")


class Contents(BaseModel):
    density_kg_m3: float = Field(..., ge=0)
    pressure_ref_z_m: float = Field(..., description="z of the free surface of the contents (gauge 0)")


class GlobalEnvironment(BaseModel):
    water_depth_m: float = Field(..., gt=0)
    water_density_kg_m3: float = Field(..., gt=0)
    seabed_normal_stiffness_kn_m_m2: float = 100.0
    seabed_shear_stiffness_kn_m_m2: float = 100.0


class RiserGlobalModelSpec(BaseModel):
    name: str = Field(..., min_length=1)
    description: str = ""
    environment: GlobalEnvironment
    contents: Contents
    upper_flex_joint: FlexJoint
    lower_flex_joint: FlexJoint
    tension_ring: TensionRing
    tensioners: Tensioners
    inner_barrel: list[LineSection] = Field(..., min_length=1)
    riser: list[LineSection] = Field(..., min_length=1)
    stack: list[LineSection] = Field(..., min_length=1)
    wellhead_datum_z_m: float
    vessel_name: str = "Vessel"
    slip_joint_axial_stiffness_n_per_m: float = Field(
        1.0e3, ge=0, description="axial spring on the telescopic-joint constraint; a small value keeps "
                                 "statics on the physical branch and carries only k x ring rise")
    provenance: dict[str, Any] = Field(default_factory=dict)

    @property
    def ring_z_geometric_m(self) -> float:
        """Unstretched tension-ring elevation (upper pivot minus the inner-barrel line)."""
        return self.upper_flex_joint.pivot_z_m - sum(s.length_m for s in self.inner_barrel)

    @model_validator(mode="after")
    def _geometry_closes(self) -> "RiserGlobalModelSpec":
        names = [s.name for s in (*self.inner_barrel, *self.riser, *self.stack)]
        dup = {n for n in names if names.count(n) > 1}
        if dup:
            raise ValueError(f"section names must be unique (line-type names): {sorted(dup)}")
        z_lfj = self.ring_z_geometric_m - sum(s.length_m for s in self.riser)
        if abs(z_lfj - self.lower_flex_joint.pivot_z_m) > LENGTH_TOL_M:
            raise ValueError(
                f"riser sections end at z = {z_lfj:.4f} m, not at the lower flex-joint pivot "
                f"{self.lower_flex_joint.pivot_z_m:.4f} m")
        z_datum = z_lfj - sum(s.length_m for s in self.stack)
        if abs(z_datum - self.wellhead_datum_z_m) > LENGTH_TOL_M:
            raise ValueError(
                f"stack sections end at z = {z_datum:.4f} m, not at the wellhead datum "
                f"{self.wellhead_datum_z_m:.4f} m")
        if self.wellhead_datum_z_m < -self.environment.water_depth_m - LENGTH_TOL_M:
            raise ValueError("wellhead datum is below the seabed")
        if self.tensioners.sheave_z_m <= self.tension_ring.z_static_m:
            raise ValueError("tensioner sheaves must be above the tension ring")
        return self
