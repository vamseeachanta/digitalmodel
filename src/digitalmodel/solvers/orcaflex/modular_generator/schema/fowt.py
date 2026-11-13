"""Floating offshore wind turbine (FOWT) model type for the modular generator.

The spec describes one floating turbine: a hull, an optional flexible tower,
the rotor-nacelle assembly (RNA), an optional steady rotor thrust and the
mooring. :class:`~..routers.fowt_router.FowtRouter` converts it into generic
OrcaFlex objects.

Units follow OrcaFlex: length m, mass te, force kN, angle degree, time s.

Two representations are supported:

``rigid``
    Hull, tower and RNA form one rigid body carried by the hull VesselType.
    ``hull`` mass properties are the whole-system values. No tower line or RNA
    buoy is emitted. Suited to statics, rigid-body free decay and mean
    offsets.
``flexible``
    ``hull`` mass properties are the platform alone. The tower is a
    finite-element Line from the hull to a lumped 6D buoy that carries the RNA
    mass. The tower line references that buoy; see issue #3971 on the generic
    builder section order before relying on a licensed load of this form.

The OrcaFlex Turbine object (blades, aerofoils, controller) is not emitted.
Rotor thrust is represented by a constant applied load on a massless hub load
point that is rigidly connected to the hull (rigid) or to the RNA buoy
(flexible). This is a documented equivalent for statics, decay and mean-offset
cases only; it carries no aerodynamic damping or rotor dynamics.
"""

from __future__ import annotations

import math
from typing import Literal

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator

class _FiniteModel(BaseModel):
    """Base for every fowt model: NaN and infinity are rejected in all float
    fields, including floats nested in vectors, matrices and radiation lists,
    so that no non-finite value reaches the generated model."""

    model_config = ConfigDict(allow_inf_nan=False)


Vector3 = list[float]
Matrix3 = list[list[float]]


# OrcaFlex default gravity (m/s2); the generated General section does not override it.
ORCAFLEX_DEFAULT_GRAVITY = 9.80665


def _check_vector(value: list[float], size: int, name: str) -> list[float]:
    if len(value) != size:
        raise ValueError(f"{name} must have {size} entries, got {len(value)}")
    return value


def _check_square(value: list[list[float]], size: int, name: str) -> list[list[float]]:
    if len(value) != size or any(len(row) != size for row in value):
        raise ValueError(f"{name} must be {size}x{size}")
    return value


class FowtRadiationEntry(_FiniteModel):
    """Added mass and radiation damping at one wave period.

    ``period`` 0 denotes infinite frequency (OrcaFlex convention with waves
    referred to by period).
    """

    period: float = Field(..., ge=0, description="Wave period (s); 0 = infinite frequency")
    added_mass: list[list[float]] = Field(..., description="6x6 added mass (te, te.m, te.m2)")
    damping: list[list[float]] = Field(..., description="6x6 radiation damping (kN, kN.m units per m/s, rad/s)")

    @field_validator("added_mass", "damping")
    @classmethod
    def _six_by_six(cls, value: list[list[float]]) -> list[list[float]]:
        return _check_square(value, 6, "radiation matrix")


class FowtHull(_FiniteModel):
    """Floating hull carried by an OrcaFlex Vessel and VesselType.

    ``hydrostatic_stiffness_excluding_gravity`` is the heave/roll/pitch block
    in the convention of diffraction outputs computed with the centre of
    gravity at the origin (WAMIT ``.hst``): it holds the waterplane and
    buoyancy moment-arm terms but not the weight term. The router adds the
    weight moment-arm term ``m g (-z_G)`` to the roll and pitch diagonals using
    this hull's own mass and centre of mass, because OrcaFlex expects the full
    matrix and removes both moment-arm terms itself when the centre of
    buoyancy is specified (Orcina, OrcaFlex help, Vessel theory: stiffness,
    added mass and damping).
    """

    name: str = Field(default="Hull", min_length=1)
    type_name: str | None = Field(default=None, description="VesselType name; default '<name> type'")
    length: float = Field(..., gt=0, description="Nominal length for the VesselType (m)")
    mass: float = Field(..., gt=0, description="Mass (te)")
    centre_of_mass: Vector3 = Field(..., description="Centre of mass in hull axes (m)")
    inertia_about_centre_of_mass: Matrix3 = Field(..., description="Inertia tensor about the centre of mass (te.m2)")
    displaced_volume: float = Field(..., gt=0, description="Displaced volume (m3)")
    centre_of_buoyancy: Vector3 = Field(..., description="Centre of buoyancy in hull axes (m)")
    hydrostatic_stiffness_excluding_gravity: Matrix3 = Field(
        ..., description="Heave/roll/pitch block (kN/m, kN/rad, kN.m/m, kN.m/rad), weight term excluded")
    radiation: list[FowtRadiationEntry] = Field(..., min_length=1, description="Frequency-dependent added mass and damping")
    other_linear_damping: list[float] = Field(default_factory=lambda: [0.0] * 6)
    other_quadratic_damping: list[float] = Field(default_factory=lambda: [0.0] * 6,
                                                 description="Diagonal quadratic damping (kN/(m/s)^2, kN.m/(rad/s)^2)")
    initial_position: Vector3 = Field(default_factory=lambda: [0.0, 0.0, 0.0])
    initial_orientation: Vector3 = Field(
        default_factory=lambda: [0.0, 0.0, 0.0],
        description="Initial roll, pitch, heading (deg); a non-zero roll or pitch is "
                    "used for free-decay starts with included_in_statics 'None'")
    included_in_statics: Literal["6 DOF", "None"] = Field(
        default="6 DOF", description="'None' holds the initial position for decay tests")
    include_wave_load_first_order: bool = Field(
        default=False, description="Must remain false until sourced load RAOs are supported")

    @field_validator("include_wave_load_first_order")
    @classmethod
    def _sourced_wave_loads_only(cls, value: bool) -> bool:
        if value:
            raise ValueError("first-order wave loading requires sourced load RAOs; not supported")
        return value

    @field_validator("centre_of_mass", "centre_of_buoyancy", "initial_position",
                     "initial_orientation")
    @classmethod
    def _three(cls, value: list[float]) -> list[float]:
        return _check_vector(value, 3, "vector")

    @field_validator("inertia_about_centre_of_mass", "hydrostatic_stiffness_excluding_gravity")
    @classmethod
    def _three_by_three(cls, value: list[list[float]]) -> list[list[float]]:
        return _check_square(value, 3, "matrix")

    @field_validator("other_linear_damping", "other_quadratic_damping")
    @classmethod
    def _six(cls, value: list[float]) -> list[float]:
        return _check_vector(value, 6, "damping vector")

    @model_validator(mode="after")
    def _infinite_frequency_present(self) -> "FowtHull":
        periods = [entry.period for entry in self.radiation]
        if periods != sorted(periods) or len(set(periods)) != len(periods):
            raise ValueError("radiation periods must be strictly increasing")
        return self


class FowtTowerSection(_FiniteModel):
    """One prismatic tower section, listed from the base upward.

    Material properties, including Poisson ratio, must be supplied from the
    chosen material basis and documented in ``FowtSpec.source_note``. There
    is no assumed incompressible material or reference-design default.
    """

    length: float = Field(..., gt=0, description="Section length (m)")
    outer_diameter: float = Field(..., gt=0, description="Outer diameter (m)")
    mass_per_length: float = Field(..., gt=0, description="Mass per unit length (te/m)")
    bending_stiffness: float = Field(..., gt=0, description="EI (kN.m2)")
    axial_stiffness: float = Field(..., gt=0, description="EA (kN)")
    torsional_stiffness: float = Field(..., gt=0, description="GJ (kN.m2)")
    poisson_ratio: float = Field(
        ..., allow_inf_nan=False, description="Material Poisson ratio; document its basis in source_note")


class FowtTower(_FiniteModel):
    """Flexible tower: a finite-element line from the hull to the RNA buoy."""

    name: str = Field(default="Tower", min_length=1)
    base_elevation: float = Field(..., description="Tower base z in hull axes (m)")
    sections: list[FowtTowerSection] = Field(..., min_length=1)
    target_segment_length: float = Field(default=5.0, gt=0)

    @property
    def top_elevation(self) -> float:
        return self.base_elevation + sum(section.length for section in self.sections)


class FowtRna(_FiniteModel):
    """Rotor-nacelle assembly in principal axes (flexible representation).

    The lumped buoy takes diagonal moments only. Nonzero products of inertia
    require an axes transformation of the full model and are not supported.
    """

    name: str = Field(default="RNA", min_length=1)
    mass: float = Field(..., gt=0, description="Mass (te)")
    centre_of_mass_from_tower_top: Vector3 = Field(..., description="(m), tower-top axes")
    inertia_about_centre_of_mass: Matrix3 = Field(..., description="(te.m2)")

    @field_validator("centre_of_mass_from_tower_top")
    @classmethod
    def _three(cls, value: list[float]) -> list[float]:
        return _check_vector(value, 3, "vector")

    @field_validator("inertia_about_centre_of_mass")
    @classmethod
    def _three_by_three(cls, value: list[list[float]]) -> list[list[float]]:
        _check_square(value, 3, "matrix")
        if any(value[i][j] != 0 for i in range(3) for j in range(3) if i != j):
            raise ValueError("RNA inertia must be in principal axes with zero products of inertia")
        return value


class FowtRotorThrust(_FiniteModel):
    """Steady rotor thrust applied at the hub along the tilted shaft.

    The force acts downwind (positive x for a rotor upwind of the tower at
    negative x) along the shaft axis, which descends toward the downwind end
    by ``shaft_tilt``: force = thrust * (cos tilt, 0, -sin tilt) in hull axes.
    """

    name: str = Field(default="Hub load point", min_length=1)
    thrust: float = Field(..., ge=0, description="Thrust (kN)")
    hub_position: Vector3 = Field(..., description="Hub centre in hull axes (m)")
    shaft_tilt: float = Field(
        ..., allow_inf_nan=False,
        description="Signed shaft tilt (deg); document its basis in source_note. "
                    "No reference-design angle cap is imposed by the force rotation")
    wind_speed: float | None = Field(default=None, ge=0, description="Hub wind speed the thrust belongs to (m/s)")

    @field_validator("hub_position")
    @classmethod
    def _three(cls, value: list[float]) -> list[float]:
        return _check_vector(value, 3, "vector")


class FowtMooringLineType(_FiniteModel):
    """Mooring line type with directly specified properties.

    All hydrodynamic coefficients are required inputs. Record their source,
    coefficient convention and any assumptions in ``FowtSpec.source_note``;
    coefficients of one reference design are not generic line-type defaults.
    """

    name: str = Field(..., min_length=1)
    outer_diameter: float = Field(..., gt=0, description="Volume-equivalent diameter (m)")
    mass_per_length: float = Field(..., gt=0, description="(te/m)")
    axial_stiffness: float = Field(..., gt=0, description="EA (kN)")
    minimum_breaking_load: float | None = Field(
        default=None, gt=0, description="(kN); None where the reference design publishes none")
    normal_drag_coefficient: float = Field(..., ge=0, allow_inf_nan=False)
    axial_drag_coefficient: float = Field(..., ge=0, allow_inf_nan=False)
    normal_added_mass_coefficient: float = Field(..., ge=0, allow_inf_nan=False)
    axial_added_mass_coefficient: float = Field(..., ge=0, allow_inf_nan=False)


class FowtMooringLine(_FiniteModel):
    """Single-segment line from a hull fairlead to a seabed anchor."""

    name: str = Field(..., min_length=1)
    line_type: str = Field(..., min_length=1)
    fairlead: Vector3 = Field(..., description="Fairlead in hull axes (m)")
    anchor_xy: list[float] = Field(..., description="Anchor plan position in global axes (m)")
    anchor_height_above_seabed: float = Field(
        default=0.0, ge=0, description="OrcaFlex 'Anchored' z is measured from the seabed (m)")
    length: float = Field(..., gt=0, description="Unstretched length (m)")
    target_segment_length: float = Field(default=10.0, gt=0)

    @field_validator("fairlead")
    @classmethod
    def _three(cls, value: list[float]) -> list[float]:
        return _check_vector(value, 3, "vector")

    @field_validator("anchor_xy")
    @classmethod
    def _two(cls, value: list[float]) -> list[float]:
        return _check_vector(value, 2, "anchor_xy")


class FowtSpec(_FiniteModel):
    """Floating offshore wind turbine model."""

    representation: Literal["rigid", "flexible"] = Field(default="rigid")
    gravity: float = Field(
        default=ORCAFLEX_DEFAULT_GRAVITY, gt=0,
        description="Gravity used for the weight term of the hydrostatic matrix (m/s2); "
                    "must equal the OrcaFlex default because the generated General "
                    "section does not set gravity")
    hull: FowtHull
    tower: FowtTower | None = None
    rna: FowtRna | None = None
    rotor_thrust: FowtRotorThrust | None = None
    mooring_line_types: list[FowtMooringLineType] = Field(..., min_length=1)
    mooring_lines: list[FowtMooringLine] = Field(..., min_length=1)
    source_note: str = Field(default="", description="Where the values come from")

    @field_validator("gravity")
    @classmethod
    def _default_gravity(cls, value: float) -> float:
        if not math.isclose(value, ORCAFLEX_DEFAULT_GRAVITY, rel_tol=1e-9):
            raise ValueError(
                f"gravity {value} is not supported: the generated model uses the OrcaFlex "
                f"default {ORCAFLEX_DEFAULT_GRAVITY} m/s2, and a different value would make "
                "the hydrostatic weight term inconsistent with the solved model")
        return value

    @model_validator(mode="after")
    def _consistency(self) -> "FowtSpec":
        if self.representation == "flexible":
            if self.tower is None or self.rna is None:
                raise ValueError("flexible representation needs tower and rna")
        elif self.tower is not None or self.rna is not None:
            raise ValueError("rigid representation carries tower and RNA mass in the hull; "
                             "omit tower and rna")
        type_names = {t.name for t in self.mooring_line_types}
        if len(type_names) != len(self.mooring_line_types):
            raise ValueError("duplicate mooring line type names")
        for line in self.mooring_lines:
            if line.line_type not in type_names:
                raise ValueError(f"mooring line {line.name} references unknown type {line.line_type}")
        names = [line.name for line in self.mooring_lines]
        if len(set(names)) != len(names):
            raise ValueError("duplicate mooring line names")
        return self
