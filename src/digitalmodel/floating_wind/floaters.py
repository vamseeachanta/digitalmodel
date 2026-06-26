"""Parametric floater archetype models for floating-wind concept screening.

Floating-wind *global sizing & concept screening* (issue #1023, epic #1022)
starts from a handful of floater **archetypes**, each described by a small
parameter vector, and derives the naval-architecture properties needed to
screen a design: displacement, mass split, intact stability (``GM``) and the
rigid-body natural periods that decide whether a hull sits inside or outside the
wave-energy band.

Four archetypes are modelled, matching the PyFloatSizer-class concept set:

* **Semi-submersible** -- columns piercing the waterline on submerged pontoons.
  Stability from waterplane area spread over a wide column footprint.
* **Spar** -- a single deep-draft cylinder. Stability from a low centre of
  gravity (deep ballast) far below the centre of buoyancy.
* **TLP** -- columns on pontoons held down by vertical tendons. Vertical-plane
  stiffness (heave/pitch) comes from the tendons, not the waterplane, so the
  hydrostatic ``GM`` can be small or negative *by design*.
* **Barge** -- a shallow-draft rectangular box. Large waterplane area gives a
  large ``GM`` (stiff in roll/pitch) at the cost of lively heave.

All models are **closed-form and licence-free** -- a preliminary screen.
Hydrodynamic added mass and radii of gyration are represented by documented
coefficients with sensible defaults; the final motion picture needs a
diffraction solve (OrcaWave) and a coupled mooring/motion solve (OrcaFlex),
which the solver tier (issue #1025) wires in for shortlisted variants.

Sign / datum convention
-----------------------
* Lengths in metres, masses in tonnes (1 t = 1000 kg), periods in seconds.
* The **keel** (lowest point of the hull) is the vertical datum ``z = 0``; all
  vertical centroids (``KB``, ``KG``) are heights above the keel.
* The draft ``T`` is the keel-to-waterline distance.

References
----------
* Faltinsen, *Sea Loads on Ships and Offshore Structures* (1990) -- hydrostatic
  stability (``GM = KB + BM - KG``), heave/pitch natural periods.
* DNV-OS-C301 / DNV-RP-0286 -- floating-wind stability and design practice.
* Allen et al., *Definition of the UMaine VolturnUS-S Reference Platform* (IEA
  15 MW reference semi), NREL/TP-5000-76773 (2020) -- order-of-magnitude
  sanity values for the semi archetype.
"""

from __future__ import annotations

import math
from enum import Enum

from pydantic import BaseModel, Field, model_validator

__all__ = [
    "RHO_SEAWATER",
    "G_STANDARD",
    "FloaterArchetype",
    "TurbineTopside",
    "FloaterProperties",
    "SemiSubmersible",
    "Spar",
    "TLP",
    "Barge",
    "build_floater",
    "IEA_15MW_RNA",
]

# --- physical constants ------------------------------------------------------

RHO_SEAWATER = 1025.0  # kg/m^3, standard seawater density
G_STANDARD = 9.80665  # m/s^2

_KG_PER_TONNE = 1000.0


class FloaterArchetype(str, Enum):
    """Floater hull archetype for concept screening."""

    SEMI = "semi"
    SPAR = "spar"
    TLP = "tlp"
    BARGE = "barge"


# --- topside (turbine) -------------------------------------------------------


class TurbineTopside(BaseModel):
    """Wind-turbine RNA + tower lumped topside carried by the floater.

    Floating-wind pitch behaviour is dominated by the high, heavy turbine, so a
    concept screen must carry it explicitly rather than fold it into the hull.
    """

    rna_mass_t: float = Field(
        ..., gt=0.0, description="Rotor-nacelle-assembly mass (t)"
    )
    tower_mass_t: float = Field(..., gt=0.0, description="Tower mass (t)")
    hub_height_m: float = Field(
        ...,
        gt=0.0,
        description="Hub height above the still-water line (m)",
    )
    tower_base_above_swl_m: float = Field(
        15.0,
        ge=0.0,
        description="Tower base (floater interface) above still-water line (m)",
    )

    @property
    def total_mass_t(self) -> float:
        return self.rna_mass_t + self.tower_mass_t

    def cg_above_waterline_m(self) -> float:
        """Combined RNA + tower vertical CG above the still-water line (m).

        RNA CG is taken at the hub; the tower CG is taken at the midpoint
        between its base and the hub (uniform-tower idealisation).
        """
        tower_cg = 0.5 * (self.tower_base_above_swl_m + self.hub_height_m)
        moment = (
            self.rna_mass_t * self.hub_height_m + self.tower_mass_t * tower_cg
        )
        return moment / self.total_mass_t


# A convenient reference topside (IEA 15 MW class) for examples / sanity tests.
IEA_15MW_RNA = TurbineTopside(
    rna_mass_t=1017.0,
    tower_mass_t=1263.0,
    hub_height_m=150.0,
    tower_base_above_swl_m=15.0,
)


# --- derived properties ------------------------------------------------------


class FloaterProperties(BaseModel):
    """Derived naval-architecture properties of a sized floater concept."""

    archetype: FloaterArchetype
    draft_m: float
    displacement_t: float = Field(..., description="Displaced mass = buoyancy (t)")
    steel_mass_t: float
    ballast_mass_t: float = Field(
        ..., description="Solid/water ballast to reach equilibrium draft (t)"
    )
    topside_mass_t: float
    total_mass_t: float

    waterplane_area_m2: float
    KB_m: float = Field(..., description="Centre of buoyancy above keel (m)")
    BM_m: float = Field(..., description="Metacentric radius I_wp / V (m)")
    KG_m: float = Field(..., description="Combined centre of gravity above keel (m)")
    GM_m: float = Field(..., description="Metacentric height KB + BM - KG (m)")

    heave_natural_period_s: float
    pitch_natural_period_s: float

    pitch_radius_gyration_m: float = Field(
        0.0,
        description="Pitch radius of gyration about the CG, kyy = sqrt(I55/m) (m)",
    )

    tendon_stabilised: bool = Field(
        False,
        description="True when vertical stability is tendon- not waterplane-derived (TLP)",
    )
    feasible: bool = Field(
        ...,
        description="Floats at the requested draft with non-negative ballast",
    )
    notes: tuple[str, ...] = ()


# --- shared helpers ----------------------------------------------------------


def _heave_period(mass_t: float, waterplane_area_m2: float, ca_heave: float) -> float:
    """Uncoupled heave natural period (s).

    ``T = 2*pi*sqrt((m + a33) / (rho*g*A_wp))`` with added mass
    ``a33 = ca_heave * m`` (heave added-mass ratio).
    """
    if waterplane_area_m2 <= 0.0:
        return math.inf
    m = mass_t * _KG_PER_TONNE
    restoring = RHO_SEAWATER * G_STANDARD * waterplane_area_m2
    return 2.0 * math.pi * math.sqrt(m * (1.0 + ca_heave) / restoring)


def _pitch_period(
    inertia_t_m2: float,
    displacement_t: float,
    gm_m: float,
    ca_pitch: float,
) -> float:
    """Uncoupled pitch natural period (s).

    ``T = 2*pi*sqrt((I55 + a55) / (rho*g*V*GM_L))`` with mass moment of inertia
    ``I55`` about the combined CG, added inertia ``a55 = ca_pitch * I55`` and
    (for an axisymmetric or near-square hull) ``GM_L ~ GM``. Returns ``inf`` for
    a non-positive ``GM`` (the hull is not statically stable in pitch on its
    waterplane alone -- e.g. a TLP, handled separately).
    """
    if gm_m <= 0.0:
        return math.inf
    inertia = inertia_t_m2 * _KG_PER_TONNE * (1.0 + ca_pitch)
    vol = displacement_t * _KG_PER_TONNE / RHO_SEAWATER
    restoring = RHO_SEAWATER * G_STANDARD * vol * gm_m
    return 2.0 * math.pi * math.sqrt(inertia / restoring)


def _assemble(
    *,
    archetype: FloaterArchetype,
    draft_m: float,
    displacement_t: float,
    steel_mass_t: float,
    topside: TurbineTopside,
    waterplane_area_m2: float,
    kb_m: float,
    bm_m: float,
    kg_hull_m: float,
    topside_cg_above_keel_m: float,
    k_horizontal_m: float,
    ca_heave: float,
    ca_pitch: float,
    tendon_stabilised: bool = False,
    tendon_heave_period_s: float | None = None,
    tendon_pitch_period_s: float | None = None,
    extra_notes: tuple[str, ...] = (),
) -> FloaterProperties:
    """Close the mass balance and assemble a :class:`FloaterProperties`.

    Ballast makes up the difference between buoyancy and the (steel + topside)
    light weight. A negative ballast means the concept is too heavy to float at
    the requested draft -> ``feasible = False`` and the ballast is clamped to 0.
    """
    notes = list(extra_notes)
    topside_mass_t = topside.total_mass_t
    ballast_mass_t = displacement_t - steel_mass_t - topside_mass_t
    feasible = ballast_mass_t >= 0.0
    if not feasible:
        notes.append(
            f"infeasible: light weight (steel {steel_mass_t:.0f} t + topside "
            f"{topside_mass_t:.0f} t) exceeds buoyancy {displacement_t:.0f} t "
            "at this draft"
        )
        ballast_mass_t = 0.0

    total_mass_t = steel_mass_t + ballast_mass_t + topside_mass_t

    # Combined CG: steel at hull centroid, ballast assumed at the keel-adjacent
    # base (conservative-low, typical of floating-wind solid/water ballast),
    # topside high above the keel.
    ballast_cg_m = 0.05 * draft_m
    cg_moment = (
        steel_mass_t * kg_hull_m
        + ballast_mass_t * ballast_cg_m
        + topside_mass_t * topside_cg_above_keel_m
    )
    kg_m = cg_moment / total_mass_t if total_mass_t > 0 else kg_hull_m
    gm_m = kb_m + bm_m - kg_m

    # Pitch mass moment of inertia about the combined CG (t.m^2). The vertical
    # term -- each lumped mass at its height above the CG -- is dominant for a
    # floating-wind unit because the high, heavy turbine has a long lever arm.
    # The horizontal term captures the hull's in-plane mass spread (column
    # footprint for a semi/TLP, hull breadth for a barge, ~0 for a spar).
    i_vertical = (
        steel_mass_t * (kg_hull_m - kg_m) ** 2
        + ballast_mass_t * (ballast_cg_m - kg_m) ** 2
        + topside_mass_t * (topside_cg_above_keel_m - kg_m) ** 2
    )
    i_horizontal = total_mass_t * k_horizontal_m**2
    i55_t_m2 = i_vertical + i_horizontal
    kyy_m = math.sqrt(i55_t_m2 / total_mass_t) if total_mass_t > 0 else 0.0

    if tendon_stabilised:
        heave_period = tendon_heave_period_s if tendon_heave_period_s else math.nan
        pitch_period = tendon_pitch_period_s if tendon_pitch_period_s else math.nan
    else:
        heave_period = _heave_period(total_mass_t, waterplane_area_m2, ca_heave)
        pitch_period = _pitch_period(
            i55_t_m2, displacement_t, gm_m, ca_pitch
        )
        if gm_m <= 0.0:
            notes.append("non-positive GM: statically unstable on waterplane alone")

    return FloaterProperties(
        archetype=archetype,
        draft_m=draft_m,
        displacement_t=displacement_t,
        steel_mass_t=steel_mass_t,
        ballast_mass_t=ballast_mass_t,
        topside_mass_t=topside_mass_t,
        total_mass_t=total_mass_t,
        waterplane_area_m2=waterplane_area_m2,
        KB_m=kb_m,
        BM_m=bm_m,
        KG_m=kg_m,
        GM_m=gm_m,
        heave_natural_period_s=heave_period,
        pitch_natural_period_s=pitch_period,
        pitch_radius_gyration_m=kyy_m,
        tendon_stabilised=tendon_stabilised,
        feasible=feasible,
        notes=tuple(notes),
    )


# --- archetype models --------------------------------------------------------


class SemiSubmersible(BaseModel):
    """Column-stabilised semi-submersible floater.

    ``n_columns`` circular columns of diameter ``column_diameter`` sit on a
    radius ``column_radius`` from the platform centre, supported by submerged
    pontoons of total volume ``pontoon_volume``. Only the columns pierce the
    waterline, so the waterplane area is the sum of the column sections and the
    metacentric radius is dominated by their wide spacing (parallel-axis term).
    """

    n_columns: int = Field(3, ge=3, le=6)
    column_diameter: float = Field(..., gt=0.0, description="Column diameter (m)")
    column_radius: float = Field(
        ..., gt=0.0, description="Column centre offset from platform centre (m)"
    )
    draft: float = Field(..., gt=0.0, description="Draft, keel to waterline (m)")
    pontoon_height: float = Field(
        ..., gt=0.0, description="Pontoon vertical height (m)"
    )
    pontoon_volume: float = Field(
        ..., gt=0.0, description="Total submerged pontoon volume (m^3)"
    )
    steel_mass_t: float = Field(..., gt=0.0, description="Hull steel mass (t)")
    steel_vcg_frac: float = Field(
        0.45, gt=0.0, le=1.0, description="Steel VCG as a fraction of draft"
    )
    ca_heave: float = Field(1.0, ge=0.0, description="Heave added-mass ratio a33/m")
    ca_pitch: float = Field(0.4, ge=0.0, description="Pitch added-inertia ratio")
    @model_validator(mode="after")
    def _check_geometry(self) -> "SemiSubmersible":
        if self.pontoon_height >= self.draft:
            raise ValueError("pontoon_height must be less than draft")
        return self

    def properties(self, topside: TurbineTopside) -> FloaterProperties:
        a_col = math.pi / 4.0 * self.column_diameter**2
        # Submerged column volume above the pontoon top up to the waterline.
        col_sub_h = self.draft - self.pontoon_height
        v_col = self.n_columns * a_col * col_sub_h
        v = v_col + self.pontoon_volume
        displacement_t = RHO_SEAWATER * v / _KG_PER_TONNE

        waterplane_area = self.n_columns * a_col
        # I_wp about a centroidal horizontal axis: own section + parallel axis.
        # For n columns evenly spread on a ring, mean(r_perp^2) = R^2 / 2.
        i_own = self.n_columns * (math.pi / 64.0 * self.column_diameter**4)
        i_spread = a_col * self.column_radius**2 * (self.n_columns / 2.0)
        i_wp = i_own + i_spread
        bm = i_wp / v

        # KB: volume-weighted centroid of pontoon block and submerged columns.
        kb_pontoon = self.pontoon_height / 2.0
        kb_col = self.pontoon_height + col_sub_h / 2.0
        kb = (self.pontoon_volume * kb_pontoon + v_col * kb_col) / v

        kg_hull = self.steel_vcg_frac * self.draft
        topside_cg = self.draft + topside.cg_above_waterline_m()
        # Horizontal mass spread: columns sit at radius R; rms offset ~ R/sqrt(2).
        k_horiz = self.column_radius / math.sqrt(2.0)

        return _assemble(
            archetype=FloaterArchetype.SEMI,
            draft_m=self.draft,
            displacement_t=displacement_t,
            steel_mass_t=self.steel_mass_t,
            topside=topside,
            waterplane_area_m2=waterplane_area,
            kb_m=kb,
            bm_m=bm,
            kg_hull_m=kg_hull,
            topside_cg_above_keel_m=topside_cg,
            k_horizontal_m=k_horiz,
            ca_heave=self.ca_heave,
            ca_pitch=self.ca_pitch,
        )


class Spar(BaseModel):
    """Deep-draft spar (single cylinder, optional deep ballast).

    A spar's stability is set by a low centre of gravity (deep ballast) rather
    than its small waterplane. The metacentric radius ``BM = D^2 / (16 T)`` is
    small, so a positive ``GM`` requires ``KG`` well below ``KB``.
    """

    diameter: float = Field(..., gt=0.0, description="Cylinder diameter (m)")
    draft: float = Field(..., gt=0.0, description="Draft, keel to waterline (m)")
    steel_mass_t: float = Field(..., gt=0.0, description="Hull steel mass (t)")
    steel_vcg_frac: float = Field(
        0.55, gt=0.0, le=1.0, description="Steel VCG as a fraction of draft"
    )
    ca_heave: float = Field(0.9, ge=0.0, description="Heave added-mass ratio a33/m")
    ca_pitch: float = Field(0.25, ge=0.0, description="Pitch added-inertia ratio")

    def properties(self, topside: TurbineTopside) -> FloaterProperties:
        a_wp = math.pi / 4.0 * self.diameter**2
        v = a_wp * self.draft
        displacement_t = RHO_SEAWATER * v / _KG_PER_TONNE

        i_wp = math.pi / 64.0 * self.diameter**4
        bm = i_wp / v  # = D^2 / (16 T)
        kb = self.draft / 2.0

        kg_hull = self.steel_vcg_frac * self.draft
        topside_cg = self.draft + topside.cg_above_waterline_m()
        # A spar is slender: in-plane mass spread ~ cylinder radius of gyration.
        k_horiz = self.diameter / 4.0

        return _assemble(
            archetype=FloaterArchetype.SPAR,
            draft_m=self.draft,
            displacement_t=displacement_t,
            steel_mass_t=self.steel_mass_t,
            topside=topside,
            waterplane_area_m2=a_wp,
            kb_m=kb,
            bm_m=bm,
            kg_hull_m=kg_hull,
            topside_cg_above_keel_m=topside_cg,
            k_horizontal_m=k_horiz,
            ca_heave=self.ca_heave,
            ca_pitch=self.ca_pitch,
        )


class TLP(BaseModel):
    """Tension-leg platform: columns + pontoons restrained by vertical tendons.

    Vertical-plane stiffness comes from ``n_tendons`` tendons of axial stiffness
    ``EA`` (N) over length ``tendon_length`` (the water depth less draft), not
    from the waterplane. Hydrostatic ``GM`` is still reported (it is often small
    or negative -- the tendons carry pitch), and the heave/pitch periods are
    derived from the tendon stiffness, placing them well below the wave band by
    design.
    """

    n_columns: int = Field(4, ge=3, le=6)
    column_diameter: float = Field(..., gt=0.0, description="Column diameter (m)")
    column_radius: float = Field(
        ..., gt=0.0, description="Column centre offset from platform centre (m)"
    )
    draft: float = Field(..., gt=0.0, description="Draft, keel to waterline (m)")
    pontoon_height: float = Field(..., gt=0.0, description="Pontoon height (m)")
    pontoon_volume: float = Field(
        ..., gt=0.0, description="Total submerged pontoon volume (m^3)"
    )
    steel_mass_t: float = Field(..., gt=0.0, description="Hull steel mass (t)")
    steel_vcg_frac: float = Field(0.45, gt=0.0, le=1.0)
    n_tendons: int = Field(4, ge=3, description="Number of tendons")
    tendon_EA_N: float = Field(
        ..., gt=0.0, description="Per-tendon axial stiffness EA (N)"
    )
    tendon_length_m: float = Field(
        ..., gt=0.0, description="Tendon free length (water depth - draft) (m)"
    )

    @model_validator(mode="after")
    def _check_geometry(self) -> "TLP":
        if self.pontoon_height >= self.draft:
            raise ValueError("pontoon_height must be less than draft")
        return self

    def properties(self, topside: TurbineTopside) -> FloaterProperties:
        a_col = math.pi / 4.0 * self.column_diameter**2
        col_sub_h = self.draft - self.pontoon_height
        v_col = self.n_columns * a_col * col_sub_h
        v = v_col + self.pontoon_volume
        displacement_t = RHO_SEAWATER * v / _KG_PER_TONNE

        waterplane_area = self.n_columns * a_col
        i_own = self.n_columns * (math.pi / 64.0 * self.column_diameter**4)
        i_spread = a_col * self.column_radius**2 * (self.n_columns / 2.0)
        bm = (i_own + i_spread) / v
        kb_pontoon = self.pontoon_height / 2.0
        kb_col = self.pontoon_height + col_sub_h / 2.0
        kb = (self.pontoon_volume * kb_pontoon + v_col * kb_col) / v

        kg_hull = self.steel_vcg_frac * self.draft
        topside_cg = self.draft + topside.cg_above_waterline_m()

        # Tendon-derived vertical-plane periods (the design-driving stiffness).
        total_light = self.steel_mass_t + topside.total_mass_t
        # Pretension carries (buoyancy - weight); the moving mass for heave is
        # the structural + topside + entrained mass.
        m_heave = (total_light) * _KG_PER_TONNE * 1.3  # +30% heave added mass
        k_heave = self.n_tendons * self.tendon_EA_N / self.tendon_length_m
        heave_period = 2.0 * math.pi * math.sqrt(m_heave / k_heave)
        # Pitch tendon stiffness ~ k_axial * R^2 per tendon pair about centre.
        k_pitch = (
            self.n_tendons
            * (self.tendon_EA_N / self.tendon_length_m)
            * self.column_radius**2
            / 2.0
        )
        kyy = 0.45 * (2.0 * self.column_radius)
        i_pitch = total_light * _KG_PER_TONNE * kyy**2 * 1.3
        pitch_period = 2.0 * math.pi * math.sqrt(i_pitch / k_pitch)

        return _assemble(
            archetype=FloaterArchetype.TLP,
            draft_m=self.draft,
            displacement_t=displacement_t,
            steel_mass_t=self.steel_mass_t,
            topside=topside,
            waterplane_area_m2=waterplane_area,
            kb_m=kb,
            bm_m=bm,
            kg_hull_m=kg_hull,
            topside_cg_above_keel_m=topside_cg,
            k_horizontal_m=kyy,
            ca_heave=0.0,
            ca_pitch=0.0,
            tendon_stabilised=True,
            tendon_heave_period_s=heave_period,
            tendon_pitch_period_s=pitch_period,
            extra_notes=(
                "tendon-stabilised: heave/pitch periods set by tendon axial "
                "stiffness, kept below the wave band by design",
            ),
        )


class Barge(BaseModel):
    """Shallow-draft rectangular barge floater.

    A large waterplane gives a large ``BM`` (stiff in roll/pitch) but a lively
    heave. Block and waterplane coefficients capture the departure from a pure
    box.
    """

    length: float = Field(..., gt=0.0, description="Overall length (m)")
    beam: float = Field(..., gt=0.0, description="Overall beam (m)")
    draft: float = Field(..., gt=0.0, description="Draft, keel to waterline (m)")
    block_coeff: float = Field(0.95, gt=0.0, le=1.0, description="Block coefficient")
    waterplane_coeff: float = Field(
        1.0, gt=0.0, le=1.0, description="Waterplane area coefficient"
    )
    steel_mass_t: float = Field(..., gt=0.0, description="Hull steel mass (t)")
    steel_vcg_frac: float = Field(0.50, gt=0.0, le=1.0)
    ca_heave: float = Field(0.8, ge=0.0)
    ca_pitch: float = Field(0.3, ge=0.0)

    def properties(self, topside: TurbineTopside) -> FloaterProperties:
        v = self.length * self.beam * self.draft * self.block_coeff
        displacement_t = RHO_SEAWATER * v / _KG_PER_TONNE
        a_wp = self.length * self.beam * self.waterplane_coeff
        # Transverse waterplane inertia governs the smaller (roll) GM; use it as
        # the conservative screening value.
        i_wp = self.waterplane_coeff * self.length * self.beam**3 / 12.0
        bm = i_wp / v
        kb = self.draft / 2.0
        kg_hull = self.steel_vcg_frac * self.draft
        topside_cg = self.draft + topside.cg_above_waterline_m()
        # Roll (beam-governed) is the conservative screening axis; rms ~ B/sqrt(12).
        k_horiz = self.beam / math.sqrt(12.0)

        return _assemble(
            archetype=FloaterArchetype.BARGE,
            draft_m=self.draft,
            displacement_t=displacement_t,
            steel_mass_t=self.steel_mass_t,
            topside=topside,
            waterplane_area_m2=a_wp,
            kb_m=kb,
            bm_m=bm,
            kg_hull_m=kg_hull,
            topside_cg_above_keel_m=topside_cg,
            k_horizontal_m=k_horiz,
            ca_heave=self.ca_heave,
            ca_pitch=self.ca_pitch,
        )


# --- factory -----------------------------------------------------------------

_ARCHETYPE_MODELS: dict[FloaterArchetype, type[BaseModel]] = {
    FloaterArchetype.SEMI: SemiSubmersible,
    FloaterArchetype.SPAR: Spar,
    FloaterArchetype.TLP: TLP,
    FloaterArchetype.BARGE: Barge,
}


def build_floater(archetype: FloaterArchetype | str, **params):
    """Construct the floater model for ``archetype`` from keyword parameters.

    A small dispatch helper so callers (the screening orchestrator, the UV
    workflow) can build any archetype from a parameter dict without importing
    each class.
    """
    arch = FloaterArchetype(archetype)
    return _ARCHETYPE_MODELS[arch](**params)
