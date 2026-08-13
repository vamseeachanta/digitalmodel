"""
ABOUTME: KCS calm-water hull resistance by interFoam towing (#1173). Ports the
shipped DTCHull tutorial to the KCS hull at 1/31.6 and gates the computed force
decomposition against the Gothenburg 2000 / Tokyo 2005 Case 1.1 benchmark.

THE G5 DECISION - reuse, no new CaseType
----------------------------------------
This module emits a case directory from frozen literal ``@TOKEN@`` templates
ported from ``$FOAM_TUTORIALS/multiphase/interFoam/RAS/DTCHull``. It does NOT
introduce a ``CaseType.RESISTANCE`` and does not route through the generic
``OpenFOAMCaseBuilder``.

The reasoning, recorded here because a later reader will otherwise assume the
generic builder was simply overlooked:

* Ten of the eleven existing validation cases already emit frozen templates.
  The routing layer owns the ``CaseType`` enum, and adding a member to it makes
  a validation case a first-class production case type, which it is not - see
  the standing decision at ``validation/cylinder.py:147``.
* #1192's lesson is that the generic builder's emitted dicts drift from what a
  tutorial actually needs, and the drift surfaces as a solver failure hours
  into a run rather than as a build error in milliseconds. For a case whose
  production solve is measured in days, that trade is not close.
* A frozen template diffed against the shipped tutorial is *checkable*: the
  test suite asserts the emitted case differs from the tutorial only by an
  explicitly enumerated allowlist, in both directions. A generic builder's
  output cannot be diffed against anything.

THE REFERENT - why every function here takes the tuple, not just a number
-------------------------------------------------------------------------
A bare Ct is worthless. The referent is the tuple

    (attitude, appendage, normalising area, viscosity/Reynolds)

and a Ct quoted without it cannot be gated against at any tolerance. Two
revisions of this issue's plan were rejected for carrying a coefficient forward
without its tuple - once gating a body fixed in heave and pitch against a
free-to-sink-and-trim measurement, once re-scoring a with-rudder study against
a bare-hull reference. Both were transcription drift, not arithmetic slips.

The fixture this module loads therefore carries a provenance marker and a
citation on every field, and :func:`load_referent` refuses to return a row
whose condition tuple is incomplete.
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Optional, Sequence

from digitalmodel.naval_architecture.kcs_geometry import (
    KCS_GENERATED_WETTED_SURFACE as _KCS_GENERATED_WETTED_SURFACE,
)

# --------------------------------------------------------------------------- #
#  The gate thresholds. Mirrored in the fixture, which carries the derivation
#  of each one; these constants exist so a test can import them by name.
# --------------------------------------------------------------------------- #

#: V1 - relative tolerance on the total resistance coefficient Ct.
SHIP_RESISTANCE_CT_TOLERANCE = 0.0300

#: V2a - asymmetric band on the computed pressure coefficient. The low side is
#: wide because the offset is definitional: since Ct = Cp + Cv exactly while
#: Cr = Ct - Cf, the offset between a computed pressure coefficient and the
#: reference residuary is Cp - Cr = Cf - Cv at a matched total, and a viscous
#: coefficient above the ITTC-57 line drives Cp below Cr in one direction only.
SHIP_RESISTANCE_CP_TOLERANCE_LOW = -0.15
SHIP_RESISTANCE_CP_TOLERANCE_HIGH = 0.06

#: V2b - symmetric tolerance on the computed viscous coefficient vs ITTC-57.
SHIP_RESISTANCE_CV_TOLERANCE = 0.05

#: V3 - two-level self-consistency threshold, sized on the grid-uncertainty
#: scale rather than on the validation tolerance.
SHIP_RESISTANCE_MESH_CONSISTENCY_THRESHOLD = 0.015

#: V3 escalation band. A pair landing in here does not silently pass and does
#: not hard-fail: it re-derives the V1 budget from the measured Richardson
#: error estimate. The branch is pre-committed here, before any result exists.
SHIP_RESISTANCE_MESH_ESCALATION_BAND = (0.015, 0.030)

#: The limit of the decomposition gate, stated rather than discovered. Because
#: Ct = Cp + Cv holds identically, a compensating pair sitting on both
#: component boundaries lands inside V1 at +0.91%.
SHIP_RESISTANCE_DETECTION_FLOOR = 0.0091

#: Standard gravity used for the Froude number.
GRAVITY = 9.80665

#: The area every gated coefficient is normalised on (m^2). STATED by the
#: publishing workshop, hull only, static orientation, i.e. S_DWL. This is the
#: area the experimenters reduced their own Ct with, and matching it is what
#: makes the comparison like-for-like.
KCS_PUBLISHED_WETTED_SURFACE = 9.4379

#: The wetted area of the surface this repository GENERATES (m^2). Named here
#: so the guard below can reject it explicitly; it is never a normalisation.
KCS_GENERATED_WETTED_SURFACE = _KCS_GENERATED_WETTED_SURFACE

_REQUIRED_CONDITION_FIELDS = (
    "body_condition",
    "appendages",
    "wetted_surface",
    "reynolds",
)


# --------------------------------------------------------------------------- #
#  Analytic references - no OpenFOAM dependency, always exercised
# --------------------------------------------------------------------------- #

def ittc57_friction_coefficient(reynolds: float) -> float:
    """ITTC 1957 model-ship correlation line, Cf = 0.075 / (log10(Re) - 2)^2.

    This is a correlation line, not a measured friction coefficient. It is the
    line the publishing workshop used to reduce its own data, which is the only
    reason it is admissible as the V2b reference here.
    """
    if reynolds <= 0:
        raise ValueError(f"Reynolds number must be positive, got {reynolds}")
    denom = math.log10(reynolds) - 2.0
    if denom == 0:
        raise ValueError(f"ITTC-57 line is singular at Re = {reynolds}")
    return 0.075 / denom**2


def froude_number(velocity: float, length: float, gravity: float = GRAVITY) -> float:
    """Fr = U / sqrt(g * L)."""
    if length <= 0:
        raise ValueError(f"length must be positive, got {length}")
    return velocity / math.sqrt(gravity * length)


def kinematic_viscosity_for_reynolds(
    velocity: float, length: float, reynolds: float
) -> float:
    """nu = U * L / Re.

    The case's viscosity is *chosen to reproduce the benchmark's Reynolds
    number*, not looked up as a water property. The benchmark reduced its own
    experimental data at exactly its stated Re using the ITTC-57 line, so Re is
    the defining condition of the case and nu is downstream of it.
    """
    if reynolds <= 0:
        raise ValueError(f"Reynolds number must be positive, got {reynolds}")
    return velocity * length / reynolds


def resistance_coefficient(
    force: float, density: float, wetted_surface: float, velocity: float
) -> float:
    """C = R / (0.5 * rho * S * U^2).

    ``force`` must already be the FULL-BODY force. A half-domain solve reports
    half of it, and the doubling is the caller's responsibility - see
    :func:`parse_hull_force`, which does it explicitly and says so.
    """
    if density <= 0 or wetted_surface <= 0 or velocity == 0:
        raise ValueError(
            "density, wetted_surface must be positive and velocity non-zero"
        )
    return force / (0.5 * density * wetted_surface * velocity**2)


def richardson_error_estimate(
    epsilon: float, refinement_ratio: float, observed_order: float
) -> float:
    """delta_RE = eps / (r^p - 1) - the fine-grid error implied by a two-level
    difference.

    This is what makes the V3 escalation branch honest: a pair that lands in
    the escalation band re-derives the V1 uncertainty budget from THIS measured
    quantity, rather than from the borrowed grid uncertainty V1 was built on.
    """
    denom = refinement_ratio**observed_order - 1.0
    if denom <= 0:
        raise ValueError(
            f"r^p - 1 must be positive; got r={refinement_ratio}, p={observed_order}"
        )
    return epsilon / denom


# --------------------------------------------------------------------------- #
#  The referent fixture
# --------------------------------------------------------------------------- #

def referent_fixture_path() -> Path:
    """Path of the committed KCS benchmark fixture (repo checkout)."""
    here = Path(__file__).resolve()
    for parent in here.parents:
        cand = (parent / "tests" / "fixtures" / "test_vectors" / "marine" /
                "kcs_resistance_efd.yaml")
        if cand.is_file():
            return cand
    raise FileNotFoundError(
        "kcs_resistance_efd.yaml not found - requires a repo checkout with "
        "tests/fixtures/test_vectors/marine/"
    )


def load_fixture() -> Dict[str, Any]:
    """Load the whole committed benchmark fixture."""
    import yaml

    with referent_fixture_path().open() as handle:
        return yaml.safe_load(handle)


#: The row the #1173 gate is scored against. Named as a constant rather than
#: passed as a default argument so that selecting a different row is a visible
#: act at the call site. Selecting the T2015 row here would reproduce exactly
#: the defect that rejected revision 1 of the plan.
REFERENT_ROW_ID = "kcs_t2005_case_1_1_fixed_bare"


@dataclass(frozen=True)
class Referent:
    """The reference condition TUPLE, plus the coefficients defined on it.

    Every field is populated from the committed fixture. There are no defaults:
    a missing field raises rather than silently returning a plausible number,
    because a gate whose reference quietly became a default reads green.
    """

    row_id: str
    body_condition: str
    appendages: str
    wetted_surface: float
    reynolds: float
    ct: float
    lpp: float
    velocity: float
    froude: float
    #: Cf from the ITTC-57 line at this row's Re. Recomputed here at full
    #: precision rather than read from the source's rounded 2.83e-3.
    cf: float
    #: Cr = Ct - Cf, the V2a centre. DERIVED, at full precision. Note this is a
    #: small difference of large numbers: the +0.28% correction that moved Ct
    #: from 3.55e-3 to 3.56e-3 moved this by +1.39%, five times as far.
    cr: float
    #: nu chosen to reproduce ``reynolds`` at ``velocity`` and ``lpp``.
    nu: float
    provenance: Dict[str, Any]

    @property
    def residuary_fraction(self) -> float:
        """Cr / Ct = 0.2045. The fraction of the total this case exists to
        compute, and therefore the error of a solution that develops no free
        surface at all."""
        return self.cr / self.ct


def load_referent(row_id: str = REFERENT_ROW_ID) -> Referent:
    """Load the reference row and re-derive its coefficients from its own tuple.

    Fails closed in three ways, each of which corresponds to a defect that
    actually occurred in this issue's history:

    * a row whose condition tuple is incomplete is refused, because a Ct
      without its tuple cannot be gated against;
    * every value field must carry a provenance marker and a citation;
    * the derived coefficients are recomputed here from the row's own stated
      Ct and Re rather than read back from the fixture, so the fixture's
      arithmetic and this module's arithmetic must agree independently.
    """
    fixture = load_fixture()
    rows = {row["id"]: row for row in fixture.get("reference_rows", [])}
    if row_id not in rows:
        raise KeyError(
            f"reference row '{row_id}' not in fixture; have {sorted(rows)}"
        )
    row = rows[row_id]

    missing = [f for f in _REQUIRED_CONDITION_FIELDS if row.get(f) is None]
    if missing:
        raise ValueError(
            f"reference row '{row_id}' is missing condition-tuple field(s) "
            f"{missing}. A total resistance coefficient without its condition "
            f"tuple cannot be gated against at any tolerance."
        )

    values = row["values"]
    for name, entry in values.items():
        if not isinstance(entry, dict) or "provenance" not in entry:
            raise ValueError(
                f"reference row '{row_id}' field '{name}' carries no provenance "
                f"marker. Both rejections of this plan were provenance failures."
            )
        if "source" not in entry:
            raise ValueError(
                f"reference row '{row_id}' field '{name}' carries no citation."
            )

    ct = float(values["ct"]["value"])
    reynolds = float(row["reynolds"])
    lpp = float(values["lpp"]["value"])
    velocity = float(values["velocity_model"]["value"])

    cf = ittc57_friction_coefficient(reynolds)
    return Referent(
        row_id=row_id,
        body_condition=str(row["body_condition"]),
        appendages=str(row["appendages"]),
        wetted_surface=float(row["wetted_surface"]),
        reynolds=reynolds,
        ct=ct,
        lpp=lpp,
        velocity=velocity,
        froude=froude_number(velocity, lpp),
        cf=cf,
        cr=ct - cf,
        nu=kinematic_viscosity_for_reynolds(velocity, lpp, reynolds),
        provenance={name: dict(entry) for name, entry in values.items()},
    )


# --------------------------------------------------------------------------- #
#  The gates
# --------------------------------------------------------------------------- #

def v1_total_resistance(ct_cfd: float, referent: Referent) -> Dict[str, Any]:
    """V1 - |Ct_CFD - Ct_ref| / Ct_ref <= 3%."""
    error = (ct_cfd - referent.ct) / referent.ct
    return {
        "criterion": "V1",
        "quantity": "ct",
        "computed": ct_cfd,
        "reference": referent.ct,
        "relative_error": error,
        "tolerance": SHIP_RESISTANCE_CT_TOLERANCE,
        "passed": abs(error) <= SHIP_RESISTANCE_CT_TOLERANCE,
    }


def v2a_pressure_coefficient(cp_cfd: float, referent: Referent) -> Dict[str, Any]:
    """V2a - computed pressure coefficient inside an ASYMMETRIC band about Cr.

    Both bounds are reported separately so the band cannot degrade into a
    symmetric one by refactor without a test noticing.
    """
    error = (cp_cfd - referent.cr) / referent.cr
    return {
        "criterion": "V2a",
        "quantity": "cp",
        "computed": cp_cfd,
        "reference": referent.cr,
        "relative_error": error,
        "tolerance_low": SHIP_RESISTANCE_CP_TOLERANCE_LOW,
        "tolerance_high": SHIP_RESISTANCE_CP_TOLERANCE_HIGH,
        "passed_low_bound": error >= SHIP_RESISTANCE_CP_TOLERANCE_LOW,
        "passed_high_bound": error <= SHIP_RESISTANCE_CP_TOLERANCE_HIGH,
        "passed": (
            SHIP_RESISTANCE_CP_TOLERANCE_LOW
            <= error
            <= SHIP_RESISTANCE_CP_TOLERANCE_HIGH
        ),
    }


def v2b_viscous_coefficient(cv_cfd: float, referent: Referent) -> Dict[str, Any]:
    """V2b - computed viscous coefficient within 5% of the ITTC-57 line.

    This is the criterion that catches the tutorial's inherited
    ``nutkRoughWallFunction`` - a 100 micron sand-grain roughness on a smooth
    towing-tank model - without depending on whether the pressure side happens
    to compensate for it on the total.
    """
    error = (cv_cfd - referent.cf) / referent.cf
    return {
        "criterion": "V2b",
        "quantity": "cv",
        "computed": cv_cfd,
        "reference": referent.cf,
        "relative_error": error,
        "tolerance": SHIP_RESISTANCE_CV_TOLERANCE,
        "passed": abs(error) <= SHIP_RESISTANCE_CV_TOLERANCE,
    }


def v3_mesh_consistency(
    ct_fine: float,
    ct_coarse: float,
    *,
    refinement_ratio: float = math.sqrt(2.0),
    observed_order: float = 2.0,
) -> Dict[str, Any]:
    """V3 - two-level self-consistency, with a pre-committed escalation branch.

    V1 is deliberately NOT required on both levels. On a two-point study the
    coarse level's agreement with the experiment is not the property under
    test; self-consistency is. A coarse level that misses the experiment while
    tracking the fine level is informative. A pair that disagree with each
    other means the answer is mesh-dependent, and then no validation claim
    stands regardless of which level happens to match.

    Three outcomes, all decided before any number existed:

    * ``eps <= 1.5%``  - pass.
    * ``1.5% < eps <= 3.0%`` - ESCALATE. The borrowed numerical uncertainty is
      replaced by the measured Richardson estimate and the V1 budget is
      re-derived from it. This is the honest outcome for a near miss, and the
      arithmetic is fixed here rather than chosen after seeing the result.
    * ``eps > 3.0%`` - fail. The answer is mesh-dependent; no validation claim.
    """
    if ct_fine == 0:
        raise ValueError("ct_fine must be non-zero")
    epsilon = abs(ct_fine - ct_coarse) / abs(ct_fine)
    low, high = SHIP_RESISTANCE_MESH_ESCALATION_BAND

    result: Dict[str, Any] = {
        "criterion": "V3",
        "ct_fine": ct_fine,
        "ct_coarse": ct_coarse,
        "epsilon": epsilon,
        "threshold": SHIP_RESISTANCE_MESH_CONSISTENCY_THRESHOLD,
        "escalation_band": SHIP_RESISTANCE_MESH_ESCALATION_BAND,
        "escalated": False,
        "delta_re": None,
        "passed": epsilon <= SHIP_RESISTANCE_MESH_CONSISTENCY_THRESHOLD,
    }
    if low < epsilon <= high:
        delta_re = richardson_error_estimate(
            epsilon, refinement_ratio, observed_order
        )
        result["escalated"] = True
        result["delta_re"] = delta_re
        result["reopened_tolerance"] = _reopened_v1_tolerance(delta_re)
    return result


def _reopened_v1_tolerance(delta_re: float) -> float:
    """Re-derive the V1 budget with the MEASURED discretisation error in place
    of the borrowed numerical uncertainty.

    The V1 tolerance is 2.13x an uncertainty floor of 1.41%, itself
    RSS(U_D 1.00, U_SN 0.96, U_i 0.24) in percent. The escalation swaps the
    borrowed U_SN = 0.96 for the measured delta_RE and keeps the ratio.
    """
    u_d, u_i = 1.00, 0.24
    floor = math.sqrt(u_d**2 + (delta_re * 100.0) ** 2 + u_i**2)
    return floor / 100.0 * (SHIP_RESISTANCE_CT_TOLERANCE / 0.0141)


# --------------------------------------------------------------------------- #
#  The emitter
#
#  The case is emitted from FROZEN literal @TOKEN@ templates ported verbatim
#  from $FOAM_TUTORIALS/multiphase/interFoam/RAS/DTCHull. Every difference from
#  the tutorial is DECLARED below and asserted in both directions by
#  test_emitted_case_matches_dtc_tutorial_modulo_declared_deviations: every
#  allowlisted deviation must be present, and no un-allowlisted deviation may
#  exist. A hand-maintained diff drifts; an asserted one cannot.
# --------------------------------------------------------------------------- #

#: Geometry of the shipped DTC hull, measured from the tutorial's own STL on
#: the execution host. The domain is ported by scaling it to the KCS hull, so
#: these are the reference dimensions that scaling is relative to.
DTC_HULL_BOUNDS = {
    "x": (-0.113504, 6.16209),
    "y": (-0.429253, 0.429252),
    "z": (0.0, 0.572322),
}
#: The tutorial's free surface, and therefore the DTC model's draft, since its
#: STL is positioned keel-at-zero.
DTC_WATERLINE_Z = 0.244
DTC_HULL_LOA = DTC_HULL_BOUNDS["x"][1] - DTC_HULL_BOUNDS["x"][0]

#: The tutorial's own domain, in the same units.
DTC_DOMAIN = {
    "x": (-26.0, 16.0),
    "y": (-19.0, 0.0),
    "z_levels": (-16.0, -1.0, 0.185, 0.244, 0.3, 1.6, 4.0),
    "divisions": {"nx": 42, "ny": 19, "nza": 50, "nzb": 4, "nzc": 40, "nzd": 20},
    "location_in_mesh": (-0.7, 0.0, 0.0),
    "boxes": (
        ((-10.0, -6.0, -3.0), (10.0, 0.0, 3.0)),
        ((-5.0, -3.0, -2.5), (9.0, 0.0, 2.0)),
        ((-3.0, -1.5, -1.0), (8.0, 0.0, 1.5)),
        ((-2.0, -1.0, -0.6), (7.0, 0.0, 1.0)),
        ((-1.0, -0.6, -0.3), (6.5, 0.0, 0.8)),
        ((-0.5, -0.55, -0.15), (6.25, 0.0, 0.65)),
    ),
}

#: Every intended difference from the shipped tutorial, with the reason. The
#: test asserts this list is exactly the set of differences - no more, no less.
DECLARED_DEVIATIONS = {
    "stl_name": "the hull is KCS, not DTC",
    "umean": "the tow speed is the referent's 2.1962 m/s (Fr = 0.26)",
    "nu": "viscosity chosen to reproduce the referent's Re = 1.4e7",
    "rho": "water density stated consistently in transportProperties and the "
           "forces function object",
    "forces_rho": "the forces function object states its density source "
                  "explicitly; leaving it implicit in a two-phase run is how a "
                  "factor slips in unnoticed",
    "hull_wall_function": "nutkRoughWallFunction with Ks = 100e-6 replaced by "
                          "the smooth-wall nutkWallFunction; a 100 micron "
                          "sand-grain roughness on a towing-tank model is a "
                          "physics error against this reference, and V2b is "
                          "the criterion that would otherwise absorb it",
    "endtime": "25000 LTS iterations, not the tutorial's 4000; published "
               "practice for this case is 20000-40000",
    "write_interval": "fewer full writes over a run 6x longer, to bound disk",
    "cofr": "centre of rotation moved to the KCS hull's own midship",
    "waterline": "the free surface sits at the KCS draft above the keel",
    "domain": "domain extents, block divisions, refinement boxes and "
              "locationInMesh scaled to the KCS hull",
    "mesh_density": "block divisions scaled to the target cell count",
}


@dataclass(frozen=True)
class ShipResistanceConfig:
    """Configuration for a calm-water towing case.

    Defaults are the #1173 production case: KCS at 1/31.6, fixed even keel,
    bare hull, at the referent's own condition.
    """

    name: str = "kcs_calm_water_resistance"
    stl_name: str = "kcs.stl"

    #: Hull particulars at model scale (m).
    lpp: float = 7.2786
    beam: float = 1.0190
    draft: float = 0.3418
    #: Bare-hull wetted surface at the design waterline (m^2). This is the
    #: PUBLISHED value, and the reduction must use it: a coefficient is defined
    #: by its normalisation, so comparing against the workshop's Ct requires
    #: dividing by the workshop's S.
    wetted_surface: float = 9.4379

    #: Condition.
    velocity: float = 2.1962
    reynolds: float = 1.4e7
    density: float = 998.8

    #: Iterations. 25000 LTS steps, force mean over the final 4000.
    end_time: int = 25000
    averaging_window: int = 4000
    write_interval: int = 2500

    #: Mesh density multiplier on the tutorial's block divisions. 1.0
    #: reproduces the tutorial's resolution scaled to this hull.
    mesh_scale: float = 1.0

    #: Parallel ranks. Eight is the measured efficiency ceiling on the
    #: execution host; sixteen regresses.
    ranks: int = 8

    @property
    def nu(self) -> float:
        """Viscosity that reproduces the referent's Reynolds number."""
        return kinematic_viscosity_for_reynolds(
            self.velocity, self.lpp, self.reynolds
        )

    @property
    def froude(self) -> float:
        return froude_number(self.velocity, self.lpp)

    @property
    def hull_scale(self) -> float:
        """Ratio of this hull's overall length to the DTC tutorial hull's.

        The domain is ported by uniform scaling, which keeps every refinement
        box in the same position relative to the hull as the tutorial put it -
        a proven layout rather than a re-authored one.
        """
        return self.loa / DTC_HULL_LOA

    #: Overall length of the generated hull surface (m). Set from the geometry.
    loa: float = 7.6900


def ship_resistance_templates_dir() -> Path:
    """Directory of the frozen tutorial templates (installed package data)."""
    here = Path(__file__).resolve()
    cand = here.parent.parent / "templates" / "ship_resistance"
    if not (cand / "system" / "controlDict").is_file():
        raise FileNotFoundError(f"ship_resistance templates not found at {cand}")
    return cand


def _fmt(value: float) -> str:
    return f"{value:.6g}"


def _vec(v: Sequence[float]) -> str:
    return " ".join(_fmt(c) for c in v)


def emesh_name_for(stl_name: str) -> str:
    """The feature-edge file ``surfaceFeatureExtract`` will write for an STL.

    ``surfaceFeatureExtract`` names its output after the surface it was given:
    ``kcs.stl`` yields ``kcs.eMesh``. snappyHexMesh then reads that name back
    out of ``constant/triSurface``, so the two dictionaries have to agree.

    They agree here, once, because both are computed from ``stl_name``. Writing
    the extracted name as a second literal is what broke the first production
    mesh: ``surfaceFeatureExtractDict`` had been retargeted to ``kcs.stl`` and
    ``snappyHexMeshDict`` still asked for the tutorial's ``DTC-scaled.eMesh``,
    which no longer existed. snappyHexMesh cannot know the file was never going
    to be produced, so it ran blockMesh, the extraction and six topoSet /
    refineMesh pairs first and only then aborted on the missing file.
    """
    return Path(stl_name).with_suffix(".eMesh").name


def ship_resistance_tokens(config: ShipResistanceConfig) -> Dict[str, str]:
    """Every @TOKEN@ value, derived from the config.

    The domain is the tutorial's, scaled by ``hull_scale`` and with its free
    surface moved to this hull's draft. Nothing here is a fresh design: the
    proportions, grading and refinement staging are the tutorial's, which is
    the point of porting rather than authoring.
    """
    s = config.hull_scale
    waterline = DTC_WATERLINE_Z * s
    div = DTC_DOMAIN["divisions"]
    m = config.mesh_scale

    tokens: Dict[str, str] = {
        "STL": config.stl_name,
        # Derived, never stated: see emesh_name_for.
        "EMESH": emesh_name_for(config.stl_name),
        "UMEAN": _fmt(config.velocity),
        "NU": f"{config.nu:.6e}",
        "RHO": _fmt(config.density),
        "ENDTIME": str(config.end_time),
        "WRITEINTERVAL": str(config.write_interval),
        "WATERLINE": _fmt(waterline),
        "X0": _fmt(DTC_DOMAIN["x"][0] * s),
        "X1": _fmt(DTC_DOMAIN["x"][1] * s),
        "Y0": _fmt(DTC_DOMAIN["y"][0] * s),
        "NX": str(max(1, round(div["nx"] * m))),
        "NY": str(max(1, round(div["ny"] * m))),
        "NZA": str(max(1, round(div["nza"] * m))),
        "NZB": str(max(1, round(div["nzb"] * m))),
        "NZC": str(max(1, round(div["nzc"] * m))),
        "NZD": str(max(1, round(div["nzd"] * m))),
        "LOCATIONINMESH": _vec(
            [c * s for c in DTC_DOMAIN["location_in_mesh"]]
        ),
        # Midship of the placed hull, at the free surface.
        "COFR": _vec(
            [
                (DTC_HULL_BOUNDS["x"][0] + DTC_HULL_BOUNDS["x"][1]) / 2.0 * s,
                0.0,
                waterline,
            ]
        ),
    }
    for i, z in enumerate(DTC_DOMAIN["z_levels"]):
        tokens[f"Z{i}"] = _fmt(z * s)
    for i, (lo, hi) in enumerate(DTC_DOMAIN["boxes"], start=1):
        tokens[f"B{i}LO"] = _vec([c * s for c in lo])
        tokens[f"B{i}HI"] = _vec([c * s for c in hi])
    return tokens


def hull_placement(config: ShipResistanceConfig) -> Dict[str, Any]:
    """How the generated hull must be transformed into the emitted domain.

    Two operations, and the first is easy to miss with expensive consequences:

    * MIRROR IN X. The tutorial's inlet is at +x and its internal field is
      ``-Umean``, so the flow runs in the -x direction and the bow faces +x.
      The workshop grid uses X increasing downstream, so its bow is at -x. A
      hull installed without this flip is a hull towed stern-first, which
      solves perfectly happily and answers the wrong question.
    * TRANSLATE so the hull occupies the scaled tutorial hull's station, with
      its own waterline on the domain's free surface.
    """
    s = config.hull_scale
    waterline = DTC_WATERLINE_Z * s
    # After mirroring, the bow (originally at -x) is at +x.
    bow_target = DTC_HULL_BOUNDS["x"][1] * s
    return {
        "mirror_x": True,
        "translate": (bow_target - config.loa / 2.0, 0.0, waterline),
        "waterline_z": waterline,
        "scale": s,
    }


def build_ship_resistance_case(
    config: ShipResistanceConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Emit the case directory from the frozen templates."""
    config = config or ShipResistanceConfig()
    tokens = ship_resistance_tokens(config)
    templates = ship_resistance_templates_dir()

    case = Path(parent_dir) / config.name
    for src in sorted(templates.rglob("*")):
        if not src.is_file():
            continue
        rel = src.relative_to(templates)
        dst = case / rel
        dst.parent.mkdir(parents=True, exist_ok=True)
        text = src.read_text()
        for key, value in tokens.items():
            text = text.replace(f"@{key}@", value)
        leftover = re.findall(r"@[A-Z0-9]+@", text)
        if leftover:
            raise ValueError(
                f"unsubstituted token(s) {sorted(set(leftover))} in {rel}"
            )
        dst.write_text(text)

    # The runner and the tutorial both expect a 0/ directory to exist; the
    # Allrun pipeline restores it from 0.orig after meshing, exactly as the
    # tutorial does.
    (case / "0").mkdir(exist_ok=True)
    (case / "constant" / "triSurface").mkdir(parents=True, exist_ok=True)
    return case


def _provenance(config: ShipResistanceConfig) -> Dict[str, Any]:
    """Everything a reader needs to know what was actually solved."""
    referent = load_referent()
    return {
        "hull": "KCS (KRISO Container Ship)",
        "geometry_source": "CFD Workshop Tokyo 2005 structured surface grid "
                           "(1999 KRISO surface), condition-matched to the "
                           "Gothenburg 2000 / Tokyo 2005 Case 1.1 referent",
        "model_scale": "1/31.6",
        "body_condition": referent.body_condition,
        "appendages": referent.appendages,
        "wetted_surface_m2": config.wetted_surface,
        "froude": config.froude,
        "reynolds": config.reynolds,
        "velocity_used_for_nu": config.velocity,
        "nu": config.nu,
        "density": config.density,
        "iterations": config.end_time,
        "averaging_window": config.averaging_window,
        "ranks": config.ranks,
        "declared_deviations": dict(DECLARED_DEVIATIONS),
        "reference": {
            "ct": referent.ct,
            "cf_ittc57": referent.cf,
            "cr": referent.cr,
            "row_id": referent.row_id,
        },
    }


# --------------------------------------------------------------------------- #
#  Reading the solved forces
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class HullForce:
    """Mean hull force over an averaging window, split into its components.

    All three are FULL-BODY forces. The solve is on a half domain cut at the
    centreplane, so the function object reports half of each, and the doubling
    happens exactly once - here - rather than being left to a caller who may
    not know the domain was halved. Getting this wrong is a clean factor of two
    in Ct, which is why the test suite asserts the reduction against a
    published coefficient rather than only against itself.
    """

    total: float
    pressure: float
    viscous: float
    samples: int
    first_iteration: int
    last_iteration: int
    #: Standard deviation of the total over the window - the iterative scatter
    #: the acceptance criteria require to be recorded alongside the mean.
    scatter: float

    def __post_init__(self) -> None:
        residual = abs(self.total - (self.pressure + self.viscous))
        if residual > 1e-6 * max(abs(self.total), 1.0):
            raise ValueError(
                f"force components do not sum to the total: "
                f"{self.pressure} + {self.viscous} != {self.total}. "
                f"Ct = Cp + Cv must hold identically or the decomposition "
                f"gate is meaningless."
            )


def parse_hull_force(
    force_dat: Path | str,
    *,
    window: int = 4000,
    half_domain: bool = True,
    drag_axis: int = 0,
) -> HullForce:
    """Mean drag over the final ``window`` iterations of a forces log.

    The file is OpenFOAM's ``postProcessing/forces/<t>/force.dat``, whose
    columns are ``Time, total_xyz, pressure_xyz, viscous_xyz``. The drag
    component is taken as a magnitude: the tutorial tows in the -x direction,
    so the raw number is negative, and a sign convention is not something the
    gate should depend on.
    """
    rows: list[tuple[int, float, float, float]] = []
    for raw in Path(force_dat).read_text().splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.replace("(", " ").replace(")", " ").split()
        if len(parts) < 10:
            continue
        try:
            it = int(float(parts[0]))
            total = float(parts[1 + drag_axis])
            press = float(parts[4 + drag_axis])
            visc = float(parts[7 + drag_axis])
        except ValueError:
            continue
        rows.append((it, total, press, visc))

    if not rows:
        raise ValueError(f"no force samples parsed from {force_dat}")
    tail = rows[-window:] if window > 0 else rows
    n = len(tail)
    factor = 2.0 if half_domain else 1.0

    mean_total = sum(r[1] for r in tail) / n
    mean_press = sum(r[2] for r in tail) / n
    mean_visc = sum(r[3] for r in tail) / n
    var = sum((r[1] - mean_total) ** 2 for r in tail) / n

    sign = -1.0 if mean_total < 0 else 1.0
    return HullForce(
        total=sign * mean_total * factor,
        pressure=sign * mean_press * factor,
        viscous=sign * mean_visc * factor,
        samples=n,
        first_iteration=tail[0][0],
        last_iteration=tail[-1][0],
        scatter=math.sqrt(var) * factor,
    )


def coefficients_from_force(
    force: HullForce, config: ShipResistanceConfig
) -> Dict[str, float]:
    """Ct, Cp and Cv from a parsed force, on the PUBLISHED wetted surface.

    All three coefficients are normalised on the SAME published area. Cp and Cv
    are not exempt from this: they are components of Ct, so normalising them on
    a different area would break the Ct = Cp + Cv identity that V2a and V2b's
    independence rests on.

    The area is the workshop's, not our mesh's. A coefficient is defined by
    what it is divided by, so comparing against a published Ct requires
    dividing by the published S. The generated surface's own wetted area is
    +1.3% larger; using it here would introduce a systematic -1.29% on Ct that
    is pure bookkeeping and would spend a third of V1's budget before the
    physics is considered. That difference is a disclosed diagnostic, reported
    alongside the result, and is deliberately NOT corrected for.
    """
    _assert_normalisation_area(config.wetted_surface)

    def c(value: float) -> float:
        return resistance_coefficient(
            value, config.density, config.wetted_surface, config.velocity
        )

    return {
        "ct": c(force.total),
        "cp": c(force.pressure),
        "cv": c(force.viscous),
        "scatter": c(force.scatter),
        "reference_area": config.wetted_surface,
    }


def _assert_normalisation_area(area: float) -> None:
    """Refuse to score against the mesh-derived area.

    The two areas differ by only 1.3%, which is small enough to look like a
    rounding difference and large enough to move the verdict by a third of the
    tolerance. Transposing them would not throw, would not look wrong in a log,
    and would silently shift every coefficient in the same direction - so the
    guard is explicit rather than left to the default value being correct.
    """
    if abs(area - KCS_GENERATED_WETTED_SURFACE) < 5e-3:
        raise ValueError(
            f"refusing to normalise on {area} m^2: that is the wetted area of "
            f"the GENERATED surface, not the published reference area "
            f"({KCS_PUBLISHED_WETTED_SURFACE} m^2). A resistance coefficient "
            f"is defined by what it is divided by; scoring against the "
            f"workshop's Ct requires the workshop's S. The generated area is a "
            f"diagnostic and is reported separately."
        )
    if area <= 0:
        raise ValueError(f"normalisation area must be positive, got {area}")


# --------------------------------------------------------------------------- #
#  Scoring a completed run
# --------------------------------------------------------------------------- #

def evaluate_ship_resistance_run(
    force_dat: Path | str,
    config: ShipResistanceConfig | None = None,
    *,
    companion_force_dat: Path | str | None = None,
    companion_config: ShipResistanceConfig | None = None,
    mesh_cells: Optional[int] = None,
    companion_mesh_cells: Optional[int] = None,
) -> Dict[str, Any]:
    """Score a solved run against V1, V2a, V2b and (if given a pair) V3.

    Returns the manifest that goes into the verification artifact. Every
    criterion reports its own verdict and its own numbers; there is no single
    aggregate "passed" that could hide which one failed.

    The detection floor is included in the output unconditionally, whether the
    gates passed or not. It is a limit of the method — a compensating pair
    whose net effect on Ct is below ~0.91% cannot be detected by construction —
    and stating it only when convenient would make it a disclaimer rather than
    a property.
    """
    config = config or ShipResistanceConfig()
    referent = load_referent()

    force = parse_hull_force(
        force_dat, window=config.averaging_window, half_domain=True
    )
    coeffs = coefficients_from_force(force, config)

    v1 = v1_total_resistance(coeffs["ct"], referent)
    v2a = v2a_pressure_coefficient(coeffs["cp"], referent)
    v2b = v2b_viscous_coefficient(coeffs["cv"], referent)

    manifest: Dict[str, Any] = {
        "provenance": _provenance(config),
        "measurement": {
            "ct": coeffs["ct"],
            "cp": coeffs["cp"],
            "cv": coeffs["cv"],
            "force_total_N": force.total,
            "force_pressure_N": force.pressure,
            "force_viscous_N": force.viscous,
            "averaging_window": force.samples,
            "window_first_iteration": force.first_iteration,
            "window_last_iteration": force.last_iteration,
            "iterative_scatter_ct": coeffs["scatter"],
            "mesh_cells": mesh_cells,
        },
        "criteria": {"V1": v1, "V2a": v2a, "V2b": v2b},
        "normalisation": {
            "reference_area_m2": config.wetted_surface,
            "reference_area_source": (
                "PUBLISHED S_DWL, stated by the Gothenburg 2000 EFD table "
                "(hull only, no rudder, static orientation). S/Lpp^2 = 0.1781."
            ),
            "gated_number_uses": "reference_area_m2",
            "applies_to": ["ct", "cp", "cv"],
            "generated_surface_area_m2": KCS_GENERATED_WETTED_SURFACE,
            "generated_vs_reference": (
                KCS_GENERATED_WETTED_SURFACE - config.wetted_surface
            )
            / config.wetted_surface,
            "note": (
                "THE VERDICT RESTS ON reference_area_m2. The generated "
                "surface's own wetted area is reported as a diagnostic and is "
                "NOT used to normalise anything. A coefficient is defined by "
                "what it is divided by, so scoring against the workshop's Ct "
                "requires the workshop's S; normalising on our own area "
                "instead would put a systematic -1.29% on Ct that has nothing "
                "to do with solver accuracy and would spend a third of V1's "
                "budget on bookkeeping. All three of Ct, Cp and Cv use the "
                "same area - normalising the components differently would "
                "break the Ct = Cp + Cv identity V2a and V2b depend on."
            ),
            "diagnosis": (
                "Same hull, different reduction. Displaced volume over the "
                "SAME surface agrees with the published figure to 0.01%, so "
                "the geometry is right and the disagreement is in how area was "
                "computed. Volume is a third-moment integral and averages "
                "local surface detail away; area accumulates it directly, so "
                "station spacing, girth interpolation and transom/bulb "
                "treatment move S while leaving displacement untouched. "
                "Demonstrated rather than asserted: a girth integration over "
                "the workshop's own 57-station offsets table returns 9.83 m^2, "
                "+4.2% on the other side of the published value. Three "
                "reductions of one hull spanning 4%, with the volume fixed."
            ),
            "bias_direction": (
                "If the solved hull genuinely carries 1.3% more wetted area "
                "than the towed model, friction is correspondingly higher and "
                "Ct lands HIGH by roughly 1% - about a third of V1's budget - "
                "before the solver contributes anything."
            ),
        },
        "detection_floor": {
            "ct_fraction": SHIP_RESISTANCE_DETECTION_FLOOR,
            "note": (
                "A limit of the method, stated rather than discovered. Because "
                "Ct = Cp + Cv holds identically, a compensating pair sitting on "
                "both component boundaries lands inside V1 at +0.91%. The "
                "decomposition gate cannot detect a compensating pair whose net "
                "effect on Ct is below roughly that. This is a property of the "
                "arithmetic, not of the mesh."
            ),
        },
        "identity_check": {
            "cp_plus_cv": coeffs["cp"] + coeffs["cv"],
            "ct": coeffs["ct"],
            "holds": abs(coeffs["cp"] + coeffs["cv"] - coeffs["ct"])
            <= 1e-9 + 1e-6 * abs(coeffs["ct"]),
        },
    }

    if companion_force_dat is not None:
        companion_config = companion_config or config
        companion_force = parse_hull_force(
            companion_force_dat,
            window=companion_config.averaging_window,
            half_domain=True,
        )
        companion_coeffs = coefficients_from_force(
            companion_force, companion_config
        )
        v3 = v3_mesh_consistency(coeffs["ct"], companion_coeffs["ct"])
        v3["ct_coarse_cells"] = companion_mesh_cells
        v3["ct_fine_cells"] = mesh_cells
        if mesh_cells and companion_mesh_cells:
            v3["cell_ratio"] = mesh_cells / companion_mesh_cells
            v3["linear_refinement_ratio"] = (
                mesh_cells / companion_mesh_cells
            ) ** (1.0 / 3.0)
        manifest["criteria"]["V3"] = v3
        manifest["companion"] = {
            "ct": companion_coeffs["ct"],
            "cp": companion_coeffs["cp"],
            "cv": companion_coeffs["cv"],
            "mesh_cells": companion_mesh_cells,
            "iterative_scatter_ct": companion_coeffs["scatter"],
        }

    manifest["summary"] = {
        name: bool(verdict["passed"])
        for name, verdict in manifest["criteria"].items()
    }
    manifest["all_passed"] = all(manifest["summary"].values())
    return manifest
