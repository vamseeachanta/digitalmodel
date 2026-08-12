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
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict

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
