"""KCS calm-water hull resistance (#1173) - referent, gates, emitter.

The tests in this module are the structural guard against the failure mode that
rejected two revisions of this issue's plan: a resistance coefficient carried
forward without the condition tuple that gives it meaning.

The most important test in the file is
:func:`test_reference_row_is_fixed_even_keel_bare_hull`. It asserts something
the primary source STATES, and it fails if a free-to-sink-and-trim or appended
row is ever selected as the gate reference.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.solvers.openfoam.validation.ship_resistance import (
    REFERENT_ROW_ID,
    SHIP_RESISTANCE_CP_TOLERANCE_HIGH,
    SHIP_RESISTANCE_CP_TOLERANCE_LOW,
    SHIP_RESISTANCE_CT_TOLERANCE,
    SHIP_RESISTANCE_CV_TOLERANCE,
    SHIP_RESISTANCE_DETECTION_FLOOR,
    SHIP_RESISTANCE_MESH_CONSISTENCY_THRESHOLD,
    Referent,
    froude_number,
    ittc57_friction_coefficient,
    kinematic_viscosity_for_reynolds,
    load_fixture,
    load_referent,
    resistance_coefficient,
    richardson_error_estimate,
    v1_total_resistance,
    v2a_pressure_coefficient,
    v2b_viscous_coefficient,
    v3_mesh_consistency,
)


@pytest.fixture(scope="module")
def referent() -> Referent:
    return load_referent()


@pytest.fixture(scope="module")
def fixture_data() -> dict:
    return load_fixture()


# --------------------------------------------------------------------------- #
#  The referent tuple
# --------------------------------------------------------------------------- #

def test_reference_row_is_fixed_even_keel_bare_hull(referent: Referent) -> None:
    """THE load-bearing test.

    Revision 1 of the #1173 plan gated a body fixed in heave and pitch against
    a free-to-sink-and-trim measurement. This test makes that impossible: the
    selected row must declare the fixed, bare-hull, S_DWL condition that the
    workshop states verbatim, or the gate does not run at all.
    """
    assert referent.body_condition == "fixed_even_keel"
    assert referent.appendages == "none"
    assert referent.wetted_surface == pytest.approx(9.4379)
    assert referent.reynolds == pytest.approx(1.4e7)
    assert referent.ct == pytest.approx(3.56e-3)


def test_referent_is_not_the_free_to_sink_and_trim_lineage(
    fixture_data: dict,
) -> None:
    """The OTHER lineage is present in the fixture and must never be the gate.

    T2015 Case 2.1 is free to heave and pitch, with a rudder, normalised on a
    different area, at a different Reynolds number. It is recorded so a future
    criterion has something correct to gate against - and so nobody
    re-discovers it by mistaking it for the referent.
    """
    rows = {row["id"]: row for row in fixture_data["reference_rows"]}
    other = rows["kcs_t2015_case_2_1_free_with_rudder"]
    assert other["body_condition"] == "free_to_heave_and_pitch"
    assert other["appendages"] == "rudder"
    assert other["wetted_surface"] == pytest.approx(9.5531)
    assert REFERENT_ROW_ID != other["id"]
    # Every field of the tuple differs. That is why blending them is a defect
    # rather than an approximation.
    gate = rows[REFERENT_ROW_ID]
    for field in ("body_condition", "appendages", "wetted_surface", "reynolds"):
        assert gate[field] != other[field], (
            f"the two lineages agree on '{field}' - check the transcription"
        )


def test_reference_row_carries_per_field_provenance(referent: Referent) -> None:
    """Every field carries a provenance marker AND a citation.

    Both rejections were provenance failures. This is the structural guard
    against a third.
    """
    assert referent.provenance, "no provenance recorded at all"
    for name, entry in referent.provenance.items():
        assert entry.get("provenance") in {"stated", "derived"}, (
            f"field '{name}' has provenance marker {entry.get('provenance')!r}"
        )
        assert entry.get("source"), f"field '{name}' carries no citation"


def test_referent_refuses_a_row_without_a_condition_tuple(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """A row missing any tuple field must raise, not return a plausible number."""
    from digitalmodel.solvers.openfoam.validation import ship_resistance

    broken = {
        "reference_rows": [
            {
                "id": "broken",
                "body_condition": "fixed_even_keel",
                "appendages": "none",
                # wetted_surface and reynolds deliberately absent
                "values": {},
            }
        ]
    }
    monkeypatch.setattr(ship_resistance, "load_fixture", lambda: broken)
    with pytest.raises(ValueError, match="condition-tuple"):
        ship_resistance.load_referent("broken")


def test_referent_refuses_a_field_without_provenance(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    from digitalmodel.solvers.openfoam.validation import ship_resistance

    broken = {
        "reference_rows": [
            {
                "id": "broken",
                "body_condition": "fixed_even_keel",
                "appendages": "none",
                "wetted_surface": 9.4379,
                "reynolds": 1.4e7,
                "values": {"ct": {"value": 3.56e-3}},
            }
        ]
    }
    monkeypatch.setattr(ship_resistance, "load_fixture", lambda: broken)
    with pytest.raises(ValueError, match="provenance"):
        ship_resistance.load_referent("broken")


# --------------------------------------------------------------------------- #
#  Derived quantities - recomputed independently of the fixture's own arithmetic
# --------------------------------------------------------------------------- #

def test_ittc57_line_reproduces_the_workshops_own_reduction(
    referent: Referent,
) -> None:
    """The workshop reduced its EFD at Re = 1.4e7 with the ITTC-57 line and
    published C_F0 = 2.83e-3 and C_R = 0.731e-3. Both must fall out."""
    assert referent.cf == pytest.approx(2.832045e-3, rel=1e-6)
    assert referent.cf == pytest.approx(2.83e-3, abs=5e-6)
    assert referent.cr == pytest.approx(7.27955e-4, rel=1e-5)
    # The workshop publishes C_R = 0.731e-3. Our derived value is 0.727955e-3,
    # 0.42% lower, and the gap is fully explained: the workshop reduced with
    # its own ROUNDED figures (3.56e-3 - 2.83e-3 = 0.730e-3, which it then
    # reports as 0.731e-3), whereas this module derives from the unrounded
    # ITTC-57 line. The V2a centre is deliberately the full-precision value -
    # rounding it back to the published digit would move the centre by 0.4%,
    # which on a band this narrow is not free.
    assert referent.cr == pytest.approx(0.731e-3, rel=0.005)


def test_residuary_fraction_sets_the_degenerate_case(referent: Referent) -> None:
    """Cr/Ct = 20.45%, so a solution developing no free surface at all returns
    pure friction and misses V1 by 6.8x. That is the known-negative control."""
    assert referent.residuary_fraction == pytest.approx(0.2045, abs=1e-4)


def test_viscosity_is_derived_from_the_stated_reynolds_number(
    referent: Referent,
) -> None:
    """nu is chosen to reproduce Re, not looked up as a water property.

    Using the rounded 2.196 m/s instead of the stated 2.1962 gives 1.1416e-6 -
    a 0.02% error that propagates straight into the condition the case is
    defined by.
    """
    assert referent.nu == pytest.approx(1.14180e-6, rel=1e-5)
    rounded = kinematic_viscosity_for_reynolds(2.196, referent.lpp, referent.reynolds)
    assert rounded != pytest.approx(referent.nu, rel=1e-5)


def test_froude_number_agrees_with_the_stated_condition(referent: Referent) -> None:
    assert referent.froude == pytest.approx(0.26, abs=1e-4)
    assert froude_number(2.1962, 7.2786) == pytest.approx(0.25995, abs=1e-5)


def test_wetted_surface_coefficient_agrees_with_the_stated_value(
    referent: Referent,
) -> None:
    assert referent.wetted_surface / referent.lpp**2 == pytest.approx(
        0.1781, abs=5e-5
    )


def test_ct_from_force_is_dimensionally_correct(fixture_data: dict) -> None:
    """Ct = R / (0.5 rho S U^2) must reproduce a published coefficient from its
    own tabulated force, area and speed.

    Scored on the T2015 row because it is the only row whose raw force is
    published (85.44 N at Fr 0.26). It doubles as the with-rudder-area check:
    the bare-hull area does NOT reproduce the published number, which is how
    the normalisation was confirmed independently of the prose.
    """
    ct = resistance_coefficient(85.44, 999.5, 9.5531, 2.196)
    assert ct == pytest.approx(3.7111e-3, rel=1e-4)

    wrong_area = resistance_coefficient(85.44, 999.5, 9.4379, 2.196)
    assert wrong_area == pytest.approx(3.7564e-3, rel=1e-4)
    assert wrong_area != pytest.approx(3.7111e-3, rel=1e-3)


def test_ittc57_rejects_nonsense_reynolds() -> None:
    with pytest.raises(ValueError):
        ittc57_friction_coefficient(0.0)
    with pytest.raises(ValueError):
        ittc57_friction_coefficient(-1.0)


# --------------------------------------------------------------------------- #
#  The gates - vacuity guards first (#1977)
# --------------------------------------------------------------------------- #

def test_gate_rejects_known_bad_value(fixture_data: dict, referent: Referent) -> None:
    """The known-negative control.

    A solution that develops no free-surface deformation returns pure friction.
    It must FAIL V1. Without this control, a gate whose reference silently
    became None reads green.
    """
    control = fixture_data["known_negative_control"]
    verdict = v1_total_resistance(control["ct"], referent)
    assert not verdict["passed"]
    assert verdict["relative_error"] == pytest.approx(
        control["expected_v1_error"], abs=1e-4
    )
    # It misses by 6.8x, not marginally.
    assert abs(verdict["relative_error"]) > 6 * SHIP_RESISTANCE_CT_TOLERANCE


def test_gate_reference_is_non_null_and_loaded_from_the_fixture(
    referent: Referent,
) -> None:
    """Vacuity guard: the reference must come from the committed fixture and
    must not be a default."""
    assert referent.ct is not None and referent.ct > 0
    assert referent.cf is not None and referent.cf > 0
    assert referent.row_id == REFERENT_ROW_ID
    assert referent.provenance["ct"]["provenance"] == "stated"


def test_v1_passes_a_condition_matched_published_result(
    fixture_data: dict, referent: Referent
) -> None:
    """All four condition-matched published results must pass V1 with margin.

    This is the whole achievability argument for a 3% gate, and every point in
    it is like-for-like with the referent: bare hull, fixed even keel,
    Re = 1.4e7.
    """
    rows = fixture_data["condition_matched_literature"]["rows"]
    assert len(rows) == 4
    for row in rows:
        verdict = v1_total_resistance(row["ct"], referent)
        assert verdict["passed"], f"{row['grid']} fails V1"
        assert verdict["relative_error"] == pytest.approx(row["error"], abs=5e-4)
        # at least 2.4x margin
        assert abs(verdict["relative_error"]) < SHIP_RESISTANCE_CT_TOLERANCE / 2.0


def test_condition_matched_error_shrinks_with_refinement(fixture_data: dict) -> None:
    """Coarse -1.24% -> medium -0.90% -> fine -0.96%: the error shrinks and then
    flattens. Revision 2 of the plan asserted the opposite from rows that had
    been transcribed rotated by one grid level."""
    rows = {r["grid"]: r for r in fixture_data["condition_matched_literature"]["rows"]}
    coarse = abs(rows["S3_coarse"]["error"])
    medium = abs(rows["S2_medium"]["error"])
    fine = abs(rows["S1_fine"]["error"])
    assert medium < coarse, "error must shrink from coarse to medium"
    assert fine < coarse
    # and the whole spread across a sixfold cell-count range is small
    assert max(coarse, medium, fine) - min(coarse, medium, fine) < 0.005


# --------------------------------------------------------------------------- #
#  Independence - the property revision 1's V2 did not have
# --------------------------------------------------------------------------- #

def test_v2_can_fail_while_v1_passes(fixture_data: dict, referent: Referent) -> None:
    """Vector 1 - compensating errors, the case V2 exists for.

    Revision 1's V2 admitted no such vector at all: it was algebraically
    implied by V1 and could never fail independently.
    """
    vec = {v["id"]: v for v in fixture_data["independence_vectors"]}[
        "v1_passes_v2_fails"
    ]
    assert vec["cp"] + vec["cv"] == pytest.approx(vec["ct"], rel=1e-9), (
        "the vector must satisfy Ct = Cp + Cv exactly, or it proves nothing"
    )
    assert v1_total_resistance(vec["ct"], referent)["passed"]
    assert not v2a_pressure_coefficient(vec["cp"], referent)["passed"]
    assert v2b_viscous_coefficient(vec["cv"], referent)["passed"]


def test_v1_can_fail_while_v2_passes(fixture_data: dict, referent: Referent) -> None:
    """Vector 2 - both components individually tolerable, the total is not."""
    vec = {v["id"]: v for v in fixture_data["independence_vectors"]}[
        "v2_passes_v1_fails"
    ]
    assert vec["cp"] + vec["cv"] == pytest.approx(vec["ct"], rel=1e-9)
    assert not v1_total_resistance(vec["ct"], referent)["passed"]
    assert v2a_pressure_coefficient(vec["cp"], referent)["passed"]
    assert v2b_viscous_coefficient(vec["cv"], referent)["passed"]


def test_v2a_band_is_asymmetric_and_both_bounds_are_asserted(
    referent: Referent,
) -> None:
    """The asymmetry is definitional, not caution, and must not degrade into a
    symmetric band by refactor.

    Since Ct = Cp + Cv exactly while Cr = Ct - Cf, the offset between a
    computed pressure coefficient and the reference residuary is Cp - Cr =
    Cf - Cv at a matched total. A viscous coefficient 1.2% above the ITTC-57
    line displaces Cp by -0.012 * Cf/Cr = -4.67%, one-sided.
    """
    assert SHIP_RESISTANCE_CP_TOLERANCE_LOW == -0.15
    assert SHIP_RESISTANCE_CP_TOLERANCE_HIGH == 0.06
    assert abs(SHIP_RESISTANCE_CP_TOLERANCE_LOW) != SHIP_RESISTANCE_CP_TOLERANCE_HIGH

    # the definitional displacement the low side exists to absorb
    displacement = -0.012 * referent.cf / referent.cr
    assert displacement == pytest.approx(-0.0467, abs=5e-4)
    assert SHIP_RESISTANCE_CP_TOLERANCE_LOW < displacement

    # a point just outside the tight (high) side fails on that bound alone
    just_high = referent.cr * (1.0 + SHIP_RESISTANCE_CP_TOLERANCE_HIGH + 0.001)
    verdict = v2a_pressure_coefficient(just_high, referent)
    assert not verdict["passed"]
    assert verdict["passed_low_bound"] and not verdict["passed_high_bound"]

    just_low = referent.cr * (1.0 + SHIP_RESISTANCE_CP_TOLERANCE_LOW - 0.001)
    verdict = v2a_pressure_coefficient(just_low, referent)
    assert not verdict["passed"]
    assert verdict["passed_high_bound"] and not verdict["passed_low_bound"]


def test_detection_floor_is_a_stated_limit_not_a_discovery(
    fixture_data: dict, referent: Referent
) -> None:
    """The decomposition gate cannot detect a compensating pair whose net
    effect on Ct is below ~0.91%. That is a property of the arithmetic, and it
    is stated in the artifact rather than discovered afterwards.

    The corner is evaluated a hair INSIDE each band. Constructing a value that
    sits exactly on a tolerance boundary and asking whether it passes is not a
    meaningful floating-point question: ``cr * 0.85`` divided back by ``cr``
    returns -0.15000000000000005, which is outside a ``>= -0.15`` comparison by
    5e-17. The gates are deliberately NOT epsilon-padded to absorb that -
    padding a gate is how a tolerance quietly grows - so the test moves instead.
    """
    inward = 1.0 - 1e-9
    cv = referent.cf * (1.0 + SHIP_RESISTANCE_CV_TOLERANCE * inward)
    cp = referent.cr * (1.0 + SHIP_RESISTANCE_CP_TOLERANCE_LOW * inward)
    ct = cv + cp
    error = (ct - referent.ct) / referent.ct
    assert error == pytest.approx(SHIP_RESISTANCE_DETECTION_FLOOR, abs=1e-4)
    assert error == pytest.approx(fixture_data["detection_floor"]["ct_percent"] / 100,
                                  abs=1e-4)
    # and it passes everything - which is exactly the point being disclosed
    assert v1_total_resistance(ct, referent)["passed"]
    assert v2a_pressure_coefficient(cp, referent)["passed"]
    assert v2b_viscous_coefficient(cv, referent)["passed"]


def test_asymmetry_shrinks_the_undetectable_set(referent: Referent) -> None:
    """The opposite corner: under the asymmetric band a (-5%, +6%) pair lands at
    -2.75% on the total and is caught by V1, where under a symmetric +/-15%
    band it would have hidden at -0.91%."""
    cv = referent.cf * 0.95
    cp_asym = referent.cr * 1.06
    cp_sym = referent.cr * 1.15
    err_asym = (cv + cp_asym - referent.ct) / referent.ct
    err_sym = (cv + cp_sym - referent.ct) / referent.ct
    assert err_asym == pytest.approx(-0.0275, abs=1e-3)
    assert err_sym == pytest.approx(-0.0091, abs=1e-3)
    assert abs(err_asym) > abs(err_sym), (
        "the asymmetric band must push the opposite corner further from zero, "
        "i.e. into V1's reach"
    )


# --------------------------------------------------------------------------- #
#  V3
# --------------------------------------------------------------------------- #

def test_mesh_levels_agree_within_grid_uncertainty() -> None:
    """V3 at 1.5%, on the grid-uncertainty scale rather than the validation
    tolerance."""
    assert SHIP_RESISTANCE_MESH_CONSISTENCY_THRESHOLD == 0.015
    verdict = v3_mesh_consistency(3.520e-3, 3.516e-3)
    assert verdict["passed"]
    assert not verdict["escalated"]
    assert verdict["epsilon"] < 0.015


def test_v3_near_miss_reopens_tolerance() -> None:
    """A synthetic pair at eps = 2.0% must trigger the escalation branch and
    re-derive the budget - not silently pass, and not hard-fail."""
    ct_fine = 3.560e-3
    ct_coarse = ct_fine * (1.0 - 0.020)
    verdict = v3_mesh_consistency(ct_fine, ct_coarse)
    assert verdict["epsilon"] == pytest.approx(0.020, abs=1e-6)
    assert not verdict["passed"], "2.0% must not pass the 1.5% threshold"
    assert verdict["escalated"], "2.0% must trigger the escalation branch"
    assert verdict["delta_re"] is not None and verdict["delta_re"] > 0
    assert verdict["reopened_tolerance"] > 0


def test_v3_hard_fails_above_the_escalation_band() -> None:
    """Above 3% the answer is mesh-dependent and no validation claim stands -
    regardless of which level happens to match the experiment."""
    ct_fine = 3.560e-3
    verdict = v3_mesh_consistency(ct_fine, ct_fine * (1.0 - 0.05))
    assert not verdict["passed"]
    assert not verdict["escalated"], (
        "5% is outside the escalation band and must not re-derive anything"
    )


def test_richardson_estimate_reproduces_the_published_order() -> None:
    """A 2.9% level-to-level difference implies a fine-grid error at or above
    the 1.41% floor V1 was derived from - which is why a 3% V3 threshold would
    have certified a result whose own discretisation error exceeded its
    budget."""
    at_published_order = richardson_error_estimate(0.029, math.sqrt(2.0), 2.813)
    assert at_published_order == pytest.approx(0.0176, abs=5e-4)
    at_formal_order = richardson_error_estimate(0.029, math.sqrt(2.0), 2.0)
    assert at_formal_order == pytest.approx(0.029, abs=1e-4)
    assert at_published_order > 0.0141
