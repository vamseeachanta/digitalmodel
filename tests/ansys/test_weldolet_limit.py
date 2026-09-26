"""Limit-load sensitivity deck and P_L derivation (#2157 P0b, owner card S02).

Collapse is accepted only on corroborated evidence (Codex P0b review): converged
substeps come from the solver's monitor record cross-checked with the solver
log, and non-convergence counts as collapse only when every corroboration
check passes. Any failure refuses the receipt.
"""

from __future__ import annotations

import pytest

from digitalmodel.ansys import cint_parser
from digitalmodel.ansys import weldolet_crack as wc
from digitalmodel.ansys import weldolet_limit as wl


# --------------------------------------------------------------------------- #
# Deck
# --------------------------------------------------------------------------- #
def test_limit_deck_has_epp_material_and_no_cint():
    deck = wl.generate_limit_apdl(wl.LimitLoadSpec())
    for token in ("TB,BISO,1", "TBDATA,1,127.0,0.0", "NLGEOM,OFF", "NCNV,2",
                  "NSUBST,16,320,16", "*CFOPEN,weldolet_lpl_L0,txt", "PAPP = PAPP*4.0",
                  "*CFOPEN,weldolet_reacset_L0,txt"):
        assert token in deck, token
    assert "\nCINT," not in deck
    assert "PINT = 4.7*4.0" in deck
    assert "DTLIM" not in deck  # acceptance is not inferred from load increments


def test_limit_deck_shares_the_elastic_model():
    elastic = wc.generate_weldolet_apdl(wc.WeldoletSpec())
    limit = wl.generate_limit_apdl(wl.LimitLoadSpec())
    nodes_e = [ln for ln in elastic.splitlines() if ln.startswith(("N,", "E,", "EMORE,"))]
    nodes_l = [ln for ln in limit.splitlines() if ln.startswith(("N,", "E,", "EMORE,"))]
    assert nodes_e == nodes_l


def test_limit_deck_on_the_crotch_plane():
    spec = wl.LimitLoadSpec(plane="crotch")
    deck = wl.generate_limit_apdl(spec)
    assert "! crotch-plane root flaw" in deck
    assert "TB,BISO,1" in deck and "\nCINT," not in deck
    assert "D,ALL,UY,0.0" in deck  # symmetry plane of the half model
    assert wl.state_name(spec) == "p0b_limit_load_crotch_a2p35"
    receipt = {"spec": wl.spec_dict(spec)}
    assert wl.deck_sha256_for_receipt(receipt, 0) == wc.deck_sha256(deck)
    with pytest.raises(ValueError, match="plane"):
        wl.generate_limit_apdl(wl.LimitLoadSpec(plane="other"))


def test_limit_spec_requires_a_crack():
    with pytest.raises(ValueError, match="cracked model"):
        wl.generate_limit_apdl(wl.LimitLoadSpec(base=wc.WeldoletSpec(crack_depth_mm=None)))


def test_limit_deck_regenerates_from_receipt_spec():
    spec = wl.LimitLoadSpec()
    receipt = {"spec": wl.spec_dict(spec)}
    assert wl.deck_sha256_for_receipt(receipt, 0) == wc.deck_sha256(
        wl.generate_limit_apdl(spec)
    )
    assert wl.state_name(spec) == "p0b_limit_load_a2p35"


# --------------------------------------------------------------------------- #
# Log classification (a solver log gate, necessary but not sufficient)
# --------------------------------------------------------------------------- #
_NCNV = """
 *** ERROR ***                           ELAPSED TIME =    6825.345   TIME= 17:24:58
 The value of UZ at node 68450 is 9728418.3.  It is greater than the
 current limit of 1000000 (which can be reset on the NCNV command).
 This generally indicates rigid body motion as a result of an
 unconstrained model.  Verify that your model is properly constrained.

 *** ERROR ***                           ELAPSED TIME =    6825.345   TIME= 17:24:58
 *** MESSAGE CONTINUATION ---- DIAGNOSTIC INFORMATION ***
 Rigid body motion can also occur when net section yielding has
 occurred resulting in large displacements for small increments of load
"""


def test_only_nonconvergence_errors():
    ok = "x\n *** ERROR ***   TIME\n Solution not converged at time 0.7\n more\n"
    assert wl.only_nonconvergence_errors(ok)
    assert wl.only_nonconvergence_errors(_NCNV)
    assert not wl.only_nonconvergence_errors("clean log")
    assert not wl.only_nonconvergence_errors(ok + " *** ERROR ***\n bad element\n\n\n")
    assert not wl.only_nonconvergence_errors(ok + " *** FATAL ***\n")
    orphan = " *** ERROR ***\n *** MESSAGE CONTINUATION ---- DIAGNOSTIC INFORMATION ***\n\n\n"
    assert not wl.only_nonconvergence_errors(orphan)


# --------------------------------------------------------------------------- #
# Synthetic solver records
# --------------------------------------------------------------------------- #
# load factor = 4 x time (ramp to 4 x design pressure); maximum step 1/16 of the
# ramp (time 0.0625), minimum 1/320 (time 0.003125)
_CURVE = [  # (time, branch uz) along a curve with an elastic start and a plateau
    (0.0625, 0.010), (0.1250, 0.020), (0.1875, 0.030), (0.2500, 0.042),
    (0.2813, 0.060), (0.2844, 0.080), (0.2875, 0.150), (0.2906, 0.600),
]


def _completed_lines(times) -> str:
    out = []
    for k, t in enumerate(times, start=1):
        out.append(f" *** LOAD STEP     1   SUBSTEP {k:5d}  COMPLETED.    CUM ITER = {2 * k:6d}")
        out.append(f" *** TIME =   {t:.6E}     TIME INC =  0.625000E-01")
    return "\n".join(out) + "\n"


def _mntr(times) -> str:
    head = (" SOLUTION HISTORY INFORMATION FOR JOB: file.mntr\n"
            " ANSYS RELEASE 2026 R1.01       BUILD 26.1        08:23:43    09/25/2026\n\n"
            "  LOAD   SUB-  NO.  NO.    TOTL  INCREMENT    TOTAL         VARIAB 1\n"
            "  STEP   STEP ATTMP ITER   ITER  TIME/LFACT   TIME/LFACT    MONITOR\n\n")
    prev = 0.0
    rows = []
    for k, t in enumerate(times, start=1):
        rows.append(f"     1 {k:6d}    1     2 {2 * k:6d}   {t - prev:.5E}  {t:.5E}   1.0")
        prev = t
    return head + "\n".join(rows) + "\n"


def _lpl(rows) -> str:
    """rows: (time, uz); every result set written by POST1."""
    return "# columns: set time load_factor pressure_mpa uz_branch_end_mm ur_far_field_od_mm\n" + "".join(
        f"{k:8d}. {t:.10E} {4.0 * t:.10E} {4.0 * t * 4.7:.10E} {u:.10E} 0.0\n"
        for k, (t, u) in enumerate(rows, start=1)
    )


_PIVOT = (" *** WARNING ***                         ELAPSED TIME =       7.089\n"
          " There is at least 1 small equation solver pivot term (e.g., at the UZ\n"
          " degree of freedom of node 8) which may indicate a numerically unstable\n"
          " model.  Please check your results carefully.\n\n")
_INFO_PIVOT = " Sparse solver maximum pivot= 38906.6952 at node 4 UX.\n"


def _log(times, *, ncnv=True, net_section=True, pivot=None, other_error=False,
         bisection_attempts=True) -> str:
    """Solver log shaped like MAPDL's: informational pivot lines, and failed
    bisection attempts (pivot warning + NCNV divergence) before the late
    substeps, as in a real EPP run approaching collapse."""
    text = " START OF SOLUTION\n" + _INFO_PIVOT
    if pivot == "before_first":
        text += _PIVOT
    lines = _completed_lines(times).splitlines(keepends=True)
    for k in range(0, len(lines), 2):
        sub = k // 2 + 1
        if bisection_attempts and sub == len(times) - 1:  # a failed attempt, bisected
            text += _PIVOT + _NCNV + " *** BEGIN BISECTION NUMBER   1\n"
        if pivot == "converged" and sub == len(times) - 2:  # warning on a converged attempt
            text += _PIVOT
        text += lines[k] + lines[k + 1]
    if ncnv:
        text += _PIVOT  # the diverging attempts at collapse carry the singular tangent
    if ncnv:
        block = _NCNV if net_section else _NCNV.split(" *** ERROR ***")[0] + \
            " *** ERROR ***" + _NCNV.split(" *** ERROR ***")[1]
        text += block
    if other_error:
        text += " *** ERROR ***\n Element 12 has a negative Jacobian.\n\n\n"
    return text


def _result(curve=_CURVE, *, log_times=None, mon_times=None, extra_sets=((1.0, 5.0e6),),
            **log_kw):
    times = [t for t, _ in curve]
    log = _log(log_times if log_times is not None else times, **log_kw)
    mntr = _mntr(mon_times if mon_times is not None else times)
    lpl = _lpl(list(curve) + list(extra_sets))
    return wl.limit_load_result(
        wl.LimitLoadSpec(), lpl, wl.extract_convergence_record(mntr),
        wl.extract_log_evidence(log),
    )


# --------------------------------------------------------------------------- #
# Converged substeps from the solver (finding 3)
# --------------------------------------------------------------------------- #
def test_extracted_records_are_host_free_numbers():
    conv = wl.extract_convergence_record(_mntr([0.0625, 0.125]))
    assert "RELEASE" not in conv and "file.mntr" not in conv
    assert cint_parser.find_host_tokens(conv) == []
    ev = wl.extract_log_evidence(_log([0.0625, 0.125]))
    assert "completed_substeps 2" in ev
    assert "net_section_yield_diagnostic 1" in ev
    assert cint_parser.find_host_tokens(ev) == []


def test_accepted_sets_are_those_the_solver_reports_converged():
    res = _result()
    assert res["n_converged_substeps"] == len(_CURVE)
    assert res["rejected_sets"] == [{"set": len(_CURVE) + 1, "load_factor": 4.0}]
    assert res["p_limit_mpa"] == pytest.approx(4.0 * 0.2906 * 4.7)


def test_small_increment_set_not_in_the_monitor_record_is_not_accepted():
    """A set written after the last converged substep at a small bisection
    increment (the defect the increment-continuity rule could not catch) is
    rejected; the limit is taken at the last solver-confirmed substep."""
    curve = _CURVE + [(0.2922, 5.0)]  # small increment, but never converged
    times = [t for t, _ in _CURVE]
    res = _result(curve, log_times=times, mon_times=times, extra_sets=())
    assert res["n_converged_substeps"] == len(_CURVE)
    assert {"set": len(curve), "load_factor": pytest.approx(4.0 * 0.2922)} in [
        {"set": r["set"], "load_factor": pytest.approx(r["load_factor"])}
        for r in res["rejected_sets"]
    ]
    assert res["p_limit_mpa"] == pytest.approx(4.0 * 0.2906 * 4.7)


def test_monitor_and_log_disagreement_is_refused():
    times = [t for t, _ in _CURVE]
    res = _result(log_times=times[:-1])  # the log confirms one substep fewer
    assert res["collapse"]["accepted"] is False
    assert any("monitor" in p and "log" in p for p in res["collapse"]["problems"])


def test_converged_set_after_a_rejected_one_is_refused():
    times = [t for t, _ in _CURVE]
    curve = _CURVE[:3] + [(0.2000, 0.035)] + _CURVE[3:]  # stray set in the middle
    res = _result(curve, log_times=times, mon_times=times)
    assert res["collapse"]["accepted"] is False
    assert any("contiguous" in p for p in res["collapse"]["problems"])


# --------------------------------------------------------------------------- #
# Collapse corroboration (findings 1, 2, 4)
# --------------------------------------------------------------------------- #
def test_corroborated_collapse_is_accepted():
    res = _result()
    col = res["collapse"]
    assert col["accepted"] is True, col["problems"]
    assert col["thresholds"] == {
        "tes_gap_max": wl.COLLAPSE_TES_GAP_MAX,
        "tangent_ratio_max": wl.COLLAPSE_TANGENT_RATIO_MAX,
        "min_converged_substeps": wl.COLLAPSE_MIN_CONVERGED_SUBSTEPS,
        "bisection_exhausted_factor": wl.COLLAPSE_BISECTION_FACTOR,
    }
    assert res["reached_nonconvergence"] is True


def test_thresholds_are_the_stated_values():
    assert wl.COLLAPSE_TES_GAP_MAX == 0.10
    assert wl.COLLAPSE_TANGENT_RATIO_MAX == 0.02
    assert wl.COLLAPSE_MIN_CONVERGED_SUBSTEPS == 3
    assert wl.COLLAPSE_BISECTION_FACTOR == 1.001


def test_all_accepted_curve_to_the_final_load_factor_is_rejected():
    """Finding 4: a run that converges to the requested end of the ramp has no
    collapse evidence and must not yield a limit load."""
    curve = [(k / 16.0, 0.01 * k) for k in range(1, 17)]  # elastic to time 1.0
    times = [t for t, _ in curve]
    res = _result(curve, log_times=times, mon_times=times, extra_sets=(), ncnv=False)
    col = res["collapse"]
    assert col["accepted"] is False
    assert res["reached_nonconvergence"] is False
    assert any("non-convergence" in p for p in col["problems"])


def test_informational_pivot_lines_and_failed_attempt_warnings_are_not_indicators():
    ev = wl._kv(wl.extract_log_evidence(_log([t for t, _ in _CURVE])))
    assert ev["pivot_warnings_before_first_substep"] == 0
    assert ev["pivot_warnings_on_converged_attempts"] == 0
    assert ev["pivot_warnings_on_failed_attempts"] >= 2
    assert _result()["collapse"]["accepted"] is True


@pytest.mark.parametrize(
    "kwargs, reason",
    [
        ({"net_section": False}, "net-section"),
        ({"pivot": "before_first"}, "unconstrained"),
        ({"pivot": "converged"}, "unconstrained"),
        ({"other_error": True}, "error"),
    ],
)
def test_log_evidence_failures_refuse_collapse(kwargs, reason):
    col = _result(**kwargs)["collapse"]
    assert col["accepted"] is False
    assert any(reason in p for p in col["problems"]), col["problems"]


def test_rising_curve_fails_the_tangent_and_tes_checks():
    # still close to elastic at non-convergence: no plateau
    curve = [(0.0625 * k, 0.010 * k) for k in range(1, 5)] + [(0.2531, 0.0405)]
    col = _result(curve)["collapse"]
    assert col["accepted"] is False
    assert any("tangent" in p for p in col["problems"])
    assert any("TES" in p for p in col["problems"])


def test_bisection_not_exhausted_is_refused():
    # the last converged step is a full step, not the minimum step
    curve = [(0.0625 * k, 0.010 * k) for k in range(1, 4)] + [(0.25, 0.9)]
    col = _result(curve)["collapse"]
    assert col["accepted"] is False
    assert any("bisection" in p for p in col["problems"])


def test_too_few_converged_substeps_is_refused():
    curve = [(0.003125, 0.001), (0.00625, 5.0)]
    col = _result(curve)["collapse"]
    assert col["accepted"] is False
    assert any("substeps" in p for p in col["problems"])


def test_reaction_text_at_the_last_converged_set():
    table = ("# columns: set time pressure_mpa reaction_sum loaded_area_mm2\n"
             "       1.  0.6250000000E-01  0.1175000000E+01 -0.1095448000E+05  0.9322930040E+04\n"
             "       2.  0.1000000000E+01  0.1880000000E+02  0.4961935094E-27  0.9322930040E+04\n")
    text = wl.reaction_text_at(table, set_no=1, mapdl_rev="26.1", level=0)
    rec = cint_parser.parse_reaction_file(text)
    assert rec.stress_mpa == pytest.approx(1.175)
    assert rec.reaction_sum == pytest.approx(-10954.48)
    assert rec.mapdl_rev == "26.1"
    with pytest.raises(ValueError, match="set 3"):
        wl.reaction_text_at(table, set_no=3, mapdl_rev="26.1", level=0)


def test_check_hook_refuses_a_receipt_without_collapse():
    entry = {"limit_load": {"collapse": {"accepted": False, "problems": ["no non-convergence"]}}}
    problems = wl.limit_variant(wl.LimitLoadSpec(plane="crotch")).check(0, {}, entry)
    assert problems and "no non-convergence" in problems[0]


def test_receipt_check_refuses_a_limit_receipt_without_accepted_collapse():
    from digitalmodel.ansys import crack_receipt

    receipt = {"kind": "limit_load", "meshes": [{"level": 0, "artifacts": {}}],
               "limit_load": {"collapse": {"accepted": False},
                              "reached_nonconvergence": False}}
    problems = " | ".join(crack_receipt.limit_load_problems(receipt))
    assert "limit load without corroborated collapse" in problems
    assert "non-convergence" in problems
    assert "'conv' missing" in problems