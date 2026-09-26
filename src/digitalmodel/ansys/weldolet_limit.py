"""Elastic-perfectly-plastic limit load of the cracked weldolet model (#2157 P0b).

Owner card S02: the FE-linearised sigma_ref governs, and a limit-load
Lr = P / P_L is reported beside it as a sensitivity. This module writes the
limit-load deck on the same structured mesh as ``weldolet_crack`` (same
geometry, supports and proportional pressure loads, no CINT) through
``weldolet_crack.DeckHooks``, and derives P_L from the load-deflection record.

Collapse criterion (stated before the first run)
    Elastic-perfectly-plastic von Mises material with sigma_y = Rp0.2 =
    127 MPa (register M-03) and zero hardening, small-displacement theory, all
    pressure loads scaled by one load factor. An EPP small-displacement model
    has a true limit load: at collapse the deflection grows without bound and
    the Newton solution cannot converge. P_L is the pressure at the last
    converged substep, with automatic bisection down to 1/20 of the initial
    load step, so it is resolved to that step.

Corroboration (Codex P0b review; thresholds fixed before any re-run)
    Converged substeps come from the solver: the MAPDL monitor file (one row per
    converged substep), cross-checked with the substeps the solver log reports
    completed; the result sets are matched to them in order and any other set is
    rejected. Non-convergence is accepted as collapse only when every check
    passes: the log shows only non-convergence errors and no fatal error, the
    ramp stopped below the requested load, T1 bisection exhausted, T2 TES within
    10 % below P_L, T3 last tangent stiffness at most 2 % of elastic, T4 at
    least 3 converged substeps, no solver pivot warning, no "unconstrained" text
    outside the NCNV message, and the net-section-yielding diagnostic with any
    NCNV divergence. Otherwise the receipt is refused (fail-closed).
"""

from __future__ import annotations

import math
import re
from dataclasses import asdict, dataclass
from itertools import pairwise
from pathlib import Path

from digitalmodel.ansys import weldolet_crack as wc

GENERATOR_FILES = (
    "src/digitalmodel/ansys/weldolet_limit.py",
    "src/digitalmodel/ansys/weldolet_crotch.py",
    *wc.GENERATOR_FILES,
)

LIMIT_LOAD_CRITERION = (
    "Elastic-perfectly-plastic (sigma_y = Rp0.2 = 127 MPa, von Mises, no hardening), "
    "small displacement, all pressure loads (bore, crack faces, end thrusts) scaled "
    "proportionally. P_L is the pressure at the last converged substep when the "
    "Newton solution fails to converge with automatic bisection down to 1/20 of the "
    "initial load step (the limit of an EPP small-displacement model, where the "
    "deflection becomes unbounded). Converged substeps are those of the solver's "
    "monitor record, cross-checked with the solver log. Collapse is accepted only "
    "when corroborated: T1 bisection exhausted; T2 twice-elastic-slope pressure "
    "within 10 % below P_L; T3 last tangent stiffness <= 2 % of elastic; T4 >= 3 "
    "converged substeps, no solver pivot warning, no unconstrained indicator, and "
    "the net-section-yielding diagnostic with any NCNV divergence."
)


@dataclass(frozen=True)
class LimitLoadSpec:
    """Limit-load run on a cracked model: the fusion-face model ``base``
    (``plane = "fusion_face"``) or the crotch-plane model with the same crack
    depth, crack-face pressure and mesh level (``plane = "crotch"``)."""

    base: wc.WeldoletSpec = wc.WeldoletSpec()
    load_factor: float = 4.0  # final load factor of the proportional ramp
    substeps: int = 16  # initial step = 1/substeps of the ramp; minimum 1/(20 x)
    plane: str = "fusion_face"

    def crotch_spec(self):
        from digitalmodel.ansys.weldolet_crotch import CrotchSpec

        geometry = wc.WeldoletSpec(**{**self.base.__dict__, "crack_depth_mm": None,
                                      "mesh_level": 0})
        return CrotchSpec(crack_depth_mm=self.base.crack_depth_mm,
                          crack_face_pressure=self.base.crack_face_pressure,
                          mesh_level=self.base.mesh_level, base=geometry)

    def validate(self) -> list[str]:
        if self.plane not in ("fusion_face", "crotch"):
            return ["plane must be 'fusion_face' or 'crotch'"]
        if self.base.crack_depth_mm is None:
            return ["the limit-load run is on the cracked model"]
        issues = (self.crotch_spec().validate() if self.plane == "crotch"
                  else self.base.validate())
        if self.load_factor <= 1.0 or self.substeps < 2:
            issues.append("load_factor must exceed 1 and substeps be at least 2")
        return issues


# --------------------------------------------------------------------------- #
# Collapse evidence (Codex P0b review): thresholds fixed before any re-run
# --------------------------------------------------------------------------- #
# (T1) The solver must have bisected to its minimum step before giving up: the
#      last converged load increment is at most this factor times the minimum
#      step allowed by NSUBST (1/(20 n) of the ramp). A failure at a large step
#      says nothing about a limit; only exhausting the bisection does.
COLLAPSE_BISECTION_FACTOR = 1.001
# (T2) Twice-elastic-slope consistency: p_TES must exist, lie at or below P_L,
#      and (P_L - p_TES)/P_L <= 10 %. For an EPP body whose load-deflection
#      curve ends on a plateau, the TES intersection sits on the knee just
#      before the plateau; if the curve is still rising by more than a tenth of
#      the load beyond the TES point when the solver stops, the end point is not
#      a plateau and the non-convergence cannot be read as the limit.
COLLAPSE_TES_GAP_MAX = 0.10
# (T3) Plateau: the tangent stiffness of the last converged increment is at
#      most 2 % of the elastic stiffness. At 2 % a load increase of 1 % needs
#      fifty times the elastic displacement increment, i.e. unrestricted plastic
#      flow; an unconstrained (rigid-body) failure instead shows a normal
#      stiffness up to a sudden jump.
COLLAPSE_TANGENT_RATIO_MAX = 0.02
# (T4) No rigid-body or unconstrained indicator: at least this many converged
#      substeps before the divergence (an unconstrained model fails in the first
#      substeps), no equation-solver pivot warning in the log, no
#      "unconstrained" text outside the NCNV message template, and MAPDL's
#      net-section-yielding diagnostic present with the NCNV divergence.
COLLAPSE_MIN_CONVERGED_SUBSTEPS = 3


def lpl_file_name(level: int) -> str:
    return f"weldolet_lpl_L{level}"


def reacset_file_name(level: int) -> str:
    return f"weldolet_reacset_L{level}"


def conv_file_name(level: int) -> str:
    return f"weldolet_conv_L{level}"


def logev_file_name(level: int) -> str:
    return f"weldolet_logev_L{level}"


def state_name(spec: LimitLoadSpec) -> str:
    tag = wc.depth_tag(spec.base.crack_depth_mm)
    cfp = "" if spec.base.crack_face_pressure else "_cfp_off"
    plane = "_crotch" if spec.plane == "crotch" else ""
    return f"p0b_limit_load{plane}_{tag}{cfp}"


def _post(spec: LimitLoadSpec):
    """Every result set: load-deflection row and solved reaction sum. Which
    sets are converged is decided afterwards from the solver's own records."""

    def write(w, base: wc.WeldoletSpec, mesh: wc.WeldoletMesh, lv: int) -> None:
        geo = wc.derived_geometry(base)
        top_z = geo["run_outer_radius_mm"] + base.weldolet_a_mm + base.branch_length_mm
        length = base.run_half_length_mm
        ri = geo["run_inner_radius_mm"]
        w("*GET,NSETS,ACTIVE,0,SET,NSET")
        w(f"NSEL,S,LOC,Z,{top_z - 1.0e-6!r},{top_z + 1.0e-6!r}")
        w("*GET,NTOP,NODE,0,NUM,MIN")
        w("ALLSEL,ALL")
        w(f"AREAC = {math.pi * ri * ri * mesh.area_factor!r}")
        w(f"*CFOPEN,{lpl_file_name(lv)},txt")
        w("*VWRITE")
        w("('# digitalmodel weldolet_limit load-deflection record (every result set)')")
        w("*VWRITE")
        w("('# units: length=mm force=N stress=MPa')")
        w("*VWRITE")
        w("('# columns: set time load_factor pressure_mpa uz_branch_end_mm "
          "ur_far_field_od_mm')")
        w("*DO,KS,1,NSETS")
        w("SET,,,,,,,KS")
        w("*GET,TK,ACTIVE,0,SET,TIME")
        w("*GET,UZK,NODE,NTOP,U,Z")
        # far-field OD node at the bottom of the run pipe: radial = -UZ
        w(f"*GET,URK,NODE,{mesh.hoop_line[-1]},U,Z")
        w("URK = -URK")
        w(f"LFK = {spec.load_factor!r}*TK")
        w(f"PK = {base.pressure_mpa!r}*LFK")
        w("*VWRITE,KS,TK,LFK,PK,UZK,URK")
        w("(F8.0,5(1X,E18.10))")
        w("*ENDDO")
        w("*CFCLOS")
        w(f"*CFOPEN,{reacset_file_name(lv)},txt")
        w("*VWRITE")
        w("('# digitalmodel weldolet_limit solved reaction sum per result set')")
        w("*VWRITE")
        w("('# units: length=mm force=N stress=MPa')")
        w("*VWRITE,MREV")
        w("('# mapdl_rev ',F8.2)")
        w("*VWRITE")
        w("('# columns: set time pressure_mpa reaction_sum loaded_area_mm2')")
        w("*DO,KS,1,NSETS")
        w("SET,,,,,,,KS")
        w("*GET,TK,ACTIVE,0,SET,TIME")
        w(f"PK = {base.pressure_mpa!r}*{spec.load_factor!r}*TK")
        w(f"NSEL,S,LOC,X,{-length - 1.0e-6!r},{-length + 1.0e-6!r}")
        w("*GET,NFIXC,NODE,0,COUNT")
        w("FXC = 0.0")
        w("NN = 0")
        w("*DO,II,1,NFIXC")
        w("NN = NDNEXT(NN)")
        w("*GET,RV,NODE,NN,RF,FX")
        w("FXC = FXC + RV")
        w("*ENDDO")
        w("ALLSEL,ALL")
        w("*VWRITE,KS,TK,PK,FXC,AREAC")
        w("(F8.0,4(1X,E18.10))")
        w("*ENDDO")
        w("*CFCLOS")

    return write


def hooks(spec: LimitLoadSpec) -> wc.DeckHooks:
    n = spec.substeps
    return wc.DeckHooks(
        name="limit_load",
        material=("TB,BISO,1", f"TBDATA,1,{spec.base.yield_mpa!r},0.0"),
        solution=("NLGEOM,OFF", "AUTOTS,ON", f"NSUBST,{n},{20 * n},{n}", "NCNV,2",
                  "OUTRES,ALL,ALL"),
        load_factor=f"*{spec.load_factor!r}",
        papp=(f"PAPP = PAPP*{spec.load_factor!r}", "*GET,TLAST,ACTIVE,0,SET,TIME",
              "PAPP = PAPP*TLAST"),
        post=_post(spec),
    )


def at_level(spec: LimitLoadSpec, level: int) -> LimitLoadSpec:
    base = wc.WeldoletSpec(**{**spec.base.__dict__, "mesh_level": level})
    return LimitLoadSpec(base=base, load_factor=spec.load_factor, substeps=spec.substeps,
                         plane=spec.plane)


def generate_limit_apdl(spec: LimitLoadSpec) -> str:
    issues = spec.validate()
    if issues:
        raise ValueError("invalid limit-load spec: " + "; ".join(issues))
    if spec.plane == "crotch":
        from digitalmodel.ansys.weldolet_crotch import generate_crotch_apdl

        return generate_crotch_apdl(spec.crotch_spec(), hooks(spec))
    return wc.generate_weldolet_apdl(spec.base, hooks(spec))


def spec_dict(spec: LimitLoadSpec) -> dict:
    base = {k: v for k, v in asdict(spec.base).items() if k != "mesh_level"}
    return {"base": base, "load_factor": spec.load_factor, "substeps": spec.substeps,
            "plane": spec.plane}


def spec_from_receipt(receipt: dict, level: int) -> LimitLoadSpec:
    s = receipt["spec"]
    base = wc.WeldoletSpec(**{**s["base"], "mesh_level": level})
    return LimitLoadSpec(base=base, load_factor=s["load_factor"], substeps=s["substeps"],
                         plane=s.get("plane", "fusion_face"))


def deck_sha256_for_receipt(receipt: dict, level: int) -> str:
    return wc.deck_sha256(generate_limit_apdl(spec_from_receipt(receipt, level)))


# --------------------------------------------------------------------------- #
# Solver-log gate and host-free evidence records
# --------------------------------------------------------------------------- #
def _error_blocks(out_text: str) -> list[dict]:
    """Classified ``*** ERROR ***`` blocks of the solver log."""
    lines = out_text.splitlines()
    blocks: list[dict] = []
    for i, line in enumerate(lines):
        if "*** ERROR ***" not in line:
            continue
        body = " ".join(lines[i + 1:i + 5]).lower()
        if "message continuation" in body:
            kind = "continuation"
        elif "ncnv" in body and "limit" in body:
            kind = "ncnv_limit"
        elif "converge" in body:
            kind = "not_converged"
        else:
            kind = "other"
        blocks.append({"kind": kind, "body": body})
    return blocks


def only_nonconvergence_errors(out_text: str) -> bool:
    """Solver-log gate for the limit-load run (necessary, not sufficient):
    every ``*** ERROR ***`` is a non-convergence (Newton non-convergence, or the
    NCNV displacement-limit divergence with its diagnostic continuation) and no
    ``*** FATAL ***`` occurred. Collapse also needs ``collapse_checks``."""
    if "*** FATAL ***" in out_text:
        return False
    blocks = _error_blocks(out_text)
    previous_ok = False
    for b in blocks:
        ok = previous_ok if b["kind"] == "continuation" else b["kind"] != "other"
        if not ok:
            return False
        previous_ok = ok
    return bool(blocks)


_PIVOT_WARNING = re.compile(r"(?i)(small|negative|zero)\s+(equation\s+solver\s+)?pivot")
_COMPLETED = re.compile(r"SUBSTEP\s+\d+\s+COMPLETED")


def _pivot_positions(out_text: str) -> tuple[int, int, int, int]:
    """Classify solver pivot WARNINGS by where they occur (informational
    'Sparse solver ... pivot=' lines are not warnings):

    (before the first converged substep, on an attempt that then converged,
    on an attempt that failed, non-convergence errors before the first
    converged substep). A pivot warning followed by a non-convergence error
    before the next completed substep belongs to a failed attempt.
    """
    before = on_converged = on_failed = nc_before = 0
    pending = 0  # pivot warnings since the last completion or error
    seen_completed = False
    lines = out_text.splitlines()
    for i, line in enumerate(lines):
        if _COMPLETED.search(line):
            if seen_completed:
                on_converged += pending
            else:
                before += pending
            pending = 0
            seen_completed = True
        elif "*** ERROR ***" in line:
            body = " ".join(lines[i + 1:i + 5]).lower()
            if "converge" in body or ("ncnv" in body and "limit" in body):
                if not seen_completed:
                    nc_before += 1
                    before += pending
                else:
                    on_failed += pending
                pending = 0
        elif _PIVOT_WARNING.search(line) and "pivot=" not in line:
            pending += 1
    if pending:  # warnings after the last error without a later completion
        if seen_completed:
            on_failed += pending
        else:
            before += pending
    return before, on_converged, on_failed, nc_before


def extract_log_evidence(out_text: str) -> str:
    """Host-free flags and counts from the solver log (committed artifact)."""
    completed = re.findall(r"SUBSTEP\s+\d+\s+COMPLETED", out_text)
    times = re.findall(r"\*\*\*\s*TIME\s*=\s*([0-9.Ee+-]+)", out_text)
    blocks = _error_blocks(out_text)
    ncnv_bodies = [b["body"] for b in blocks if b["kind"] in ("ncnv_limit", "continuation")]
    # every NCNV divergence must carry MAPDL's net-section-yielding continuation
    n_ncnv = n_ncnv_ns = 0
    for j, b in enumerate(blocks):
        if b["kind"] == "ncnv_limit":
            n_ncnv += 1
            nxt = blocks[j + 1] if j + 1 < len(blocks) else None
            if nxt and nxt["kind"] == "continuation" and "net section yield" in nxt["body"]:
                n_ncnv_ns += 1
    net_section = n_ncnv > 0 and n_ncnv_ns == n_ncnv
    low = out_text.lower()
    piv = _pivot_positions(out_text)
    unconstrained_all = low.count("unconstrained")
    unconstrained_ncnv = sum(body.count("unconstrained") for body in ncnv_bodies)
    previous_ok, other = False, 0
    for b in blocks:
        ok = previous_ok if b["kind"] == "continuation" else b["kind"] != "other"
        other += 0 if ok else 1
        previous_ok = ok
    rows = [
        ("completed_substeps", len(completed)),
        ("last_completed_time", float(times[len(completed) - 1]) if completed and
         len(times) >= len(completed) else -1.0),
        ("nonconvergence_errors",
         sum(1 for b in blocks if b["kind"] in ("ncnv_limit", "not_converged"))),
        ("ncnv_displacement_limit", int(any(b["kind"] == "ncnv_limit" for b in blocks))),
        ("net_section_yield_diagnostic", int(net_section)),
        ("other_errors", other),
        ("fatal_errors", out_text.count("*** FATAL ***")),
        ("pivot_warnings_before_first_substep", piv[0]),
        ("pivot_warnings_on_converged_attempts", piv[1]),
        ("pivot_warnings_on_failed_attempts", piv[2]),
        ("nonconvergence_before_first_substep", piv[3]),
        ("unconstrained_outside_ncnv", max(0, unconstrained_all - unconstrained_ncnv)),
    ]
    head = ("# digitalmodel weldolet_limit solver-log evidence (host-free counts and "
            "flags extracted from the MAPDL output)\n")
    return head + "".join(f"{k} {v!r}\n" for k, v in rows)


def extract_convergence_record(mntr_text: str) -> str:
    """Numeric rows of the MAPDL monitor file: one row per converged substep
    (load step, substep, attempts, iterations, total iterations, time
    increment, cumulative time). The header (release, date, job) is dropped."""
    out = ["# digitalmodel weldolet_limit convergence record (converged substeps, "
           "numeric columns of the MAPDL monitor file)",
           "# columns: step substep attempts iterations total_iterations time_increment time"]
    for line in mntr_text.splitlines():
        toks = line.split()
        if len(toks) < 7:
            continue
        try:
            ints = [int(t) for t in toks[:5]]
            flts = [float(t) for t in toks[5:7]]
        except ValueError:
            continue
        out.append(" ".join(str(v) for v in ints) + " " + " ".join(f"{v:.5E}" for v in flts))
    return "\n".join(out) + "\n"


def _kv(text: str) -> dict[str, float]:
    out = {}
    for line in text.replace("\r\n", "\n").splitlines():
        parts = line.split()
        if len(parts) == 2 and not line.startswith("#"):
            out[parts[0]] = float(parts[1])
    return out


def _rows(text: str) -> list[list[float]]:
    return wc._data_rows(text)


def _close(a: float, b: float) -> bool:
    return abs(a - b) <= 1.0e-5 * max(1.0, abs(a), abs(b))


def limit_load_result(spec: LimitLoadSpec, lpl_text: str, conv_text: str,
                      logev_text: str) -> dict:
    """P_L and the corroborated collapse verdict from the committed records.

    Accepted (converged) result sets are those whose time matches, in order,
    the solver's monitor record of converged substeps, which must itself agree
    with the substeps the solver log reports completed. Collapse is accepted
    only when every check (T1-T4 and the log gate) passes; otherwise
    ``collapse.accepted`` is false and no receipt may be written.
    """
    sets = _rows(lpl_text)  # set, time, load_factor, pressure, uz, ur
    mon = [r[6] for r in _rows(conv_text)]
    ev = _kv(logev_text)
    problems: list[str] = []
    n_log = round(ev.get("completed_substeps", -1))
    if n_log != len(mon):
        problems.append(f"monitor record has {len(mon)} converged substeps but the solver "
                        f"log reports {n_log} completed")
    elif mon and not _close(ev.get("last_completed_time", -1.0), mon[-1]):
        problems.append("last converged time differs between the monitor record and the "
                        "solver log")
    accepted, rejected, k = [], [], 0
    for row in sets:
        if k < len(mon) and _close(row[1], mon[k]):
            if rejected:
                problems.append(f"converged set {round(row[0])} follows a rejected set: "
                                "accepted sets are not contiguous")
            accepted.append(row)
            k += 1
        else:
            rejected.append(row)
    if k != len(mon):
        problems.append(f"only {k} of {len(mon)} converged substeps have a result set")
    rejected_sets = [{"set": round(r[0]), "load_factor": r[2]} for r in rejected]
    if not accepted:
        return {"criterion": LIMIT_LOAD_CRITERION, "n_converged_substeps": 0,
                "rejected_sets": rejected_sets, "p_limit_mpa": None,
                "reached_nonconvergence": False,
                "collapse": {"accepted": False, "checks": {}, "thresholds": _thresholds(),
                             "problems": problems + ["no converged result set"]}}
    p = [r[3] for r in accepted]
    u = [r[4] for r in accepted]
    k_el = p[0] / u[0] if u[0] else math.inf
    tes = None
    for (p0, u0), (p1, u1) in pairwise(zip(p, u, strict=True)):
        f0, f1 = p0 - 0.5 * k_el * u0, p1 - 0.5 * k_el * u1
        if f0 > 0.0 >= f1:
            tes = p0 + f0 / (f0 - f1) * (p1 - p0)
            break
    k_last = None
    if len(p) > 1 and u[-1] != u[-2]:
        k_last = (p[-1] - p[-2]) / (u[-1] - u[-2])
    ratio = None if k_last is None else k_last / k_el
    design = spec.base.pressure_mpa
    p_lim = p[-1]
    full = design * spec.load_factor
    min_step = full / (20 * spec.substeps)
    last_inc = p[-1] - p[-2] if len(p) > 1 else p[-1]
    nonconv = ev.get("nonconvergence_errors", 0) > 0
    reached = nonconv and p_lim < full * 0.999
    checks = {
        "log_gate_nonconvergence_only": bool(nonconv and ev.get("other_errors", 1) == 0
                                             and ev.get("fatal_errors", 1) == 0),
        "below_requested_load": p_lim < full * 0.999,
        "t1_bisection_exhausted": last_inc <= COLLAPSE_BISECTION_FACTOR * min_step,
        "t2_tes_consistent": tes is not None and tes <= p_lim
        and (p_lim - tes) / p_lim <= COLLAPSE_TES_GAP_MAX,
        "t3_tangent_plateau": ratio is not None and ratio <= COLLAPSE_TANGENT_RATIO_MAX,
        "t4_min_substeps": len(accepted) >= COLLAPSE_MIN_CONVERGED_SUBSTEPS,
        "t4_no_pivot_warning": ev.get("pivot_warnings_before_first_substep", 1) == 0
        and ev.get("pivot_warnings_on_converged_attempts", 1) == 0
        and ev.get("nonconvergence_before_first_substep", 1) == 0,
        "t4_no_unconstrained_text": ev.get("unconstrained_outside_ncnv", 1) == 0,
        "t4_net_section_diagnostic": ev.get("ncnv_displacement_limit", 0) == 0
        or ev.get("net_section_yield_diagnostic", 0) == 1,
    }
    msgs = {
        "log_gate_nonconvergence_only":
            "solver log: no non-convergence error, or other/fatal errors present",
        "below_requested_load": "no non-convergence: the ramp reached the requested load",
        "t1_bisection_exhausted": "T1 failed: bisection not exhausted at the last step",
        "t2_tes_consistent": "T2 failed: TES pressure missing or not within 10 % below P_L",
        "t3_tangent_plateau": "T3 failed: last tangent stiffness above 2 % of elastic",
        "t4_min_substeps": "T4 failed: fewer than 3 converged substeps",
        "t4_no_pivot_warning": "T4 failed: unconstrained indicator (solver pivot warning "
                               "or divergence before the first converged substep, or a "
                               "pivot warning on a converged attempt)",
        "t4_no_unconstrained_text":
            "T4 failed: unconstrained indicator outside the NCNV message",
        "t4_net_section_diagnostic": "T4 failed: NCNV divergence without the net-section "
                                     "yielding diagnostic",
    }
    if ev.get("other_errors", 0) > 0:
        msgs["log_gate_nonconvergence_only"] += " (other error present)"
    problems += [msgs[name] for name, ok in checks.items() if not ok]
    if not nonconv and "no non-convergence" not in " ".join(problems):
        problems.append("no non-convergence error in the solver log")
    return {
        "criterion": LIMIT_LOAD_CRITERION,
        "n_converged_substeps": len(accepted),
        "converged_sets": [round(r[0]) for r in accepted],
        "rejected_sets": rejected_sets,
        "final_load_factor_requested": spec.load_factor,
        "last_load_increment_mpa": last_inc,
        "min_load_increment_allowed_mpa": min_step,
        "p_limit_mpa": p_lim,
        "p_tes_mpa": tes,
        "tes_gap": None if tes is None else (p_lim - tes) / p_lim,
        "tangent_over_elastic_stiffness_last": ratio,
        "design_pressure_mpa": design,
        "lr_design": design / p_lim,
        "lr_design_tes": None if tes is None else design / tes,
        "reached_nonconvergence": reached,
        "collapse": {"accepted": not problems, "checks": checks,
                     "thresholds": _thresholds(), "problems": problems},
    }


def _thresholds() -> dict:
    return {
        "tes_gap_max": COLLAPSE_TES_GAP_MAX,
        "tangent_ratio_max": COLLAPSE_TANGENT_RATIO_MAX,
        "min_converged_substeps": COLLAPSE_MIN_CONVERGED_SUBSTEPS,
        "bisection_exhausted_factor": COLLAPSE_BISECTION_FACTOR,
    }


def reaction_text_at(reacset_text: str, *, set_no: int, mapdl_rev: str, level: int) -> str:
    """Reaction-file text (the elastic format) at result set ``set_no``,
    derived from the per-set reaction record, for guard (a)."""
    row = next((r for r in _rows(reacset_text) if round(r[0]) == set_no), None)
    if row is None:
        raise ValueError(f"reaction record has no set {set_no}")
    return (
        "# digitalmodel weldolet_limit reactions at the last converged set (derived)\n"
        "# units: length=mm force=N stress=MPa\n"
        f"mesh_level {level}\n"
        f"mapdl_rev {mapdl_rev}\n"
        f"stress_mpa {row[2]!r}\n"
        f"loaded_area_mm2 {row[4]!r}\n"
        f"reaction_sum {row[3]!r}\n"
        f"converged_set {set_no}\n"
    )


def _mapdl_rev(reacset_text: str) -> str:
    for line in reacset_text.splitlines():
        parts = line.split()
        if len(parts) == 3 and parts[1] == "mapdl_rev":
            return f"{float(parts[2]):.2f}".rstrip("0").rstrip(".")
    return ""


def receipt_reac_text(spec: LimitLoadSpec, texts: dict[str, str], level: int) -> str:
    """Reaction text at the last solver-confirmed converged set (fail-closed)."""
    res = limit_load_result(spec, texts["lpl"], texts["conv"], texts["logev"])
    if not res.get("converged_sets"):
        raise ValueError("no converged result set: no reaction state")
    return reaction_text_at(texts["reacset"], set_no=res["converged_sets"][-1],
                            mapdl_rev=_mapdl_rev(texts["reacset"]), level=level)


def reac_file_name(level: int) -> str:
    return f"weldolet_reac_L{level}"


def collapse_check(entry: dict) -> list[str]:
    """Receipt gate: problems unless collapse is corroborated (fail-closed)."""
    col = entry["limit_load"]["collapse"]
    return [] if col["accepted"] else list(col["problems"]) or ["collapse not accepted"]


def limit_variant(spec: LimitLoadSpec) -> wc.StateVariant:
    def outputs(level: int) -> dict[str, str]:
        return {"lpl": lpl_file_name(level), "reacset": reacset_file_name(level),
                "conv": conv_file_name(level), "logev": logev_file_name(level),
                "reac": reac_file_name(level)}

    def derive(level: int, texts: dict[str, str]) -> dict:
        return {"limit_load": limit_load_result(at_level(spec, level), texts["lpl"],
                                                texts["conv"], texts["logev"])}

    def check(level: int, texts: dict[str, str], entry: dict) -> list[str]:
        return collapse_check(entry)

    def mesh_info(level: int) -> dict:
        sl = at_level(spec, level)
        if sl.plane == "crotch":
            from digitalmodel.ansys import weldolet_crotch as cr

            cs = sl.crotch_spec()
            mesh = cr.build_mesh(cs)
            par = cr.mesh_parameters(cs)
        else:
            mesh = wc.build_mesh(sl.base)
            par = wc.mesh_parameters(sl.base)
        return {"n_nodes": len(mesh.nodes), "n_elements": len(mesh.elements),
                "mesh_parameters": par}

    def top(primary: dict) -> dict:
        if spec.plane == "crotch":
            from digitalmodel.ansys import weldolet_crotch as cr

            geom = cr.front_geometry(spec.crotch_spec())
            crack = cr.crack_summary(spec.crotch_spec())
        else:
            geom, crack = {"type": "polar_z"}, wc.crack_summary(spec.base)
        return {"limit_load": primary["limit_load"], "front_geometry": geom,
                "crack": crack, "plane": spec.plane}

    variant = wc.StateVariant(
        state=state_name(spec),
        kind="limit_load",
        spec_dict=spec_dict(spec),
        generator_files=GENERATOR_FILES,
        deck=lambda level: generate_limit_apdl(at_level(spec, level)),
        outputs=outputs,
        derive=derive,
        mesh_info=mesh_info,
        top=top,
        accept_log=only_nonconvergence_errors,
        cracked=False,
        meshing={
            "approach": wc.MESHING_APPROACH,
            "element": "SOLID186 (20-node, full integration)",
            "modelling_route": "global structured crack-block model (no submodel)",
            "material": "elastic-perfectly-plastic, TB,BISO with zero tangent modulus",
        },
    )
    variant.check = check  # used by run_limit_state (the receipt gate)
    return variant


def run_limit_state(spec: LimitLoadSpec, workdir: Path | str, fe_states: Path | str, *,
                    levels: tuple[int, ...] = (0,), cores: int = 4,
                    timeout_seconds: int = 36000, repo_dir: Path | None = None) -> dict:
    """Solve the limit-load deck (fail-closed runner), extract the solver's
    convergence record and log evidence, and write the receipt only when
    collapse is corroborated; otherwise raise without writing anything."""
    import json
    import platform

    from digitalmodel.ansys import cint_parser
    from digitalmodel.ansys.crack_receipt import generator_blobs, generator_tree_clean, git
    from digitalmodel.ansys.crack_verification import save_artifact
    from digitalmodel.ansys.runner import ANSYSRunConfig, ANSYSRunner, ANSYSRunStatus

    variant = limit_variant(spec)
    workdir, fe_states = Path(workdir), Path(fe_states)
    repo = repo_dir or Path(__file__).resolve().parents[3]
    state = variant.state
    extra = ["-smp", "-np", str(cores)]  # distributed MPI hangs after a FATAL (#2196)
    commit = git(repo, "rev-parse", "HEAD")
    clean = generator_tree_clean(repo, list(variant.generator_files))
    staged: list[tuple[str, str]] = []  # (text, relative path) written only on success
    meshes, reac_texts = [], {}
    for lv in levels:
        run_dir = workdir / state / f"L{lv}"
        run_dir.mkdir(parents=True, exist_ok=True)
        deck = run_dir / f"{state}_L{lv}.inp"
        text = variant.deck(lv)
        deck.write_bytes(text.encode("utf-8"))
        runner = ANSYSRunner(ANSYSRunConfig(output_dir=run_dir,
                                            timeout_seconds=timeout_seconds,
                                            extra_args=extra))
        exe = runner._detect_executable()
        result = runner.run(deck)
        log_text = result.log_file.read_text(errors="replace") if result.log_file else ""
        if result.status != ANSYSRunStatus.COMPLETED and not only_nonconvergence_errors(
            log_text
        ):
            raise RuntimeError(f"{state} L{lv}: MAPDL {result.status.value}: "
                               f"{result.error_message}")
        mntr = run_dir / "file.mntr"
        if not mntr.is_file():
            raise RuntimeError(f"{state} L{lv}: no monitor file: cannot prove which "
                               "substeps converged")
        names = variant.outputs(lv)
        texts = {k: (run_dir / f"{names[k]}.txt").read_bytes().decode("utf-8")
                 for k in ("lpl", "reacset")}
        texts["conv"] = extract_convergence_record(mntr.read_text(errors="replace"))
        texts["logev"] = extract_log_evidence(log_text)
        derived = variant.derive(lv, texts)
        problems = collapse_check(derived)
        if problems:
            raise RuntimeError(f"{state} L{lv}: receipt refused: " + "; ".join(problems))
        texts["reac"] = receipt_reac_text(at_level(spec, lv), texts, lv)
        reac_texts[lv] = texts["reac"]
        artifacts = {}
        for k in names:
            rel = f"solved/{state}/{names[k]}.txt"
            staged.append((texts[k], rel))
            artifacts[k] = {"path": rel,
                            "sha256": __import__("hashlib").sha256(
                                texts[k].replace("\r\n", "\n").encode("utf-8")).hexdigest()}
        record = cint_parser.build_mesh_record(level=lv, cint_text=None,
                                               reac_text=texts["reac"])
        rev = cint_parser.parse_reaction_file(texts["reac"]).mapdl_rev or ""
        meshes.append({
            "level": lv,
            "deck_sha256": wc.deck_sha256(text),
            "run": {"argv": [exe.name if exe else "mapdl", "-b", "-i", deck.name, "-o",
                             f"{deck.stem}.out", *extra],
                    "mapdl_version": rev,
                    "solve_seconds": round(result.duration_seconds, 1)},
            "artifacts": artifacts,
            **variant.mesh_info(lv),
            **{k: v for k, v in record.items() if k != "level"},
            **derived,
        })
    guards = cint_parser.evaluate_guards({}, reac_texts, cracked=False)
    if len(levels) == 1:
        guards["b_mesh_load"] = cint_parser.single_mesh_not_applicable()
    failed = [n for n in cint_parser.GUARD_NAMES
              if guards[n].status not in ("pass", "not_applicable")]
    if failed:
        raise RuntimeError(f"{state}: receipt refused: guards {failed} failed")
    primary = next(m for m in meshes if m["level"] == max(levels))
    rev = primary["run"]["mapdl_version"]
    receipt = {
        "schema": cint_parser.RECEIPT_SCHEMA_ID,
        "state": state,
        "kind": "limit_load",
        "issue": 2157,
        "spec": variant.spec_dict,
        "design_basis": "examples/workflows/crack-fe-weldolet/design-data-register.json",
        "units": {"length": "mm", "force": "N", "stress": "MPa",
                  "k": cint_parser.K_UNIT, "k_raw": cint_parser.RAW_K_UNIT,
                  "k_conversion_factor": cint_parser.K_RAW_TO_SI, "j": cint_parser.J_UNIT},
        "meshing": variant.meshing,
        "run": {"producing_commit": commit, "generator_tree_clean": clean,
                "generator_files": generator_blobs(repo, commit, list(GENERATOR_FILES)),
                "mapdl_version": rev, "mapdl_release": wc._release_name(rev),
                "cores": cores, "platform": platform.system().lower(),
                "solver_wrapper": "digitalmodel.ansys fail-closed MAPDL subprocess (#940)"},
        "meshes": meshes,
        "primary_level": max(levels),
        "guards": {name: g.to_dict() for name, g in guards.items()},
        **variant.top(primary),
    }
    for text_, rel in staged:
        save_artifact(text_, fe_states, rel)
    (fe_states / f"{state}.receipt.json").write_bytes(
        (json.dumps(receipt, indent=1) + "\n").encode("utf-8"))
    return receipt
