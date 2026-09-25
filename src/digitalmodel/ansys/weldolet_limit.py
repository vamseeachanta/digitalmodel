"""Elastic-perfectly-plastic limit load of the cracked weldolet model (#2157 P0b).

Owner card S02: the FE-linearised sigma_ref governs, and a limit-load
Lr = P / P_L is reported beside it as a sensitivity. This module writes the
limit-load deck on the same structured mesh as ``weldolet_crack`` (same
geometry, supports and proportional pressure loads, no CINT) through
``weldolet_crack.DeckHooks``, and derives P_L from the load-deflection record.

Collapse criterion (stated before the run)
    Elastic-perfectly-plastic von Mises material with sigma_y = Rp0.2 =
    127 MPa (register M-03) and zero hardening, small-displacement theory, all
    pressure loads scaled by one load factor. An EPP small-displacement model
    has a true limit load: at collapse the deflection grows without bound and
    the Newton solution cannot converge. P_L is the pressure at the last
    converged substep, with automatic bisection down to 1/20 of the initial
    load step, so it is resolved to that step. Two checks are recorded: the
    twice-elastic-slope (TES) pressure on the branch-end lift curve, and the
    tangent stiffness of the last converged increment relative to the elastic
    stiffness (a plateau shows as a ratio near zero). Only the run's
    non-convergence errors are accepted from the solver log.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from itertools import pairwise
from pathlib import Path

from digitalmodel.ansys import weldolet_crack as wc

GENERATOR_FILES = ("src/digitalmodel/ansys/weldolet_limit.py", *wc.GENERATOR_FILES)

LIMIT_LOAD_CRITERION = (
    "Elastic-perfectly-plastic (sigma_y = Rp0.2 = 127 MPa, von Mises, no hardening), "
    "small displacement, all pressure loads (bore, crack faces, end thrusts) scaled "
    "proportionally. P_L is the pressure at the last converged substep when the "
    "Newton solution fails to converge with automatic bisection down to 1/20 of the "
    "initial load step (the limit of an EPP small-displacement model, where the "
    "deflection becomes unbounded). Checks recorded: the twice-elastic-slope (TES) "
    "pressure on the branch-end lift curve, and the tangent stiffness of the last "
    "converged increment relative to the elastic stiffness (plateau)."
)


@dataclass(frozen=True)
class LimitLoadSpec:
    """Limit-load run on the cracked model ``base`` (crack depth required)."""

    base: wc.WeldoletSpec = wc.WeldoletSpec()
    load_factor: float = 4.0  # final load factor of the proportional ramp
    substeps: int = 16  # initial step = 1/substeps of the ramp; minimum 1/(20 x)

    def validate(self) -> list[str]:
        issues = self.base.validate()
        if self.base.crack_depth_mm is None:
            issues.append("the limit-load run is on the cracked model")
        if self.load_factor <= 1.0 or self.substeps < 2:
            issues.append("load_factor must exceed 1 and substeps be at least 2")
        return issues


def lpl_file_name(level: int) -> str:
    return f"weldolet_lpl_L{level}"


def state_name(spec: LimitLoadSpec) -> str:
    tag = wc.depth_tag(spec.base.crack_depth_mm)
    cfp = "" if spec.base.crack_face_pressure else "_cfp_off"
    return f"p0b_limit_load_{tag}{cfp}"


def _post(spec: LimitLoadSpec):
    def write(w, base: wc.WeldoletSpec, mesh: wc.WeldoletMesh, lv: int) -> None:
        geo = wc.derived_geometry(base)
        top_z = geo["run_outer_radius_mm"] + base.weldolet_a_mm + base.branch_length_mm
        w("*GET,NSETS,ACTIVE,0,SET,NSET")
        w(f"*CFOPEN,{lpl_file_name(lv)},txt")
        w("*VWRITE")
        w("('# digitalmodel weldolet_limit load-deflection record')")
        w("*VWRITE")
        w("('# units: length=mm force=N stress=MPa')")
        w("*VWRITE")
        w("('# columns: set load_factor pressure_mpa uz_branch_end_mm "
          "ur_far_field_od_mm')")
        w(f"NSEL,S,LOC,Z,{top_z - 1.0e-6!r},{top_z + 1.0e-6!r}")
        w("*GET,NTOP,NODE,0,NUM,MIN")
        w("ALLSEL,ALL")
        w("*DO,KS,1,NSETS")
        w("SET,,,,,,,KS")
        w("*GET,TK,ACTIVE,0,SET,TIME")
        w("*GET,UZK,NODE,NTOP,U,Z")
        # far-field OD node at the bottom of the run pipe: radial = -UZ
        w(f"*GET,URK,NODE,{mesh.hoop_line[-1]},U,Z")
        w("URK = -URK")
        w(f"LFK = {spec.load_factor!r}*TK")
        w(f"PK = {base.pressure_mpa!r}*LFK")
        w("*VWRITE,KS,LFK,PK,UZK,URK")
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
    return LimitLoadSpec(base=base, load_factor=spec.load_factor, substeps=spec.substeps)


def generate_limit_apdl(spec: LimitLoadSpec) -> str:
    issues = spec.validate()
    if issues:
        raise ValueError("invalid limit-load spec: " + "; ".join(issues))
    return wc.generate_weldolet_apdl(spec.base, hooks(spec))


def spec_dict(spec: LimitLoadSpec) -> dict:
    base = {k: v for k, v in asdict(spec.base).items() if k != "mesh_level"}
    return {"base": base, "load_factor": spec.load_factor, "substeps": spec.substeps}


def spec_from_receipt(receipt: dict, level: int) -> LimitLoadSpec:
    s = receipt["spec"]
    base = wc.WeldoletSpec(**{**s["base"], "mesh_level": level})
    return LimitLoadSpec(base=base, load_factor=s["load_factor"], substeps=s["substeps"])


def deck_sha256_for_receipt(receipt: dict, level: int) -> str:
    return wc.deck_sha256(generate_limit_apdl(spec_from_receipt(receipt, level)))


def only_nonconvergence_errors(out_text: str) -> bool:
    """True when every ``*** ERROR ***`` in the log reports non-convergence and
    no ``*** FATAL ***`` occurred (accepted only for the limit-load run)."""
    if "*** FATAL ***" in out_text:
        return False
    lines = out_text.splitlines()
    found = False
    for i, line in enumerate(lines):
        if "*** ERROR ***" in line:
            found = True
            block = " ".join(lines[i + 1:i + 4]).lower()
            if "converge" not in block:
                return False
    return found


def limit_load_result(spec: LimitLoadSpec, lpl_text: str) -> dict:
    """Limit load and its checks from the load-deflection record."""
    rows = wc._data_rows(lpl_text)
    p = [r[2] for r in rows]
    u = [r[3] for r in rows]
    p_nc = p[-1]
    k_el = p[0] / u[0]
    tes = None
    for (p0, u0), (p1, u1) in pairwise(zip(p, u, strict=True)):
        f0, f1 = p0 - 0.5 * k_el * u0, p1 - 0.5 * k_el * u1
        if f0 > 0.0 >= f1:
            t = f0 / (f0 - f1)
            tes = p0 + t * (p1 - p0)
            break
    k_last = None
    if len(p) > 1 and u[-1] != u[-2]:
        k_last = (p[-1] - p[-2]) / (u[-1] - u[-2])
    design = spec.base.pressure_mpa
    return {
        "criterion": LIMIT_LOAD_CRITERION,
        "n_converged_substeps": len(rows),
        "final_load_factor_requested": spec.load_factor,
        "last_load_increment_mpa": p[-1] - p[-2] if len(p) > 1 else None,
        "min_load_increment_allowed_mpa": design * spec.load_factor / (20 * spec.substeps),
        "p_limit_mpa": p_nc,
        "p_tes_mpa": tes,
        "tangent_over_elastic_stiffness_last": None if k_last is None else k_last / k_el,
        "design_pressure_mpa": design,
        "lr_design": design / p_nc,
        "lr_design_tes": None if tes is None else design / tes,
        "reached_nonconvergence": p_nc < design * spec.load_factor * 0.999,
    }


def limit_variant(spec: LimitLoadSpec) -> wc.StateVariant:
    def outputs(level: int) -> dict[str, str]:
        return {"reac": wc.reaction_file_name(level), "lpl": lpl_file_name(level)}

    def derive(level: int, texts: dict[str, str]) -> dict:
        return {"limit_load": limit_load_result(at_level(spec, level), texts["lpl"])}

    def mesh_info(level: int) -> dict:
        s = at_level(spec, level).base
        mesh = wc.build_mesh(s)
        return {"n_nodes": len(mesh.nodes), "n_elements": len(mesh.elements),
                "mesh_parameters": wc.mesh_parameters(s)}

    def top(primary: dict) -> dict:
        return {"limit_load": primary["limit_load"],
                "front_geometry": {"type": "polar_z"},
                "crack": wc.crack_summary(spec.base)}

    return wc.StateVariant(
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


def run_limit_state(spec: LimitLoadSpec, workdir: Path | str, fe_states: Path | str, *,
                    levels: tuple[int, ...] = (0,), cores: int = 4,
                    timeout_seconds: int = 36000) -> dict:
    return wc.run_variant(limit_variant(spec), workdir, fe_states, levels=levels,
                          cores=cores, timeout_seconds=timeout_seconds)
