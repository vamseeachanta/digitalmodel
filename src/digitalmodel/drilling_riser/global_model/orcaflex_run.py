"""OrcaFlex solver steps for the riser global model (statics, end tensions, modes,
as-analysed section data).

Pin the solver first: ``digitalmodel.solvers.orcaflex.orcaflex_api.configure("11.6")``
before anything imports OrcFxAPI; this module only calls ``orcaflex_api.api()``.
Results are returned in SI (N, m, kg).
"""

from __future__ import annotations

import hashlib
import math
from pathlib import Path
from typing import Any

from digitalmodel.solvers.orcaflex import orcaflex_api

KN = 1000.0
DEGENERATE_REL = 1.0e-4


def _api():
    return orcaflex_api.api()


def _value(x: Any) -> float | None:
    """OrcFxAPI returns ~1.5e307 for a defaulted ('~') real."""
    if x is None or isinstance(x, str):
        return None
    x = float(x)
    return None if abs(x) > 1e300 else x


def load_model(master_path: Path):
    return _api().Model(str(master_path))


def load_and_solve_statics(master_path: Path):
    model = load_model(master_path)
    model.CalculateStatics()
    return model


# ---------------------------------------------------------------- dynamics (G4 sensitivity)
def set_time_step(model, dt_s: float) -> None:
    """Implicit constant time step (s); the log interval is raised to the step if smaller."""
    g = model.general
    g.ImplicitUseVariableTimeStep = "No"
    g.ImplicitConstantTimeStep = dt_s


def set_log_interval(model, dt_s: float) -> None:
    model.general.TargetLogSampleInterval = dt_s


def apply_rayleigh_damping(model, *, ratio_percent: float, period_s: float, name: str = "Structural") -> None:
    """Stiffness-proportional Rayleigh damping, ``ratio_percent`` of critical at ``period_s``, on
    every line type (applied to the geometric stiffness too): classical coefficients, mass 0 and
    stiffness beta = 2 zeta / omega, so zeta(T) = ratio x period_s / T (less at longer periods)."""
    ofx = _api()
    r = model.CreateObject(ofx.ObjectType.RayleighDampingCoefficients, name)
    r.Mode = "Coefficients (classical)"
    r.MassCoefficient = 0.0
    r.StiffnessCoefficient = 2.0 * ratio_percent / 100.0 / (2.0 * math.pi / period_s)
    r.ApplyToGeometricStiffness = "Yes"
    for o in model.objects:
        if o.typeName == "Line type":
            o.RayleighDampingCoefficients = name


def run_dynamics(model) -> None:
    model.RunSimulation()


def _stressed_arc_range(model, spec) -> tuple[float, float]:
    """Arc length on line Riser covering the sections that carry stress diameters."""
    arc, lo, hi = 0.0, None, None
    for s in spec.riser:
        if s.stress_od_m is not None:
            lo = arc if lo is None else lo
            hi = arc + s.length_m
        arc += s.length_m
    if lo is None:
        raise ValueError("no riser section carries a stress diameter")
    return lo, hi


def governing_responses(model, spec, period=None) -> dict[str, float]:
    """G4 governing responses over the last stage (default): maximum flex-joint angles (line-end
    Ez-angle, i.e. rotation relative to the connection), maximum von Mises stress in the stressed
    riser sections, and the extreme effective tensions below the ring and at the lower flex joint."""
    ofx = _api()
    if period is None:
        period = ofx.Period(1)  # stage 1 = the main stage after the build-up
    ib, riser = model["InnerBarrel"], model["Riser"]
    ufj = ib.TimeHistory("Ez-Angle", period, ofx.oeEndA)
    lfj = riser.TimeHistory("Ez-Angle", period, ofx.oeEndB)
    te_a = riser.TimeHistory("Effective tension", period, ofx.oeEndA)
    te_b = riser.TimeHistory("Effective tension", period, ofx.oeEndB)
    lo, hi = _stressed_arc_range(model, spec)
    vm = riser.RangeGraph("Max von Mises stress", period, arclengthRange=ofx.arSpecifiedArclengths(lo, hi))
    return {
        "ufj_angle_max_deg": float(max(abs(x) for x in ufj)),
        "lfj_angle_max_deg": float(max(abs(x) for x in lfj)),
        "riser_von_mises_max_pa": float(max(vm.Max)) * KN,
        "te_top_max_n": float(max(te_a)) * KN, "te_top_min_n": float(min(te_a)) * KN,
        "te_bottom_max_n": float(max(te_b)) * KN, "te_bottom_min_n": float(min(te_b)) * KN,
    }


def static_responses(model, spec) -> dict[str, float]:
    """Static-state governing responses (after ``CalculateStatics``): flex-joint angles (line-end
    Ez-angle, rotation relative to the connection), maximum von Mises stress over the stressed riser
    sections, and the effective tensions below the ring and at the lower flex joint."""
    ofx = _api()
    ib, riser = model["InnerBarrel"], model["Riser"]
    lo, hi = _stressed_arc_range(model, spec)
    vm = riser.RangeGraph("Max von Mises stress", ofx.Period(ofx.pnStaticState),
                          arclengthRange=ofx.arSpecifiedArclengths(lo, hi))
    return {
        "ufj_angle_deg": abs(float(ib.StaticResult("Ez-Angle", ofx.oeEndA))),
        "lfj_angle_deg": abs(float(riser.StaticResult("Ez-Angle", ofx.oeEndB))),
        "riser_von_mises_max_pa": float(max(vm.Mean)) * KN,
        "te_top_n": float(riser.StaticResult("Effective tension", ofx.oeEndA)) * KN,
        "te_bottom_n": float(riser.StaticResult("Effective tension", ofx.oeEndB)) * KN,
    }


def model_files_sha256(master_path: Path) -> dict[str, Any]:
    """SHA-256 of master.yml and every include, plus one digest over all of them."""
    master_path = Path(master_path)
    root = master_path.parent
    files = [master_path, *sorted((root / "includes").glob("*.yml"))]
    per = {p.relative_to(root).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest() for p in files}
    combined = hashlib.sha256("".join(f"{k}:{v}\n" for k, v in per.items()).encode()).hexdigest()
    return {"files": per, "combined": combined}


def end_effective_tensions(model) -> dict[str, float]:
    ofx = _api()
    riser, stack = model["Riser"], model["Stack"]
    return {
        "riser_top_n": riser.StaticResult("Effective tension", ofx.oeEndA) * KN,
        "riser_bottom_n": riser.StaticResult("Effective tension", ofx.oeEndB) * KN,
        "stack_bottom_n": stack.StaticResult("Effective tension", ofx.oeEndA) * KN,
        "riser_top_wall_n": riser.StaticResult("Wall tension", ofx.oeEndA) * KN,
        "riser_bottom_wall_n": riser.StaticResult("Wall tension", ofx.oeEndB) * KN,
    }


def ring_static_z_m(model) -> float:
    return model["TensionRing"].StaticResult("Z")


def tensioner_vertical_sum_n(model) -> float:
    """Sum of the vertical components of the tensioner-line tensions at the ring."""
    total = 0.0
    ring = model["TensionRing"]
    rx, ry, rz = (ring.StaticResult(c) for c in ("X", "Y", "Z"))
    for obj in model.objects:
        if obj.typeName != "Winch":
            continue
        tension = obj.StaticResult("Tension") * KN
        xs, ys, zs = (obj.GetData(f"Connection{c}", 0) for c in "XYZ")
        xa, ya, za = (obj.GetData(f"Connection{c}", 1) for c in "XYZ")
        dx, dy, dz = xs - (rx + xa), ys - (ry + ya), zs - (rz + za)
        total += tension * dz / math.sqrt(dx * dx + dy * dy + dz * dz)
    return total


def ring_vertical_residual_n(model, ring_weight_n: float) -> float:
    """Vertical force balance of the tension ring (N); ~0 on a physical static solution.

    tensioner verticals + inner-barrel end tension (the inner barrel rises from the ring, so
    tension pulls the ring up) - ring weight - riser top effective tension.
    """
    ofx = _api()
    te_riser = model["Riser"].StaticResult("Effective tension", ofx.oeEndA) * KN
    te_ib = model["InnerBarrel"].StaticResult("Effective tension", ofx.oeEndB) * KN
    return tensioner_vertical_sum_n(model) + te_ib - ring_weight_n - te_riser


def _dof_shares(details: dict, modes, line_name: str) -> tuple[float, float, float]:
    shape = details.shapeWrtGlobal
    sx = sy = sz = 0.0
    for i in range(modes.dofCount):
        if modes.owner[i].name != line_name:
            continue
        v = shape[i] ** 2
        d = modes.dof[i]
        if d == 1:  # OrcFxAPI numbers the DOFs 1..6 (X, Y, Z, Rx, Ry, Rz)
            sx += v
        elif d == 2:
            sy += v
        elif d == 3:
            sz += v
    tot = sx + sy + sz
    return (sx / tot, sy / tot, sz / tot) if tot > 0 else (0.0, 0.0, 0.0)


def riser_modal_periods(model, n_modes: int = 5, *, line_name: str = "Riser",
                        max_modes: int = 60) -> list[dict[str, Any]]:
    """First ``n_modes`` transverse x-z modes of ``line_name`` from a whole-system modal analysis.

    Modes are classified by the share of the squared modal displacement of the riser nodes
    in global x, y and z. A degenerate pair (periods within 1e-4, together transverse) counts
    as one transverse mode, since each plane then has one mode at that period. Axial modes
    and y-only modes are skipped.
    """
    ofx = _api()
    spec = ofx.ModalAnalysisSpecification(calculateShapes=True, firstMode=1, lastMode=max_modes)
    modes = ofx.Modes(model, spec)
    rows = []
    for i in range(modes.modeCount):
        d = modes.modeDetails(i)
        sx, sy, sz = _dof_shares(d, modes, line_name)
        rows.append({"mode_number": int(d.modeNumber), "period_s": float(d.period),
                     "share_x": sx, "share_y": sy, "share_z": sz})
    out, i = [], 0
    while i < len(rows) and len(out) < n_modes:
        r = rows[i]
        pair = (i + 1 < len(rows)
                and abs(rows[i + 1]["period_s"] - r["period_s"]) <= DEGENERATE_REL * r["period_s"])
        if pair:
            q = rows[i + 1]
            if (r["share_x"] + r["share_y"]) > 0.5 and (q["share_x"] + q["share_y"]) > 0.5:
                xm, other = (r, q) if r["share_x"] >= q["share_x"] else (q, r)
                out.append({**xm, "classification": "transverse (degenerate x/y pair)",
                            "paired_mode_number": other["mode_number"]})
            i += 2
            continue
        if r["share_x"] > 0.5:
            out.append({**r, "classification": "transverse x"})
        i += 1
    return out


def as_analysed_sections(model, line_names: tuple[str, ...] = ("InnerBarrel", "Riser", "Stack"),
                         rho_water_kg_m3: float = 1025.0) -> list[dict[str, Any]]:
    """Appendix C.1 / C.5 rows read back from the solved model (static elevations)."""
    ofx = _api()
    rows = []
    for name in line_names:
        line = model[name]
        rho_c = float(line.GetData("ContentsDensity", -1)) * 1000.0
        arc = 0.0
        for k in range(line.GetDataRowCount("LineType")):
            lt_name = line.GetData("LineType", k)
            length = float(line.GetData("Length", k))
            seg = float(line.GetData("TargetSegmentLength", k))
            lt = model[lt_name]
            od, idm = float(lt.GetData("OD", -1)), float(lt.GetData("ID", -1))
            mass = float(lt.GetData("MassPerUnitLength", -1)) * 1000.0
            ca = float(lt.GetData("Cax", -1))
            cd = float(lt.GetData("Cdx", -1))
            drag = _value(lt.GetData("NormalDragLiftDiameter", -1))
            drag = od if drag is None else drag
            disp = math.pi / 4 * od * od
            contents = rho_c * math.pi / 4 * idm * idm
            z_top = line.StaticResult("Z", ofx.oeArcLength(arc))
            z_bot = line.StaticResult("Z", ofx.oeArcLength(arc + length))
            rows.append({
                "line": name, "section": lt_name, "arc_top_m": arc, "length_m": length,
                "z_top_m": z_top, "z_bottom_m": z_bot, "target_segment_m": seg,
                "hydro_od_m": od, "bore_id_m": idm, "drag_diameter_m": drag,
                "stress_od_m": _value(lt.GetData("StressOD", -1)),
                "stress_id_m": _value(lt.GetData("StressID", -1)),
                "mass_dry_kg_m": mass, "contents_kg_m": contents,
                "mass_wet_kg_m": mass + contents - rho_water_kg_m3 * disp,
                "ea_n": float(lt.GetData("EA", -1)) * KN,
                "ei_nm2": float(lt.GetData("EIx", -1)) * KN,
                "gj_nm2": float(lt.GetData("GJ", -1)) * KN,
                "cd_normal": cd, "cd_axial": float(lt.GetData("Cdz", -1)),
                "ca_normal": ca, "ca_axial": float(lt.GetData("Caz", -1)),
                "added_mass_kg_m": ca * rho_water_kg_m3 * disp,
                "drag_factor_n_s2_m3": 0.5 * rho_water_kg_m3 * cd * drag,
            })
            arc += length
    return rows
