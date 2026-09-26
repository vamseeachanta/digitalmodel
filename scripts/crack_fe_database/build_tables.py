"""Build data/crack_fe_database/ from the committed FE receipts and the coordinator (#2157).

Owner cards D03 (repository ecosystem only, no Hugging Face) and D04 (extend the
structural-ffs domain). Mechanism only: every number is read from a committed receipt
(``examples/workflows/crack-fe-weldolet/fe_states/*.receipt.json``) or computed by
``crack_fe_assessment.run`` from ``examples/workflows/crack-fe-weldolet/input.yml``; no
value is typed here. The output is deterministic (fixed row order, ``repr`` floats, LF
line endings, sorted manifest keys), so a rebuild is byte-identical and the manifest
hashes can be checked in CI.

Tables:

- ``crack_fe_states.csv``: one row per declared FE state and plane (a, ligament, K_gov,
  mode mix, J, sigma_m, sigma_b, receipt SHA-256, solver version);
- ``crack_results.csv``: one row per case and depth (FAD point, margin, lives,
  disposition, evidence status).

``n/a`` marks a field that does not apply to the row (for example K on an uncracked
state); it is not a zero and not a missing value.

Usage: python scripts/crack_fe_database/build_tables.py
"""

from __future__ import annotations

import csv
import hashlib
import io
import json
from pathlib import Path

import yaml

from digitalmodel.ansys.crack_receipt import text_sha256
from digitalmodel.asset_integrity.assessment import crack_fe_assessment as cfa

NA = "n/a"
STATE_COLUMNS = [
    "state", "kind", "plane", "a_mm", "ligament_mm", "k_gov_mpa_sqrt_m",
    "k1_mpa_sqrt_m", "k2_mpa_sqrt_m", "k3_mpa_sqrt_m", "j_n_per_mm",
    "sigma_ref_path", "sigma_ref_component", "sigma_m_mpa", "sigma_b_mpa",
    "p_limit_mpa", "receipt_sha256", "mapdl_version", "mapdl_release",
    "producing_commit",
]
RESULT_COLUMNS = [
    "case", "a_mm", "depth_status", "governing_plane", "governing_state",
    "governing_receipt_sha256", "k_gov_mpa_sqrt_m", "lr", "kr", "envelope_margin",
    "margin_mode", "fad_inside", "remaining_life_to_last_fe_cycles",
    "extrapolated_life_to_limit_cycles", "disposition", "verdict", "evidence_status",
]


def _fmt(value) -> str:
    if value is None:
        return NA
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float):
        return repr(value)
    return str(value)


def _c(value):
    """A computed value rounded to 10 significant digits.

    Values read from a receipt are written exactly (``repr``). Values computed through
    libm functions (exp, pow) can differ in the last bit between platforms, so they are
    rounded; 10 digits is far below any engineering resolution of these results.
    """
    return None if value is None else float(f"{value:.10g}")


def _csv(columns: list[str], rows: list[dict]) -> bytes:
    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=columns, lineterminator="\n")
    writer.writeheader()
    for r in rows:
        writer.writerow({k: _fmt(r.get(k)) for k in columns})
    return buf.getvalue().encode("utf-8")


def _receipt_sha(fe_states: Path, state: str) -> str:
    return text_sha256((fe_states / f"{state}.receipt.json").read_text("utf-8"))


def state_rows(case: dict) -> list[dict]:
    repo = Path(case["_repo_root"])
    fe_states = repo / case["fe_states_dir"]
    declared = json.loads((fe_states / "declared_states.json").read_text("utf-8"))
    uncracked = json.loads(
        (fe_states / f"{case['states']['uncracked']}.receipt.json").read_text("utf-8"))
    limits = case["plane_limits"]
    sigma = {
        plane: cfa.plane_sigma_ref(uncracked, pl["sigma_ref_paths"],
                                    pl["sigma_ref_component"])
        for plane, pl in limits.items()
    }
    rows = []
    for entry in declared["states"]:
        state = entry["state"]
        r = json.loads((fe_states / f"{state}.receipt.json").read_text("utf-8"))
        base = {
            "state": state, "kind": r["kind"], "receipt_sha256": _receipt_sha(fe_states, state),
            "mapdl_version": r["run"]["mapdl_version"],
            "mapdl_release": r["run"].get("mapdl_release"),
            "producing_commit": r["run"]["producing_commit"],
        }
        if r["kind"] == "weldolet_uncracked":
            planes = list(limits)
        elif r["kind"] == "verification":
            planes = ["verification_plate"]
        else:
            planes = [r["plane"]]
        for plane in planes:
            row = dict(base, plane=plane)
            if plane in sigma:
                s = sigma[plane]
                row.update(sigma_ref_path=s["path"], sigma_ref_component=s["component"],
                           sigma_m_mpa=s["sigma_m_mpa"], sigma_b_mpa=s["sigma_b_mpa"])
            crack = r.get("crack") or {}
            if crack:
                row.update(a_mm=crack.get("depth_mm"),
                           ligament_mm=crack.get("remaining_ligament_mm"))
            elif r["kind"] == "verification":
                row.update(a_mm=r["spec"]["crack_depth_mm"])
            if r["kind"] == "weldolet_crack":
                gov = r["governing"]
                mm = gov["mode_mix_at_governing_mpa_sqrt_m"]
                row.update(k_gov_mpa_sqrt_m=gov["k_gov_max_mpa_sqrt_m"],
                           k1_mpa_sqrt_m=mm["K1"], k2_mpa_sqrt_m=mm["K2"],
                           k3_mpa_sqrt_m=mm["K3"], j_n_per_mm=gov["j_at_governing_n_per_mm"])
            if r["kind"] == "limit_load":
                row.update(p_limit_mpa=r["limit_load"]["p_limit_mpa"])
            rows.append(row)
    return rows


def result_rows(case: dict, result: dict) -> list[dict]:
    fe_states = Path(case["_repo_root"]) / case["fe_states_dir"]
    common = {"verdict": result["verdict"], "evidence_status": result["evidence_status"]}
    rows = []
    for d in result["depths"]:
        m = d["envelope_margin"]
        rows.append({
            **common, "case": "base", "a_mm": d["a_mm"], "depth_status": d["status"],
            "governing_plane": d["governing_plane"], "governing_state": d["governing_state"],
            "governing_receipt_sha256": _receipt_sha(fe_states, d["governing_state"]),
            "k_gov_mpa_sqrt_m": d["k_gov_mpa_sqrt_m"], "lr": _c(d["lr"]), "kr": _c(d["kr"]),
            "envelope_margin": _c(m["factor"]), "margin_mode": m["mode"],
            "fad_inside": d["fad_inside"],
            "remaining_life_to_last_fe_cycles": _c(d["remaining_life_to_last_fe_cycles"]),
            "disposition": d["disposition"],
        })
    sens = result["sensitivities"]
    # Extrapolated life appears only on labelled SENSITIVITY rows (Codex P3 review).
    ext = sens["life_to_ligament_exhaustion"]
    by_a = {round(d["a_mm"], 6): d for d in result["depths"]}
    for item in ext.get("remaining_by_depth", []):
        d = by_a[round(item["a_mm"], 6)]
        rows.append({
            **common, "case": "sensitivity_life_to_ligament_exhaustion",
            "a_mm": item["a_mm"], "depth_status": "SENSITIVITY",
            "governing_plane": d["governing_plane"], "governing_state": d["governing_state"],
            "governing_receipt_sha256": _receipt_sha(fe_states, d["governing_state"]),
            "k_gov_mpa_sqrt_m": d["k_gov_mpa_sqrt_m"],
            "extrapolated_life_to_limit_cycles": _c(item["cycles"]),
            "disposition": f"SENSITIVITY: {ext['basis']}",
        })
    ll = sens["limit_load_lr"]
    base0 = rows[0]
    rows.append({
        **common, "case": "sensitivity_limit_load_lr", "a_mm": ll["a_mm"],
        "depth_status": "SENSITIVITY", "governing_plane": ll["plane"],
        "governing_state": ll["state"],
        "governing_receipt_sha256": _receipt_sha(fe_states, ll["state"]),
        "k_gov_mpa_sqrt_m": base0["k_gov_mpa_sqrt_m"], "lr": _c(ll["lr"]), "kr": _c(ll["kr"]),
        "envelope_margin": _c(ll["envelope_margin"]["factor"]),
        "margin_mode": ll["envelope_margin"]["mode"],
        "fad_inside": ll["envelope_margin"]["factor"] > 1.0,
        "disposition": f"SENSITIVITY: {ll['basis']}",
    })
    for label, s in sorted(sens.items()):
        if label in ("limit_load_lr", "life_to_ligament_exhaustion") or "state" not in s:
            continue
        rows.append({
            **common, "case": f"sensitivity_{label}", "a_mm": s["a_mm"],
            "depth_status": "SENSITIVITY", "governing_plane": s["plane"],
            "governing_state": s["state"],
            "governing_receipt_sha256": _receipt_sha(fe_states, s["state"]),
            "k_gov_mpa_sqrt_m": s["k_gov_mpa_sqrt_m"], "lr": _c(s["lr"]), "kr": _c(s["kr"]),
            "envelope_margin": _c(s["envelope_margin"]["factor"]),
            "margin_mode": s["envelope_margin"]["mode"], "fad_inside": s["fad_inside"],
            "disposition": f"SENSITIVITY: {label}",
        })
    return rows


def build(input_path: Path, out_dir: Path) -> None:
    input_path = Path(input_path)
    case = cfa.load_case(input_path)
    result = cfa.run(case).to_dict()
    repo = Path(case["_repo_root"])
    fe_states = repo / case["fe_states_dir"]
    declared = json.loads((fe_states / "declared_states.json").read_text("utf-8"))

    tables = {
        "crack_fe_states": _csv(STATE_COLUMNS, state_rows(case)),
        "crack_results": _csv(RESULT_COLUMNS, result_rows(case, result)),
    }
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    for name, blob in tables.items():
        (out_dir / f"{name}.csv").write_bytes(blob)

    manifest = {
        "generated_by": "scripts/crack_fe_database/build_tables.py",
        "issue": 2157,
        "route": "public",
        "domain": "structural-ffs",
        "distribution": ("repository ecosystem only (digitalmodel data/); not published to "
                         "Hugging Face (owner card D03)"),
        "rights_decision": (
            "Our own computed results: FE crack-state values read from the committed "
            "receipts of our own MAPDL model on an assumed design basis, and assessment "
            "results computed by crack_fe_assessment from them. No standard-derived table, "
            "clause text or published-case value (owner cards D03, D04, G05, S04)."),
        "evidence_status": result["evidence_status"],
        "missing_evidence": result["missing_evidence"],
        "inputs": {
            "case": input_path.resolve().relative_to(repo).as_posix(),
            "case_sha256": text_sha256(input_path.read_text("utf-8")),
            "design_data_register": case["design_data_register"],
            "design_data_register_sha256": text_sha256(
                (repo / case["design_data_register"]).read_text("utf-8")),
            "receipts": {s["state"]: _receipt_sha(fe_states, s["state"])
                         for s in declared["states"]},
        },
        "tables": {
            name: {"file": f"{name}.csv", "rows": blob.count(b"\n") - 1,
                   "sha256": hashlib.sha256(blob).hexdigest()}
            for name, blob in tables.items()
        },
    }
    text = yaml.safe_dump(manifest, sort_keys=True, allow_unicode=True, width=100)
    (out_dir / "manifest.yaml").write_bytes(text.replace("\r\n", "\n").encode("utf-8"))


if __name__ == "__main__":
    here = Path(__file__).resolve()
    repo_root = here.parents[2]
    build(repo_root / "examples" / "workflows" / "crack-fe-weldolet" / "input.yml",
          repo_root / "data" / "crack_fe_database")
