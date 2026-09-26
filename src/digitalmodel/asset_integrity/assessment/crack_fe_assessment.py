# ABOUTME: Crack-like-flaw FFS coordinator on own FE crack parameters (#2157 P3): shared FFS
# ABOUTME: result Protocol, CrackAssessmentResult, evidence completeness, FAD + growth + checks.
"""Crack-like-flaw assessment from committed FE crack-state receipts (#2157 P3).

Result model (owner card G11). :class:`FFSResultProtocol` is the consumer contract that
:class:`~digitalmodel.asset_integrity.assessment.ffs_coordinator.FFSAssessmentResult`
already satisfies (``component_id``, ``assessment_type``, ``verdict``, ``passes``,
``to_dict()``, ``code_reference``). :class:`CrackAssessmentResult` is a sibling that
implements it with ``assessment_type = "CRACK"``; the metal-loss record is unchanged.

``passes`` is true only when the engineering verdict is ACCEPT or MONITOR **and** the
evidence status is COMPLETE. COMPLETE needs every item in :data:`EVIDENCE_ITEMS`:

- ``kmat_basis``: the toughness input comes from an establishing source class;
- ``lr_max_basis``: the flow rule with established sigma_y and sigma_u, or a fixed value
  with an establishing basis;
- ``residual_stress_basis``: a residual profile and a named plasticity-interaction method
  (owner cards G01, G02) from an establishing source;
- ``psf_basis``: partial safety factors from an establishing source (owner card G03);
- ``geometry_validity``: every FE state lies inside its declared modelled range and
  short of its plane's limit state;
- ``fe_receipts``: every declared FE state has a receipt that passes the schema, the
  provenance check, the committed-artifact re-derivation, the deck-hash regeneration and
  every gating guard (a)-(g), and whose design basis matches the register.

Source classes ``public``, ``derived`` and ``owner_decision`` establish an input;
``assumed`` and ``missing_evidence`` do not. A missing item is reported by name, never
filled in.

Assessment (:func:`run`):

- FAD at each declared depth. The governing plane at a depth is the solved plane with
  the larger K_gov (owner card G15). A depth is *established* only when every plane that
  still has a ligament there is solved; otherwise the depth is recorded with its reason
  and excluded from the verdict.
- ``Kr = K_gov / Kmat`` (plus the named residual method when supplied);
  ``Lr = sigma_ref / sigma_y`` with ``sigma_ref = sigma_m + sigma_b`` from the uncracked
  FE linearisation on the flaw plane, with no net-section amplification (owner card
  B02). The limit-load ``Lr = p / P_L`` is a sensitivity (owner card S02).
- API 579-1:2016 Level 2 curve with the cut-off from :func:`fad_curves.lr_max`, and the
  load factor to the envelope from :func:`fad_curves.envelope_margin`.
- Growth over the established depths with a :class:`TabulatedDeltaK` that never
  extrapolates in the base result; life to the last FE state, its margin on the demand,
  the DeltaK multiplier and the threshold margins under both temperature rules (owner
  card B14). Life to ligament exhaustion by explicit linear extrapolation is a labelled
  SENSITIVITY.
- Consistency checks (K against sigma_ref, small-scale yielding, shakedown, growth
  validity), residual screening bounds (owner card B11) and metadata-only citations.

Units: mm, MPa, MPa*sqrt(m), cycles.
"""

from __future__ import annotations

import copy
import dataclasses
import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, Mapping, Optional, Protocol, runtime_checkable

import yaml

from digitalmodel.asset_integrity.assessment import crack_checks, secondary_stress
from digitalmodel.asset_integrity.assessment.fad_curves import (
    api579_2016_level2,
    envelope_margin,
    lr_max,
)
from digitalmodel.fatigue.crack_growth_history import (
    GrowthLaw,
    TabulatedDeltaK,
    Threshold,
    convert_coefficient_n_mm_to_mpa_sqrt_m,
    dk_multiplier_to_demand,
    life,
    threshold_margin,
)

CODE_REFERENCE = "API 579-1/ASME FFS-1 (2016) Part 9, Level 2 FAD"
EVIDENCE_ITEMS = (
    "kmat_basis",
    "lr_max_basis",
    "residual_stress_basis",
    "psf_basis",
    "geometry_validity",
    "fe_receipts",
)
EVIDENCE_STATUSES = ("COMPLETE", "INCOMPLETE")
VERDICTS = ("ACCEPT", "MONITOR", "REPAIR")
ESTABLISHING_SOURCE_CLASSES = frozenset({"public", "derived", "owner_decision"})
ASSUMED_LABEL = "ASSUMED - to be confirmed"
SIGMA_REF_DEFINITION = (
    "sigma_ref = sigma_m + sigma_b, the membrane plus bending stress of the uncracked FE "
    "linearisation on the flaw plane (owner card B02), taken as the largest value over "
    "the plane's declared linearisation paths; no net-section amplification is applied, "
    "so sigma_ref is the same at every crack depth of a plane. The limit-load Lr = p / P_L "
    "is reported as a sensitivity (owner card S02)."
)
_COMPONENT_FIELDS = {
    "hoop": ("hoop_sigma_m_mpa", "hoop_sigma_b_mpa", "peak_shh_mpa"),
    "normal": ("sigma_m_mpa", "sigma_b_mpa", "peak_srr_mpa"),
}


# --------------------------------------------------------------------------- #
# Result model (owner card G11)
# --------------------------------------------------------------------------- #
@runtime_checkable
class FFSResultProtocol(Protocol):
    """Consumer contract shared by metal-loss and crack FFS results."""

    component_id: str
    assessment_type: str
    verdict: str
    code_reference: str

    @property
    def passes(self) -> bool: ...

    def to_dict(self) -> dict: ...


@dataclass(frozen=True)
class EvidenceItem:
    """One evidence-completeness item: whether it is established, and why."""

    name: str
    established: bool
    basis: str

    def to_dict(self) -> dict:
        return {"name": self.name, "established": self.established, "basis": self.basis}


def evidence_status(items: Mapping[str, EvidenceItem]) -> tuple[str, list[str]]:
    """``("COMPLETE", [])`` when every required item is established, else the gaps."""
    missing = [n for n in EVIDENCE_ITEMS if n not in items or not items[n].established]
    return ("COMPLETE" if not missing else "INCOMPLETE", missing)


def _json_safe(node: Any, where: str = "result") -> Any:
    if isinstance(node, Mapping):
        return {str(k): _json_safe(v, f"{where}.{k}") for k, v in node.items()}
    if isinstance(node, (list, tuple)):
        return [_json_safe(v, f"{where}[{i}]") for i, v in enumerate(node)]
    if isinstance(node, bool) or node is None or isinstance(node, (str, int)):
        return node
    if isinstance(node, float):
        if not math.isfinite(node):
            raise ValueError(f"non-finite value at {where}")
        return node
    raise TypeError(f"unserialisable {type(node).__name__} at {where}")


@dataclass
class CrackAssessmentResult:
    """Crack-like-flaw FFS result; a sibling of ``FFSAssessmentResult`` (owner G11)."""

    component_id: str
    verdict: str
    evidence_status: str
    evidence: dict
    missing_evidence: list
    assessment_type: str = "CRACK"
    code_reference: str = CODE_REFERENCE
    engineering: dict = field(default_factory=dict)
    depths: list = field(default_factory=list)
    growth: dict = field(default_factory=dict)
    checks: dict = field(default_factory=dict)
    screening: dict = field(default_factory=dict)
    sensitivities: dict = field(default_factory=dict)
    findings: list = field(default_factory=list)
    citations: list = field(default_factory=list)
    basis: dict = field(default_factory=dict)
    inputs: dict = field(default_factory=dict)
    receipts: dict = field(default_factory=dict)
    design_basis_status: str = ""

    def __post_init__(self) -> None:
        if self.assessment_type != "CRACK":
            raise ValueError("CrackAssessmentResult.assessment_type must be 'CRACK'.")
        if self.evidence_status not in EVIDENCE_STATUSES:
            raise ValueError(f"evidence_status must be one of {EVIDENCE_STATUSES}.")
        if self.verdict not in VERDICTS:
            raise ValueError(f"verdict must be one of {VERDICTS}.")
        if (self.evidence_status == "COMPLETE") != (not self.missing_evidence):
            raise ValueError("evidence_status and missing_evidence disagree.")

    @property
    def passes(self) -> bool:
        """ACCEPT/MONITOR **and** COMPLETE evidence; otherwise False."""
        return self.verdict in ("ACCEPT", "MONITOR") and self.evidence_status == "COMPLETE"

    def to_dict(self) -> dict:
        payload = {
            "component_id": self.component_id,
            "assessment_type": self.assessment_type,
            "verdict": self.verdict,
            "evidence_status": self.evidence_status,
            "missing_evidence": list(self.missing_evidence),
            "passes": self.passes,
            "code_reference": self.code_reference,
            "design_basis_status": self.design_basis_status,
            "engineering": self.engineering,
            "evidence": self.evidence,
            "depths": self.depths,
            "growth": self.growth,
            "checks": self.checks,
            "screening": self.screening,
            "sensitivities": self.sensitivities,
            "findings": self.findings,
            "citations": self.citations,
            "basis": self.basis,
            "inputs": self.inputs,
            "receipts": self.receipts,
        }
        return _json_safe(copy.deepcopy(payload))


# --------------------------------------------------------------------------- #
# Inputs: register ids or inline values with a basis
# --------------------------------------------------------------------------- #
@dataclass(frozen=True)
class Input:
    """A resolved input value with its provenance."""

    name: str
    value: Any
    unit: str
    source_class: str
    basis: str
    status_label: str
    register_id: Optional[str] = None

    @property
    def establishes(self) -> bool:
        return self.source_class in ESTABLISHING_SOURCE_CLASSES

    def to_dict(self) -> dict:
        return dataclasses.asdict(self)


def resolve_input(name: str, spec: Mapping, register: Mapping[str, Mapping]) -> Input:
    """Resolve ``{register_id: ...}`` or an inline ``{value, basis, ...}`` input."""
    if not isinstance(spec, Mapping):
        raise ValueError(f"input {name!r} must be a mapping with a register id or a basis.")
    rid = spec.get("register_id")
    if rid:
        if rid not in register:
            raise KeyError(f"input {name!r} cites unknown register id {rid!r}.")
        item = register[rid]
        return Input(
            name=name,
            value=item["value"],
            unit=str(item.get("unit", "")),
            source_class=str(item["source_class"]),
            basis=f"register {rid} ({item['parameter']}): {item['note']}",
            status_label=str(item.get("status_label", "")),
            register_id=rid,
        )
    basis = str(spec.get("basis", "")).strip()
    if not basis:
        raise ValueError(f"input {name!r} has neither a register id nor a basis.")
    if "value" not in spec:
        raise ValueError(f"input {name!r} has no value.")
    return Input(
        name=name,
        value=spec["value"],
        unit=str(spec.get("unit", "")),
        source_class=str(spec.get("source_class", "assumed")),
        basis=" ".join(basis.split()),
        status_label=str(spec.get("status_label", "")),
    )


def _inline(name: str, spec: Mapping, value_key: str) -> Input:
    """An inline input whose value sits under ``value_key`` (e.g. the law's ``A``)."""
    return resolve_input(name, {**spec, "value": spec[value_key]}, {})


# --------------------------------------------------------------------------- #
# Case loading
# --------------------------------------------------------------------------- #
def find_repo_root(start: Path) -> Path:
    """First ancestor of ``start`` holding ``docs/registry/workflows.yaml``."""
    start = Path(start).resolve()
    for cand in (start, *start.parents):
        if (cand / "docs" / "registry" / "workflows.yaml").is_file():
            return cand
    raise FileNotFoundError(f"no repository root above {start}")


def load_case(path: Path) -> dict:
    """The ``crack_fe_assessment`` block of an input file, with its repository root."""
    path = Path(path)
    cfg = yaml.safe_load(path.read_text(encoding="utf-8"))
    block = copy.deepcopy(cfg["crack_fe_assessment"])
    block["_repo_root"] = str(find_repo_root(path.parent))
    return block


def case_from_cfg(cfg: Mapping) -> dict:
    """The case from an engine cfg (repository root from the input file or this module)."""
    block = cfg.get("crack_fe_assessment")
    if not block:
        raise KeyError("crack_fe_ffs workflow requires a 'crack_fe_assessment' block.")
    case = copy.deepcopy(dict(block))
    start = cfg.get("_config_file_path")
    try:
        root = find_repo_root(Path(start).parent) if start else find_repo_root(Path(__file__))
    except FileNotFoundError:
        root = find_repo_root(Path(__file__))
    case["_repo_root"] = str(root)
    return case


# --------------------------------------------------------------------------- #
# Receipt validation (plan P3: every FE state validated, guards passing)
# --------------------------------------------------------------------------- #
_DECK_CACHE: dict[tuple[str, int], str] = {}


def _deck_sha(receipt: Mapping, level: int) -> str:
    """Regenerated deck hash; memoised on the receipt content for this process."""
    from digitalmodel.ansys import crack_receipt

    key = (crack_receipt.text_sha256(json.dumps(receipt, sort_keys=True)), level)
    if key not in _DECK_CACHE:
        _DECK_CACHE[key] = crack_receipt.regenerate_deck_sha256(receipt, level)
    return _DECK_CACHE[key]


def receipt_problems(
    receipt: Mapping, fe_states: Path, repo: Path, deltas: Optional[list] = None
) -> list[str]:
    """Why a receipt cannot be used as evidence (empty list = validated).

    Schema, provenance (producing commit, generator blobs, reviewed code deltas),
    committed-artifact re-derivation, limit-load collapse corroboration, deck-hash
    regeneration, and every gating guard stored and recomputed as ``pass`` (or
    ``not_applicable`` where the kind has no crack front).
    """
    from digitalmodel.ansys import cint_parser, crack_receipt

    receipt = dict(receipt)
    state = receipt.get("state", "?")
    problems = [f"{state}: schema: {p}" for p in cint_parser.validate_receipt_schema(receipt)]
    if problems:
        return problems
    problems += crack_receipt.provenance_problems(receipt, Path(repo), deltas or [])
    problems += crack_receipt.artifact_problems(receipt, Path(fe_states))
    problems += [f"{state}: {p}" for p in crack_receipt.limit_load_problems(receipt)]
    for mesh in receipt["meshes"]:
        if _deck_sha(receipt, mesh["level"]) != mesh["deck_sha256"]:
            problems.append(f"{state}: level {mesh['level']} deck hash is stale")
    allowed_na = crack_receipt.NOT_APPLICABLE_GUARDS.get(receipt["kind"], ())
    recomputed = cint_parser.evaluate_receipt_guards(receipt)
    for name in cint_parser.GUARD_NAMES:
        expected = "not_applicable" if name in allowed_na else "pass"
        stored = receipt["guards"].get(name, {}).get("status")
        if stored != expected:
            problems.append(f"{state}: guard {name} stored {stored!r}, needs {expected!r}")
        if recomputed[name].status != expected:
            problems.append(
                f"{state}: guard {name} recomputes to {recomputed[name].status!r}, "
                f"needs {expected!r}"
            )
    return problems


Validator = Callable[..., list]


def declared_receipt_problems(
    fe_states: Path, repo: Path, *, validator: Validator = receipt_problems
) -> dict[str, list[str]]:
    """Problems per declared state (a missing receipt is a problem, never a skip)."""
    from digitalmodel.ansys import crack_receipt

    fe_states = Path(fe_states)
    manifest = json.loads((fe_states / "declared_states.json").read_text("utf-8"))
    deltas = crack_receipt.load_code_deltas(fe_states / "code_deltas.json")
    out: dict[str, list[str]] = {}
    for entry in manifest["states"]:
        state = entry["state"]
        path = fe_states / f"{state}.receipt.json"
        if not path.is_file():
            out[state] = [f"declared state {state} has no receipt"]
            continue
        receipt = json.loads(path.read_text("utf-8"))
        problems = list(validator(receipt, fe_states, repo, deltas))
        if receipt.get("kind") != entry.get("kind"):
            problems.append(f"{state}: kind {receipt.get('kind')!r} differs from the manifest")
        out[state] = problems
    return out


# --------------------------------------------------------------------------- #
# Coordinator
# --------------------------------------------------------------------------- #
def _finding(fid: str, criterion: str, comparator: Any, value: Any, disposition: str) -> dict:
    return {
        "id": fid,
        "criterion": criterion,
        "comparator": comparator,
        "value": value,
        "disposition": disposition,
    }


def _margin_dict(m) -> dict:
    return {
        "factor": m.factor,
        "mode": m.mode,
        "contact_lr": m.contact_lr,
        "contact_kr": m.contact_kr,
    }


def plane_sigma_ref(uncracked: Mapping, paths: list, component: str) -> dict:
    f_m, f_b, f_peak = _COMPONENT_FIELDS[component]
    rows = [p for p in uncracked["sigma_ref"]["paths"] if p["path"] in paths]
    if not rows:
        raise ValueError(f"no uncracked linearisation path among {paths}.")
    best = max(rows, key=lambda p: p[f_m] + p[f_b])
    peaks = [p[f_peak] for p in rows if f_peak in p]
    return {
        "sigma_ref_mpa": best[f_m] + best[f_b],
        "sigma_m_mpa": best[f_m],
        "sigma_b_mpa": best[f_b],
        "path": best["path"],
        "component": component,
        "peak_mpa": max(peaks) if peaks else None,
        "peak_field": f_peak,
    }


def _state_record(state: str, receipt: Mapping) -> dict:
    gov = receipt["governing"]
    return {
        "state": state,
        "a_mm": float(receipt["crack"]["depth_mm"]),
        "k_gov_mpa_sqrt_m": float(gov["k_gov_max_mpa_sqrt_m"]),
        "mode_mix_mpa_sqrt_m": dict(gov["mode_mix_at_governing_mpa_sqrt_m"]),
        "j_n_per_mm": float(gov["j_at_governing_n_per_mm"]),
        "ligament_mm": float(receipt["crack"]["remaining_ligament_mm"]),
        "governing_phi_deg": gov.get("governing_phi_deg"),
    }


def _spec_value(receipt: Mapping, key: str):
    spec = receipt.get("spec", {})
    if key in spec and spec[key] is not None:
        return spec[key]
    return spec.get("base", {}).get(key)


def run(case: Mapping) -> CrackAssessmentResult:
    """Assess the case and return a :class:`CrackAssessmentResult`.

    The production path always validates every FE receipt with :func:`receipt_problems`
    (schema, provenance, deck hash, gating guards). No caller can substitute a validator
    here, so ``passes`` cannot be reached with unvalidated receipts (Codex P3 review).
    """
    return _assess(case, receipt_problems)


def _assess(case: Mapping, validator: Validator) -> CrackAssessmentResult:
    """Internal: the assessment with an explicit receipt validator.

    Tests use this to keep numeric checks fast. Workflow and database routes call
    :func:`run` only.
    """
    from digitalmodel.ansys.crack_receipt import text_sha256
    from digitalmodel.citations.registry import get_api579_reference, get_bs7910_reference

    repo = Path(case.get("_repo_root") or find_repo_root(Path(__file__)))
    reg_path = repo / case["design_data_register"]
    register_doc = json.loads(reg_path.read_text("utf-8"))
    register = {item["id"]: item for item in register_doc["design_data"]}
    fe_states = repo / case["fe_states_dir"]

    def load(state: str) -> dict:
        return json.loads((fe_states / f"{state}.receipt.json").read_text("utf-8"))

    inputs: dict[str, Input] = {}

    def use(name: str, spec: Mapping) -> Input:
        inputs[name] = resolve_input(name, spec, register)
        return inputs[name]

    mat = case["material"]
    sy = use("sigma_y", mat["sigma_y"])
    su = use("sigma_u", mat["sigma_u"])
    e_mod = use("youngs_modulus", mat["youngs_modulus"])
    nu = use("poisson", mat["poisson"])
    kmat_in = use("kmat", mat["kmat"])
    sigma_y, sigma_u, kmat = float(sy.value), float(su.value), float(kmat_in.value)
    e_t_gpa = float(e_mod.value) / 1000.0

    # ---- receipts -------------------------------------------------------------
    states_cfg = case["states"]
    planes_cfg: dict = states_cfg["planes"]
    uncracked = load(states_cfg["uncracked"])
    limit_rec = load(states_cfg["limit_load"])
    sens_states = dict(states_cfg.get("sensitivities") or {})
    per_state = declared_receipt_problems(fe_states, repo, validator=validator)
    referenced = [states_cfg["uncracked"], states_cfg["limit_load"], *sens_states.values()]
    referenced += [s for lst in planes_cfg.values() for s in lst]
    for s in referenced:
        if s not in per_state:
            per_state[s] = [f"{s}: referenced by the case but not declared"]
    receipts_used = {s: load(s) for s in dict.fromkeys(referenced)}
    for s, r in receipts_used.items():
        if r.get("design_basis") and r["design_basis"] != case["design_data_register"]:
            per_state[s].append(f"{s}: design basis {r['design_basis']} is not the case register")
        e_r, nu_r = _spec_value(r, "youngs_modulus_mpa"), _spec_value(r, "poisson")
        if e_r is not None and not math.isclose(float(e_r), float(e_mod.value), rel_tol=1e-12):
            per_state[s].append(f"{s}: E {e_r} MPa differs from register {e_mod.register_id}")
        if nu_r is not None and not math.isclose(float(nu_r), float(nu.value), rel_tol=1e-12):
            per_state[s].append(f"{s}: poisson {nu_r} differs from register {nu.register_id}")
    receipt_summary = {
        s: {
            "sha256": text_sha256((fe_states / f"{s}.receipt.json").read_text("utf-8"))
            if (fe_states / f"{s}.receipt.json").is_file() else None,
            "validated": not per_state[s],
            "problems": per_state[s],
        }
        for s in sorted(per_state)
    }
    bad = {s: p for s, p in per_state.items() if p}

    # ---- Lr cut-off -----------------------------------------------------------
    lr_cfg = case.get("lr_max") or {}
    if lr_cfg.get("rule", "flow") == "flow":
        lrm = lr_max("flow", sigma_y_mpa=sigma_y, sigma_u_mpa=sigma_u)
        lr_established = sy.establishes and su.establishes
        lr_why = (f"{lrm.basis}; sigma_y {sy.register_id or 'inline'} ({sy.source_class}), "
                  f"sigma_u {su.register_id or 'inline'} ({su.source_class})")
    else:
        fixed = use("lr_max", lr_cfg)
        lrm = lr_max("fixed", value=float(fixed.value), basis=fixed.basis)
        lr_established = fixed.establishes
        lr_why = f"fixed {lrm.value} ({fixed.source_class}): {fixed.basis}"
    lr_cut = lrm.value

    def curve(x: float) -> float:
        return api579_2016_level2(x, lr_cut)

    # ---- residual stress and PSFs --------------------------------------------
    res_cfg = case.get("residual_stress")
    residual = None
    if res_cfg:
        res_in = use("residual_stress", res_cfg)
        prof = res_cfg["profile"]
        if prof.get("kind") == "uniform":
            profile = secondary_stress.UniformResidual(
                stress_mpa=float(prof["stress_mpa"]), y=float(prof["y"]),
                basis=res_in.basis, relaxation=float(prof.get("relaxation", 1.0)))
        elif prof.get("kind") == "polynomial":
            profile = secondary_stress.PolynomialResidual(
                coefficients_mpa=tuple(prof["coefficients_mpa"]),
                influence=tuple(prof["influence"]), wall_mm=float(prof["wall_mm"]),
                basis=res_in.basis, relaxation=float(prof.get("relaxation", 1.0)))
        else:
            raise ValueError("residual profile kind must be 'uniform' or 'polynomial'.")
        residual = (res_cfg["method"], float(res_cfg["value"]), res_in, profile)
    psf_cfg = case.get("partial_safety_factors")
    psf_stress, psf_kmat, psf_in = 1.0, 1.0, None
    if psf_cfg:
        psf_in = resolve_input("partial_safety_factors",
                               {**psf_cfg, "value": [psf_cfg["stress"], psf_cfg["kmat"]]}, {})
        inputs["partial_safety_factors"] = psf_in
        psf_stress, psf_kmat = float(psf_cfg["stress"]), float(psf_cfg["kmat"])
        if psf_stress < 1.0 or psf_kmat < 1.0:
            raise ValueError("partial safety factors must be >= 1.")

    def fad(k_gov: float, sigma_ref: float, a_mm: float) -> dict:
        kmat_eff = kmat / psf_kmat
        k_p = k_gov * psf_stress
        kr_p = k_p / kmat_eff
        kr_s = 0.0
        if residual is not None:
            method, val, res_in, profile = residual
            k_s = profile.k(a_mm)
            kr_s = secondary_stress.kr_total(
                method, k_p, k_s, kmat_eff, value=val, basis=res_in.basis) - kr_p
        lr = psf_stress * sigma_ref / sigma_y
        m = envelope_margin(lr, kr_p, curve=curve, lr_cut=lr_cut, kr_secondary=max(kr_s, 0.0))
        return {"lr": lr, "kr": kr_p + kr_s, "kr_primary": kr_p, "kr_secondary": kr_s,
                "envelope_margin": _margin_dict(m), "fad_inside": m.factor > 1.0}

    # ---- planes, limits and geometry validity -------------------------------
    plane_limits = case["plane_limits"]
    planes: dict[str, dict] = {}
    geometry_problems: list[str] = []
    for plane, states in planes_cfg.items():
        pl = plane_limits[plane]
        lim = use(f"limit_state_{plane}", pl["limit_state"])
        sref = plane_sigma_ref(uncracked, pl["sigma_ref_paths"], pl["sigma_ref_component"])
        recs = sorted((_state_record(s, receipts_used[s]) for s in states),
                      key=lambda r: r["a_mm"])
        for r in recs:
            if r["a_mm"] > float(pl["max_modelled_depth_mm"]) + 1e-9:
                geometry_problems.append(f"{r['state']}: a = {r['a_mm']} mm beyond the "
                                         f"modelled range ({pl['max_modelled_depth_basis']})")
            if r["a_mm"] >= float(lim.value) or r["ligament_mm"] <= 0.0:
                geometry_problems.append(f"{r['state']}: a = {r['a_mm']} mm at or beyond the "
                                         f"limit state {lim.value} mm ({lim.register_id})")
        planes[plane] = {"limit_mm": float(lim.value), "limit_input": lim,
                         "sigma_ref": sref, "states": recs}

    # ---- FAD at each declared depth -----------------------------------------
    all_depths = sorted({round(r["a_mm"], 6) for p in planes.values() for r in p["states"]})
    depths: list[dict] = []
    for a in all_depths:
        solved = {pn: next(r for r in p["states"] if round(r["a_mm"], 6) == a)
                  for pn, p in planes.items()
                  if any(round(r["a_mm"], 6) == a for r in p["states"])}
        beyond = [pn for pn in planes if pn not in solved and planes[pn]["limit_mm"] <= a]
        pending = [pn for pn in planes if pn not in solved and planes[pn]["limit_mm"] > a]
        if beyond:
            status = "beyond_limit_state"
            reason = "; ".join(
                f"the {pn} plane reaches its limit state at {planes[pn]['limit_mm']} mm "
                f"(register {planes[pn]['limit_input'].register_id}) before this depth"
                for pn in beyond)
        elif pending:
            status = "governing_plane_not_established"
            reason = "; ".join(f"the {pn} plane is not solved at this depth and still has a "
                               "ligament" for pn in pending)
        else:
            status, reason = "established", "every plane with a ligament is solved"
        gov_plane = max(solved, key=lambda pn: solved[pn]["k_gov_mpa_sqrt_m"])
        rec = solved[gov_plane]
        sref = planes[gov_plane]["sigma_ref"]
        point = fad(rec["k_gov_mpa_sqrt_m"], sref["sigma_ref_mpa"], a)
        plane_rows = {}
        for pn, r in solved.items():
            ps = planes[pn]["sigma_ref"]
            plane_rows[pn] = {"state": r["state"], "k_gov_mpa_sqrt_m": r["k_gov_mpa_sqrt_m"],
                              "sigma_ref_mpa": ps["sigma_ref_mpa"], "sigma_ref_path": ps["path"],
                              "ligament_mm": r["ligament_mm"],
                              **fad(r["k_gov_mpa_sqrt_m"], ps["sigma_ref_mpa"], a)}
        if status == "established":
            disposition = ("inside the envelope" if point["fad_inside"]
                           else "outside the envelope")
        else:
            disposition = f"record only, excluded from the verdict: {reason}"
        depths.append({
            "a_mm": a, "status": status, "reason": reason,
            "governing_plane": gov_plane, "governing_state": rec["state"],
            "k_gov_mpa_sqrt_m": rec["k_gov_mpa_sqrt_m"],
            "mode_mix_mpa_sqrt_m": rec["mode_mix_mpa_sqrt_m"], "j_n_per_mm": rec["j_n_per_mm"],
            "ligament_mm": rec["ligament_mm"], "sigma_ref_mpa": sref["sigma_ref_mpa"],
            "sigma_m_mpa": sref["sigma_m_mpa"], "sigma_b_mpa": sref["sigma_b_mpa"],
            "sigma_ref_path": sref["path"], "sigma_ref_component": sref["component"],
            **point, "planes": plane_rows, "disposition": disposition,
        })
    est = [d for d in depths if d["status"] == "established"]
    if not est:
        raise ValueError("no established depth: no depth has every live plane solved.")
    gov_plane0 = est[0]["governing_plane"]

    # ---- growth (owner card B14) ---------------------------------------------
    g = case["growth"]
    law_in = _inline("growth_law_A", g["law"], "A")
    inputs["growth_law_A"] = law_in
    m_exp = float(g["law"]["m"])
    law = GrowthLaw(A=convert_coefficient_n_mm_to_mpa_sqrt_m(float(g["law"]["A"]), m_exp),
                    m=m_exp, basis=law_in.basis,
                    citation="user input; BS 7910 law family (see citations)")
    e_ref = None
    if g.get("e_ratio"):
        er = g["e_ratio"]
        e_ref = float(er["e_ref_gpa"])
        inputs["growth_e_ratio"] = _inline("growth_e_ratio", er, "e_ref_gpa")
        e_t_in = use("growth_e_t", er["e_t"])
        law = law.with_e_ratio(e_ref, float(e_t_in.value) / 1000.0)
    r_in = use("r_ratio", g["r_ratio"])
    r_ratio = float(r_in.value)
    if not 0.0 <= r_ratio < 1.0:
        raise ValueError("R must satisfy 0 <= R < 1.")
    th_in = _inline("threshold", g["threshold"], "dk_th")
    inputs["threshold"] = th_in
    dem_in = use("demand_cycles", g["demand_cycles"])
    demand = float(dem_in.value)
    rules = list(g["threshold"]["rules"])

    def threshold(rule: str) -> Threshold:
        if rule == "e_ratio":
            if e_ref is None:
                raise ValueError("the e_ratio threshold rule needs growth.e_ratio.")
            return Threshold(float(g["threshold"]["dk_th"]), "e_ratio", e_ref, e_t_gpa)
        return Threshold(float(g["threshold"]["dk_th"]), rule)

    a_tab = [d["a_mm"] for d in est]
    dk_tab = [(1.0 - r_ratio) * d["k_gov_mpa_sqrt_m"] for d in est]
    if len(a_tab) < 2:
        raise ValueError("growth needs at least two established depths.")
    tab = TabulatedDeltaK(a_tab, dk_tab)  # extrapolation raises (base result)
    a0, a_last = a_tab[0], a_tab[-1]
    life_last, th_margins = {}, {}
    for rule in rules:
        th = threshold(rule)
        lr_ = life(law, tab, a0, a_last, threshold=th)
        entry = {"status": lr_.status, "threshold_rule": rule}
        if lr_.status == "GROWS":
            entry.update(cycles=lr_.cycles, margin_on_demand=lr_.cycles / demand,
                         dk_multiplier_to_demand=dk_multiplier_to_demand(
                             law, tab, a0, a_last, n_demand=demand, threshold=th))
        else:
            entry.update(a_arrest_mm=lr_.a_arrest_mm,
                         note="growth arrests at the threshold; no finite life")
        life_last[rule] = entry
        th_margins[rule] = {"dk_th_effective": th.effective,
                            "margin": threshold_margin(tab, a0, a_last, th)}
    finite = [e["cycles"] for e in life_last.values() if e["status"] == "GROWS"]
    gov_life = min(finite) if finite else None
    gov_rule = (min((r for r in rules if life_last[r]["status"] == "GROWS"),
                    key=lambda r: life_last[r]["cycles"]) if finite else rules[0])
    life_ok = gov_life is None or gov_life >= demand

    def remaining(a_from: float, a_to: float, model) -> Optional[float]:
        if a_to <= a_from:
            return 0.0
        r = life(law, model, a_from, a_to, threshold=threshold(gov_rule))
        return r.cycles if r.status == "GROWS" else None

    for d in depths:
        d["remaining_life_to_last_fe_cycles"] = (
            remaining(d["a_mm"], a_last, tab) if d["status"] == "established" else None)

    non_gov = {}
    for pn, p in planes.items():
        if pn == gov_plane0 or len(p["states"]) < 2:
            continue
        t2 = TabulatedDeltaK([r["a_mm"] for r in p["states"]],
                             [(1.0 - r_ratio) * r["k_gov_mpa_sqrt_m"] for r in p["states"]])
        rec2 = {}
        for rule in rules:
            lr2 = life(law, t2, t2.a[0], t2.a[-1], threshold=threshold(rule))
            rec2[rule] = {"status": lr2.status, "cycles": lr2.cycles,
                          "a_arrest_mm": lr2.a_arrest_mm,
                          "k_gov_max_mpa_sqrt_m": max(t2.dk) / (1.0 - r_ratio)}
        non_gov[pn] = rec2

    lim_gov = planes[gov_plane0]["limit_mm"]
    tab_lin = TabulatedDeltaK(a_tab, dk_tab, extrapolation="linear")
    sens_life = {"label": "SENSITIVITY", "a_limit_mm": lim_gov,
                 "basis": (f"linear extrapolation of the last DeltaK segment "
                           f"({a_tab[-2]}-{a_last} mm) from {a_last} mm to the {gov_plane0} "
                           f"limit state {lim_gov} mm (register "
                           f"{planes[gov_plane0]['limit_input'].register_id}); no FE state "
                           "supports this range")}
    by_rule = {}
    for rule in rules:
        rr = life(law, tab_lin, a0, lim_gov, threshold=threshold(rule))
        by_rule[rule] = {"status": rr.status, "cycles": rr.cycles, "a_arrest_mm": rr.a_arrest_mm}
    fin = [v["cycles"] for v in by_rule.values() if v["status"] == "GROWS"]
    sens_life["cycles"] = min(fin) if fin else None
    sens_life["margin_on_demand"] = (sens_life["cycles"] / demand) if fin else None
    sens_life["by_rule"] = by_rule
    # Extrapolated remaining lives stay inside the labelled sensitivity; base depth rows
    # carry no extrapolated value (Codex P3 review).
    sens_life["remaining_by_depth"] = [
        {"a_mm": d["a_mm"], "cycles": remaining(d["a_mm"], lim_gov, tab_lin)}
        for d in depths
        if d["status"] == "established"
    ]

    growth = {
        "law": {"A_input": float(g["law"]["A"]), "A_input_units": g["law"].get("A_units", ""),
                "A_mpa_sqrt_m": law.A, "m": law.m, "basis": law.basis,
                "status_label": law_in.status_label},
        "r_ratio": r_ratio,
        "delta_k_basis": ("DeltaK = (1 - R) K_gov of the governing plane at each established "
                          "depth, tabulated piecewise-linearly; no extrapolation in the base "
                          "result"),
        "table": {"a_mm": a_tab, "delta_k_mpa_sqrt_m": dk_tab},
        "a0_mm": a0, "a_last_fe_mm": a_last, "demand_cycles": demand,
        "life_to_last_fe_state": life_last, "governing_rule": gov_rule,
        "governing_life_cycles": gov_life, "threshold_margin": th_margins,
        "life_to_limit_state": {
            "status": "Not Evaluated",
            "reason": (f"no FE state between {a_last} mm and the limit state {lim_gov} mm; "
                       "the base result does not extrapolate (see "
                       "sensitivities.life_to_ligament_exhaustion)"),
        },
        "non_governing_planes": non_gov,
    }

    # ---- consistency checks --------------------------------------------------
    ssy_in = use("ssy_max_ratio", case["checks"]["ssy_max_ratio"])
    sref_chk, ssy_chk = {}, {}
    for d in est:
        key = f"{d['a_mm']:.2f}"
        rc = crack_checks.sigma_ref_consistency(d["sigma_ref_mpa"], d["k_gov_mpa_sqrt_m"],
                                                d["a_mm"])
        sref_chk[key] = {"passed": rc.passed, "ratio_min": rc.ratio_min,
                         "ratio_max": rc.ratio_max, "band": list(rc.band),
                         "y_range": list(rc.y_range)}
        sc = crack_checks.ssy_check(d["k_gov_mpa_sqrt_m"], sigma_y,
                                    ligament_mm=d["ligament_mm"],
                                    max_ratio=float(ssy_in.value))
        ssy_chk[key] = {"passed": sc.passed, "ratio": sc.ratio,
                        "max_ratio": float(ssy_in.value),
                        "plastic_zone": "Irwin, plane stress",
                        "ligament_mm": d["ligament_mm"],
                        # owner card J03 "add meaning": the cyclic plastic zone governing
                        # fatigue growth is (dK / 2 sigma_y)^2, i.e. 1/4 of the monotonic
                        # zone at R = 0; reported for meaning, it does not gate.
                        "cyclic_ratio": sc.ratio / 4.0}
    sref0 = planes[gov_plane0]["sigma_ref"]
    if sref0["peak_mpa"] is not None:
        rng = (1.0 - r_ratio) * sref0["peak_mpa"]
        sd = crack_checks.shakedown_check(elastic_range_mpa=rng, sigma_y_mpa=sigma_y)
        shakedown = {
            "status": "EVALUATED", "passed": sd.passed, "ratio": sd.ratio,
            "elastic_range_mpa": rng, "limit_mpa": 2.0 * sigma_y,
            "basis": (f"elastic range = (1 - R) x the peak {sref0['component']} stress "
                      f"({sref0['peak_field']}) on the {gov_plane0}-plane paths of the "
                      "uncracked receipt at the design pressure; a stress-component range, "
                      "not an equivalent-stress range"),
        }
    else:
        shakedown = {"status": "Not Evaluated",
                     "reason": "the uncracked receipt gives no elastic peak on the plane"}
    # Owner card J03: report the life to the last small-scale-yielding-valid depth as
    # well as to the last FE state. The limit is linearly crossed between the last
    # passing and the first failing depth (the limit itself is not changed).
    ssy_keys = [f"{d['a_mm']:.2f}" for d in est]
    first_fail = next((i for i, k in enumerate(ssy_keys) if not ssy_chk[k]["passed"]), None)
    lim_ssy = float(ssy_in.value)
    if first_fail is None:
        a_star = a_last
    elif first_fail == 0:
        a_star = a0
    else:
        k_ok, k_bad = ssy_keys[first_fail - 1], ssy_keys[first_fail]
        a_ok, a_bad = est[first_fail - 1]["a_mm"], est[first_fail]["a_mm"]
        r_ok, r_bad = ssy_chk[k_ok]["ratio"], ssy_chk[k_bad]["ratio"]
        a_star = a_ok + (lim_ssy - r_ok) / (r_bad - r_ok) * (a_bad - a_ok)
    n_star = remaining(a0, a_star, tab) if a_star > a0 else 0.0
    growth["life_to_last_ssy_valid"] = {
        "a_mm": a_star,
        "cycles": n_star,
        "margin_on_demand": (n_star / demand) if n_star else 0.0,
        "basis": (f"depth where the Irwin plane-stress r_p/ligament reaches the stated limit "
                  f"{lim_ssy:g} (ASSUMED - to be confirmed), by linear interpolation between "
                  "the last passing and first failing FE depths (owner card J03); beyond it, "
                  "linear-elastic K and Paris growth lose validity"),
    }
    # Meaning only (does not gate): the same crossing on the cyclic plastic zone.
    cyc = [ssy_chk[k]["cyclic_ratio"] for k in ssy_keys]
    cfail = next((i for i, r in enumerate(cyc) if r > lim_ssy), None)
    if cfail is None:
        a_cyc = a_last
    elif cfail == 0:
        a_cyc = a0
    else:
        a_lo, a_hi = est[cfail - 1]["a_mm"], est[cfail]["a_mm"]
        a_cyc = a_lo + (lim_ssy - cyc[cfail - 1]) / (cyc[cfail] - cyc[cfail - 1]) * (a_hi - a_lo)
    n_cyc = remaining(a0, a_cyc, tab) if a_cyc > a0 else 0.0
    growth["life_to_last_cyclic_ssy_valid"] = {
        "a_mm": a_cyc, "cycles": n_cyc, "label": "MEANING (not gating)",
        "basis": (f"same crossing on the cyclic plastic zone (dK / 2 sigma_y)^2 = r_p/4 at "
                  f"R = 0 against the same limit {lim_ssy:g}; reported for meaning (owner "
                  "card J03)"),
    }
    ssy_meaning = (
        "Meaning (owner card J03): the monotonic plastic zone grows from "
        + ", ".join(f"{ssy_chk[k]['ratio']:.0%} at a = {k} mm" for k in ssy_keys)
        + " of the remaining ligament. The static FAD already accounts for plasticity "
        "through Lr, so this check bears mainly on the growth life. The cyclic plastic "
        "zone that governs fatigue growth, (dK / 2 sigma_y)^2, is a quarter of the "
        "monotonic zone at R = 0: "
        + ", ".join(f"{ssy_chk[k]['cyclic_ratio']:.0%}" for k in ssy_keys)
        + ". Growth near ligament exhaustion is therefore outside linear-elastic validity, "
        "and the life beyond the last SSY-valid depth is reported separately."
    )
    gv = crack_checks.growth_validity([d["lr"] for d in est])
    checks = {
        "sigma_ref_consistency": sref_chk,
        "ssy": ssy_chk,
        "ssy_basis": ssy_in.basis,
        "ssy_meaning": ssy_meaning,
        "shakedown": shakedown,
        "growth_validity": {"status": gv.status, "reason": gv.reason,
                            "lr_max_seen": gv.lr_max_seen},
    }

    # ---- residual screening bounds (owner card B11) ------------------------
    scr = case["residual_screening"]
    d0 = est[0]
    # Owner card J05: derive Y and the relaxation factor from our own model rather than
    # choosing them. Y comes from the crack-face-pressure pair at a0: by superposition,
    # a uniform crack-face traction p gives the same K as a uniform residual stress p on
    # the crack plane, so Y = (K_on - K_off) / (p sqrt(pi a)).
    y_spec, rel_spec = scr["y"], scr["relaxation"]
    if isinstance(y_spec, Mapping) and y_spec.get("derive") == "crack_face_pressure":
        off_rec = load(str(y_spec["off_state"]))
        k_off = float(off_rec["governing"]["k_gov_max_mpa_sqrt_m"])
        p_face = float(limit_rec["limit_load"]["design_pressure_mpa"])
        y_val = (d0["k_gov_mpa_sqrt_m"] - k_off) / (
            p_face * math.sqrt(math.pi * d0["a_mm"] / 1000.0))
        y_basis = (f"derived from the model's own crack-face pressure pair at a0: "
                   f"Y = (K_on - K_off)/(p sqrt(pi a)) = ({d0['k_gov_mpa_sqrt_m']:.4g} - "
                   f"{k_off:.4g})/({p_face:g} x sqrt(pi x {d0['a_mm']:g} mm)), governing node "
                   f"(receipt {y_spec['off_state']}); ASSUMED - to be confirmed")
    else:
        y_val, y_basis = float(y_spec), "stated input (ASSUMED - to be confirmed)"
    if isinstance(rel_spec, Mapping) and rel_spec.get("derive") == "linear_cap":
        flow = 0.5 * (sigma_y + sigma_u)
        rel_val = min(1.0, max(0.0, float(rel_spec["intercept"]) - d0["sigma_ref_mpa"] / flow))
        rel_basis = (f"derived: min(1, max(0, {float(rel_spec['intercept']):g} - "
                     f"sigma_ref/sigma_f)) with sigma_ref = {d0['sigma_ref_mpa']:.4g} MPa and "
                     f"sigma_f = {flow:.4g} MPa; rule stated in the input, ASSUMED - to be "
                     "confirmed")
    else:
        rel_val, rel_basis = float(rel_spec), "stated input (ASSUMED - to be confirmed)"
    scr_in = resolve_input("residual_screening", {**scr, "value": y_val}, {})
    inputs["residual_screening"] = scr_in
    kr_p0 = d0["k_gov_mpa_sqrt_m"] / kmat
    bounds = []
    for b in secondary_stress.residual_screening_bounds(
            k_primary=d0["k_gov_mpa_sqrt_m"], kmat=kmat, a_mm=d0["a_mm"], y=y_val,
            sigma_y_mpa=sigma_y, sigma_u_mpa=sigma_u, relaxation=rel_val,
            basis=scr_in.basis, rho=float(scr["rho"])):
        mb = envelope_margin(d0["lr"], kr_p0, curve=curve, lr_cut=lr_cut,
                             kr_secondary=b.kr - kr_p0)
        bounds.append({"label": b.label, "kind": b.kind, "sigma_r_mpa": b.sigma_r_mpa,
                       "k_secondary": b.k_secondary, "kr": b.kr,
                       "envelope_margin": _margin_dict(mb)})
    screening = {"a_mm": d0["a_mm"], "rho": float(scr["rho"]), "y": y_val,
                 "y_basis": y_basis, "relaxation": rel_val, "relaxation_basis": rel_basis,
                 "basis": (f"rho = {float(scr['rho']):g} (stated, not computed); "
                           "screening bounds are sensitivities and never the disposition. "
                           f"Input: {scr_in.basis}"),
                 "residual_bounds": bounds}

    # ---- sensitivities ---------------------------------------------------------
    ll = limit_rec["limit_load"]
    ll_a = float(limit_rec["crack"]["depth_mm"]) if limit_rec.get("crack") else None
    ll_lr = float(ll["design_pressure_mpa"]) / float(ll["p_limit_mpa"])
    d_ll = next(d for d in depths if ll_a is not None and math.isclose(d["a_mm"], ll_a))
    m_ll = envelope_margin(ll_lr, d_ll["kr_primary"], curve=curve, lr_cut=lr_cut,
                           kr_secondary=max(d_ll["kr_secondary"], 0.0))
    sensitivities = {
        "limit_load_lr": {
            "label": "SENSITIVITY", "state": states_cfg["limit_load"], "a_mm": ll_a,
            "plane": limit_rec.get("plane"), "p_design_mpa": ll["design_pressure_mpa"],
            "p_limit_mpa": ll["p_limit_mpa"], "lr": ll_lr, "kr": d_ll["kr"],
            "envelope_margin": _margin_dict(m_ll),
            "basis": "Lr = p / P_L from the elastic-perfectly-plastic limit load (owner S02)",
        },
        "life_to_ligament_exhaustion": sens_life,
    }
    # Owner card J02: an indicative partial-factor sensitivity, NOT a code PSF. Factors
    # are user inputs with a basis; the evidence gate still needs a code PSF basis.
    psf_s = case.get("psf_sensitivity")
    if psf_s:
        f_s, f_k = float(psf_s["stress_factor"]), float(psf_s["kmat_factor"])
        if f_s < 1.0 or f_k < 1.0:
            raise ValueError("indicative factors must be >= 1.")
        rows_psf = []
        for d in est:
            lr_f = f_s * d["lr"]
            kr_f = f_s * d["k_gov_mpa_sqrt_m"] / (kmat / f_k)
            m_f = envelope_margin(lr_f, kr_f, curve=curve, lr_cut=lr_cut)
            rows_psf.append({"a_mm": d["a_mm"], "lr": lr_f, "kr": kr_f,
                             "envelope_margin": _margin_dict(m_f),
                             "fad_inside": m_f.factor > 1.0})
        sensitivities["psf_indicative"] = {
            "label": "SENSITIVITY", "stress_factor": f_s, "kmat_factor": f_k,
            "depths": rows_psf,
            "basis": (f"indicative factors {f_s:g} on stress and {f_k:g} on Kmat "
                      f"({psf_s['basis']}); not code partial safety factors, so the "
                      "evidence gate is unchanged"),
        }
    for label, state in sens_states.items():
        rec = _state_record(state, receipts_used[state])
        plane = receipts_used[state].get("plane")
        sref_s = planes[plane]["sigma_ref"]
        sensitivities[label] = {"label": "SENSITIVITY", "state": state, "plane": plane,
                                "a_mm": rec["a_mm"], "k_gov_mpa_sqrt_m": rec["k_gov_mpa_sqrt_m"],
                                **fad(rec["k_gov_mpa_sqrt_m"], sref_s["sigma_ref_mpa"],
                                      rec["a_mm"])}

    # ---- evidence completeness (plan P3) ---------------------------------------
    items = {
        "kmat_basis": EvidenceItem(
            "kmat_basis", kmat_in.establishes,
            f"{kmat_in.register_id or 'inline'} source class {kmat_in.source_class}: "
            f"{kmat_in.basis}"),
        "lr_max_basis": EvidenceItem("lr_max_basis", lr_established, lr_why),
        "residual_stress_basis": (
            EvidenceItem("residual_stress_basis", residual[2].establishes,
                         f"{residual[0]} method, source class {residual[2].source_class}: "
                         f"{residual[2].basis}")
            if residual is not None else EvidenceItem(
                "residual_stress_basis", False,
                "no residual-stress profile and plasticity-interaction method with a basis "
                "is supplied (owner cards G01, G02)")),
        "psf_basis": (
            EvidenceItem("psf_basis", psf_in.establishes,
                         f"source class {psf_in.source_class}: {psf_in.basis}")
            if psf_in is not None else EvidenceItem(
                "psf_basis", False,
                "no partial safety factors with a basis are supplied (owner card G03)")),
        "geometry_validity": EvidenceItem(
            "geometry_validity", not geometry_problems,
            "; ".join(geometry_problems) if geometry_problems else
            "every FE state lies inside its declared modelled range and short of its "
            "plane's limit state"),
        "fe_receipts": EvidenceItem(
            "fe_receipts", not bad,
            ("; ".join(f"{s}: {len(p)} problem(s), first: {p[0]}" for s, p in sorted(bad.items()))
             if bad else f"{len(per_state)} of {len(per_state)} declared FE states verified "
                         "(schema, provenance, artifacts, deck hash, gating guards)")),
    }
    status, missing = evidence_status(items)

    # ---- verdict and findings --------------------------------------------------
    fad_ok = all(d["fad_inside"] for d in est)
    failed_checks = [f"sigma_ref_consistency at {k} mm" for k, v in sref_chk.items()
                     if not v["passed"]]
    failed_checks += [f"ssy at {k} mm" for k, v in ssy_chk.items() if not v["passed"]]
    if shakedown["status"] == "EVALUATED" and not shakedown["passed"]:
        failed_checks.append("shakedown")
    if gv.status != "VALID":
        failed_checks.append("growth_validity")
    if not (fad_ok and life_ok):
        verdict = "REPAIR"
    elif failed_checks:
        verdict = "MONITOR"
    else:
        verdict = "ACCEPT"

    findings = []
    for d in depths:
        mg = d["envelope_margin"]
        findings.append(_finding(
            f"fad.{d['a_mm']:.2f}",
            f"assessment point inside the API 579-1:2016 Level 2 envelope (load factor to "
            f"the envelope F > 1, Lr_max = {lr_cut:.3f}); governing plane {d['governing_plane']}",
            1.0, mg["factor"], d["disposition"] if d["status"] != "established" else
            f"{d['disposition']}: F = {mg['factor']:.3f} at Lr = {d['lr']:.3f}, "
            f"Kr = {d['kr']:.4f} ({mg['mode']} contact)"))
    for rule, e in life_last.items():
        findings.append(_finding(
            f"life.{rule}",
            f"cycles from a0 = {a0} mm to the last FE state {a_last} mm at least the demand "
            f"(threshold rule {rule})", demand, e.get("cycles"),
            (f"exceeds the demand by a factor {e['margin_on_demand']:.3f}; DeltaK multiplier "
             f"to the demand {e['dk_multiplier_to_demand']:.3f}")
            if e["status"] == "GROWS" else f"arrested at {e['a_arrest_mm']:.4f} mm"))
        tm = th_margins[rule]
        findings.append(_finding(
            f"threshold.{rule}",
            f"fractional DeltaK reduction to reach DeltaK_th,eff = {tm['dk_th_effective']:.4f} "
            f"MPa*sqrt(m) over [{a0}, {a_last}] mm (rule {rule})", 0.0, tm["margin"],
            "DeltaK above the threshold at every depth; growth predicted"
            if tm["margin"] > 0 else "DeltaK at or below the threshold; arrest predicted"))
    sv = growth["life_to_last_ssy_valid"]
    cv = growth.get("life_to_last_cyclic_ssy_valid", {})
    findings.append(_finding(
        "life.within_ssy_validity",
        f"cycles from a0 = {a0} mm to the last small-scale-yielding-valid depth "
        f"({sv['a_mm']:.3f} mm, monotonic r_p/ligament limit {lim_ssy:g}) at least the demand",
        demand, sv["cycles"],
        (f"BELOW the demand (factor {sv['margin_on_demand']:.3f}): the linear-elastic growth "
         f"life is not established beyond a = {sv['a_mm']:.3f} mm under the stated monotonic "
         f"limit. On the cyclic plastic zone (meaning only, not gating) validity extends to "
         f"a = {cv.get('a_mm', float('nan')):.3f} mm, {cv.get('cycles', float('nan')):.0f} "
         "cycles. An elastic-plastic (J-based) growth assessment is needed to close this")
        if sv["cycles"] < demand else
        f"meets the demand by a factor {sv['margin_on_demand']:.3f}"))
    for k, v in sref_chk.items():
        findings.append(_finding(
            f"check.sigma_ref_consistency.{k}",
            "sigma_ref / sigma_implied interval intersects the band "
            f"{v['band'][0]}-{v['band'][1]} (Y {v['y_range'][0]:.3f}-{v['y_range'][1]:.3f})",
            v["band"], [v["ratio_min"], v["ratio_max"]],
            "consistent" if v["passed"] else "K and sigma_ref describe different fields"))
    for k, v in ssy_chk.items():
        findings.append(_finding(
            f"check.ssy.{k}", "Irwin plane-stress plastic zone / remaining ligament at most "
            f"{v['max_ratio']} (assumed limit)", v["max_ratio"], v["ratio"],
            "within the limit" if v["passed"] else
            "exceeds the limit: K-based growth at this depth is outside small-scale yielding"))
    findings.append(_finding(
        "check.shakedown", "elastic stress range at most 2 sigma_y", 2.0 * sigma_y,
        shakedown.get("elastic_range_mpa"),
        (("within 2 sigma_y" if shakedown["passed"] else "exceeds 2 sigma_y")
         + f" ({shakedown['basis']})") if shakedown["status"] == "EVALUATED"
        else f"Not Evaluated: {shakedown['reason']}"))
    findings.append(_finding(
        "check.growth_validity", "Lr <= 1 at every established depth (owner card B15)",
        1.0, gv.lr_max_seen, f"{gv.status}: {gv.reason}"))
    for name in missing:
        findings.append(_finding(
            f"evidence.{name}", f"evidence item {name} established", "established",
            "not established", f"INCOMPLETE: {items[name].basis}"))
    findings.append(_finding(
        "verdict", "every established depth inside the envelope and the life to the last "
        "FE state at least the demand; any failed consistency check downgrades ACCEPT to "
        "MONITOR", "ACCEPT", verdict,
        f"{verdict}; evidence {status}"
        + (f" (missing: {', '.join(missing)})" if missing else "")
        + f"; passes = {verdict in ('ACCEPT', 'MONITOR') and status == 'COMPLETE'}"
        + (f"; failed checks: {', '.join(failed_checks)}" if failed_checks else "")))

    # ---- citations (metadata only, fail closed) ----------------------------
    getters = {"api579": get_api579_reference, "bs7910": get_bs7910_reference}
    citations = []
    for c in case.get("citations", []):
        cv = getters[c["getter"]](c["section"], note=c.get("note", ""), repo_root=repo)
        citations.append(dataclasses.asdict(cv.citation))

    labels = [item.get("status_label") for item in register.values()]
    n_assumed = sum(1 for lab in labels if lab == ASSUMED_LABEL)
    unconfirmed = sorted(n for n, i in inputs.items() if i.status_label == ASSUMED_LABEL)
    design_basis_status = (
        f"{ASSUMED_LABEL}: {n_assumed} of {len(labels)} register items and "
        f"{len(unconfirmed)} of {len(inputs)} resolved inputs carry the label")

    return CrackAssessmentResult(
        component_id=str(case["component_id"]),
        verdict=verdict,
        evidence_status=status,
        evidence={n: i.to_dict() for n, i in items.items()},
        missing_evidence=missing,
        engineering={
            "fad_inside_all_established_depths": fad_ok,
            "life_exceeds_demand": life_ok,
            "failed_checks": failed_checks,
            "established_depths_mm": a_tab,
            "criteria": ("ACCEPT when every established depth is inside the envelope and "
                         "the life to the last FE state is at least the demand; MONITOR "
                         "when those hold but a consistency check fails; REPAIR otherwise"),
        },
        depths=depths,
        growth=growth,
        checks=checks,
        screening=screening,
        sensitivities=sensitivities,
        findings=findings,
        citations=citations,
        basis={
            "sigma_ref": SIGMA_REF_DEFINITION,
            "fad_curve": "API 579-1:2016 Level 2 (fad_curves.api579_2016_level2)",
            "lr_max": {"value": lrm.value, "rule": lrm.rule, "basis": lrm.basis},
            "kr": ("Kr = K_gov / Kmat, K_gov = sqrt(E' J) maximised over the crack front "
                   "(owner cards G14, G16); unfactored unless partial safety factors are "
                   "supplied; residual stress enters only through a supplied named method"),
            "kmat_mpa_sqrt_m": kmat,
            "sigma_y_mpa": sigma_y,
            "sigma_u_mpa": sigma_u,
            "unconfirmed_inputs": unconfirmed,
        },
        inputs={n: i.to_dict() for n, i in inputs.items()},
        receipts=receipt_summary,
        design_basis_status=design_basis_status,
    )
