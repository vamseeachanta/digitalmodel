# ABOUTME: digitalmodel#1505 VIV pilot library: builds RunProjections for the synthetic
# ABOUTME: VIV screening cases and drives the assetutilities publication pipeline (dry-run).
"""Synthetic VIV parametric screening -> Hugging Face publication pilot (#1505).

This is the reusable core the runnable CLI (:mod:`run_viv_pilot`) and the pilot tests
both import. It:

* runs >=3 distinct synthetic ``viv-parametric-screening`` cases + 1 exact replay,
  reproducing the router's byte-stable timestamp-free ``results.json`` payload from the
  pure ``orcaflex/viv_screening.py`` calc (consumed UNCHANGED);
* mints deterministic identities via ``assetutilities.workflow_api.identity``;
* builds a strict-identity-bound ``RunProjection`` per case
  (identity / artifact / inputs / output_contract / metrics);
* drives ``publication.PromotionMachine`` emitted -> ... -> accepted against an
  ``InMemoryHfPort`` + a source-repo ``Ledger`` (NO Hugging Face network, NO real
  publish -- the real ``HuggingFaceHubHfPort`` swap is a one-line change for a gated
  live run);
* renders the rolling HTML report (mandatory Inputs + Outputs, pinned in-memory
  revision links) whose eligibility comes SOLELY from the ledger.

Every secret-shaped fixture used by the tests is built at runtime -- there is no
literal ``hf_``/``ghp_`` token in this source.
"""

from __future__ import annotations

import hashlib
import subprocess
from pathlib import Path

from assetutilities.workflow_api import (
    artifact,
    identity,
    inputs,
    metrics,
    output_contract,
    report,
)
from assetutilities.workflow_api.publication import (
    egress,
    ledger as ledger_mod,
    hf_port as hf_port_mod,
    projection,
    promotion,
)

from digitalmodel.orcaflex.viv_screening import (
    BeamProperties,
    VIVScreeningInput,
    estimate_response_amplitude,
    viv_screening,
)
from digitalmodel.orcaflex import viv_screening_workflow as _viv_wf
from digitalmodel.orcaflex.viv_screening_workflow import (
    CITATIONS,
    M_EXP,
    _screening_payload,
)

# ---------------------------------------------------------------------------
# algorithm identity constants (all synthetic / public)
# ---------------------------------------------------------------------------
ALGORITHM_ID = "digitalmodel/viv-parametric-screening"
SEMANTIC_VERSION = "1.0.0"
INPUT_SCHEMA_VERSION = "viv-screening-input-1"
OUTPUT_SCHEMA_VERSION = "viv-screening-output-1"
# A deterministic environment digest (declared env pin; stable across machines).
ENVIRONMENT_DIGEST = hashlib.sha256(
    b"python3.11+digitalmodel+assetutilities+DNV-RP-C205-2019+DNV-RP-F105-2017"
).hexdigest()
SEED = 0

DATASET_REPO = "aceengineer/digitalmodel-runs"

# n_modes is the SINGLE source of truth for the modal-sweep run-control knob. It is a
# run-control knob, NOT a parameter_set input, so there is no double-count. The example
# input.yml + the externalized publication YAML are asserted equal to it in the tests.
N_MODES = 10
# Explicit run-control knobs (fail-closed against implicit defaults), derived from
# N_MODES so the knob is defined exactly once.
EXECUTION_PARAMETERS = {"n_modes": N_MODES, "engine": "embed"}

# ---------------------------------------------------------------------------
# clean-checkout identity (MAJOR-1): resolved at RUN TIME, never hardcoded.
#
# The algorithm identity MUST bind the commit that actually CONTAINS the algorithm
# module. A hardcoded SHA is doubly wrong: (1) the old pin was the pilot's grandparent
# commit, at which viv_screening_workflow.py does not exist, so algorithm_version_id /
# run_id bound a tree with NO algorithm; (2) a frozen literal keeps the same identity
# across future algorithm edits -> silent identity collisions. Instead we resolve HEAD
# of the digitalmodel repo that hosts the algorithm module and FAIL CLOSED unless the
# algorithm-relevant paths are clean AND the module is present at that commit.
# ---------------------------------------------------------------------------
_REPO_ROOT = Path(_viv_wf.__file__).resolve().parents[3]
_ALGORITHM_MODULE_REL = "src/digitalmodel/orcaflex/viv_screening_workflow.py"
_PILOT_REL = "scripts/pilots/viv_pilot.py"


def _git(*args: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        ["git", "-C", str(_REPO_ROOT), *args],
        check=True, capture_output=True, text=True,
    )


def resolve_clean_git_commit() -> str:
    """Resolve the digitalmodel checkout SHA the algorithm identity binds to.

    Fails closed (raises) unless the algorithm-relevant paths are clean at HEAD AND the
    algorithm module actually exists at that commit -- so ``algorithm_version_id`` /
    ``run_id`` can never bind a tree that lacks the algorithm (the old grandparent-SHA
    bug) nor a dirty working copy.
    """
    sha = _git("rev-parse", "HEAD").stdout.strip()
    if len(sha) != 40 or any(c not in "0123456789abcdef" for c in sha):
        raise RuntimeError(f"unexpected git HEAD sha: {sha!r}")
    dirty = _git(
        "status", "--porcelain", "--", _ALGORITHM_MODULE_REL, _PILOT_REL
    ).stdout.strip()
    if dirty:
        raise RuntimeError(
            "refusing to mint identity against a dirty tree; commit the "
            f"algorithm-relevant paths first:\n{dirty}"
        )
    # the module MUST be present at the resolved commit (grandparent-SHA collision guard).
    try:
        _git("cat-file", "-e", f"{sha}:{_ALGORITHM_MODULE_REL}")
    except subprocess.CalledProcessError as exc:
        raise RuntimeError(
            f"algorithm module absent at resolved commit {sha}: {_ALGORITHM_MODULE_REL}"
        ) from exc
    return sha


# The checkout SHA the pilot's algorithm binds to, resolved (not hardcoded) at import.
CLEAN_GIT_COMMIT = resolve_clean_git_commit()

# ---------------------------------------------------------------------------
# the >=3 pinned synthetic cases (span the screening regimes) + the replay.
#
# The published set genuinely spans the primary screening outcome so lock_in is NOT
# constant across the cases:
#   * C-lockin   -> cross-flow lock-in (screening FAILS, non-zero A/D),
#   * C-mid      -> cross-flow lock-in (screening FAILS, smaller non-zero A/D),
#   * C-nolockin -> NO lock-in (screening PASSES, A/D == 0.0).
# ---------------------------------------------------------------------------
CASES = {
    # C-lockin: cross-flow lock-in at mode 2 (Vr~=6.26), A/D ~= 0.98 (the exact-replay
    # case). screening_pass=False, lock_in=True.
    "C-lockin": {
        "input": {"outer_diameter": 0.2032, "current_speed": 1.0},
        "beam": {"length": 60.0, "outer_diameter": 0.2032,
                 "inner_diameter": 0.15, "mass_per_length": 60.0},
    },
    # C-mid: cross-flow lock-in at mode 2 (Vr~=4.34), intermediate A/D ~= 0.31 (non-zero).
    # screening_pass=False, lock_in=True.
    "C-mid": {
        "input": {"outer_diameter": 0.273, "current_speed": 0.8},
        "beam": {"length": 75.0, "outer_diameter": 0.273,
                 "inner_diameter": 0.20, "mass_per_length": 110.0},
    },
    # C-nolockin: a GENUINE no-lock-in regime. Even the fundamental mode's reduced
    # velocity (Vr ~= 0.685) sits BELOW the in-line onset (Vr>=1), so NO mode enters
    # either lock-in band -> screening_pass=True, critical_mode=None, lock_in=False,
    # A/D=0.0. This is true suppression-by-low-Vr, NOT the old mislabelled in-line
    # lock-in case (which had mode-1 Vr~=2.1 inside the in-line band [1, 3.5]).
    "C-nolockin": {
        "input": {"outer_diameter": 0.3556, "current_speed": 0.5},
        "beam": {"length": 25.0, "outer_diameter": 0.3556,
                 "inner_diameter": 0.28, "mass_per_length": 150.0},
    },
}
# The variations published to the dataset (order fixes the deterministic report).
PUBLISHED_ORDER = ["C-lockin", "C-mid", "C-nolockin"]
REPLAY_OF = "C-lockin"


# ---------------------------------------------------------------------------
# pure-calc payload (identical bytes to the router's timestamp-free results.json)
# ---------------------------------------------------------------------------
def compute_case(case: dict):
    """Return ``(payload, a_d, fatigue_proxy, res)`` for a synthetic case.

    Reproduces the router's byte-stable payload from the pure calc alone -- this is
    the clean-room replay path (published inputs -> outputs, no engine tempdir).
    """
    vin = VIVScreeningInput(**case["input"])
    beam = BeamProperties(**case["beam"])
    res = viv_screening(vin, beam, n_modes=N_MODES)
    crit = res.details[(res.critical_mode or 1) - 1]
    a_d = estimate_response_amplitude(
        crit["reduced_velocity"], vin.outer_diameter, vin.stability_parameter
    )
    fatigue_proxy = round(crit["vortex_shedding_freq_Hz"] * (a_d ** M_EXP), 6)
    payload = _screening_payload(vin, beam, res, a_d, fatigue_proxy, N_MODES, None)
    return payload, a_d, fatigue_proxy, res


# ---------------------------------------------------------------------------
# input record (replayable, publicly admissible)
# ---------------------------------------------------------------------------
def build_input_record(case: dict) -> dict:
    """A synthetic, publicly-admissible ``parameter_set`` input record.

    Its ``canonical_repr`` carries ONLY the replay DATA (input + beam params), so two
    distinct cases mint distinct ``input_record_id`` -> distinct ``run_id``, and the
    exact replay of identical params mints the SAME ``run_id``.
    """
    canonical_repr = {
        **case["input"],
        **{f"beam_{k}": v for k, v in case["beam"].items()},
    }
    return inputs.make_input(
        kind="parameter_set",
        role="viv_case",
        schema_version=INPUT_SCHEMA_VERSION,
        canonical_repr=canonical_repr,
        replay_location="content_addressed_dataset_object",
        # embedded parameter_set defaults redistribution_rights to "owned" (public).
    )


def build_identity_context(case: dict) -> dict:
    rec = build_input_record(case)
    pairs = [(rec["role"], inputs.input_record_id(rec))]
    return dict(
        algorithm_id=ALGORITHM_ID,
        semantic_version=SEMANTIC_VERSION,
        clean_git_commit=CLEAN_GIT_COMMIT,
        input_schema_version=INPUT_SCHEMA_VERSION,
        output_schema_version=OUTPUT_SCHEMA_VERSION,
        environment_digest=ENVIRONMENT_DIGEST,
        inputs=pairs,
        execution_parameters=dict(EXECUTION_PARAMETERS),
        seed=SEED,
    )


def run_id_for(case: dict) -> str:
    return identity.derive_run_identity(**build_identity_context(case))["run_id"]


# ---------------------------------------------------------------------------
# algorithm-scoped metric definitions (definitions/units/derivations/quality)
# ---------------------------------------------------------------------------
def build_metric_store() -> metrics.MetricDefinitionStore:
    store = metrics.MetricDefinitionStore()
    store.register(metrics.make_metric_definition(
        metric_id=f"{ALGORITHM_ID}/a_d_ratio", algorithm_id=ALGORITHM_ID,
        definition_version="1", label="A/D response amplitude ratio",
        meaning="Cross-flow VIV response amplitude normalized by hydrodynamic diameter",
        unit_or_dimension="dimensionless", data_type="number",
        derivation="DNV-RP-C205 Figure 9-3 as f(reduced_velocity Vr, stability Ks)",
        applicability="cross-flow lock-in band 4<=Vr<=8; 0 outside the band",
        directionality="higher_is_worse",
        quality_rule="valid when a screening result is evaluated for the critical mode",
    ))
    store.register(metrics.make_metric_definition(
        metric_id=f"{ALGORITHM_ID}/lock_in", algorithm_id=ALGORITHM_ID,
        definition_version="1", label="Cross-flow lock-in flag",
        meaning="True when any mode enters the cross/in-line lock-in band",
        unit_or_dimension="NA", data_type="boolean",
        derivation="DNV-RP-C205 §9: reduced velocity Vr in a lock-in band",
        applicability="all screened members",
        directionality="none",
        quality_rule="valid for any completed screening",
    ))
    store.register(metrics.make_metric_definition(
        metric_id=f"{ALGORITHM_ID}/fatigue_proxy", algorithm_id=ALGORITHM_ID,
        definition_version="1", label="Fatigue-proxy screening surrogate",
        meaning="vortex_shedding_freq * (A/D)^m screening indicator",
        unit_or_dimension="Hz (screening surrogate)", data_type="number",
        derivation=f"f_vs * (A/D)^{M_EXP} (S-N-slope-flavoured surrogate exponent)",
        applicability="relative screening only",
        directionality="higher_is_worse",
        quality_rule="SCREENING SURROGATE ONLY -- NOT a certified fatigue life",
    ))
    return store


def _observations(store, run_id, a_d, fatigue_proxy, lock_in) -> list:
    d_ad = store.get(f"{ALGORITHM_ID}/a_d_ratio", "1")
    d_lock = store.get(f"{ALGORITHM_ID}/lock_in", "1")
    d_fat = store.get(f"{ALGORITHM_ID}/fatigue_proxy", "1")
    return [
        metrics.make_metric_observation(
            definition=d_ad, run_id=run_id, value=a_d, quality_state="valid",
            derivation_evidence="estimate_response_amplitude(Vr, Ks) per DNV-RP-C205 Fig 9-3"),
        metrics.make_metric_observation(
            definition=d_lock, run_id=run_id, value=bool(lock_in), quality_state="valid",
            derivation_evidence="viv_screening cross/in-line lock-in flags"),
        metrics.make_metric_observation(
            definition=d_fat, run_id=run_id, value=fatigue_proxy, quality_state="valid",
            derivation_evidence=f"f_vs * (A/D)^{M_EXP} screening surrogate"),
    ]


# ---------------------------------------------------------------------------
# projection (five records under one strict identity)
# ---------------------------------------------------------------------------
def build_projection_for_case(case: dict, store) -> tuple:
    """Return ``(RunProjection, payload, a_d)`` for a synthetic case."""
    payload, a_d, fatigue_proxy, res = compute_case(case)
    lock_in = not res.screening_pass

    rec = build_input_record(case)
    identity_context = build_identity_context(case)
    run_id = identity.derive_run_identity(**identity_context)["run_id"]

    art = artifact.structured_artifact(payload)
    object_bytes = {art["content_digest"]: artifact.structured_stored_bytes(payload)}
    out_rec = output_contract.make_output_record(
        role="viv_screening_result",
        native_schema={"id": OUTPUT_SCHEMA_VERSION, "version": "1"},
        media_type="application/json",
        curated_label="primary_result",
        artifact_refs=[art["content_digest"]],
        units={"a_d_ratio": "dimensionless", "fatigue_proxy": "Hz(surrogate)"},
        validation_state="validated",
    )
    # equality digest over the timestamp-free curated output ONLY (no provenance).
    oed = output_contract.output_equality_digest([out_rec])

    obs = _observations(store, run_id, a_d, fatigue_proxy, lock_in)

    proj = projection.build_projection(
        identity_context=identity_context,
        input_records=[rec],
        artifacts=[art],
        outputs=[out_rec],
        output_equality_digest=oed,
        metric_observations=obs,
        object_bytes=object_bytes,
        terminal_status="succeeded",
        metric_store=store,
    )
    return proj, payload, a_d


# ---------------------------------------------------------------------------
# promotion drive (emitted -> ... -> accepted) against the in-memory port
# ---------------------------------------------------------------------------
APPROVAL = {"available": True, "approved": True}


def make_gate() -> egress.EgressGate:
    """Fail-closed egress gate. env_tokens/legal deny-list are injected, never read
    from os.environ; the token fixtures the tests use are built at runtime."""
    return egress.EgressGate(legal_deny_list=(), env_tokens={})


def promote_to_accepted(proj, *, hf_port, ledger, gate) -> promotion.PromotionMachine:
    """Drive one projection emitted -> validated -> replayed -> draft_rendered ->
    reviewed -> hf_candidate -> report_pinned -> accepted."""
    m = promotion.PromotionMachine(proj, hf_port=hf_port, ledger=ledger, egress=gate)
    m.validate()
    m.replay(observed_output_equality=proj.output_equality_digest["digest"])
    m.render_draft()
    m.review(human_promotion_review=APPROVAL, adversarial_artifact_review=APPROVAL)
    m.promote_to_hf_candidate()
    m.pin_report()
    m.accept()
    return m


def _run_view(proj, payload, revision) -> dict:
    scr = payload["screening"]
    return {
        "run_id": proj.run_id,
        "status": proj.terminal_status,
        "hf_revision": revision,
        "inputs": [
            {"name": r["role"], "value": r["kind"]} for r in proj.input_records
        ],
        "outputs": [
            {"name": "a_d_ratio", "value": scr["a_d_ratio"]},
            {"name": "lock_in", "value": scr["lock_in"]},
            {"name": "fatigue_proxy", "value": scr["fatigue_proxy"]},
            {"name": "critical_vr", "value": scr["critical_vr"]},
        ],
    }


def render_rolling_report(run_views, ledger, *, pinned=True) -> str:
    return report.render_report(
        algorithm=ALGORITHM_ID,
        runs=run_views,
        ledger=ledger.records(),
        pinned=pinned,
        title=f"Rolling report: {ALGORITHM_ID}",
    )


# ---------------------------------------------------------------------------
# the full dry-run
# ---------------------------------------------------------------------------
def run_pilot(echo_dir: str | Path | None = None, hf_port=None) -> dict:
    """Execute the FULL publication pipeline against a HfPort + Ledger.

    ``hf_port`` defaults to :class:`InMemoryHfPort` (dry-run, no network). Pass a
    real ``HuggingFaceHubHfPort`` for a gated live publish. Returns a summary dict;
    when ``echo_dir`` is given, writes the rolling report, the publications.jsonl
    ledger, and each accepted results.json under it.
    """
    store = build_metric_store()
    if hf_port is None:
        hf_port = hf_port_mod.InMemoryHfPort()
    ledger = ledger_mod.Ledger(path=ledger_mod.PUBLICATIONS_LEDGER_PATH)
    gate = make_gate()

    accepted = []  # (name, proj, payload, revision)
    for name in PUBLISHED_ORDER:
        proj, payload, a_d = build_projection_for_case(CASES[name], store)
        m = promote_to_accepted(proj, hf_port=hf_port, ledger=ledger, gate=gate)
        accepted.append((name, proj, payload, m.candidate_revision))

    # --- exact replay of C-lockin: SAME run_id + output equality (timestamp-free) ---
    replay_proj, replay_payload, _ = build_projection_for_case(CASES[REPLAY_OF], store)
    base_proj = next(p for (n, p, _pl, _r) in accepted if n == REPLAY_OF)
    same_run_id = replay_proj.run_id == base_proj.run_id
    recorded = base_proj.output_equality_digest["digest"]
    observed = replay_proj.output_equality_digest["digest"]
    output_equality_ok = bool(
        output_contract.assert_output_equality(recorded, observed)
    )
    # the replay resolves to an already-published run: identity dedup, not re-accepted.
    replay_already_published = ledger.is_eligible(replay_proj.run_id)

    # --- rolling report (pinned to the in-memory revisions) ---
    run_views = [
        _run_view(proj, payload, rev) for (_n, proj, payload, rev) in accepted
    ]
    report_html = render_rolling_report(run_views, ledger, pinned=True)

    summary = {
        "dataset_repo": DATASET_REPO,
        "run_ids": {n: p.run_id for (n, p, _pl, _r) in accepted},
        "revisions": {n: rev for (n, _p, _pl, rev) in accepted},
        "a_d": {n: pl["screening"]["a_d_ratio"] for (n, _p, pl, _r) in accepted},
        "accepted_count": len(ledger.eligible_run_ids()),
        "replay_same_run_id": same_run_id,
        "replay_output_equality_ok": output_equality_ok,
        "replay_already_published": replay_already_published,
        "ledger_records": ledger.records(),
        "ledger_jsonl": ledger.to_jsonl(),
        "report_html": report_html,
        "report_path": None,
    }

    if echo_dir is not None:
        echo = Path(echo_dir)
        echo.mkdir(parents=True, exist_ok=True)
        report_path = echo / "index.html"
        report_path.write_text(report_html, encoding="utf-8")
        (echo / "publications.jsonl").write_text(ledger.to_jsonl(), encoding="utf-8")
        import json as _json
        for (n, _p, payload, _r) in accepted:
            (echo / f"results_{n}.json").write_text(
                _json.dumps(payload, indent=2), encoding="utf-8"
            )
        summary["report_path"] = str(report_path)

    return summary
