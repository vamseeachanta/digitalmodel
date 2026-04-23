"""Prospect-intake adapter (canonical path per plan #2346).

Responsibilities
----------------
1. Load + validate a prospect intake YAML against `prospect-schema.json`
   plus cross-field checks (vessel-shape-vs-demo, structure-kind-vs-demo,
   canonical-ref existence, depth-vs-vessel-rating sanity).
2. Materialize the validated intake into the on-disk JSON shapes the
   existing GTM demo scripts already expect.
3. Wrap the dual-delivery state machine: email-first → gated URL,
   retry budget, terminal-state recording.
4. Emit fallback-audit sidecar records (F1-F5) to a **gitignored**
   `private-log/fallback-applied.json` path — never shipped to prospect.

This module is self-contained (no cross-repo import of
`scripts.gtm.prospect_adapter`) so the digitalmodel repo can be cloned
and tested without a workspace-hub checkout. The scripts/gtm/ scaffold
remains as a parallel workspace-hub-tracked implementation that is
expected to be consolidated in a follow-up.

Plan section C (pseudocode) is the authoritative spec. This file
implements the validation + materialization + delivery layers; the
per-demo CLI patches (section D) and full subprocess dispatch are
explicit follow-up work.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import sys
import time
from copy import deepcopy
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path
from typing import Callable, Literal, Mapping, Sequence

try:
    import jsonschema
except ImportError as exc:  # pragma: no cover — real dep required.
    raise RuntimeError(
        "prospect_adapter requires jsonschema. Install with "
        "`uv pip install jsonschema>=4.26`."
    ) from exc

try:
    import yaml
except ImportError as exc:  # pragma: no cover
    raise RuntimeError(
        "prospect_adapter requires PyYAML. Install with "
        "`uv pip install pyyaml`."
    ) from exc


# ---------------------------------------------------------------------------
# Repo paths — resolve relative to this file so the module works from
# both a digitalmodel checkout and a workspace-hub overlay.
# ---------------------------------------------------------------------------
_THIS_FILE = Path(__file__).resolve()
# digitalmodel/examples/demos/gtm/prospect_adapter.py → parents[4] is the
# digitalmodel repo root; parents[5] is workspace-hub when overlayed.
_DIGITALMODEL_ROOT = _THIS_FILE.parents[3]
_WORKSPACE_HUB_ROOT = _THIS_FILE.parents[4]

# The schema + canonical library live under workspace-hub/docs/gtm/intake/.
# The adapter checks both possible roots so it works from either mount.
_SCHEMA_CANDIDATES = (
    _WORKSPACE_HUB_ROOT / "docs" / "gtm" / "intake" / "prospect-schema.json",
    _DIGITALMODEL_ROOT / ".." / "docs" / "gtm" / "intake" / "prospect-schema.json",
)
_CANONICAL_CANDIDATES = (
    _WORKSPACE_HUB_ROOT / "docs" / "gtm" / "intake" / "canonical-vessels",
    _DIGITALMODEL_ROOT / ".." / "docs" / "gtm" / "intake" / "canonical-vessels",
)

# Fallback-audit sidecar lives under the demo directory so the
# packaging step can exclude the whole `private-log/` subtree by path prefix.
PRIVATE_LOG_DIR = _THIS_FILE.parent / "private-log"
FALLBACK_SIDECAR_PATH = PRIVATE_LOG_DIR / "fallback-applied.json"


DEMO_IDS = ("demo_01", "demo_02", "demo_03", "demo_04", "demo_05")
DemoId = Literal["demo_01", "demo_02", "demo_03", "demo_04", "demo_05"]

# Plan section C constants.
DEMO_TO_VESSEL_SHAPE: Mapping[DemoId, str | None] = {
    "demo_01": None,
    "demo_02": None,
    "demo_03": "csv_hlv",
    "demo_04": "pipelay",
    "demo_05": "csv_hlv",
}

DEMO_TO_STRUCTURE_KINDS: Mapping[DemoId, tuple[str, ...]] = {
    "demo_01": ("pipeline", "jumper_and_freespan"),
    "demo_02": ("pipeline",),
    "demo_03": ("mudmat",),
    "demo_04": ("pipeline",),
    "demo_05": ("rigid_jumper",),
}

# Canonical-ref aliases. Plan's Files-to-Change rows prescribe
# `heavy-lift-csv.yaml` as the CSV/HLV canonical; the earlier scaffold
# also shipped `seven-borealis.yaml` as a class-typical reference. Both
# resolve to the csv_hlv shape so intake-authors can use either ID.
CANONICAL_ALIASES: Mapping[str, str] = {
    "seven-borealis": "heavy-lift-csv",
}

# F3 allowlist — single scalars that may be filled in from canonical
# class values without explicit prospect authorization.
FIELDS_ALLOWED_FOR_CLASS_DEFAULT: frozenset[str] = frozenset(
    {
        "general.transit_speed_kt",
        "general.accommodation_pob",
        "general.deck_area_m2",
        "crane_main.hoist_speed_m_per_min",
        "crane_main.main_wire_mbl_te",
        "crane_main.main_wire_diameter_mm",
        "motion_characteristics.heave_rao",
        "motion_characteristics.roll_rao",
        "motion_characteristics.pitch_rao",
        "motion_characteristics.natural_periods.heave_s",
        "motion_characteristics.natural_periods.roll_s",
        "motion_characteristics.natural_periods.pitch_s",
    }
)


# ---------------------------------------------------------------------------
# Exceptions + result types
# ---------------------------------------------------------------------------


class ProspectIntakeError(ValueError):
    """Raised when an intake YAML fails schema or cross-field validation."""


class FallbackCode(str, Enum):
    F1_REFUSE = "F1"
    F2_CLOSEST_CANONICAL = "F2"
    F3_CLASS_DEFAULT_FIELD = "F3"
    F4_CLARIFY = "F4"
    F5_REDUCED_SCOPE = "F5"


class DeliveryState(str, Enum):
    DELIVERED = "DELIVERED"
    DELIVERED_EMAIL_ONLY = "DELIVERED_EMAIL_ONLY"
    FAILED_EMAIL = "FAILED_EMAIL"
    UNPUBLISHED = "UNPUBLISHED"


@dataclass(frozen=True)
class ProspectInput:
    raw: dict
    target_demo: DemoId
    vessel_shape: str | None
    structure_kind: str
    source_path: Path

    @property
    def company(self) -> str:
        return str(self.raw["prospect"]["company"])

    @property
    def contact(self) -> str:
        return str(self.raw["prospect"]["contact"])

    @property
    def prospect_id(self) -> str:
        """Stable slug derived from company + deadline date."""
        company_slug = (
            self.company.lower()
            .replace(" ", "-")
            .replace(",", "")
            .replace(".", "")
            .replace("/", "-")
        )
        deadline = self.raw["prospect"]["delivery_deadline_utc"][:10]
        return f"{company_slug}-{deadline}"


@dataclass(frozen=True)
class DemoInputBundle:
    demo_id: DemoId
    tmpdir: Path
    data_dir: Path
    env_override_path: Path | None = None
    vessel_file: Path | None = None
    structure_file: Path | None = None
    extras: dict[str, Path] = field(default_factory=dict)


@dataclass
class DeliveryResult:
    state: DeliveryState
    email_sent_utc: str | None
    email_attempt_count: int
    url_published_utc: str | None
    url_publish_attempt_count: int
    gated_url_hash: str | None
    purge_after_utc: str | None
    fallback_applied: str | None
    notes: str = ""


# ---------------------------------------------------------------------------
# Schema + canonical library loaders
# ---------------------------------------------------------------------------


def _first_existing(paths: Sequence[Path]) -> Path:
    for p in paths:
        resolved = p.resolve() if p.exists() else p
        if resolved.exists():
            return resolved
    raise ProspectIntakeError(
        "could not locate prospect-schema.json / canonical-vessels under any of: "
        + ", ".join(str(p) for p in paths)
    )


def _schema_path() -> Path:
    return _first_existing(_SCHEMA_CANDIDATES)


def _canonical_dir() -> Path:
    return _first_existing(_CANONICAL_CANDIDATES)


def _load_schema() -> dict:
    with _schema_path().open("r", encoding="utf-8") as fh:
        return json.load(fh)


def _resolve_canonical_ref(ref: str) -> str:
    """Map aliases (e.g. seven-borealis) to the canonical filename stem.

    Also hardens against path-traversal: the ref MUST be a bare
    filename stem with no path separators, `..` segments, or `/`
    characters. A prospect who writes
    `canonical_ref: "../../../etc/passwd"` gets a hard rejection at
    this layer regardless of what happens to exist on disk.
    """
    resolved = CANONICAL_ALIASES.get(ref, ref)
    if (
        not resolved
        or "/" in resolved
        or "\\" in resolved
        or ".." in resolved
        or resolved.startswith(".")
    ):
        raise ProspectIntakeError(
            f"canonical_ref {ref!r} is not a bare filename stem; "
            "path-traversal attempts are rejected"
        )
    return resolved


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------


def _canonical_shape_matches_expected(body: Mapping[str, object], expected_shape: str) -> bool:
    if expected_shape == "pipelay":
        return "pipelay_system" in body and "crane_main" not in body
    if expected_shape == "csv_hlv":
        return "crane_main" in body and "pipelay_system" not in body
    return False


def _cross_field_checks(intake: dict, demo: DemoId) -> None:
    expected_shape = DEMO_TO_VESSEL_SHAPE[demo]

    if expected_shape is None:
        if "vessel" in intake:
            raise ProspectIntakeError(
                f"{demo} must not include a vessel block (Q6 forbidding rule)"
            )
        return

    vessel = intake.get("vessel")
    if vessel is None:
        raise ProspectIntakeError(f"{demo} requires a vessel block (Q6)")

    actual_shape = vessel.get("shape")
    if actual_shape != expected_shape:
        raise ProspectIntakeError(
            f"{demo} requires vessel.shape == {expected_shape!r}, got {actual_shape!r}"
        )

    structure_kind = intake["structure"]["kind"]
    allowed_kinds = DEMO_TO_STRUCTURE_KINDS[demo]
    if structure_kind not in allowed_kinds:
        raise ProspectIntakeError(
            f"{demo} requires structure.kind in {list(allowed_kinds)!r}, "
            f"got {structure_kind!r}"
        )

    if vessel.get("source") == "canonical_ref":
        ref = vessel.get("canonical_ref")
        if not ref:
            raise ProspectIntakeError(
                "vessel.source == 'canonical_ref' requires a non-empty canonical_ref"
            )
        resolved_ref = _resolve_canonical_ref(ref)
        canonical_path = _canonical_dir() / f"{resolved_ref}.yaml"
        if not canonical_path.exists():
            raise ProspectIntakeError(
                f"canonical vessel reference not found: {canonical_path}"
            )
        with canonical_path.open("r", encoding="utf-8") as fh:
            canonical_body = yaml.safe_load(fh)
        if not isinstance(canonical_body, dict):
            raise ProspectIntakeError(
                f"canonical vessel YAML must load to a mapping: {canonical_path}"
            )
        if not _canonical_shape_matches_expected(canonical_body, expected_shape):
            raise ProspectIntakeError(
                f"canonical vessel reference {ref!r} does not match required "
                f"{expected_shape!r} shape"
            )

    environment = intake.get("environment") or {}
    depths = environment.get("water_depths_m") or []
    if depths and vessel.get("source") == "canonical_ref":
        ref = vessel["canonical_ref"]
        resolved_ref = _resolve_canonical_ref(ref)
        canonical_body = yaml.safe_load(
            (_canonical_dir() / f"{resolved_ref}.yaml").read_text(encoding="utf-8")
        )
        max_depth = (
            canonical_body.get("general", {}).get("max_water_depth_m")
            if isinstance(canonical_body, dict)
            else None
        )
        if isinstance(max_depth, (int, float)):
            exceedances = [d for d in depths if d > max_depth]
            if exceedances:
                raise ProspectIntakeError(
                    f"prospect requested deeper water ({max(exceedances)} m) than "
                    f"vessel rating ({max_depth} m) — apply F5 reduced-scope or F1 refuse"
                )


def load_and_validate(intake_path: Path) -> ProspectInput:
    intake_path = Path(intake_path)
    if not intake_path.exists():
        raise ProspectIntakeError(f"intake file not found: {intake_path}")

    try:
        with intake_path.open("r", encoding="utf-8") as fh:
            intake = yaml.safe_load(fh)
    except yaml.YAMLError as exc:
        raise ProspectIntakeError(f"malformed YAML in {intake_path}: {exc}") from exc

    if not isinstance(intake, dict):
        raise ProspectIntakeError(
            f"intake {intake_path} must be a YAML mapping at top level, "
            f"got {type(intake).__name__}"
        )

    schema = _load_schema()
    try:
        jsonschema.validate(
            instance=intake,
            schema=schema,
            cls=jsonschema.Draft7Validator,
        )
    except jsonschema.ValidationError as exc:
        raise ProspectIntakeError(
            f"schema validation failed for {intake_path}: {exc.message}"
        ) from exc

    demo = intake["prospect"]["target_demo"]
    _cross_field_checks(intake, demo)

    vessel_block = intake.get("vessel")
    return ProspectInput(
        raw=intake,
        target_demo=demo,
        vessel_shape=(vessel_block or {}).get("shape") if vessel_block else None,
        structure_kind=intake["structure"]["kind"],
        source_path=intake_path,
    )


# ---------------------------------------------------------------------------
# Materialization
# ---------------------------------------------------------------------------


def _resolved_vessel_body(prospect: ProspectInput) -> dict:
    vessel = prospect.raw.get("vessel")
    if vessel is None:
        raise ProspectIntakeError("prospect does not include a vessel block")

    if vessel.get("source") == "canonical_ref":
        ref = vessel.get("canonical_ref")
        resolved_ref = _resolve_canonical_ref(str(ref))
        canonical_path = _canonical_dir() / f"{resolved_ref}.yaml"
        with canonical_path.open("r", encoding="utf-8") as fh:
            body = yaml.safe_load(fh)
        if not isinstance(body, dict):
            raise ProspectIntakeError(
                f"canonical vessel YAML must load to a mapping: {canonical_path}"
            )
        return deepcopy(body)

    body = vessel.get("body")
    if not isinstance(body, dict):
        raise ProspectIntakeError("prospect vessel.body must be a mapping")
    return deepcopy(body)


def _write_env_override_if_present(prospect: ProspectInput, data_dir: Path) -> Path | None:
    environment = prospect.raw.get("environment")
    if not (isinstance(environment, dict) and environment):
        return None
    path = data_dir / "prospect_env.json"
    path.write_text(json.dumps(environment, indent=2) + "\n", encoding="utf-8")
    return path


def _materialize_pipelay(prospect: ProspectInput, tmpdir: Path) -> DemoInputBundle:
    data_dir = tmpdir / "data"
    data_dir.mkdir(parents=True, exist_ok=True)

    vessel_body = _resolved_vessel_body(prospect)
    vessel_file = data_dir / "pipelay_vessels.json"
    vessel_file.write_text(
        json.dumps({"vessels": [vessel_body]}, indent=2) + "\n",
        encoding="utf-8",
    )

    structure_body = deepcopy(prospect.raw["structure"]["body"])
    structure_file = data_dir / "pipelines.json"
    structure_file.write_text(
        json.dumps({"pipes": [structure_body]}, indent=2) + "\n",
        encoding="utf-8",
    )

    env_override_path = _write_env_override_if_present(prospect, data_dir)

    return DemoInputBundle(
        demo_id=prospect.target_demo,
        tmpdir=tmpdir,
        data_dir=data_dir,
        env_override_path=env_override_path,
        vessel_file=vessel_file,
        structure_file=structure_file,
    )


def _materialize_csv_hlv(prospect: ProspectInput, tmpdir: Path) -> DemoInputBundle:
    data_dir = tmpdir / "data"
    data_dir.mkdir(parents=True, exist_ok=True)

    vessel_body = _resolved_vessel_body(prospect)
    vessel_file = data_dir / "csv_hlv_vessels.json"
    vessel_file.write_text(
        json.dumps({"vessels": [vessel_body]}, indent=2) + "\n",
        encoding="utf-8",
    )

    structure_body = deepcopy(prospect.raw["structure"]["body"])

    if prospect.target_demo == "demo_03":
        structure_file = data_dir / "mudmat_structures.json"
        structure_file.write_text(
            json.dumps({"structures": [structure_body]}, indent=2) + "\n",
            encoding="utf-8",
        )
    else:  # demo_05: rigid_jumper
        structure_file = data_dir / "rigid_jumpers.json"
        structure_file.write_text(
            json.dumps(
                {
                    "common_properties": {
                        "grade": structure_body.get("material", "X65"),
                        "outer_diameter_m": structure_body.get("outer_diameter_m"),
                        "wall_thickness_m": structure_body.get("wall_thickness_m"),
                    },
                    "jumpers": [structure_body],
                },
                indent=2,
            )
            + "\n",
            encoding="utf-8",
        )

    env_override_path = _write_env_override_if_present(prospect, data_dir)

    return DemoInputBundle(
        demo_id=prospect.target_demo,
        tmpdir=tmpdir,
        data_dir=data_dir,
        env_override_path=env_override_path,
        vessel_file=vessel_file,
        structure_file=structure_file,
    )


def materialize_demo_inputs(prospect: ProspectInput, tmpdir: Path) -> DemoInputBundle:
    """Map validated intake onto the on-disk JSON shapes each demo expects.

    Currently implements demo_03, demo_04, demo_05. Demos 01/02 do not
    take a vessel block so their materialization paths are unimplemented
    stubs — they require a pipe-only input surface that the next
    follow-up task will wire.
    """
    tmpdir = Path(tmpdir)
    demo = prospect.target_demo
    if demo == "demo_04":
        return _materialize_pipelay(prospect, tmpdir)
    if demo in ("demo_03", "demo_05"):
        return _materialize_csv_hlv(prospect, tmpdir)

    raise NotImplementedError(
        f"materialize_demo_inputs not yet implemented for {demo!r}. "
        "Demos 01 / 02 materialization (pipe-only + env override) is a filed "
        "follow-up; they do not take a vessel block and so cannot reuse the "
        "csv_hlv / pipelay materializers."
    )


# ---------------------------------------------------------------------------
# Fallback sidecar (F1-F5)
# ---------------------------------------------------------------------------


def _now_utc_iso() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def record_fallback(
    *,
    prospect_id: str,
    fallback_code: FallbackCode,
    failure_mode: str,
    field_substituted: str | None = None,
    canonical_source: str | None = None,
    pre_authorization: Literal["explicit", "implicit_allowlist", "none"] = "none",
    engineer: str = "unattended-agent",
    sidecar_path: Path | None = None,
) -> Path:
    """Append an F1-F5 audit record to the private-log sidecar.

    Returns the sidecar path. The sidecar lives under `private-log/`,
    which is `.gitignored` and hard-excluded from the `deliver()`
    email-attachment + URL-publish packaging step.
    """
    target = sidecar_path or FALLBACK_SIDECAR_PATH
    target.parent.mkdir(parents=True, exist_ok=True)

    record = {
        "prospect_id": prospect_id,
        "timestamp_utc": _now_utc_iso(),
        "fallback_code": fallback_code.value,
        "failure_mode": failure_mode,
        "field_substituted": field_substituted,
        "canonical_source": canonical_source,
        "pre_authorization": pre_authorization,
        "engineer": engineer,
    }

    existing: list[dict] = []
    if target.exists():
        try:
            existing = json.loads(target.read_text(encoding="utf-8"))
            if not isinstance(existing, list):
                existing = []
        except json.JSONDecodeError:
            existing = []

    existing.append(record)
    target.write_text(
        json.dumps(existing, indent=2) + "\n",
        encoding="utf-8",
    )
    return target


# ---------------------------------------------------------------------------
# Delivery state machine (dual-channel: email then gated URL)
# ---------------------------------------------------------------------------


EmailSender = Callable[[ProspectInput, Path], bool]
UrlPublisher = Callable[[ProspectInput, Path, str], bool]

_DEFAULT_RETRY_BACKOFF_S: tuple[float, ...] = (30.0, 120.0, 600.0)


def compute_gated_url_hash(prospect_id: str, salt: str, date: str) -> str:
    """sha256 hex of the concatenated tuple, per plan §Gated URL scheme."""
    material = f"{prospect_id}:{salt}:{date}".encode("utf-8")
    return hashlib.sha256(material).hexdigest()


def _excluded_from_delivery(path: Path) -> bool:
    """Hard exclude `private-log/` from any delivered bundle."""
    parts = path.parts
    return "private-log" in parts


def package_delivery_files(bundle_dir: Path) -> list[Path]:
    """Return the file list that may be emailed / published.

    Enforces the plan's "private-log never ships" guarantee: the
    sidecar's path is filtered out at this layer regardless of what
    the caller globs.
    """
    bundle_dir = Path(bundle_dir)
    if not bundle_dir.exists():
        return []
    return [p for p in sorted(bundle_dir.rglob("*")) if p.is_file() and not _excluded_from_delivery(p)]


def _retry_with_backoff(
    op: Callable[[], bool],
    *,
    backoff_s: Sequence[float],
    sleep: Callable[[float], None] = time.sleep,
) -> tuple[bool, int]:
    """Run op() up to len(backoff_s)+1 times. Returns (success, attempt_count)."""
    attempts = 0
    for wait in (0.0, *backoff_s):
        if wait > 0:
            sleep(wait)
        attempts += 1
        try:
            if op():
                return True, attempts
        except Exception:  # noqa: BLE001 — retry transient failures.
            continue
    return False, attempts


def deliver(
    prospect: ProspectInput,
    report_html_path: Path,
    *,
    email_sender: EmailSender,
    url_publisher: UrlPublisher | None,
    salt: str = "prospect-pipeline",
    fallback_applied: FallbackCode | None = None,
    retry_backoff_s: Sequence[float] = _DEFAULT_RETRY_BACKOFF_S,
    sleep: Callable[[float], None] = time.sleep,
) -> DeliveryResult:
    """Run the dual-channel delivery state machine.

    Per plan §3: email first (retry 3×), URL second (retry 3×). URL is
    ONLY attempted on email-success. `url_publisher is None` OR
    `output.publish_private_url == false` → email-only delivery with
    terminal state `DELIVERED`.
    """
    output_cfg = prospect.raw.get("output", {})
    publish_url = bool(output_cfg.get("publish_private_url", True)) and url_publisher is not None

    # ---- Email channel ----
    email_ok, email_attempts = _retry_with_backoff(
        lambda: email_sender(prospect, report_html_path),
        backoff_s=retry_backoff_s,
        sleep=sleep,
    )

    if not email_ok:
        return DeliveryResult(
            state=DeliveryState.FAILED_EMAIL,
            email_sent_utc=None,
            email_attempt_count=email_attempts,
            url_published_utc=None,
            url_publish_attempt_count=0,
            gated_url_hash=None,
            purge_after_utc=None,
            fallback_applied=fallback_applied.value if fallback_applied else None,
            notes="email failed after retries; URL not attempted",
        )

    email_sent_utc = _now_utc_iso()
    purge_after_utc = output_cfg.get("purge_after_utc")

    # ---- URL channel ----
    if not publish_url:
        return DeliveryResult(
            state=DeliveryState.DELIVERED,
            email_sent_utc=email_sent_utc,
            email_attempt_count=email_attempts,
            url_published_utc=None,
            url_publish_attempt_count=0,
            gated_url_hash=None,
            purge_after_utc=None,
            fallback_applied=fallback_applied.value if fallback_applied else None,
            notes="email-only delivery (publish_private_url=false)",
        )

    date_slug = email_sent_utc[:10]
    gated_hash = compute_gated_url_hash(prospect.prospect_id, salt, date_slug)

    assert url_publisher is not None  # narrowed by publish_url check
    url_ok, url_attempts = _retry_with_backoff(
        lambda: url_publisher(prospect, report_html_path, gated_hash),
        backoff_s=retry_backoff_s,
        sleep=sleep,
    )

    if url_ok:
        return DeliveryResult(
            state=DeliveryState.DELIVERED,
            email_sent_utc=email_sent_utc,
            email_attempt_count=email_attempts,
            url_published_utc=_now_utc_iso(),
            url_publish_attempt_count=url_attempts,
            gated_url_hash=gated_hash[:16],
            purge_after_utc=purge_after_utc,
            fallback_applied=fallback_applied.value if fallback_applied else None,
        )

    return DeliveryResult(
        state=DeliveryState.DELIVERED_EMAIL_ONLY,
        email_sent_utc=email_sent_utc,
        email_attempt_count=email_attempts,
        url_published_utc=None,
        url_publish_attempt_count=url_attempts,
        gated_url_hash=gated_hash[:16],
        purge_after_utc=purge_after_utc,
        fallback_applied=fallback_applied.value if fallback_applied else None,
        notes="URL publish failed after retries; email is authoritative",
    )


def unpublish_url(
    gated_hash: str,
    *,
    deleter: Callable[[str], bool],
    reason: str,
) -> DeliveryResult:
    """Compensating action: delete a previously published gated file.

    Does NOT recall email (not technically possible). Records the
    unpublish reason for inclusion in the deliveries-log row.
    """
    deleter(gated_hash)
    return DeliveryResult(
        state=DeliveryState.UNPUBLISHED,
        email_sent_utc=None,
        email_attempt_count=0,
        url_published_utc=None,
        url_publish_attempt_count=0,
        gated_url_hash=gated_hash[:16],
        purge_after_utc=None,
        fallback_applied=None,
        notes=f"unpublished: {reason}",
    )


# ---------------------------------------------------------------------------
# Subprocess dispatch (STUB — per plan v3 IMPLEMENTATION-STATUS.md)
# ---------------------------------------------------------------------------


def run_demo(bundle: DemoInputBundle, demo_id: int) -> Path:
    """STUB — per IMPLEMENTATION-STATUS.md not-done item.

    Real implementation will subprocess-launch
    `digitalmodel/examples/demos/gtm/demo_0{N}_*.py` with
    `--prospect-data-dir` + `--prospect-env` + `--brand-*` flags.
    """
    raise NotImplementedError(
        "run_demo is scaffolded but not implemented. "
        f"demo_id={demo_id}, bundle.data_dir={bundle.data_dir!s}"
    )


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def _parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        prog="prospect_adapter",
        description=(
            "Validate a prospect intake YAML and route it to a GTM demo. "
            "Validation + materialization are implemented; run_demo is stubbed."
        ),
    )
    parser.add_argument(
        "intake_path",
        type=Path,
        help="Path to a filled prospect intake YAML (see docs/gtm/intake/prospect-template.yaml).",
    )
    parser.add_argument(
        "--demo",
        type=int,
        choices=[1, 2, 3, 4, 5],
        default=None,
        help="Override the target demo (defaults to prospect.target_demo from the YAML).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Validate only; do not attempt to materialize inputs or run a demo.",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv)
    try:
        prospect = load_and_validate(args.intake_path)
    except ProspectIntakeError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 2

    print(
        f"OK validated: {args.intake_path} "
        f"(demo={prospect.target_demo}, company={prospect.company!r})"
    )
    if args.dry_run:
        print("--dry-run set; skipping materialize + run.")
        return 0

    demo_id = args.demo if args.demo is not None else int(prospect.target_demo[-2:])
    try:
        bundle = materialize_demo_inputs(prospect, Path("/tmp") / "prospect-adapter-run")
        run_demo(bundle, demo_id)
    except NotImplementedError as exc:
        print(f"NOT IMPLEMENTED: {exc}", file=sys.stderr)
        return 3
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
