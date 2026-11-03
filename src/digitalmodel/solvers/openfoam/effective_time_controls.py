"""Offline, fail-closed time-control evidence for OpenCFD v2312 interFoam.

The caller supplies resolved top-level readbacks, never included-file fragments.
This module neither parses OpenFOAM syntax nor invokes a solver. Evidence is
caller-observed; the receipt does not authenticate its origin.
"""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import dataclass
from pathlib import PurePosixPath, PureWindowsPath
from types import MappingProxyType
from typing import Mapping


# OpenCFD OpenFOAM v2312 (2023), interFoam.C lines 81-103 selects LTS/global.
SOLVER_SOURCE = "https://api.openfoam.com/2312/interFoam_8C_source.html"
# Same edition, localEulerDdt.C lines 39-43 selects LTS from ddtSchemes/default.
SCHEME_SOURCE = "https://api.openfoam.com/2312/localEulerDdt_8C_source.html"
# Same edition, VoF/setRDeltaT.H lines 4-14, 51-79: PIMPLE ownership and
# conditional interface cap. No source defaults are silently substituted.
LOCAL_SOURCE = (
    "https://api.openfoam.com/2312/multiphase_2VoF_2setRDeltaT_8H_source.html"
)
# Same edition, readTimeControls.H lines 35-42 and VoF/setDeltaT.H lines 36-53:
# global Courant/time-step caps are used only when adjustTimeStep is enabled.
GLOBAL_READ_SOURCE = "https://api.openfoam.com/2312/readTimeControls_8H_source.html"
# Same edition, VoF/alphaCourantNo.H lines 34-37 reads global maxAlphaCo.
GLOBAL_ALPHA_SOURCE = "https://api.openfoam.com/2312/VoF_2alphaCourantNo_8H_source.html"
GLOBAL_SOURCE = (
    "https://api.openfoam.com/2312/"
    "applications_2solvers_2multiphase_2VoF_2setDeltaT_8H_source.html"
)
_CD = "system/controlDict"
_FV = "system/fvSolution"
_SC = "system/fvSchemes"
_PIMPLE = f"{_FV}/PIMPLE"
_CAPS = ("maxCo", "maxAlphaCo", "maxDeltaT")
_CONTROLS = (*_CAPS, "deltaT")


@dataclass(frozen=True)
class SolverIdentity:
    """Distribution, release and executable observed by the caller."""

    distribution: str
    version: str
    solver: str


def _relative_path(path: str) -> None:
    if (
        not isinstance(path, str)
        or not path
        or "\\" in path
        or "\x00" in path
        or PurePosixPath(path).is_absolute()
        or PureWindowsPath(path).drive
        or ".." in PurePosixPath(path).parts
    ):
        raise ValueError("evidence requires relative paths without traversal")


def _hash_files(files: Mapping[str, bytes]) -> Mapping[str, str]:
    result = {}
    for path, content in files.items():
        _relative_path(path)
        if not isinstance(content, bytes):
            raise ValueError("input and restart evidence must contain bytes")
        result[path] = hashlib.sha256(content).hexdigest()
    return MappingProxyType(result)


@dataclass(frozen=True)
class SettingsSnapshot:
    """Captured before or after a trial; construct with capture_snapshot."""

    identity: SolverIdentity
    dictionaries: Mapping[str, str]
    resolved: Mapping[str, str]
    input_hashes: Mapping[str, str]
    restart_hashes: Mapping[str, str]

    def __post_init__(self) -> None:
        for name in ("dictionaries", "resolved", "input_hashes", "restart_hashes"):
            object.__setattr__(self, name, MappingProxyType(dict(getattr(self, name))))

    def sha256(self) -> str:
        """Bind observations to exact snapshots, including resolved readbacks."""
        canonical = json.dumps(
            self.as_dict(),
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
            allow_nan=False,
        )
        return hashlib.sha256(canonical.encode("utf-8")).hexdigest()

    def as_dict(self) -> dict:
        """Preserve dictionary text exactly alongside its UTF-8 digest."""
        return {
            "identity": vars(self.identity).copy(),
            "dictionaries": {
                path: {
                    "text": text,
                    "sha256": hashlib.sha256(text.encode()).hexdigest(),
                }
                for path, text in self.dictionaries.items()
            },
            "resolved": dict(self.resolved),
            "input_hashes": dict(self.input_hashes),
            "restart_hashes": dict(self.restart_hashes),
        }


def capture_snapshot(
    *,
    identity: SolverIdentity,
    dictionaries: Mapping[str, str],
    resolved: Mapping[str, str],
    input_files: Mapping[str, bytes],
    restart_files: Mapping[str, bytes],
) -> SettingsSnapshot:
    """Copy evidence in memory; missing required evidence stays unevaluated.

    ``resolved`` contains full leaf paths queried from the top-level dictionaries
    after include/macro expansion. ``dictionaries`` preserves the original text,
    including any supplied fragments. Input/restart bytes are hashed, not stored.
    """
    for mapping in (dictionaries, resolved):
        for path, value in mapping.items():
            _relative_path(path)
            if not isinstance(value, str):
                raise ValueError("dictionary evidence must contain text")
    return SettingsSnapshot(
        identity,
        MappingProxyType(dict(dictionaries)),
        MappingProxyType(dict(resolved)),
        _hash_files(input_files),
        _hash_files(restart_files),
    )


@dataclass(frozen=True)
class EffectiveControl:
    """A consumed value, an inactive control, or an unsupported observation."""

    path: str | None
    value: float | None
    status: str
    reason: str
    source: str


def _positive(value: object) -> float | None:
    # Strict positivity is a domain requirement for time intervals and Courant
    # denominators (v2312 VoF/setRDeltaT.H lines 61-79), not a fitted limit.
    if isinstance(value, bool):
        return None
    try:
        number = float(value)
    except (TypeError, ValueError, OverflowError):
        return None
    return number if math.isfinite(number) and number > 0 else None


def resolve_time_controls(snapshot: SettingsSnapshot) -> dict[str, EffectiveControl]:
    """Resolve only the pinned contract; never infer omitted solver defaults.

    Euler deltaT is supported only with fixed global stepping. Its interaction
    with adaptive stepping and local pseudo-time is deliberately unevaluated.
    """
    unknown = {
        key: EffectiveControl(
            None, None, "NOT_EVALUATED", "unsupported_contract", SOLVER_SOURCE
        )
        for key in _CONTROLS
    }
    entries = snapshot.resolved
    if (
        snapshot.identity != SolverIdentity("OpenCFD", "v2312", "interFoam")
        or entries.get(f"{_CD}/application") != snapshot.identity.solver
    ):
        return unknown
    scheme = entries.get(f"{_SC}/ddtSchemes/default")
    if scheme == "localEuler":
        origin, source, enabled = _PIMPLE, LOCAL_SOURCE, True
    elif scheme == "Euler":
        switch = entries.get(f"{_CD}/adjustTimeStep")
        # Require canonical yes/no readback; do not guess alternate spellings.
        if switch not in ("yes", "no"):
            return unknown
        origin, source = _CD, GLOBAL_SOURCE
        enabled = switch == "yes"
    else:
        return unknown
    result = dict(unknown)
    for key in _CAPS:
        path = f"{origin}/{key}"
        value = _positive(entries.get(path))
        status = "ACTIVE" if enabled else "INACTIVE"
        reason = "consumed_by_time_scheme" if enabled else "adjustTimeStep_disabled"
        if enabled and value is None:
            status, reason = "NOT_EVALUATED", "missing_or_invalid_active_value"
        result[key] = EffectiveControl(path, value, status, reason, source)
    if scheme == "localEuler":
        co, alpha = result["maxCo"], result["maxAlphaCo"]
        if co.value is None:
            result["maxAlphaCo"] = EffectiveControl(
                alpha.path,
                alpha.value,
                "NOT_EVALUATED",
                "missing_maxCo_condition",
                source,
            )
        elif alpha.value is not None and alpha.value >= co.value:
            result["maxAlphaCo"] = EffectiveControl(
                alpha.path,
                alpha.value,
                "INACTIVE",
                "interface_cap_condition_false",
                source,
            )
    elif not enabled:
        path = f"{_CD}/deltaT"
        value = _positive(entries.get(path))
        result["deltaT"] = EffectiveControl(
            path,
            value,
            "ACTIVE" if value is not None else "NOT_EVALUATED",
            "fixed_global_step"
            if value is not None
            else "missing_or_invalid_active_value",
            GLOBAL_SOURCE,
        )
    return result


def _target_check(
    path: str,
    requested: object,
    before: Mapping[str, EffectiveControl],
    after: Mapping[str, EffectiveControl],
) -> tuple[str, str]:
    key = path.rsplit("/", 1)[-1]
    if key not in after:
        return "NOT_EVALUATED", "unsupported_target"
    old, new = before[key], after[key]
    if old.path is None or new.path is None:
        return "NOT_EVALUATED", "unsupported_contract"
    if path != new.path or path != old.path:
        return "FAIL", "inactive_target_path"
    if new.status == "INACTIVE":
        return "FAIL", "inactive_target"
    if new.status != "ACTIVE" or old.value is None:
        return "NOT_EVALUATED", "missing_active_readback"
    expected = _positive(requested)
    if expected is None:
        return "NOT_EVALUATED", "invalid_requested_value"
    # Exact equality compares caller-normalized readbacks, not CFD convergence.
    # No tolerance is silently introduced for an authored dictionary value.
    if new.value != expected:
        return "FAIL", "readback_mismatch"
    if new.value == old.value:
        return "FAIL", "unchanged_active_value"
    return "PASS", "active_value_changed"


@dataclass(frozen=True)
class TimeStepResponse:
    """Matched observations in seconds (deltaT) or inverse seconds (rDeltaT).

    The caller supplies a minimum absolute change and its engineering basis.
    No numerical acceptance limit is built in. Paired local samples must refer
    to the same cells in the same order; global samples to comparable steps.
    """

    metric: str
    before_values: tuple[float, ...]
    after_values: tuple[float, ...]
    before_snapshot_sha256: str
    after_snapshot_sha256: str
    minimum_change: float
    criterion_source: str

    def __post_init__(self) -> None:
        object.__setattr__(self, "before_values", tuple(self.before_values))
        object.__setattr__(self, "after_values", tuple(self.after_values))


def _response_check(
    response: TimeStepResponse | None,
    before: SettingsSnapshot,
    after: SettingsSnapshot,
) -> tuple[str, str, dict | None]:
    if response is None:
        return "NOT_EVALUATED", "missing_measured_response", None
    if (
        response.before_snapshot_sha256 != before.sha256()
        or response.after_snapshot_sha256 != after.sha256()
    ):
        return "FAIL", "response_snapshot_mismatch", None
    scheme = after.resolved.get(f"{_SC}/ddtSchemes/default")
    if scheme not in ("localEuler", "Euler"):
        return "NOT_EVALUATED", "unsupported_response_scheme", None
    metric = "rDeltaT" if scheme == "localEuler" else "deltaT"
    if response.metric != metric:
        return "FAIL", "wrong_time_step_metric", None
    old, new = response.before_values, response.after_values
    if (
        not old
        or len(old) != len(new)
        or any(
            not isinstance(value, (int, float)) or _positive(value) is None
            for value in (*old, *new)
        )
    ):
        return "FAIL", "invalid_response_samples", None
    try:
        limit = float(response.minimum_change)
    except (ValueError, TypeError, OverflowError):
        return "NOT_EVALUATED", "missing_or_invalid_response_criterion", None
    if (
        isinstance(response.minimum_change, bool)
        or not isinstance(response.minimum_change, (int, float))
        or not math.isfinite(limit)
        or limit < 0
        or not isinstance(response.criterion_source, str)
        or not response.criterion_source.strip()
    ):
        return "NOT_EVALUATED", "missing_or_invalid_response_criterion", None
    # Independent observation arithmetic: D = max_i |after_i - before_i|.
    # The strict D > caller limit is this API's explicit evidence criterion;
    # it is not a CFD stability bound, solver prediction or causal attribution.
    difference = max(abs(right - left) for left, right in zip(old, new))
    record = {
        "metric": metric,
        "units": "1/s" if metric == "rDeltaT" else "s",
        "before_values": list(old),
        "after_values": list(new),
        "before_snapshot_sha256": response.before_snapshot_sha256,
        "after_snapshot_sha256": response.after_snapshot_sha256,
        "minimum_change": limit,
        "criterion_source": response.criterion_source,
        "maximum_absolute_change": difference,
    }
    if difference <= limit:
        return "FAIL", "response_change_not_above_threshold", record
    return "PASS", "measured_time_step_response_changed", record


def verify_experiment(
    *,
    requested_identity: SolverIdentity,
    before: SettingsSnapshot,
    after: SettingsSnapshot,
    edits: Mapping[str, object],
    response: TimeStepResponse | None = None,
) -> dict:
    """Return one receipt; effective edits alone never establish execution.

    Every edit is a full consumed leaf path and its requested positive value.
    All requested edits must take effect, every required active control must
    have valid readbacks in both snapshots, and unrequested resolved settings
    must remain unchanged. REJECTED takes precedence over incomplete evidence.
    """
    checks = []

    def record(name: str, status: str, reason: str) -> None:
        checks.append({"name": name, "status": status, "reason": reason})

    identities_match = before.identity == after.identity == requested_identity
    applications_match = all(
        snap.resolved.get(f"{_CD}/application") == requested_identity.solver
        for snap in (before, after)
    )
    record(
        "solver_identity",
        "PASS" if identities_match and applications_match else "FAIL",
        "requested_actual_identity_match"
        if identities_match and applications_match
        else "requested_actual_identity_mismatch",
    )
    scheme_path = f"{_SC}/ddtSchemes/default"
    schemes_match = before.resolved.get(scheme_path) == after.resolved.get(scheme_path)
    record(
        "time_scheme",
        "PASS" if schemes_match else "FAIL",
        "time_scheme_unchanged" if schemes_match else "time_scheme_changed",
    )
    dictionaries_present = all(
        all(snap.dictionaries.get(path, "").strip() for path in (_CD, _FV, _SC))
        for snap in (before, after)
    )
    record(
        "dictionaries",
        "PASS" if dictionaries_present else "NOT_EVALUATED",
        "pre_post_dictionaries_preserved"
        if dictionaries_present
        else "missing_dictionary",
    )
    active_before = resolve_time_controls(before)
    active_after = resolve_time_controls(after)
    for phase, resolved in (("before", active_before), ("after", active_after)):
        for key, control in resolved.items():
            # No path denotes an unsupported control, including adaptive/local
            # deltaT. Established inactive controls need no active readback.
            if control.path is None or control.status == "INACTIVE":
                continue
            record(
                f"active_control:{phase}:{key}",
                "PASS" if control.status == "ACTIVE" else "NOT_EVALUATED",
                control.reason,
            )
    if not edits:
        record("targets", "NOT_EVALUATED", "no_requested_edits")
    for path, value in edits.items():
        _relative_path(path)
        record(
            f"target:{path}", *_target_check(path, value, active_before, active_after)
        )
    unchanged = {
        key: value for key, value in before.resolved.items() if key not in edits
    } == {key: value for key, value in after.resolved.items() if key not in edits}
    record(
        "untargeted_settings",
        "PASS" if unchanged else "FAIL",
        "untargeted_readbacks_unchanged"
        if unchanged
        else "unrequested_settings_changed",
    )
    for name in ("input_hashes", "restart_hashes"):
        old, new = getattr(before, name), getattr(after, name)
        if not old or not new:
            record(name, "NOT_EVALUATED", "missing_provenance")
        elif old != new:
            record(name, "FAIL", "trial_source_changed")
        else:
            record(name, "PASS", "trial_source_unchanged")
    response_status, response_reason, response_record = _response_check(
        response, before, after
    )
    record("response", response_status, response_reason)
    if any(row["status"] == "FAIL" for row in checks):
        status = "REJECTED"
    elif any(row["status"] == "NOT_EVALUATED" for row in checks):
        status = "NOT_EVALUATED"
    else:
        status = "EXECUTED"
    return {
        "status": status,
        "executed": status == "EXECUTED",
        "requested_identity": vars(requested_identity).copy(),
        "requested_edits": {path: _positive(value) for path, value in edits.items()},
        "checks": checks,
        "before": before.as_dict(),
        "after": after.as_dict(),
        "before_snapshot_sha256": before.sha256(),
        "after_snapshot_sha256": after.sha256(),
        "response": response_record,
        "effective_before": {
            key: vars(value).copy() for key, value in active_before.items()
        },
        "effective_after": {
            key: vars(value).copy() for key, value in active_after.items()
        },
    }
