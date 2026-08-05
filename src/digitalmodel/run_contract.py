"""The single place a run's verdict becomes a process exit status.

Issue #1631. ``python -m digitalmodel <input.yml>`` had no failure exit path
for the engine contract: ``__main__.main()`` ended with a bare ``engine()``
whose return value was discarded, so the command exited 0 no matter what the
engine concluded, on any host, licensed or not. Deckhand consumes that command
through a thin ``subprocess.run`` wrapper
(``deckhand/src/deckhand/licensed_run_agent_runtime.py:37-50``), so the exit
status is the entire success signal. Twelve licensed runs drained with
``returncode: 0``; two of them carried a validator ``FAIL``.

This module owns the mapping in one place rather than letting each workflow
call :func:`sys.exit` itself, which is how the pre-solve and post-solve gates
diverged to begin with (``diffraction/quality_gates.py`` raises on a blocking
``FAIL`` before a solve; nothing did the equivalent after one).

**The verdict vocabulary is not defined here.** It is the pre-existing closed
five-value contract from
:mod:`digitalmodel.hydrodynamics.diffraction.validation_runner`, specified
under #611/#625 long before the runs this module exists to catch. Nothing in
this module retunes that boundary; doing so would be fitting a constant to the
evidence being judged.

Which verdicts refuse
---------------------

``FAIL`` and ``ERROR`` refuse. ``PASS``, ``WARNING`` and ``SKIPPED`` do not.

``WARNING`` deliberately does **not** refuse. Five of the seven validated runs
in the queue are ``WARNING``; a gate that refused them would read as rigorous
and would make the lane unusable within a day. A gate that never passes is as
useless as one that never fails, and harder to notice.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from digitalmodel.hydrodynamics.diffraction.validation_runner import (
    ALL_VERDICTS,
    VERDICT_ERROR,
    VERDICT_FAIL,
    VERDICT_SKIPPED,
)

#: Verdicts that make a run refuse. Reused from validation_runner, not redefined.
REFUSING_VERDICTS = frozenset({VERDICT_FAIL, VERDICT_ERROR})

#: Everything else in the closed vocabulary.
PASSING_VERDICTS = frozenset(ALL_VERDICTS) - REFUSING_VERDICTS

#: Process exit statuses. Convention, not measurement.
EXIT_SUCCESS = 0
EXIT_REFUSED = 1

#: Name of the structured sidecar, so no consumer regexes stdout_tail again.
SIDECAR_NAME = "run_verdict.json"


@dataclass
class RunVerdict:
    """What a run concluded, in a form an exit status can be derived from."""

    verdict: str
    workflow: str
    solver_available: bool = False
    solver_identity: str | None = None
    artifacts: list[str] = field(default_factory=list)
    issues: list[str] = field(default_factory=list)
    output_dir: str | None = None

    def __post_init__(self) -> None:
        if self.verdict not in ALL_VERDICTS:
            raise ValueError(
                f"Unknown run verdict {self.verdict!r}. The vocabulary is closed "
                f"at {sorted(ALL_VERDICTS)} (digitalmodel.hydrodynamics."
                f"diffraction.validation_runner)."
            )

    @property
    def host_kind(self) -> str:
        """Recorded positively, so a consumer reads one field instead of logs."""
        return "licensed" if self.solver_available else "unlicensed"

    def is_refusal(self) -> bool:
        if self.verdict in REFUSING_VERDICTS:
            return True
        # A completed run that produced nothing does not pass. Reproduces
        # lr_acma_ff132001b7ad, which recorded returncode 0 with no
        # returned_files key at all -- indistinguishable, at the result level,
        # from a run that produced everything.
        #
        # SKIPPED is exempt: an explicitly requested dry run legitimately
        # produces no solve artifacts, and D4 keeps unlicensed development
        # possible.
        if self.verdict != VERDICT_SKIPPED and not self.artifacts:
            return True
        return False

    def to_dict(self) -> dict[str, Any]:
        return {
            "verdict": self.verdict,
            "workflow": self.workflow,
            "solver_available": self.solver_available,
            "solver_identity": self.solver_identity,
            "host_kind": self.host_kind,
            "artifacts": list(self.artifacts),
            "issues": list(self.issues),
            "refused": self.is_refusal(),
        }


def exit_status_for(verdict: RunVerdict) -> int:
    """Map a verdict to the process exit status Deckhand observes."""
    return EXIT_REFUSED if verdict.is_refusal() else EXIT_SUCCESS


def write_run_verdict(verdict: RunVerdict, output_dir: Path | str) -> Path:
    """Write the structured sidecar next to the run's outputs."""
    directory = Path(output_dir)
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / SIDECAR_NAME
    path.write_text(json.dumps(verdict.to_dict(), indent=2, sort_keys=True) + "\n")
    return path


def from_cfg(cfg: Any) -> RunVerdict | None:
    """Build a verdict from a workflow's returned config.

    Returns ``None`` when the workflow declares no verdict, which keeps every
    un-migrated workflow exiting 0 exactly as before.
    """
    if not isinstance(cfg, dict):
        return None

    section = _verdict_section(cfg)
    if section is None:
        return None

    raw = section.get("validation_verdict")
    if raw is None:
        return None

    outputs = section.get("outputs") or {}
    artifacts: list[str] = []
    if isinstance(outputs, dict):
        for value in outputs.values():
            if isinstance(value, str) and value:
                artifacts.append(value)
            elif isinstance(value, list):
                artifacts.extend(str(item) for item in value if item)

    return RunVerdict(
        verdict=str(raw),
        workflow=str(cfg.get("basename", "unknown")),
        solver_available=bool(section.get("solver_available", False)),
        solver_identity=section.get("solver_identity"),
        artifacts=artifacts,
        issues=list(section.get("validation_issues") or []),
        output_dir=section.get("output_directory"),
    )


def _verdict_section(cfg: dict) -> dict | None:
    """Find the workflow section carrying the verdict.

    Prefers the section named by ``basename`` so the lookup cannot latch onto
    an unrelated key, then falls back to a scan for workflows whose section
    name differs from their basename.
    """
    basename = cfg.get("basename")
    if isinstance(basename, str):
        candidate = cfg.get(basename)
        if isinstance(candidate, dict) and "validation_verdict" in candidate:
            return candidate

    for key, value in cfg.items():
        if key.startswith("_"):
            continue
        if isinstance(value, dict) and "validation_verdict" in value:
            return value
    return None


def solver_provenance(module_name: str = "OrcFxAPI") -> tuple[bool, str | None]:
    """Determine positively whether the solver API is importable here.

    Mirrors ``solvers/smoke/probes.py:check_orcaflex``: report honestly rather
    than degrade to a silent fallback. Returns ``(available, identity)``.
    """
    try:
        module = __import__(module_name)
    except Exception:
        return False, None

    version = None
    for attr in ("DLLVersion", "Version"):
        getter = getattr(module, attr, None)
        if callable(getter):
            try:
                version = str(getter())
                break
            except Exception:  # pragma: no cover - version call is advisory
                continue
    return True, f"{module_name} {version}" if version else module_name


def executable_provenance(name: str) -> tuple[bool, str | None]:
    """Positively locate a solver driven by an executable rather than an API.

    AQWA is resolved this way (``aqwa_runner`` falls back to
    ``shutil.which("aqwa")``), so the same one-field answer covers both
    diffraction solvers.
    """
    import shutil

    found = shutil.which(name)
    return (found is not None), found
