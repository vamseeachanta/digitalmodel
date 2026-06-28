"""Generic, intent-type-agnostic plumbing for LLM spec-authoring front-ends.

This module promotes the reusable pieces shared by the diffraction
(``hydrodynamics.diffraction.spec_author``) and OrcaFlex
(``solvers.orcaflex.spec_author``) authoring front-ends so they are not
duplicated per domain:

* :class:`ProjectBundle` -- the upstream project single-source-of-truth handed
  to the LLM, rendered to text via :meth:`ProjectBundle.to_prompt_context`.
* :class:`ProvenanceEntry` -- one value the LLM grounded, with its source.
* :func:`_extract_json_object` -- pull the first balanced JSON object out of
  model text (tolerates ```` ```json ```` fences and surrounding prose).
* :data:`CliRunner` / :func:`_default_cli_runner` -- the injectable
  ``claude -p`` subprocess hook (so the front-ends stay testable offline).
* :func:`claude_cli_complete` -- a generic ``claude -p`` JSON caller that
  returns the parsed inner JSON object.
* :func:`build_outcome_menu` -- render an outcome-description map as a menu.

It is intentionally dependency-light: only ``pydantic`` (and stdlib). The
``anthropic`` SDK is *not* imported here -- domain front-ends import it lazily.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path
from typing import Any, Callable, Optional

from pydantic import BaseModel, ConfigDict, Field, field_validator

# ---------------------------------------------------------------------------
# Project single-source-of-truth contract (LLM input context)
# ---------------------------------------------------------------------------


class ProjectBundle(BaseModel):
    """The upstream project SSOT handed to the authoring LLM.

    Deliberately loose *inside* the data slots (``vessel_particulars`` /
    ``environment`` are open dicts; ``documents`` / ``notes`` carry free text) so
    heterogeneous real project data fits. The *contract* around them is hardened:

    * unknown TOP-LEVEL keys are rejected (``extra="forbid"``) -- a misspelled
      ``vessel_particular`` is an error, not a silently-dropped field;
    * ``units`` declares the unit system explicitly (silent unit assumptions are
      a classic engineering footgun) and :meth:`contract_warnings` flags it when
      missing alongside numeric data;
    * ``codes`` / ``references`` carry design standards and citations, surfaced
      to the LLM so it can ground against them.

    The LLM reads :meth:`to_prompt_context` and extracts only what it can ground;
    the deterministic resolver fills and ledgers the rest.
    """

    model_config = ConfigDict(extra="forbid")

    project_name: Optional[str] = None
    units: Optional[str] = Field(
        default=None,
        description="Declared unit system for numeric data, e.g. 'SI' or 'metric'.",
    )
    vessel_particulars: dict[str, Any] = Field(default_factory=dict)
    environment: dict[str, Any] = Field(default_factory=dict)
    codes: list[str] = Field(
        default_factory=list, description="Design codes / standards in scope."
    )
    references: list[str] = Field(
        default_factory=list, description="Citations / source links."
    )
    documents: list[str] = Field(default_factory=list)
    notes: Optional[str] = None

    @field_validator("project_name", "units", "notes")
    @classmethod
    def _blank_to_none(cls, v: Optional[str]) -> Optional[str]:
        if v is None:
            return None
        v = v.strip()
        return v or None

    @field_validator("documents", "codes", "references")
    @classmethod
    def _drop_empty_strings(cls, v: list[str]) -> list[str]:
        return [s for s in (item.strip() for item in v) if s]

    @classmethod
    def from_yaml(cls, path: str | Path) -> "ProjectBundle":
        import yaml

        with open(path) as f:
            data = yaml.safe_load(f) or {}
        return cls.model_validate(data)

    @classmethod
    def to_json_schema(cls) -> dict[str, Any]:
        """JSON Schema for the project-SSOT contract (for docs / validation)."""
        return cls.model_json_schema()

    def is_empty(self) -> bool:
        """True when the bundle carries no project data at all."""
        return not any(
            (
                self.project_name,
                self.vessel_particulars,
                self.environment,
                self.codes,
                self.references,
                self.documents,
                self.notes,
            )
        )

    def contract_warnings(self) -> list[str]:
        """Non-fatal gaps worth surfacing before authoring (advisory, not fatal).

        Empty list means the bundle is well-formed. The resolver still ledgers
        every assumed value downstream regardless.
        """
        warnings: list[str] = []
        if self.is_empty():
            warnings.append("Project bundle is empty -- nothing to ground against.")
        if (self.vessel_particulars or self.environment) and self.units is None:
            warnings.append(
                "No 'units' declared while numeric particulars are present -- "
                "unit interpretation is ambiguous; declare e.g. units: SI."
            )
        return warnings

    def to_prompt_context(self) -> str:
        """Render the bundle as text for the LLM."""
        lines: list[str] = []
        if self.project_name:
            lines.append(f"Project: {self.project_name}")
        if self.units:
            lines.append(f"Units: {self.units}")
        if self.codes:
            lines.append("Design codes / standards: " + ", ".join(self.codes))
        if self.vessel_particulars:
            lines.append("\nVessel particulars (as provided):")
            for key, value in self.vessel_particulars.items():
                lines.append(f"  - {key}: {value}")
        if self.environment:
            lines.append("\nEnvironment / site (as provided):")
            for key, value in self.environment.items():
                lines.append(f"  - {key}: {value}")
        if self.notes:
            lines.append(f"\nNotes:\n{self.notes}")
        if self.references:
            lines.append("\nReferences:")
            for ref in self.references:
                lines.append(f"  - {ref}")
        for i, doc in enumerate(self.documents, 1):
            lines.append(f"\n--- Document {i} ---\n{doc}")
        return "\n".join(lines).strip() or "(no project data provided)"


# ---------------------------------------------------------------------------
# Shared structured-output fragment
# ---------------------------------------------------------------------------


class ProvenanceEntry(BaseModel):
    """One value the LLM set, with where in the project data it came from."""

    field: str
    value: str
    source: str = Field(description="Where in the project bundle this was found.")


# ---------------------------------------------------------------------------
# Outcome menu rendering
# ---------------------------------------------------------------------------


def build_outcome_menu(descriptions: dict[Any, str]) -> str:
    """Render an ``{outcome: description}`` map as a bulleted menu.

    Keys may be ``Enum`` members (their ``.value`` is used) or plain strings.
    """
    lines: list[str] = []
    for outcome, desc in descriptions.items():
        value = getattr(outcome, "value", outcome)
        lines.append(f"  - {value}: {desc}")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# JSON extraction from model text
# ---------------------------------------------------------------------------


def _extract_json_object(text: str) -> str:
    """Pull the first top-level JSON object out of model text.

    Tolerates ```` ```json ```` fences and surrounding prose by scanning for
    the first balanced ``{...}`` (brace depth, string-aware).
    """
    start = text.find("{")
    if start == -1:
        raise ValueError(f"No JSON object in model output: {text[:200]!r}")
    depth = 0
    in_string = False
    escaped = False
    for i in range(start, len(text)):
        ch = text[i]
        if in_string:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                in_string = False
            continue
        if ch == '"':
            in_string = True
        elif ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return text[start : i + 1]
    raise ValueError(f"Unbalanced JSON object in model output: {text[:200]!r}")


# ---------------------------------------------------------------------------
# claude -p CLI plumbing
# ---------------------------------------------------------------------------

# Runs the CLI: (argv, stdin) -> CompletedProcess. Injectable for tests.
CliRunner = Callable[[list[str], str], "subprocess.CompletedProcess[str]"]


def _default_cli_runner(
    argv: list[str], stdin: str
) -> "subprocess.CompletedProcess[str]":
    return subprocess.run(
        argv, input=stdin, capture_output=True, text=True, timeout=300
    )


def claude_cli_complete(
    system_prompt: str,
    user_prompt: str,
    *,
    model: str,
    cli: str = "claude",
    extra_args: Optional[list[str]] = None,
    runner: Optional[CliRunner] = None,
) -> dict[str, Any]:
    """Call the ``claude -p`` CLI and return the parsed inner JSON object.

    Runs ``claude -p --output-format json --model <model> --system-prompt <s>``
    with ``user_prompt`` on stdin, using the local Claude Code install and its
    existing auth -- no ``ANTHROPIC_API_KEY`` and no ``anthropic`` dependency.

    The CLI wraps the model's reply in an envelope (``{"is_error": ..., \
"result": ...}``). This helper validates the envelope and returns the JSON
    object found inside ``result`` (tolerating fences / prose).

    Raises ``RuntimeError`` on a non-zero return code, an unparseable envelope,
    or an error envelope (``is_error`` true).
    """
    runner = runner or _default_cli_runner
    argv = [
        cli,
        "-p",
        "--output-format",
        "json",
        "--model",
        model,
        "--system-prompt",
        system_prompt,
        *(extra_args or []),
    ]
    proc = runner(argv, user_prompt)
    if proc.returncode != 0:
        raise RuntimeError(
            f"`{cli} -p` failed (rc={proc.returncode}): "
            f"{(proc.stderr or proc.stdout or '').strip()[:500]}"
        )
    try:
        envelope = json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(
            f"Could not parse `{cli} -p` JSON envelope: {proc.stdout[:500]!r}"
        ) from exc
    if envelope.get("is_error"):
        raise RuntimeError(f"claude returned an error: {envelope.get('result')}")
    result_text = envelope.get("result", "")
    return json.loads(_extract_json_object(result_text))


__all__ = [
    "ProjectBundle",
    "ProvenanceEntry",
    "build_outcome_menu",
    "claude_cli_complete",
    "CliRunner",
]
