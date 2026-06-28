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

from pydantic import BaseModel, Field

# ---------------------------------------------------------------------------
# Project single-source-of-truth contract (LLM input context)
# ---------------------------------------------------------------------------


class ProjectBundle(BaseModel):
    """The upstream project SSOT handed to the authoring LLM.

    Intentionally loose: real project data is heterogeneous. Structured fields
    are convenience slots; ``documents`` and ``notes`` carry whatever free text
    the project has (data-sheet excerpts, emails, scope notes). The LLM reads
    the whole thing rendered as text and extracts what it can ground.
    """

    project_name: Optional[str] = None
    vessel_particulars: dict[str, Any] = Field(default_factory=dict)
    environment: dict[str, Any] = Field(default_factory=dict)
    documents: list[str] = Field(default_factory=list)
    notes: Optional[str] = None

    @classmethod
    def from_yaml(cls, path: str | Path) -> "ProjectBundle":
        import yaml

        with open(path) as f:
            data = yaml.safe_load(f) or {}
        return cls.model_validate(data)

    def to_prompt_context(self) -> str:
        """Render the bundle as text for the LLM."""
        lines: list[str] = []
        if self.project_name:
            lines.append(f"Project: {self.project_name}")
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
