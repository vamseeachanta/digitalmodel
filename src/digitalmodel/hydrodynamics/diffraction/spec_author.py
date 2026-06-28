"""LLM spec-authoring: project SSOT + natural-language request -> analysis SSOT.

This is the **front-end** of the model-generation pipeline (digitalmodel#1095):

    project single-source-of-truth
        |  LLM authors a structured intent (this module)
        v
    AuthoredIntent  (outcome + only the facts stated in the project data)
        |  deterministic inverse resolver (resolver.resolve, #622)
        v
    DiffractionSpec  +  AssumptionLedger   <-- the analysis single-source-of-truth
        |  spec_converter / *_backend
        v
    OrcaWave .yml / AQWA .dat   (licensed-program input)

Responsibility split (deliberate, to keep the hallucination surface small):

* The **LLM** only *extracts facts that are stated* in the project bundle and
  picks the matching outcome. It must leave anything it cannot ground as
  ``None`` and record provenance for every value it does set. It never invents
  vessel dimensions or physics.
* The **deterministic resolver** fills every remaining field from maintained
  reference data and records each filled value in the ``AssumptionLedger`` --
  the #622 "no silent assumptions" contract. Engineering assumptions therefore
  live in auditable code, not in the model output.

The LLM call is injected through the :class:`IntentAuthor` protocol so the core
is fully unit-testable offline (pass a stub author). The real implementation,
:class:`AnthropicIntentAuthor`, uses the Anthropic SDK with structured outputs
and is imported lazily -- ``anthropic`` is not a project dependency.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional, Protocol

from pydantic import BaseModel, Field

from digitalmodel.common.spec_authoring import (
    CliRunner,
    ProjectBundle,
    ProvenanceEntry,
    build_outcome_menu,
    claude_cli_complete,
)
from digitalmodel.hydrodynamics.diffraction.assumption_ledger import AssumptionLedger
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.resolver import (
    OUTCOME_DESCRIPTIONS,
    Outcome,
    PrincipalDimensions,
    ResolverConfig,
    ResolverInputs,
    resolve,
)

# Default model for the authoring call. Kept here (not hard-coded at the call
# site) so it is easy to override and easy to find.
DEFAULT_MODEL = "claude-opus-4-8"


# ---------------------------------------------------------------------------
# Structured LLM output: the authored intent
# ---------------------------------------------------------------------------
#
# ``ProjectBundle`` and ``ProvenanceEntry`` are the generic project-SSOT contract
# and provenance fragment, shared with the OrcaFlex front-end via
# ``digitalmodel.common.spec_authoring`` (imported above and re-exported here for
# back-compat).


class AuthoredIntent(BaseModel):
    """Structured output the LLM produces from a project bundle + request.

    This is *not* the full spec -- only the sparse, grounded facts the resolver
    needs as a starting point, plus the chosen outcome. Every numeric field is
    optional: the LLM leaves a field ``None`` when the project data does not
    state it, and the deterministic resolver fills it (and ledgers it).
    """

    outcome: Outcome = Field(
        description="The analysis outcome that matches the request."
    )
    # Geometry / identification
    mesh_file: Optional[str] = Field(
        default=None, description="Mesh file path if explicitly provided."
    )
    hull_id: Optional[str] = Field(
        default=None, description="Reference hull identifier if named."
    )
    # Principal dimensions (metres / tonnes) -- only if stated in the data
    loa: Optional[float] = Field(default=None, description="Length overall (m).")
    length_bp: Optional[float] = Field(
        default=None, description="Length between perpendiculars (m)."
    )
    beam: Optional[float] = Field(default=None, description="Beam (m).")
    draft: Optional[float] = Field(default=None, description="Draft (m).")
    displacement_t: Optional[float] = Field(
        default=None, description="Displacement (tonnes)."
    )
    block_coefficient: Optional[float] = Field(
        default=None, description="Block coefficient Cb (-)."
    )
    # Environment
    water_depth: Optional[float] = Field(
        default=None, description="Water depth (m); omit if deep/infinite."
    )
    water_depth_infinite: bool = Field(
        default=False,
        description="True if the site is deep water / infinite depth.",
    )
    inertia_mode: str = Field(
        default="free_floating",
        description="'free_floating' or 'explicit' inertia handling.",
    )
    # Trace + handoff
    provenance: list[ProvenanceEntry] = Field(
        default_factory=list,
        description="One entry per value set, citing the project-data source.",
    )
    open_questions: list[str] = Field(
        default_factory=list,
        description="Material facts the project data did not provide.",
    )
    rationale: str = Field(
        default="",
        description="One short paragraph on why this outcome and these inputs.",
    )


# ---------------------------------------------------------------------------
# Intent author protocol + Anthropic implementation
# ---------------------------------------------------------------------------


class IntentAuthor(Protocol):
    """Anything that turns (bundle, request) into an AuthoredIntent."""

    def author(self, bundle: ProjectBundle, request: str) -> AuthoredIntent: ...


_SYSTEM_PROMPT_TEMPLATE = """\
You are a marine-hydrodynamics analysis spec author. Given a project's data and \
a natural-language request, you produce a structured authoring intent for a \
diffraction/radiation (seakeeping/RAO) analysis.

Choose the single `outcome` that best matches the request:
{outcome_menu}

Your job is fact extraction, not engineering invention:
- Fill a field ONLY with a value the project data actually states. If the data \
does not give a value, leave it null. Do NOT estimate, default, or invent \
vessel dimensions, mass, water depth, or any physical quantity -- a downstream \
deterministic resolver fills every gap from maintained reference data and \
records each assumption for review. Inventing values defeats that audit trail.
- For every value you DO set, add a `provenance` entry citing where in the \
project data you found it.
- List anything material but missing in `open_questions`.
- Set `water_depth_infinite` true only if the site is explicitly deep/infinite \
water; otherwise give `water_depth` if stated, else leave both unset.
- Keep `rationale` to one short paragraph.
"""


def build_system_prompt() -> str:
    """System prompt with the outcome menu rendered from the resolver."""
    return _SYSTEM_PROMPT_TEMPLATE.format(
        outcome_menu=build_outcome_menu(OUTCOME_DESCRIPTIONS)
    )


# Rendered once at import for convenience; ``build_system_prompt()`` stays the
# source of truth if the outcome set changes at runtime.
SYSTEM_PROMPT = build_system_prompt()


class AnthropicIntentAuthor:
    """Real intent author backed by the Anthropic SDK (structured outputs).

    ``anthropic`` is imported lazily so it stays an optional dependency. Install
    with ``uv run --with anthropic`` (or add it to the environment) and set
    ``ANTHROPIC_API_KEY``.
    """

    def __init__(self, model: str = DEFAULT_MODEL, client: Any = None) -> None:
        self.model = model
        self._client = client

    def _get_client(self) -> Any:
        if self._client is not None:
            return self._client
        try:
            import anthropic
        except ImportError as exc:  # pragma: no cover - env-dependent
            raise RuntimeError(
                "AnthropicIntentAuthor needs the 'anthropic' package. Run with "
                "`uv run --with anthropic ...` and set ANTHROPIC_API_KEY, or "
                "inject a stub IntentAuthor for offline use."
            ) from exc
        self._client = anthropic.Anthropic()
        return self._client

    def author(self, bundle: ProjectBundle, request: str) -> AuthoredIntent:
        client = self._get_client()
        user_prompt = (
            f"Analysis request:\n{request}\n\n"
            f"Project data:\n{bundle.to_prompt_context()}"
        )
        response = client.messages.parse(
            model=self.model,
            max_tokens=4000,
            thinking={"type": "adaptive"},
            output_config={"effort": "high"},
            system=SYSTEM_PROMPT,
            messages=[{"role": "user", "content": user_prompt}],
            output_format=AuthoredIntent,
        )
        return response.parsed_output


class ClaudeCliIntentAuthor:
    """Intent author backed by the ``claude -p`` CLI (Claude Code).

    Uses the local Claude Code install and its existing auth -- no
    ``ANTHROPIC_API_KEY`` and no ``anthropic`` dependency. The CLI does not
    expose typed structured outputs, so the schema is supplied in the system
    prompt and the JSON object is parsed (and Pydantic-validated) from the
    response via the shared ``claude_cli_complete`` helper. Default author.
    """

    def __init__(
        self,
        model: str = DEFAULT_MODEL,
        *,
        cli: str = "claude",
        extra_args: Optional[list[str]] = None,
        runner: Optional[CliRunner] = None,
    ) -> None:
        self.model = model
        self.cli = cli
        self.extra_args = list(extra_args or [])
        self._runner = runner

    def _system_prompt(self) -> str:
        schema = json.dumps(AuthoredIntent.model_json_schema())
        return (
            SYSTEM_PROMPT
            + "\n\nReturn ONLY a single JSON object conforming to this JSON "
            "schema -- no prose, no markdown, no code fences:\n" + schema
        )

    def author(self, bundle: ProjectBundle, request: str) -> AuthoredIntent:
        user_prompt = (
            f"Analysis request:\n{request}\n\n"
            f"Project data:\n{bundle.to_prompt_context()}"
        )
        data = claude_cli_complete(
            self._system_prompt(),
            user_prompt,
            model=self.model,
            cli=self.cli,
            extra_args=self.extra_args,
            runner=self._runner,
        )
        return AuthoredIntent.model_validate(data)


# ---------------------------------------------------------------------------
# Intent -> resolver inputs
# ---------------------------------------------------------------------------


def intent_to_resolver_inputs(
    intent: AuthoredIntent,
) -> tuple[Outcome, ResolverInputs]:
    """Map an AuthoredIntent to the inverse resolver's (outcome, inputs).

    Principal dimensions are only attached when the cross-field minimum the
    resolver requires is present (a length, plus beam and draft). Otherwise the
    resolver falls back to ``hull_id`` / ``mesh_file`` and ledgers the rest.
    """
    dimensions: Optional[PrincipalDimensions] = None
    has_length = intent.loa is not None or intent.length_bp is not None
    if has_length and intent.beam is not None and intent.draft is not None:
        dimensions = PrincipalDimensions(
            loa=intent.loa,
            length_bp=intent.length_bp,
            beam=intent.beam,
            draft=intent.draft,
            displacement_t=intent.displacement_t,
            block_coefficient=intent.block_coefficient,
        )

    water_depth: float | str | None
    if intent.water_depth_infinite:
        water_depth = "infinity"
    else:
        water_depth = intent.water_depth

    inputs = ResolverInputs(
        dimensions=dimensions,
        mesh_file=intent.mesh_file,
        water_depth=water_depth,
        hull_id=intent.hull_id,
        inertia_mode=intent.inertia_mode,
    )
    return intent.outcome, inputs


# ---------------------------------------------------------------------------
# Authored spec result
# ---------------------------------------------------------------------------


@dataclass
class AuthoredSpec:
    """Result of authoring: the analysis SSOT spec plus its audit trail."""

    spec: DiffractionSpec
    ledger: AssumptionLedger
    intent: AuthoredIntent

    def to_spec_yaml(self, path: str | Path) -> Path:
        """Write the clean, reloadable analysis-SSOT spec YAML."""
        return self.spec.to_yaml(path)

    def assumptions_markdown(self) -> str:
        """Human-readable audit of what was extracted vs assumed."""
        lines = ["# Authoring audit", ""]
        lines.append(f"**Outcome:** {self.intent.outcome.value}")
        if self.intent.rationale:
            lines.append(f"\n{self.intent.rationale}")

        lines.append("\n## Extracted from project data (LLM)")
        if self.intent.provenance:
            lines.append("\n| Field | Value | Source |")
            lines.append("|---|---|---|")
            for entry in self.intent.provenance:
                lines.append(f"| {entry.field} | {entry.value} | {entry.source} |")
        else:
            lines.append("\n_(no values were grounded in the project data)_")

        lines.append("\n## Assumed by the resolver (no silent assumptions)")
        records = self.ledger.rows()
        if records:
            lines.append("\n| Field | Value | Source | Confidence | Basis |")
            lines.append("|---|---|---|---|---|")
            for rec in records:
                lines.append(
                    f"| {rec.field} | {rec.value} | {rec.source.value} | "
                    f"{rec.confidence.value} | {rec.basis} |"
                )
        else:
            lines.append("\n_(none -- every field was supplied)_")

        if self.intent.open_questions:
            lines.append("\n## Open questions")
            for q in self.intent.open_questions:
                lines.append(f"- {q}")
        return "\n".join(lines) + "\n"

    def write(self, out_dir: str | Path) -> dict[str, Path]:
        """Write the full artifact set: spec, audit, ledger, intent.

        Returns a mapping of artifact name -> path written.
        """
        out = Path(out_dir)
        out.mkdir(parents=True, exist_ok=True)
        paths = {
            "spec": self.to_spec_yaml(out / "spec.yml"),
            "assumptions": out / "assumptions.md",
            "ledger": out / "assumption_ledger.json",
            "intent": out / "authored_intent.json",
        }
        paths["assumptions"].write_text(self.assumptions_markdown())
        paths["ledger"].write_text(self.ledger.to_json())
        paths["intent"].write_text(self.intent.model_dump_json(indent=2))
        return paths


# ---------------------------------------------------------------------------
# Top-level entry point
# ---------------------------------------------------------------------------


def author_spec(
    bundle: ProjectBundle,
    request: str,
    *,
    author: IntentAuthor | None = None,
    hull_lookup: Any = None,
    config: ResolverConfig | None = None,
) -> AuthoredSpec:
    """Author an analysis-SSOT spec from a project SSOT + a request.

    Parameters
    ----------
    bundle:
        The project single-source-of-truth (data + free text).
    request:
        The natural-language analysis request.
    author:
        The :class:`IntentAuthor` that performs the LLM call. Defaults to
        :class:`ClaudeCliIntentAuthor` (the ``claude -p`` CLI, no API key).
        Inject a stub for offline tests, or :class:`AnthropicIntentAuthor`
        to use the Anthropic SDK directly.
    hull_lookup, config:
        Passed straight through to :func:`resolver.resolve`.
    """
    if author is None:
        author = ClaudeCliIntentAuthor()
    intent = author.author(bundle, request)
    outcome, inputs = intent_to_resolver_inputs(intent)
    spec, ledger = resolve(outcome, inputs, hull_lookup=hull_lookup, config=config)
    return AuthoredSpec(spec=spec, ledger=ledger, intent=intent)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main(argv: list[str] | None = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(
        description=(
            "Author a diffraction analysis spec (analysis SSOT) from a project "
            "bundle (project SSOT) + a natural-language request, via an LLM."
        )
    )
    parser.add_argument("bundle", help="Path to the project bundle YAML.")
    parser.add_argument(
        "-r", "--request", required=True, help="Natural-language analysis request."
    )
    parser.add_argument(
        "-o", "--out-dir", default="authored_spec", help="Output directory."
    )
    parser.add_argument("--model", default=DEFAULT_MODEL, help="Model id.")
    parser.add_argument(
        "--backend",
        choices=["claude-cli", "anthropic-sdk"],
        default="claude-cli",
        help="LLM backend: 'claude-cli' (claude -p, no key) or 'anthropic-sdk'.",
    )
    args = parser.parse_args(argv)

    if args.backend == "anthropic-sdk":
        author: IntentAuthor = AnthropicIntentAuthor(model=args.model)
    else:
        author = ClaudeCliIntentAuthor(model=args.model)

    bundle = ProjectBundle.from_yaml(args.bundle)
    result = author_spec(bundle, args.request, author=author)
    paths = result.write(args.out_dir)
    print(json.dumps({k: str(v) for k, v in paths.items()}, indent=2))
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())


__all__ = [
    "ProjectBundle",
    "ProvenanceEntry",
    "AuthoredIntent",
    "IntentAuthor",
    "ClaudeCliIntentAuthor",
    "AnthropicIntentAuthor",
    "AuthoredSpec",
    "author_spec",
    "intent_to_resolver_inputs",
    "build_system_prompt",
    "DEFAULT_MODEL",
]
