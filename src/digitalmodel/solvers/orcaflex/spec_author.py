"""LLM spec-authoring front-end for OrcaFlex (project SSOT -> analysis SSOT).

The OrcaFlex counterpart of the diffraction ``spec_author`` (digitalmodel
#1095). It is the **front-end** of the OrcaFlex model-generation pipeline:

    project single-source-of-truth
        |  LLM authors a structured intent (this module)
        v
    OrcaFlexIntent  (outcome + only the mooring facts stated in the project data)
        |  deterministic inverse resolver (inverse_resolver.resolve, #1096)
        v
    ProjectInputSpec  +  AssumptionLedger   <-- the analysis single-source-of-truth
        |  ModularModelGenerator
        v
    OrcaFlex model YAML (master.yml + includes)

Responsibility split (deliberate, to keep the hallucination surface small):

* The **LLM** only *extracts mooring facts that are stated* in the project
  bundle and picks the matching outcome. It leaves anything it cannot ground as
  ``None`` and records provenance for every value it sets. It never invents
  chain sizes, line counts, or site depths.
* The **deterministic resolver** fills every remaining field from maintained
  reference defaults and records each filled value in the ``AssumptionLedger``
  -- the #622 "no silent assumptions" contract.

The LLM call is injected through the :class:`IntentAuthor` protocol so the core
is fully unit-testable offline (pass a stub author). The default implementation,
:class:`ClaudeCliIntentAuthor`, shells out to the ``claude -p`` CLI (no API key,
no ``anthropic`` dependency). :class:`AnthropicIntentAuthor` uses the Anthropic
SDK with structured outputs and is imported lazily.

It covers the resolver's three license-free model families -- **mooring**
(spread-mooring strength / pretension), **riser** (static configuration / wave
dynamic), and **pipeline** (on-bottom stability / installation lay) -- each
dispatched by the chosen outcome's family.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional, Protocol

import yaml
from pydantic import BaseModel, Field

from digitalmodel.common.assumption_ledger import AssumptionLedger
from digitalmodel.common.spec_authoring import (
    ProjectBundle,
    ProvenanceEntry,
    build_outcome_menu,
    claude_cli_complete,
)
from digitalmodel.solvers.orcaflex import inverse_resolver
from digitalmodel.solvers.orcaflex.inverse_resolver import (
    OUTCOME_DESCRIPTIONS,
    PIPELINE_OUTCOMES,
    RISER_OUTCOMES,
    MooringResolverInputs,
    Outcome,
    PipelineResolverInputs,
    ResolverInputs,
    RiserResolverInputs,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

# Default model for the authoring call. Kept here (not hard-coded at the call
# site) so it is easy to override and easy to find.
DEFAULT_MODEL = "claude-opus-4-8"


# ---------------------------------------------------------------------------
# Structured LLM output: the authored intent
# ---------------------------------------------------------------------------


class OrcaFlexIntent(BaseModel):
    """Structured output the LLM produces from a project bundle + request.

    This is *not* the full spec -- only the sparse, grounded facts the resolver
    needs as a starting point, plus the chosen outcome. Every input field is
    optional: the LLM leaves a field ``None`` when the project data does not
    state it, and the deterministic resolver fills it (and ledgers it).

    The fields are grouped by model family and mirror the resolver input models
    (:class:`inverse_resolver.MooringResolverInputs`,
    :class:`~inverse_resolver.RiserResolverInputs`,
    :class:`~inverse_resolver.PipelineResolverInputs`). Set only the fields that
    apply to the chosen outcome's family; leave the others ``None``.
    """

    outcome: Outcome = Field(
        description="The analysis outcome that matches the request."
    )
    # Common across families -- only set when the project data states them.
    name: Optional[str] = Field(default=None, description="Model / system name.")
    water_depth: Optional[float] = Field(
        default=None, description="Site water depth (m)."
    )
    vessel_name: Optional[str] = Field(
        default=None,
        description="Vessel the system attaches to (mooring fairleads / riser "
        "hang-off).",
    )
    # --- Mooring fields (outcomes: mooring_strength, mooring_pretension) ---
    num_lines: Optional[int] = Field(
        default=None, description="[mooring] Number of mooring lines."
    )
    chain_diameter: Optional[float] = Field(
        default=None, description="[mooring] Mooring chain diameter (m)."
    )
    chain_grade: Optional[str] = Field(
        default=None, description="[mooring] Mooring chain grade (e.g. R3, R4, R5)."
    )
    fairlead_radius: Optional[float] = Field(
        default=None, description="[mooring] Fairlead radius from origin (m)."
    )
    anchor_radius: Optional[float] = Field(
        default=None, description="[mooring] Anchor radius from origin (m)."
    )
    line_length: Optional[float] = Field(
        default=None, description="[mooring] Mooring line length (m)."
    )
    pretension: Optional[float] = Field(
        default=None, description="[mooring] Target line pretension (kN)."
    )
    # --- Riser fields (outcomes: riser_static, riser_dynamic) ---
    riser_outer_diameter: Optional[float] = Field(
        default=None, description="[riser] Riser pipe outer diameter (m)."
    )
    riser_wall_thickness: Optional[float] = Field(
        default=None, description="[riser] Riser pipe wall thickness (m)."
    )
    riser_length: Optional[float] = Field(
        default=None, description="[riser] Riser arc length (m)."
    )
    riser_configuration: Optional[str] = Field(
        default=None,
        description="[riser] Configuration (e.g. catenary, lazy_wave).",
    )
    riser_top_position: Optional[list[float]] = Field(
        default=None,
        description="[riser] Hang-off [x, y, z] in m (z below free surface).",
    )
    riser_bottom_position: Optional[list[float]] = Field(
        default=None,
        description="[riser] Touchdown / anchor [x, y, z] in m.",
    )
    # --- Pipeline fields (outcomes: pipeline_onbottom, pipeline_lay) ---
    pipeline_outer_diameter: Optional[float] = Field(
        default=None, description="[pipeline] Pipe outer diameter (m)."
    )
    pipeline_wall_thickness: Optional[float] = Field(
        default=None, description="[pipeline] Pipe wall thickness (m)."
    )
    pipeline_length: Optional[float] = Field(
        default=None, description="[pipeline] Pipeline length (m)."
    )
    pipeline_material: Optional[str] = Field(
        default=None, description="[pipeline] Line-pipe steel grade (e.g. X65)."
    )
    pipeline_segment_length: Optional[float] = Field(
        default=None, description="[pipeline] FE mesh segment length (m)."
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
# System prompt
# ---------------------------------------------------------------------------


_SYSTEM_PROMPT_TEMPLATE = """\
You are an offshore marine-analysis spec author. Given an offshore project's \
data and a natural-language request, you produce a structured authoring intent \
for an OrcaFlex analysis. Three license-free model families are supported -- \
mooring (spread mooring), riser (steel catenary / wave dynamic), and pipeline \
(on-bottom stability / installation lay).

Choose the single `outcome` that best matches the request:
{outcome_menu}

The outcome's prefix names its family and which input fields apply:
- `mooring_*` -> use the mooring fields: num_lines, chain_diameter, \
chain_grade, fairlead_radius, anchor_radius, line_length, pretension (plus the \
common name, water_depth, vessel_name).
- `riser_*` -> use the riser fields: riser_outer_diameter, \
riser_wall_thickness, riser_length, riser_configuration, riser_top_position, \
riser_bottom_position (plus name, water_depth, vessel_name).
- `pipeline_*` -> use the pipeline fields: pipeline_outer_diameter, \
pipeline_wall_thickness, pipeline_length, pipeline_material, \
pipeline_segment_length (plus name, water_depth).
Leave the fields of the other families null.

Your job is fact extraction, not engineering invention:
- Fill a field ONLY with a value the project data actually states. If the data \
does not give a value, leave it null. Do NOT estimate, default, or invent water \
depth, diameters, wall thickness, line counts, grades, radii, lengths, or any \
physical quantity -- a downstream deterministic resolver fills every gap from \
maintained reference data and records each assumption for review. Inventing \
values defeats that audit trail.
- For every value you DO set, add a `provenance` entry citing where in the \
project data you found it.
- List anything material but missing in `open_questions`.
- Keep `rationale` to one short paragraph.
"""


def build_system_prompt() -> str:
    """System prompt with the outcome menu rendered from the resolver."""
    menu = build_outcome_menu(OUTCOME_DESCRIPTIONS)
    return _SYSTEM_PROMPT_TEMPLATE.format(outcome_menu=menu)


# Rendered once at import for convenience; ``build_system_prompt()`` stays the
# source of truth if the outcome set changes at runtime.
SYSTEM_PROMPT = build_system_prompt()


# ---------------------------------------------------------------------------
# Intent author protocol + implementations
# ---------------------------------------------------------------------------


class IntentAuthor(Protocol):
    """Anything that turns (bundle, request) into an OrcaFlexIntent."""

    def author(self, bundle: ProjectBundle, request: str) -> OrcaFlexIntent: ...


def _user_prompt(bundle: ProjectBundle, request: str) -> str:
    return (
        f"Analysis request:\n{request}\n\n"
        f"Project data:\n{bundle.to_prompt_context()}"
    )


class ClaudeCliIntentAuthor:
    """Intent author backed by the ``claude -p`` CLI (Claude Code).

    Uses the local Claude Code install and its existing auth -- no
    ``ANTHROPIC_API_KEY`` and no ``anthropic`` dependency. The CLI does not
    expose typed structured outputs, so the schema is supplied in the system
    prompt and the JSON object is parsed (and Pydantic-validated) from the
    response. This is the default author.
    """

    def __init__(
        self,
        model: str = DEFAULT_MODEL,
        *,
        cli: str = "claude",
        extra_args: Optional[list[str]] = None,
        runner: Optional[Any] = None,
    ) -> None:
        self.model = model
        self.cli = cli
        self.extra_args = list(extra_args or [])
        self._runner = runner

    def _system_prompt(self) -> str:
        schema = json.dumps(OrcaFlexIntent.model_json_schema())
        return (
            SYSTEM_PROMPT
            + "\n\nReturn ONLY a single JSON object conforming to this JSON "
            "schema -- no prose, no markdown, no code fences:\n" + schema
        )

    def author(self, bundle: ProjectBundle, request: str) -> OrcaFlexIntent:
        data = claude_cli_complete(
            self._system_prompt(),
            _user_prompt(bundle, request),
            model=self.model,
            cli=self.cli,
            extra_args=self.extra_args,
            runner=self._runner,
        )
        return OrcaFlexIntent.model_validate(data)


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

    def author(self, bundle: ProjectBundle, request: str) -> OrcaFlexIntent:
        client = self._get_client()
        response = client.messages.parse(
            model=self.model,
            max_tokens=4000,
            thinking={"type": "adaptive"},
            output_config={"effort": "high"},
            system=SYSTEM_PROMPT,
            messages=[{"role": "user", "content": _user_prompt(bundle, request)}],
            output_format=OrcaFlexIntent,
        )
        return response.parsed_output


# ---------------------------------------------------------------------------
# Intent -> resolver inputs
# ---------------------------------------------------------------------------


def intent_to_resolver_inputs(
    intent: OrcaFlexIntent,
) -> tuple[Outcome, ResolverInputs]:
    """Map an OrcaFlexIntent to the inverse resolver's (outcome, inputs).

    Dispatches on the outcome's model family and builds the matching
    resolver-inputs object (mooring / riser / pipeline), copying only the
    fields that family accepts.
    """
    outcome = intent.outcome
    inputs: ResolverInputs
    if outcome in RISER_OUTCOMES:
        inputs = RiserResolverInputs(
            name=intent.name,
            water_depth=intent.water_depth,
            outer_diameter=intent.riser_outer_diameter,
            wall_thickness=intent.riser_wall_thickness,
            length=intent.riser_length,
            configuration=intent.riser_configuration,
            vessel_name=intent.vessel_name,
            top_position=intent.riser_top_position,
            bottom_position=intent.riser_bottom_position,
        )
    elif outcome in PIPELINE_OUTCOMES:
        inputs = PipelineResolverInputs(
            name=intent.name,
            water_depth=intent.water_depth,
            outer_diameter=intent.pipeline_outer_diameter,
            wall_thickness=intent.pipeline_wall_thickness,
            length=intent.pipeline_length,
            material=intent.pipeline_material,
            segment_length=intent.pipeline_segment_length,
        )
    else:  # mooring family (default)
        inputs = MooringResolverInputs(
            name=intent.name,
            water_depth=intent.water_depth,
            num_lines=intent.num_lines,
            chain_diameter=intent.chain_diameter,
            chain_grade=intent.chain_grade,
            fairlead_radius=intent.fairlead_radius,
            anchor_radius=intent.anchor_radius,
            line_length=intent.line_length,
            vessel_name=intent.vessel_name,
            pretension=intent.pretension,
        )
    return outcome, inputs


# ---------------------------------------------------------------------------
# Authored spec result
# ---------------------------------------------------------------------------


@dataclass
class AuthoredSpec:
    """Result of authoring: the analysis SSOT spec plus its audit trail."""

    spec: ProjectInputSpec
    ledger: AssumptionLedger
    intent: OrcaFlexIntent

    def to_spec_yaml(self, path: str | Path) -> Path:
        """Write the clean, reloadable analysis-SSOT spec YAML."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        data = self.spec.model_dump(mode="json", exclude_none=True)
        with open(path, "w", encoding="utf-8") as f:
            yaml.safe_dump(data, f, default_flow_style=False, sort_keys=False)
        return path

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
            "intent": out / "intent.json",
        }
        paths["assumptions"].write_text(self.assumptions_markdown())
        paths["ledger"].write_text(self.ledger.to_json())
        paths["intent"].write_text(self.intent.model_dump_json(indent=2))
        return paths

    def generate_model(self, out_dir: str | Path) -> Path:
        """Generate the OrcaFlex model YAML (master.yml + includes).

        Returns the output directory containing ``master.yml``.
        """
        out = Path(out_dir)
        ModularModelGenerator.from_spec(self.spec).generate(out)
        return out


# ---------------------------------------------------------------------------
# Top-level entry point
# ---------------------------------------------------------------------------


def author_spec(
    bundle: ProjectBundle,
    request: str,
    *,
    author: IntentAuthor | None = None,
) -> AuthoredSpec:
    """Author an OrcaFlex analysis-SSOT spec from a project SSOT + a request.

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
    """
    if author is None:
        author = ClaudeCliIntentAuthor()
    intent = author.author(bundle, request)
    outcome, inputs = intent_to_resolver_inputs(intent)
    spec, ledger = inverse_resolver.resolve(outcome, inputs)
    return AuthoredSpec(spec=spec, ledger=ledger, intent=intent)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main(argv: list[str] | None = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(
        description=(
            "Author an OrcaFlex analysis spec (analysis SSOT) for a mooring, "
            "riser, or pipeline outcome from a project bundle (project SSOT) + "
            "a natural-language request, via an LLM, then optionally generate "
            "the OrcaFlex model."
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
    parser.add_argument(
        "--generate-model",
        action="store_true",
        help="Also generate the OrcaFlex model YAML (into <out-dir>/model).",
    )
    args = parser.parse_args(argv)

    if args.backend == "anthropic-sdk":
        author: IntentAuthor = AnthropicIntentAuthor(model=args.model)
    else:
        author = ClaudeCliIntentAuthor(model=args.model)

    bundle = ProjectBundle.from_yaml(args.bundle)
    result = author_spec(bundle, args.request, author=author)
    paths = result.write(args.out_dir)
    output = {k: str(v) for k, v in paths.items()}
    if args.generate_model:
        model_dir = result.generate_model(Path(args.out_dir) / "model")
        output["model"] = str(model_dir / "master.yml")
    print(json.dumps(output, indent=2))
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())


__all__ = [
    "OrcaFlexIntent",
    "IntentAuthor",
    "ClaudeCliIntentAuthor",
    "AnthropicIntentAuthor",
    "AuthoredSpec",
    "author_spec",
    "intent_to_resolver_inputs",
    "build_system_prompt",
    "DEFAULT_MODEL",
]
