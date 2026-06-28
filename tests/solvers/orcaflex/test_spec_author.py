"""Tests for the OrcaFlex LLM spec-authoring front-end (offline, stub author).

The LLM call is injected, so these exercise the full project-SSOT -> intent ->
resolve -> analysis-SSOT pipeline without any network or API key.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.common.spec_authoring import ProjectBundle, ProvenanceEntry
from digitalmodel.solvers.orcaflex.inverse_resolver import (
    MooringResolverInputs,
    Outcome,
    PipelineResolverInputs,
    RiserResolverInputs,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.spec_author import (
    AuthoredSpec,
    ClaudeCliIntentAuthor,
    OrcaFlexIntent,
    author_spec,
    build_system_prompt,
    intent_to_resolver_inputs,
)


class StubAuthor:
    """An IntentAuthor that returns a fixed intent (no LLM call)."""

    def __init__(self, intent: OrcaFlexIntent) -> None:
        self.intent = intent
        self.calls: list[tuple[ProjectBundle, str]] = []

    def author(self, bundle: ProjectBundle, request: str) -> OrcaFlexIntent:
        self.calls.append((bundle, request))
        return self.intent


def _full_intent() -> OrcaFlexIntent:
    return OrcaFlexIntent(
        outcome=Outcome.MOORING_STRENGTH,
        name="West Test mooring",
        water_depth=1500.0,
        num_lines=12,
        chain_diameter=0.13,
        chain_grade="R5",
        pretension=1200.0,
        provenance=[
            ProvenanceEntry(field="water_depth", value="1500 m", source="data sheet"),
            ProvenanceEntry(field="num_lines", value="12", source="mooring layout"),
        ],
        open_questions=["Storm metocean not specified"],
        rationale="Request asks for a mooring strength check; sizing given.",
    )


# --- ProjectBundle ---------------------------------------------------------


def test_bundle_from_yaml_and_prompt_context(tmp_path: Path) -> None:
    bundle_yaml = tmp_path / "bundle.yml"
    bundle_yaml.write_text(
        "project_name: West Test\n"
        "vessel_particulars:\n"
        "  type: FPSO\n"
        "environment:\n"
        "  water_depth: 1500\n"
        "notes: Spread-mooring strength screening.\n"
    )
    bundle = ProjectBundle.from_yaml(bundle_yaml)
    assert bundle.project_name == "West Test"
    context = bundle.to_prompt_context()
    assert "West Test" in context
    assert "water_depth: 1500" in context
    assert "Spread-mooring strength" in context


# --- intent_to_resolver_inputs ---------------------------------------------


def test_intent_maps_to_resolver_inputs() -> None:
    intent = _full_intent()
    outcome, inputs = intent_to_resolver_inputs(intent)
    assert outcome == Outcome.MOORING_STRENGTH
    assert isinstance(inputs, MooringResolverInputs)
    assert inputs.water_depth == 1500.0
    assert inputs.num_lines == 12
    assert inputs.chain_diameter == 0.13
    assert inputs.chain_grade == "R5"
    assert inputs.pretension == 1200.0


def test_intent_leaves_unstated_fields_none() -> None:
    intent = OrcaFlexIntent(outcome=Outcome.MOORING_PRETENSION)
    _, inputs = intent_to_resolver_inputs(intent)
    assert inputs.water_depth is None
    assert inputs.num_lines is None
    assert inputs.chain_diameter is None


# --- author_spec end to end ------------------------------------------------


def test_author_spec_produces_spec_and_ledger() -> None:
    author = StubAuthor(_full_intent())
    bundle = ProjectBundle(project_name="Test", notes="mooring strength please")

    result = author_spec(bundle, "Check mooring strength", author=author)

    assert isinstance(result, AuthoredSpec)
    assert isinstance(result.spec, ProjectInputSpec)
    assert result.spec.mooring is not None
    assert len(result.spec.mooring.lines) == 12
    assert result.spec.environment.water.depth == 1500.0
    # The author was actually consulted with the request.
    assert author.calls[0][1] == "Check mooring strength"


def test_author_spec_ledgers_assumed_values() -> None:
    # Intent supplies only the outcome; everything else must be assumed.
    intent = OrcaFlexIntent(outcome=Outcome.MOORING_STRENGTH)
    result = author_spec(ProjectBundle(), "mooring", author=StubAuthor(intent))
    # No-silent-assumptions: derived values are recorded in the ledger.
    fields = {rec.field for rec in result.ledger.rows()}
    assert "environment.water.depth" in fields
    assert "mooring.num_lines" in fields


def test_supplied_values_not_ledgered() -> None:
    result = author_spec(ProjectBundle(), "mooring", author=StubAuthor(_full_intent()))
    fields = {rec.field for rec in result.ledger.rows()}
    assert "environment.water.depth" not in fields
    assert "mooring.num_lines" not in fields
    assert "mooring.chain_diameter" not in fields


def test_authored_spec_write_emits_all_artifacts(tmp_path: Path) -> None:
    result = author_spec(ProjectBundle(), "mooring", author=StubAuthor(_full_intent()))
    paths = result.write(tmp_path / "out")
    for key in ("spec", "assumptions", "ledger", "intent"):
        assert paths[key].exists(), key
    # The spec round-trips as a clean analysis SSOT.
    import yaml

    with open(paths["spec"]) as f:
        reloaded = ProjectInputSpec.model_validate(yaml.safe_load(f))
    assert reloaded.mooring is not None
    assert len(reloaded.mooring.lines) == 12
    # Provenance and the chosen outcome appear in the human audit.
    audit = paths["assumptions"].read_text()
    assert "mooring_strength" in audit
    assert "data sheet" in audit


def test_assumptions_markdown_handles_empty_provenance() -> None:
    intent = OrcaFlexIntent(outcome=Outcome.MOORING_STRENGTH)
    result = author_spec(ProjectBundle(), "mooring", author=StubAuthor(intent))
    md = result.assumptions_markdown()
    assert "no values were grounded" in md


def test_generate_model_via_modular_generator(tmp_path: Path) -> None:
    result = author_spec(ProjectBundle(), "mooring", author=StubAuthor(_full_intent()))
    out = result.generate_model(tmp_path / "model")
    assert (out / "master.yml").exists()
    assert any(out.rglob("*.yml"))


def test_system_prompt_lists_all_outcomes() -> None:
    prompt = build_system_prompt()
    for outcome in Outcome:
        assert outcome.value in prompt


# --- ClaudeCliIntentAuthor (mocked runner, no real CLI) --------------------


def _fake_completed(stdout: str, returncode: int = 0, stderr: str = ""):
    import subprocess

    return subprocess.CompletedProcess(
        args=["claude"], returncode=returncode, stdout=stdout, stderr=stderr
    )


def test_claude_cli_author_parses_fenced_json() -> None:
    inner = (
        '{"outcome": "mooring_strength", "water_depth": 1500.0, "num_lines": 12, '
        '"provenance": [], "open_questions": [], "rationale": "ok"}'
    )
    # claude -p returns an envelope whose `result` may wrap JSON in fences.
    envelope = json.dumps({"is_error": False, "result": "```json\n" + inner + "\n```"})

    captured: dict = {}

    def runner(argv, stdin):
        captured["argv"] = argv
        captured["stdin"] = stdin
        return _fake_completed(envelope)

    author = ClaudeCliIntentAuthor(runner=runner)
    intent = author.author(ProjectBundle(notes="x"), "mooring strength")

    assert intent.outcome == Outcome.MOORING_STRENGTH
    assert intent.water_depth == 1500.0
    assert intent.num_lines == 12
    # The CLI was invoked headlessly with the right flags + stdin prompt.
    assert "-p" in captured["argv"]
    assert "--output-format" in captured["argv"]
    assert "mooring strength" in captured["stdin"]


def test_claude_cli_author_raises_on_error_envelope() -> None:
    envelope = json.dumps({"is_error": True, "result": "boom"})
    author = ClaudeCliIntentAuthor(runner=lambda argv, stdin: _fake_completed(envelope))
    with pytest.raises(RuntimeError, match="boom"):
        author.author(ProjectBundle(), "mooring")


def test_claude_cli_author_raises_on_nonzero_returncode() -> None:
    author = ClaudeCliIntentAuthor(
        runner=lambda argv, stdin: _fake_completed("", returncode=1, stderr="no auth")
    )
    with pytest.raises(RuntimeError, match="no auth"):
        author.author(ProjectBundle(), "mooring")


def test_author_spec_end_to_end_with_cli_runner(tmp_path: Path) -> None:
    inner = (
        '{"outcome": "mooring_pretension", "water_depth": 200, "num_lines": 6, '
        '"provenance": [], "open_questions": [], "rationale": "ok"}'
    )
    envelope = json.dumps({"is_error": False, "result": inner})
    author = ClaudeCliIntentAuthor(runner=lambda argv, stdin: _fake_completed(envelope))

    result = author_spec(ProjectBundle(), "pretension check", author=author)
    assert result.spec.simulation.stages == [10]
    assert len(result.spec.mooring.lines) == 6


# --- Riser outcomes --------------------------------------------------------


def _riser_intent() -> OrcaFlexIntent:
    return OrcaFlexIntent(
        outcome=Outcome.RISER_DYNAMIC,
        name="West Test SCR",
        water_depth=1200.0,
        vessel_name="FPSO_A",
        riser_outer_diameter=0.4,
        riser_wall_thickness=0.03,
        riser_length=2000.0,
        riser_configuration="lazy_wave",
        provenance=[
            ProvenanceEntry(field="water_depth", value="1200 m", source="data sheet"),
            ProvenanceEntry(field="riser_outer_diameter", value="16 in", source="BOD"),
        ],
        open_questions=["Wave spectrum not specified"],
        rationale="Request asks for a riser wave dynamic check; geometry given.",
    )


def test_riser_intent_maps_to_riser_resolver_inputs() -> None:
    outcome, inputs = intent_to_resolver_inputs(_riser_intent())
    assert outcome == Outcome.RISER_DYNAMIC
    assert isinstance(inputs, RiserResolverInputs)
    assert inputs.water_depth == 1200.0
    assert inputs.outer_diameter == 0.4
    assert inputs.wall_thickness == 0.03
    assert inputs.length == 2000.0
    assert inputs.configuration == "lazy_wave"
    assert inputs.vessel_name == "FPSO_A"


def test_author_spec_produces_riser_spec() -> None:
    author = StubAuthor(_riser_intent())
    result = author_spec(ProjectBundle(notes="riser"), "riser dynamic", author=author)
    assert isinstance(result.spec, ProjectInputSpec)
    # Right model family is set; other families are absent.
    assert result.spec.riser is not None
    assert result.spec.mooring is None and result.spec.pipeline is None
    assert result.spec.metadata.structure == "riser"
    # Correct outcome -> simulation stages (static + wave dynamic).
    assert result.spec.simulation.stages == [8, 16]
    lt = result.spec.riser.line_types[0]
    assert lt.outer_diameter == 0.4
    assert lt.inner_diameter == 0.34  # 0.4 - 2*0.03
    assert result.spec.riser.vessel.name == "FPSO_A"


def test_riser_static_outcome_drives_single_stage() -> None:
    intent = OrcaFlexIntent(outcome=Outcome.RISER_STATIC)
    result = author_spec(ProjectBundle(), "riser config", author=StubAuthor(intent))
    assert result.spec.riser is not None
    assert result.spec.simulation.stages == [10]


def test_generate_riser_model_via_modular_generator(tmp_path: Path) -> None:
    result = author_spec(ProjectBundle(), "riser", author=StubAuthor(_riser_intent()))
    out = result.generate_model(tmp_path / "riser_model")
    assert (out / "master.yml").exists()
    assert any(out.rglob("*.yml"))


# --- Pipeline outcomes -----------------------------------------------------


def _pipeline_intent() -> OrcaFlexIntent:
    return OrcaFlexIntent(
        outcome=Outcome.PIPELINE_LAY,
        name="Trunkline",
        water_depth=300.0,
        pipeline_outer_diameter=0.6,
        pipeline_wall_thickness=0.03,
        pipeline_length=5000.0,
        pipeline_material="X70",
        provenance=[
            ProvenanceEntry(field="pipeline_material", value="X70", source="BOD"),
        ],
        open_questions=["Lay vessel not specified"],
        rationale="Request asks for an installation lay check; geometry given.",
    )


def test_pipeline_intent_maps_to_pipeline_resolver_inputs() -> None:
    outcome, inputs = intent_to_resolver_inputs(_pipeline_intent())
    assert outcome == Outcome.PIPELINE_LAY
    assert isinstance(inputs, PipelineResolverInputs)
    assert inputs.water_depth == 300.0
    assert inputs.outer_diameter == 0.6
    assert inputs.wall_thickness == 0.03
    assert inputs.length == 5000.0
    assert inputs.material == "X70"


def test_author_spec_produces_pipeline_spec() -> None:
    author = StubAuthor(_pipeline_intent())
    result = author_spec(ProjectBundle(notes="pipe"), "pipeline lay", author=author)
    assert isinstance(result.spec, ProjectInputSpec)
    assert result.spec.pipeline is not None
    assert result.spec.mooring is None and result.spec.riser is None
    assert result.spec.metadata.structure == "pipeline"
    # Correct outcome -> simulation stages (static + lay dynamic).
    assert result.spec.simulation.stages == [8, 16]
    assert result.spec.pipeline.dimensions.outer_diameter == 0.6
    assert result.spec.pipeline.dimensions.wall_thickness == 0.03
    assert result.spec.pipeline.material == "X70"


def test_pipeline_onbottom_outcome_drives_single_stage() -> None:
    intent = OrcaFlexIntent(outcome=Outcome.PIPELINE_ONBOTTOM)
    result = author_spec(ProjectBundle(), "stability", author=StubAuthor(intent))
    assert result.spec.pipeline is not None
    assert result.spec.simulation.stages == [10]


def test_generate_pipeline_model_via_modular_generator(tmp_path: Path) -> None:
    result = author_spec(
        ProjectBundle(), "pipeline", author=StubAuthor(_pipeline_intent())
    )
    out = result.generate_model(tmp_path / "pipeline_model")
    assert (out / "master.yml").exists()
    assert any(out.rglob("*.yml"))


def test_riser_intent_leaves_pipeline_fields_unmapped() -> None:
    # A riser intent must not leak into pipeline resolver inputs (family-scoped).
    intent = OrcaFlexIntent(
        outcome=Outcome.RISER_STATIC, riser_outer_diameter=0.3, water_depth=500.0
    )
    _, inputs = intent_to_resolver_inputs(intent)
    assert isinstance(inputs, RiserResolverInputs)
    assert inputs.outer_diameter == 0.3
