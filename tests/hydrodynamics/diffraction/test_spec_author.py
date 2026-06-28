"""Tests for the LLM spec-authoring front-end (offline, stub IntentAuthor).

The LLM call is injected, so these exercise the full project-SSOT -> intent ->
resolve -> analysis-SSOT pipeline without any network or API key.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.resolver import Outcome, ResolverInputs
from digitalmodel.hydrodynamics.diffraction.spec_author import (
    AuthoredIntent,
    AuthoredSpec,
    ProjectBundle,
    ProvenanceEntry,
    author_spec,
    intent_to_resolver_inputs,
)


def _write_box_mesh(path: Path) -> Path:
    path.write_text(
        "\n".join(
            [
                "# box mesh",
                "0 0 -2",
                "10 0 -2",
                "10 4 -2",
                "0 4 -2",
                "0 0 0",
                "10 0 0",
                "10 4 0",
                "0 4 0",
            ]
        ),
        encoding="utf-8",
    )
    return path


class StubAuthor:
    """An IntentAuthor that returns a fixed intent (no LLM call)."""

    def __init__(self, intent: AuthoredIntent) -> None:
        self.intent = intent
        self.calls: list[tuple[ProjectBundle, str]] = []

    def author(self, bundle: ProjectBundle, request: str) -> AuthoredIntent:
        self.calls.append((bundle, request))
        return self.intent


def _full_intent(mesh: Path) -> AuthoredIntent:
    return AuthoredIntent(
        outcome=Outcome.SHIP_RAOS,
        mesh_file=str(mesh),
        loa=120.0,
        beam=20.0,
        draft=8.0,
        displacement_t=15000.0,
        water_depth=100.0,
        provenance=[
            ProvenanceEntry(field="loa", value="120 m", source="data sheet"),
            ProvenanceEntry(field="beam", value="20 m", source="data sheet"),
        ],
        open_questions=["Wave heading range not specified"],
        rationale="Request asks for vessel RAOs; particulars given in data sheet.",
    )


# --- ProjectBundle ---------------------------------------------------------


def test_bundle_from_yaml_and_prompt_context(tmp_path: Path) -> None:
    bundle_yaml = tmp_path / "bundle.yml"
    bundle_yaml.write_text(
        "project_name: West Test\n"
        "vessel_particulars:\n"
        "  loa: 120\n"
        "  beam: 20\n"
        "environment:\n"
        "  water_depth: 100\n"
        "notes: Screening RAOs for transit.\n"
    )
    bundle = ProjectBundle.from_yaml(bundle_yaml)
    assert bundle.project_name == "West Test"
    context = bundle.to_prompt_context()
    assert "West Test" in context
    assert "loa: 120" in context
    assert "Screening RAOs" in context


def test_empty_bundle_prompt_context() -> None:
    assert ProjectBundle().to_prompt_context() == "(no project data provided)"


# --- intent_to_resolver_inputs ---------------------------------------------


def test_intent_maps_full_dimensions() -> None:
    intent = AuthoredIntent(
        outcome=Outcome.SHIP_RAOS,
        mesh_file="hull.gdf",
        loa=120.0,
        beam=20.0,
        draft=8.0,
        water_depth=100.0,
    )
    outcome, inputs = intent_to_resolver_inputs(intent)
    assert outcome == Outcome.SHIP_RAOS
    assert isinstance(inputs, ResolverInputs)
    assert inputs.dimensions is not None
    assert inputs.dimensions.beam == 20.0
    assert inputs.water_depth == 100.0
    assert inputs.mesh_file == "hull.gdf"


def test_intent_without_beam_or_draft_omits_dimensions() -> None:
    intent = AuthoredIntent(outcome=Outcome.SHIP_RAOS, mesh_file="hull.gdf", loa=120.0)
    _, inputs = intent_to_resolver_inputs(intent)
    assert inputs.dimensions is None


def test_intent_infinite_water_depth_maps_to_string() -> None:
    intent = AuthoredIntent(
        outcome=Outcome.ADDED_MASS_DAMPING,
        mesh_file="hull.gdf",
        water_depth_infinite=True,
    )
    _, inputs = intent_to_resolver_inputs(intent)
    assert inputs.water_depth == "infinity"


# --- author_spec end to end ------------------------------------------------


def test_author_spec_produces_spec_and_ledger(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    author = StubAuthor(_full_intent(mesh))
    bundle = ProjectBundle(project_name="Test", notes="RAOs please")

    result = author_spec(bundle, "Give me vessel RAOs", author=author)

    assert isinstance(result, AuthoredSpec)
    assert isinstance(result.spec, DiffractionSpec)
    assert result.spec.vessel.geometry.mesh_file == str(mesh)
    assert result.spec.vessel.inertia.mass > 0
    # The author was actually consulted with the request.
    assert author.calls[0][1] == "Give me vessel RAOs"


def test_author_spec_ledgers_assumed_values(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    # Intent supplies only the mesh + depth; everything else must be assumed.
    intent = AuthoredIntent(
        outcome=Outcome.SHIP_RAOS, mesh_file=str(mesh), water_depth=100.0
    )
    result = author_spec(ProjectBundle(), "RAOs", author=StubAuthor(intent))
    # No-silent-assumptions: derived values are recorded in the ledger.
    fields = {rec.field for rec in result.ledger.rows()}
    assert any("inertia" in f for f in fields)


def test_authored_spec_write_emits_all_artifacts(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    result = author_spec(ProjectBundle(), "RAOs", author=StubAuthor(_full_intent(mesh)))
    paths = result.write(tmp_path / "out")
    for key in ("spec", "assumptions", "ledger", "intent"):
        assert paths[key].exists(), key
    # The spec round-trips as a clean analysis SSOT.
    reloaded = DiffractionSpec.from_yaml(paths["spec"])
    assert reloaded.vessel.geometry.mesh_file == str(mesh)
    # Provenance and the chosen outcome appear in the human audit.
    audit = paths["assumptions"].read_text()
    assert "ship_raos" in audit
    assert "data sheet" in audit


def test_assumptions_markdown_handles_empty_provenance(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    intent = AuthoredIntent(
        outcome=Outcome.SHIP_RAOS, mesh_file=str(mesh), water_depth=50.0
    )
    result = author_spec(ProjectBundle(), "RAOs", author=StubAuthor(intent))
    md = result.assumptions_markdown()
    assert "no values were grounded" in md


def test_default_author_requires_anthropic(monkeypatch: pytest.MonkeyPatch) -> None:
    # With no author injected and anthropic unavailable, the error is explicit.
    from digitalmodel.hydrodynamics.diffraction import spec_author as mod

    author = mod.AnthropicIntentAuthor()
    monkeypatch.setattr(
        author, "_get_client", lambda: (_ for _ in ()).throw(RuntimeError("no sdk"))
    )
    with pytest.raises(RuntimeError):
        author.author(ProjectBundle(), "RAOs")
