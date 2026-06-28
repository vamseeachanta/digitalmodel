"""Tests for the OrcaFlex inverse resolver (license-free, offline).

Mirrors the diffraction resolver tests: outcome + sparse inputs -> complete
ProjectInputSpec + AssumptionLedger, generatable by ModularModelGenerator.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex.inverse_resolver import (
    MOORING_OUTCOMES,
    OUTCOME_DESCRIPTIONS,
    PIPELINE_OUTCOMES,
    RISER_OUTCOMES,
    MooringResolverInputs,
    Outcome,
    PipelineResolverInputs,
    RiserResolverInputs,
    resolve,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def test_barebones_outcome_resolves_to_valid_mooring_spec() -> None:
    spec, ledger = resolve(Outcome.MOORING_STRENGTH)
    assert isinstance(spec, ProjectInputSpec)
    assert spec.mooring is not None
    assert spec.pipeline is None and spec.riser is None and spec.generic is None
    assert len(spec.mooring.lines) == 8
    # Barebones -> everything assumed -> non-empty ledger (no silent assumptions).
    fields = {r.field for r in ledger.rows()}
    assert "environment.water.depth" in fields
    assert "mooring.num_lines" in fields


def test_string_outcome_accepted() -> None:
    spec, _ = resolve("mooring_strength")
    assert spec.metadata.structure == "mooring"


def test_supplied_inputs_are_honored_and_not_ledgered() -> None:
    spec, ledger = resolve(
        Outcome.MOORING_STRENGTH,
        MooringResolverInputs(
            water_depth=1500.0, num_lines=12, chain_diameter=0.13, chain_grade="R5"
        ),
    )
    assert spec.environment.water.depth == 1500.0
    assert len(spec.mooring.lines) == 12
    seg = spec.mooring.lines[0].segments[0]
    assert seg.diameter == 0.13
    assert seg.grade.value == "R5"
    fields = {r.field for r in ledger.rows()}
    # User-supplied values are not recorded as assumptions.
    assert "environment.water.depth" not in fields
    assert "mooring.num_lines" not in fields
    assert "mooring.chain_diameter" not in fields


def test_outcome_drives_simulation_stages() -> None:
    strength, _ = resolve(Outcome.MOORING_STRENGTH)
    pretension, _ = resolve(Outcome.MOORING_PRETENSION)
    assert strength.simulation.stages == [8, 16]
    assert pretension.simulation.stages == [10]


def test_anchor_radius_scales_with_water_depth() -> None:
    shallow, _ = resolve(
        Outcome.MOORING_STRENGTH, MooringResolverInputs(water_depth=100)
    )
    deep, _ = resolve(Outcome.MOORING_STRENGTH, MooringResolverInputs(water_depth=1000))
    shallow_r = shallow.mooring.lines[0].anchor.position[0]
    deep_r = deep.mooring.lines[0].anchor.position[0]
    assert deep_r > shallow_r


def test_pretension_applied_when_supplied() -> None:
    spec, _ = resolve(
        Outcome.MOORING_STRENGTH, MooringResolverInputs(pretension=1200.0)
    )
    assert all(line.pretension == 1200.0 for line in spec.mooring.lines)


def test_partial_spec_mooring_is_used_verbatim() -> None:
    partial = {
        "mooring": {
            "lines": [
                {
                    "name": "L1",
                    "segments": [
                        {"type": "chain", "diameter": 0.1, "length": 500, "grade": "R3"}
                    ],
                    "anchor": {"type": "anchor", "position": [800, 0, -200]},
                    "fairlead": {
                        "type": "fairlead",
                        "position": [40, 0, 0],
                        "vessel": "v",
                    },
                }
            ]
        }
    }
    spec, ledger = resolve(
        Outcome.MOORING_STRENGTH, MooringResolverInputs(partial_spec=partial)
    )
    assert len(spec.mooring.lines) == 1
    assert spec.mooring.lines[0].name == "L1"
    # The user-supplied mooring is not re-derived, so no mooring.* assumptions.
    assert not any(r.field.startswith("mooring.") for r in ledger.rows())


def test_generates_orcaflex_model_yaml(tmp_path: Path) -> None:
    spec, _ = resolve(Outcome.MOORING_STRENGTH)
    out = tmp_path / "model"
    ModularModelGenerator.from_spec(spec).generate(out)
    assert (out / "master.yml").exists()
    assert any(out.rglob("*.yml"))


def test_every_outcome_has_a_description() -> None:
    for outcome in Outcome:
        assert outcome in OUTCOME_DESCRIPTIONS, outcome
    assert MOORING_OUTCOMES.issubset(set(Outcome))
    assert RISER_OUTCOMES.issubset(set(Outcome))
    assert PIPELINE_OUTCOMES.issubset(set(Outcome))
    # Families partition the outcome set without overlap.
    assert MOORING_OUTCOMES | RISER_OUTCOMES | PIPELINE_OUTCOMES == set(Outcome)
    assert not (MOORING_OUTCOMES & RISER_OUTCOMES)
    assert not (RISER_OUTCOMES & PIPELINE_OUTCOMES)


def test_unsupported_outcome_raises() -> None:
    with pytest.raises(ValueError):
        resolve("buckling_check")  # not a defined Outcome


def test_inputs_must_match_outcome_family() -> None:
    # A riser outcome cannot be resolved with mooring inputs.
    with pytest.raises(ValueError):
        resolve(Outcome.RISER_STATIC, MooringResolverInputs(water_depth=500))


# ---------------------------------------------------------------------------
# Riser outcomes
# ---------------------------------------------------------------------------


def test_barebones_riser_resolves_to_valid_spec() -> None:
    spec, ledger = resolve(Outcome.RISER_STATIC)
    assert isinstance(spec, ProjectInputSpec)
    assert spec.riser is not None
    assert spec.mooring is None and spec.pipeline is None and spec.generic is None
    assert spec.metadata.structure == "riser"
    assert len(spec.riser.lines) == 1
    assert len(spec.riser.line_types) == 1
    fields = {r.field for r in ledger.rows()}
    assert "riser.outer_diameter" in fields
    assert "riser.bottom_position" in fields
    assert "environment.water.depth" in fields


def test_string_riser_outcome_accepted() -> None:
    spec, _ = resolve("riser_static")
    assert spec.riser is not None


def test_riser_supplied_inputs_honored_and_not_ledgered() -> None:
    spec, ledger = resolve(
        Outcome.RISER_DYNAMIC,
        RiserResolverInputs(
            water_depth=1200.0,
            outer_diameter=0.4,
            wall_thickness=0.03,
            length=2000.0,
            vessel_name="FPSO_A",
            configuration="lazy_wave",
        ),
    )
    assert spec.environment.water.depth == 1200.0
    lt = spec.riser.line_types[0]
    assert lt.outer_diameter == 0.4
    assert lt.inner_diameter == 0.34  # 0.4 - 2*0.03
    line = spec.riser.lines[0]
    assert line.get_total_length() == 2000.0
    assert spec.riser.vessel.name == "FPSO_A"
    assert line.configuration.value == "lazy_wave"
    fields = {r.field for r in ledger.rows()}
    assert "riser.outer_diameter" not in fields
    assert "riser.wall_thickness" not in fields
    assert "riser.length" not in fields
    assert "riser.vessel_name" not in fields
    # Stiffness/mass are always derived (never user-supplied) -> still ledgered.
    assert "riser.axial_stiffness" in fields


def test_riser_outcome_drives_simulation_stages() -> None:
    static, _ = resolve(Outcome.RISER_STATIC)
    dynamic, _ = resolve(Outcome.RISER_DYNAMIC)
    assert static.simulation.stages == [10]
    assert dynamic.simulation.stages == [8, 16]


def test_riser_bottom_position_scales_with_water_depth() -> None:
    shallow, _ = resolve(Outcome.RISER_STATIC, RiserResolverInputs(water_depth=200))
    deep, _ = resolve(Outcome.RISER_STATIC, RiserResolverInputs(water_depth=1500))
    shallow_x = shallow.riser.lines[0].end_b.position[0]
    deep_x = deep.riser.lines[0].end_b.position[0]
    assert deep_x > shallow_x


def test_partial_spec_riser_is_used_verbatim() -> None:
    partial = {
        "riser": {
            "vessel": {"name": "v"},
            "line_types": [
                {
                    "name": "lt",
                    "outer_diameter": 0.3,
                    "inner_diameter": 0.25,
                    "mass_per_length": 0.15,
                    "bending_stiffness": 20000.0,
                    "axial_stiffness": 2.0e6,
                }
            ],
            "lines": [
                {
                    "name": "R1",
                    "end_a": {"type": "vessel", "name": "v", "position": [40, 0, -10]},
                    "end_b": {"type": "anchor", "position": [800, 0, -1000]},
                    "sections": [
                        {"line_type": "lt", "length": 1400, "segment_length": 10}
                    ],
                }
            ],
        }
    }
    spec, ledger = resolve(
        Outcome.RISER_STATIC, RiserResolverInputs(partial_spec=partial)
    )
    assert len(spec.riser.lines) == 1
    assert spec.riser.lines[0].name == "R1"
    assert not any(r.field.startswith("riser.") for r in ledger.rows())


def test_generates_riser_model_yaml(tmp_path: Path) -> None:
    spec, _ = resolve(Outcome.RISER_DYNAMIC)
    out = tmp_path / "riser_model"
    ModularModelGenerator.from_spec(spec).generate(out)
    assert (out / "master.yml").exists()
    assert any(out.rglob("*.yml"))


# ---------------------------------------------------------------------------
# Pipeline outcomes
# ---------------------------------------------------------------------------


def test_barebones_pipeline_resolves_to_valid_spec() -> None:
    spec, ledger = resolve(Outcome.PIPELINE_ONBOTTOM)
    assert isinstance(spec, ProjectInputSpec)
    assert spec.pipeline is not None
    assert spec.mooring is None and spec.riser is None and spec.generic is None
    assert spec.metadata.structure == "pipeline"
    assert len(spec.pipeline.segments) == 1
    fields = {r.field for r in ledger.rows()}
    assert "pipeline.outer_diameter" in fields
    assert "pipeline.wall_thickness" in fields
    assert "pipeline.coatings.corrosion" in fields


def test_string_pipeline_outcome_accepted() -> None:
    spec, _ = resolve("pipeline_lay")
    assert spec.pipeline is not None


def test_pipeline_supplied_inputs_honored_and_not_ledgered() -> None:
    spec, ledger = resolve(
        Outcome.PIPELINE_LAY,
        PipelineResolverInputs(
            outer_diameter=0.6,
            wall_thickness=0.03,
            length=5000.0,
            material="X70",
        ),
    )
    assert spec.pipeline.dimensions.outer_diameter == 0.6
    assert spec.pipeline.dimensions.wall_thickness == 0.03
    assert spec.pipeline.material == "X70"
    assert sum(s.length for s in spec.pipeline.segments) == 5000.0
    fields = {r.field for r in ledger.rows()}
    assert "pipeline.outer_diameter" not in fields
    assert "pipeline.wall_thickness" not in fields
    assert "pipeline.length" not in fields
    assert "pipeline.material" not in fields


def test_pipeline_outcome_drives_simulation_stages() -> None:
    onbottom, _ = resolve(Outcome.PIPELINE_ONBOTTOM)
    lay, _ = resolve(Outcome.PIPELINE_LAY)
    assert onbottom.simulation.stages == [10]
    assert lay.simulation.stages == [8, 16]


def test_partial_spec_pipeline_is_used_verbatim() -> None:
    partial = {
        "pipeline": {
            "name": "Trunkline",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.5, "wall_thickness": 0.025},
            "coatings": {"corrosion": {"thickness": 0.004, "density": 1.3}},
            "segments": [{"type": "main", "length": 3000, "segment_length": 12}],
        }
    }
    spec, ledger = resolve(
        Outcome.PIPELINE_ONBOTTOM, PipelineResolverInputs(partial_spec=partial)
    )
    assert spec.pipeline.name == "Trunkline"
    assert not any(r.field.startswith("pipeline.") for r in ledger.rows())


def test_generates_pipeline_model_yaml(tmp_path: Path) -> None:
    spec, _ = resolve(Outcome.PIPELINE_ONBOTTOM)
    out = tmp_path / "pipeline_model"
    ModularModelGenerator.from_spec(spec).generate(out)
    assert (out / "master.yml").exists()
    assert any(out.rglob("*.yml"))


def test_ledger_comes_from_shared_common_module() -> None:
    # The promoted ledger is importable from the neutral location and used here.
    from digitalmodel.common.assumption_ledger import AssumptionLedger

    _, ledger = resolve(Outcome.MOORING_STRENGTH)
    assert isinstance(ledger, AssumptionLedger)
