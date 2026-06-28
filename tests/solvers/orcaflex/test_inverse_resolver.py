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
    MooringResolverInputs,
    Outcome,
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


def test_unsupported_outcome_raises() -> None:
    with pytest.raises(ValueError):
        resolve("riser_static")  # not yet implemented


def test_ledger_comes_from_shared_common_module() -> None:
    # The promoted ledger is importable from the neutral location and used here.
    from digitalmodel.common.assumption_ledger import AssumptionLedger

    _, ledger = resolve(Outcome.MOORING_STRENGTH)
    assert isinstance(ledger, AssumptionLedger)
