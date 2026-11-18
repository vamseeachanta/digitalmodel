# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Simulation-control preservation for the jumper conversion boundary (#602).

No engineering equation is evaluated here. Synthetic simulation values are
caller inputs, not recommended analysis settings. The independent oracle is
identity: inputs must equal their values at each tested boundary.
Field meanings and validation rules come from the repository's Simulation
schema (schema/simulation.py); emitted keys come from GeneralBuilder.build.
"""

from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.marine_ops.installation.jumper_to_modular_spec import (
    build_modular_spec,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


@pytest.fixture
def domain_spec(tmp_path):
    """Supply only the domain blocks needed by stage_1_load_spec.

    Existing geometry defaults are assumed, based on previous experience;
    these tests make no assertion about their engineering suitability.
    """
    def write(simulation=None):
        data = {"metadata": {"name": "sample_jumper"}, "pipe": {}, "environment": {}}
        if simulation is not None:
            data["simulation"] = simulation
        path = tmp_path / "spec.yml"
        path.write_text(yaml.safe_dump(data))
        return path

    return write


def test_explicit_simulation_controls_survive_validation(domain_spec):
    """Identity oracle: synthetic inputs are copied without arithmetic.

    Distinct, exactly representable binary fractions make exact equality
    appropriate; no numerical tolerance or engineering limit is introduced.
    """
    controls = {
        "time_step": 0.25,
        "stages": [3, 12.5, 25],
        "north_direction": 45,
        "inner_time_step": 0.0625,
        "implicit_constant": 0.5,
    }
    data = build_modular_spec(domain_spec(controls))
    assert data["simulation"] == controls
    model = ProjectInputSpec(**data)
    assert model.simulation.model_dump(exclude_none=True) == controls


@pytest.mark.parametrize("controls", [None, {}, {"time_step": 0.25}, {"stages": [3]}])
def test_missing_simulation_fields_keep_legacy_conversion_defaults(domain_spec, controls):
    """Compatibility oracle: build_modular_spec at the pre-change revision
    sets time_step=0.1 and stages=[10.0, 60.0]. These are legacy conversion
    values, not acceptance criteria. Explicit synthetic inputs replace only
    the matching field; an absent or empty block keeps both legacy values.
    """
    # Source: pre-change jumper_to_modular_spec.py, build_modular_spec return.
    expected = {"time_step": 0.1, "stages": [10.0, 60.0]}
    expected.update(controls or {})
    data = build_modular_spec(domain_spec(controls))
    assert data["simulation"] == expected
    model = ProjectInputSpec(**data)
    assert model.simulation.time_step == expected["time_step"]
    assert model.simulation.stages == expected["stages"]


@pytest.mark.parametrize("controls", [{"time_step": -1}, {"stages": []}])
def test_invalid_simulation_inputs_reach_generator_schema(domain_spec, controls):
    """Negative time and an empty stage list are synthetic invalid inputs.

    Source: Simulation.time_step (gt=0) and Simulation.stages (min_length=1)
    in schema/simulation.py. Conversion must preserve invalid inputs so the
    generator schema rejects them instead of silently replacing them.
    """
    data = build_modular_spec(domain_spec(controls))
    with pytest.raises(ValidationError) as exc:
        ProjectInputSpec(**data)
    assert any(error["loc"][0] == "simulation" for error in exc.value.errors())


def test_simulation_controls_reach_generated_yaml(domain_spec, tmp_path):
    """Identity oracle for synthetic input -> General and parameter YAML.

    Source: GeneralBuilder.build maps time_step to ImplicitConstantTimeStep
    and TargetLogSampleInterval, stages to StageDuration, and north_direction
    to NorthDirection. No solver execution or stability claim is involved.
    """
    controls = {"time_step": 0.25, "stages": [3, 12.5, 25], "north_direction": 45}
    model = ProjectInputSpec(**build_modular_spec(domain_spec(controls)))
    output = tmp_path / "model"
    ModularModelGenerator.from_spec(model).generate(output)

    # Parse every emitted include and confirm all master references exist.
    parsed = {
        path.relative_to(output): yaml.safe_load(path.read_text())
        for path in output.rglob("*.yml")
    }
    for entry in parsed[Path("master.yml")]:
        assert Path(entry["includefile"]) in parsed
    general = parsed[Path("includes/01_general.yml")]["General"]
    assert general["ImplicitConstantTimeStep"] == controls["time_step"]
    assert general["TargetLogSampleInterval"] == controls["time_step"]
    assert general["StageDuration"] == controls["stages"]
    assert general["NorthDirection"] == controls["north_direction"]
    parameters = parsed[Path("inputs/parameters.yml")]
    assert parameters["time_step"] == controls["time_step"]
    assert parameters["stage_durations"] == controls["stages"]
