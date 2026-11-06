# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Structural export checks with synthetic inputs and hand-derived outputs.

All fixture quantities are arbitrary test inputs, not design recommendations.
Unit factors follow BIPM, SI Brochure, ninth edition (2019), Tables 7 and 8.
"""

import math
import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

from digitalmodel.subsea.connectors import (
    JumperType,
    PipeSection,
    build_jumper_line_type,
    get_jumper_spec,
)


def inputs(**changes):
    """Return synthetic effective properties; no material model is implied."""
    values = dict(
        name="dynamic-jumper",
        mass_per_length_kg_m=250.0,
        axial_stiffness_n=4_000_000.0,
        bending_stiffness_nm2=(20_000.0, 30_000.0),
        torsional_stiffness_nm2=10_000.0,
    )
    values.update(changes)
    return values


def export(**changes):
    """Export a synthetic flexible-jumper section."""
    return build_jumper_line_type(
        get_jumper_spec(JumperType.FLEXIBLE),
        PipeSection(0.25, 0.025),
        **inputs(**changes),
    )


def test_structural_line_type_has_hand_calculated_units():
    """ID = 0.25 - 2*0.025 = 0.20 m, from the definition of wall thickness.

    Table 8 defines 1 tonne = 1000 kg; Table 7 defines kilo = 1000.
    Thus 250 kg/m = 0.25 te/m; 4000000 N = 4000 kN;
    (20000, 30000) N.m2 = (20, 30) kN.m2; 10000 N.m2 = 10 kN.m2.
    These divisions are independent of the exporter and the generic builder.
    """
    result = export()
    assert result == {
        "name": "dynamic-jumper",
        "category": "General",
        "outer_diameter": 0.25,
        "inner_diameter": pytest.approx(0.20),
        "mass_per_length": 0.25,
        "axial_stiffness": 4000.0,
        "bending_stiffness": [20.0, 30.0],
        "properties": {"GJ": 10.0},
    }


def test_generic_builder_emits_yaml_without_changing_structural_values():
    """Use the existing generic pipeline; expectations are the divisions above."""
    from digitalmodel.solvers.orcaflex.modular_generator.builders.context import (
        BuilderContext,
    )
    from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import (
        GenericModelBuilder,
    )
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    # Environment values are synthetic schema inputs, not design defaults.
    spec = ProjectInputSpec(
        metadata=dict(name="jumper-example", description="Synthetic unit example",
                      structure="jumper", operation="generic"),
        environment=dict(water=dict(depth=100.0, density=1.0),
                         seabed=dict(stiffness=dict(normal=100.0, shear=100.0))),
        generic=dict(line_types=[export()]),
    )
    result = GenericModelBuilder(spec, BuilderContext()).build()
    emitted = yaml.safe_load(yaml.safe_dump(result))["LineTypes"]
    assert emitted == [{
        "Name": "dynamic-jumper", "Category": "General",
        "OD": 0.25, "ID": pytest.approx(0.20), "MassPerUnitLength": 0.25,
        "EA": 4000.0, "EI": [20.0, 30.0], "GJ": 10.0,
    }]


def test_export_and_yaml_builder_neither_import_nor_probe_solver_binding():
    """Repeat the hand-calculated integration check with fresh, guarded imports."""
    script = """
import importlib.abc
import importlib.util
import runpy
import sys

def reject_solver(name):
    if name.split('.')[0].lower() == 'orcfxapi':
        raise AssertionError('Solver binding must not be imported or probed')

class Guard(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        reject_solver(fullname)

original_find_spec = importlib.util.find_spec
def guarded_find_spec(name, package=None):
    reject_solver(name)
    return original_find_spec(name, package)

sys.meta_path.insert(0, Guard())
importlib.util.find_spec = guarded_find_spec
tests = runpy.run_path(sys.argv[1])
tests['test_generic_builder_emits_yaml_without_changing_structural_values']()
"""
    test_file = Path(__file__).resolve()
    env = dict(os.environ, PYTHONPATH=str(test_file.parents[3] / "src"))
    result = subprocess.run(
        [sys.executable, "-c", script, str(test_file)], env=env,
        capture_output=True, text=True, timeout=60,
    )
    assert result.returncode == 0, result.stderr


@pytest.mark.parametrize("jumper_type", [JumperType.FLEXIBLE, JumperType.VERTICAL])
def test_dynamic_catalogue_entries_are_supported(jumper_type):
    """The catalogue dynamic flag is the export trigger, not pipe construction."""
    result = build_jumper_line_type(
        get_jumper_spec(jumper_type), PipeSection(0.25, 0.025), **inputs(),
    )
    assert result["name"] == "dynamic-jumper"


def test_static_catalogue_entry_is_rejected():
    """This slice exports dynamic entries only."""
    with pytest.raises(ValueError, match="dynamic"):
        build_jumper_line_type(
            get_jumper_spec(JumperType.RIGID), PipeSection(0.25, 0.025), **inputs(),
        )


@pytest.mark.parametrize("name", ["", "  ", None])
def test_empty_name_is_rejected(name):
    with pytest.raises(ValueError, match="name"):
        export(name=name)


@pytest.mark.parametrize("field", ["mass_per_length_kg_m", "axial_stiffness_n"])
@pytest.mark.parametrize("value", [0.0, -1.0, math.nan, math.inf, -math.inf, True])
def test_mass_and_axial_stiffness_require_finite_positive_inputs(field, value):
    with pytest.raises(ValueError, match=field):
        export(**{field: value})


@pytest.mark.parametrize("value", [-1.0, math.nan, math.inf, -math.inf, True])
def test_invalid_torsional_stiffness_is_rejected(value):
    with pytest.raises(ValueError, match="torsional_stiffness_nm2"):
        export(torsional_stiffness_nm2=value)


@pytest.mark.parametrize("axis", [0, 1])
@pytest.mark.parametrize("value", [-1.0, math.nan, math.inf, -math.inf, True])
def test_invalid_bending_stiffness_on_either_axis_is_rejected(axis, value):
    stiffness = [20_000.0, 30_000.0]
    stiffness[axis] = value
    with pytest.raises(ValueError, match="bending_stiffness_nm2"):
        export(bending_stiffness_nm2=stiffness)


@pytest.mark.parametrize("stiffness", [(), (1.0,), (1.0, 2.0, 3.0), 1.0, None])
def test_both_bending_directions_must_be_explicit(stiffness):
    with pytest.raises(ValueError, match="bending_stiffness_nm2"):
        export(bending_stiffness_nm2=stiffness)


def test_zero_bending_and_torsional_stiffness_are_preserved():
    """Zero input converts to zero; it must not select a hidden stiffness."""
    result = export(bending_stiffness_nm2=(0.0, 0.0), torsional_stiffness_nm2=0.0)
    assert result["bending_stiffness"] == [0.0, 0.0]
    assert result["properties"]["GJ"] == 0.0


def test_nonfinite_section_is_rejected():
    """PipeSection permits infinite OD, so the export boundary checks it."""
    with pytest.raises(ValueError, match="section"):
        build_jumper_line_type(
            get_jumper_spec(JumperType.FLEXIBLE), PipeSection(math.inf, 0.025),
            **inputs(),
        )


@pytest.mark.parametrize("field", list(inputs()))
def test_each_export_property_is_required(field):
    values = inputs()
    del values[field]
    with pytest.raises(TypeError, match=field):
        build_jumper_line_type(
            get_jumper_spec(JumperType.FLEXIBLE), PipeSection(0.25, 0.025), **values,
        )
