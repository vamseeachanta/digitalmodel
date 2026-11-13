"""Campaign overrides and spec files keep unset-field provenance.

A dotted sweep re-validates the spec from a dump. A full ``model_dump()``
marks every schema default as authored, so renderers that overlay authored
fields only (the riser-plus-generic route) emitted defaults over retained
native values. The overrides now dump authored and edited fields only. The
expected values below are the authored inputs of the synthetic spec.
"""

from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import (
    apply_dotted_override,
)


def _spec() -> ProjectInputSpec:
    return ProjectInputSpec(**{
        "metadata": {"name": "r", "description": "r", "structure": "riser",
                     "operation": "production", "project": "T"},
        "environment": {"water": {"depth": 100},
                        "seabed": {"stiffness": {"normal": 100, "shear": 100}}},
        "riser": {
            "vessel": {"name": "V"},
            "line_types": [{"name": "lt", "outer_diameter": 0.3, "inner_diameter": 0.2,
                            "mass_per_length": 0.1, "bending_stiffness": 100,
                            "axial_stiffness": 5e5}],
            "lines": [{
                "name": "L",
                "end_a": {"type": "vessel", "name": "V", "position": [0, 0, -5]},
                "end_b": {"type": "anchor", "position": [100, 0, 0]},
                "sections": [{"line_type": "lt", "length": 150, "segment_length": 5}],
            }],
        },
    })


def test_override_leaves_unrelated_defaults_unset():
    result = apply_dotted_override(_spec(), "environment.water.depth", 180)
    assert result.environment.water.depth == 180
    assert "friction_coefficient" not in result.environment.seabed.model_fields_set
    assert "wind" not in result.environment.model_fields_set
    (line_type,) = result.riser.line_types
    assert "axial_drag_coefficient" not in line_type.model_fields_set
    assert "drag_coefficient" not in line_type.model_fields_set


def test_override_of_default_valued_field_is_applied():
    result = apply_dotted_override(_spec(), "environment.wind.speed", 12.0)
    assert result.environment.wind.speed == 12.0


def test_in_place_edit_below_unset_parent_survives_override():
    """Typed campaign axes edit nested models in place before sweeps run."""
    from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import (
        dump_authored,
    )

    spec = _spec()
    spec.environment.wind.speed = 7.0
    result = apply_dotted_override(spec, "environment.water.depth", 180)
    assert result.environment.wind.speed == 7.0
    assert dump_authored(spec)["environment"]["wind"]["speed"] == 7.0


def test_override_does_not_mutate_input_provenance():
    spec = _spec()
    spec.environment.wind.speed = 7.0
    before = set(spec.environment.model_fields_set)
    apply_dotted_override(spec, "environment.water.depth", 180)
    assert spec.environment.model_fields_set == before


def test_unknown_override_path_is_still_rejected():
    with pytest.raises(KeyError, match="not found"):
        apply_dotted_override(_spec(), "environment.water.no_such_field", 1)
