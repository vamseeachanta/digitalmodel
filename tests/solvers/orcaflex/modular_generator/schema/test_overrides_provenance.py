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


# In-place edits inside default containers. Python spec API edits such as
# ``spec.simulation.stages[1] = 30`` mutate a list or dict without recording a
# field assignment, so an authored-only dump dropped them and the next override
# restored the default. Expected values are the synthetic edited inputs; the
# unedited neighbours must stay unset.

_LATERAL = "SeabedLateralFrictionCoefficient"


def _generic_spec() -> ProjectInputSpec:
    return ProjectInputSpec(**{
        "metadata": {"name": "g", "description": "g", "structure": "riser",
                     "operation": "production", "project": "T"},
        "environment": {"water": {"depth": 100},
                        "seabed": {"stiffness": {"normal": 100, "shear": 100}}},
        "generic": {"line_types": [{"name": "lt", "outer_diameter": 0.3}]},
    })


def _get(obj, path: str):
    for key in path.split("."):
        if isinstance(obj, list):
            obj = obj[int(key)]
        elif isinstance(obj, dict):
            obj = obj[key]
        else:
            obj = getattr(obj, key)
    return obj


def _edit_dict_item(spec):
    spec.environment.raw_properties["SeaTemperature"] = 12.5


def _edit_model_dict_item(spec):
    spec.generic.line_types[0].properties[_LATERAL] = 0.9


def _edit_list_item(spec):
    spec.simulation.stages[1] = 30


def _edit_nested_list_item(spec):
    spec.environment.current.profile[0][1] = 0.4


def _edit_list_append(spec):
    spec.environment.current.profile.append([50.0, 0.5])


def _edit_nested_model(spec):
    spec.environment.wind.speed = 7.0


# (factory, edit, read path, expected value, unedited neighbour that stays unset)
_CONTAINER_EDITS = {
    "dict_item": (_spec, _edit_dict_item, "environment.raw_properties.SeaTemperature",
                  12.5, ("environment", "waves")),
    "dict_item_in_list_model": (_generic_spec, _edit_model_dict_item,
                                f"generic.line_types.0.properties.{_LATERAL}", 0.9,
                                ("generic.line_types.0", "category")),
    "list_item": (_spec, _edit_list_item, "simulation.stages", [8, 30],
                  ("simulation", "time_step")),
    "nested_list_item": (_spec, _edit_nested_list_item, "environment.current.profile",
                         [[0, 0.4]], ("environment.current", "speed")),
    "list_append": (_spec, _edit_list_append, "environment.current.profile",
                    [[0, 1.0], [50.0, 0.5]], ("environment.current", "speed")),
    "nested_model": (_spec, _edit_nested_model, "environment.wind.speed", 7.0,
                     ("environment.wind", "direction")),
}
_CASES = pytest.mark.parametrize("case", list(_CONTAINER_EDITS))


def _edited(case):
    factory, edit, path, expected, (owner, unset_field) = _CONTAINER_EDITS[case]
    spec = factory()
    edit(spec)
    return spec, path, expected, owner, unset_field


@_CASES
def test_container_edit_survives_direct_override(case):
    spec, path, expected, owner, unset_field = _edited(case)
    result = apply_dotted_override(spec, "environment.water.depth", 180)
    assert result.environment.water.depth == 180
    assert _get(result, path) == expected
    assert unset_field not in _get(result, owner).model_fields_set
    assert "friction_coefficient" not in result.environment.seabed.model_fields_set


@_CASES
def test_container_edit_survives_authored_dump(case):
    from digitalmodel.solvers.orcaflex.modular_generator.schema._overrides import (
        dump_authored,
    )

    spec, path, expected, owner, unset_field = _edited(case)
    dumped = dump_authored(spec, mode="json")
    assert _get(dumped, path) == expected
    assert unset_field not in _get(dumped, owner)
    assert "friction_coefficient" not in dumped["environment"]["seabed"]


def _campaign_run(spec, tmp_path, spec_only):
    import yaml

    from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
        CampaignGenerator,
    )

    campaign = {
        "base": {"metadata": spec.metadata.model_dump(mode="json"),
                 "environment": {"water": {"depth": 100},
                                 "seabed": {"stiffness": {"normal": 100, "shear": 100}}},
                 "generic": {"line_types": [{"name": "lt", "outer_diameter": 0.3}]}},
        "campaign": {"sweeps": [{"parameter": "environment.water.depth",
                                 "values": [180], "alias": "wd"}]},
        "output_naming": "{base_name}_wd{wd}",
    }
    campaign_file = tmp_path / "campaign.yml"
    campaign_file.write_text(yaml.safe_dump(campaign, sort_keys=False), encoding="utf-8")
    generator = CampaignGenerator(campaign_file)
    return generator, tmp_path / "out"


@pytest.mark.parametrize("case", ["dict_item", "dict_item_in_list_model", "list_item",
                                  "nested_list_item", "list_append", "nested_model"])
def test_container_edit_survives_campaign_sweep_spec_file(case, tmp_path):
    """Sweep path: base spec edited through the Python API, then swept."""
    import yaml

    _, edit, path, expected, (owner, unset_field) = _CONTAINER_EDITS[case]
    generator, out = _campaign_run(_generic_spec(), tmp_path, spec_only=True)
    edit(generator.spec.base)
    result = generator.generate(out, spec_only=True)
    assert result.errors == [] and result.run_count == 1
    (run_dir,) = result.run_dirs
    written = yaml.safe_load((run_dir / "spec.yml").read_text(encoding="utf-8"))
    assert written["environment"]["water"]["depth"] == 180
    assert _get(written, path) == expected
    assert unset_field not in _get(written, owner)
    assert "friction_coefficient" not in written["environment"]["seabed"]


def test_container_edit_survives_campaign_sweep_generated_yaml(tmp_path):
    import yaml

    generator, out = _campaign_run(_generic_spec(), tmp_path, spec_only=False)
    _edit_model_dict_item(generator.spec.base)
    result = generator.generate(out, spec_only=False)
    assert result.errors == [] and result.run_count == 1
    (run_dir,) = result.run_dirs
    objects = yaml.safe_load(
        (run_dir / "includes" / "20_generic_objects.yml").read_text(encoding="utf-8"))
    (line_type,) = objects["LineTypes"]
    assert line_type[_LATERAL] == 0.9


def test_invalid_container_edit_is_reported_not_reset():
    """Clearing the stage list in place is an edit; the sweep must reject it."""
    spec = _spec()
    spec.simulation.stages.clear()
    with pytest.raises(ValueError, match="environment.water.depth"):
        apply_dotted_override(spec, "environment.water.depth", 180)
