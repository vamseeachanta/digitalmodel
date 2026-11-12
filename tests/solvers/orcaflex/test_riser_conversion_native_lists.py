"""Riser conversion of native list-valued line-type data (#3980, #3985).

Native OrcaFlex line types carry ``EI`` as ``[x, ~]`` and ``Cd``/``Ca`` as
``[normal, ~, axial]``. The bridge accepted scalars only, so none of the
public example risers could be upgraded, and it wrote a typed axial drag
coefficient to the non-native data name ``CdAxial``. The comparators are the
public OrcaFlex example risers under docs/domains/orcaflex/library/model_library.
"""

from __future__ import annotations

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.spec_upgrader import DetectedModelType, upgrade_spec

LIBRARY = Path(__file__).resolve().parents[3] / "docs/domains/orcaflex/library/model_library"
# Single-line public examples whose referenced line types are General
# category. A05 catenary spar and A05 lazy wave use homogeneous-pipe line
# types, which the riser schema does not represent (see the review, #3990).
SINGLE_LINE_RISERS = ["a01_catenary_riser"]


def _spec(name: str) -> dict:
    return yaml.safe_load((LIBRARY / name / "spec.yml").read_text(encoding="utf-8"))


def _generated(spec: dict, output: Path) -> dict:
    ModularModelGenerator.from_spec(ProjectInputSpec(**spec)).generate(output)
    return {
        path.name: yaml.safe_load(path.read_text(encoding="utf-8"))
        for path in (output / "includes").glob("*.yml")
    }


def _numeric(value):
    """PyYAML (YAML 1.1) reads native '711.2e3' as a string; the typed schema
    stores the same number as 711200.0. Compare such strings as numbers."""
    if isinstance(value, dict):
        return {k: _numeric(v) for k, v in value.items()}
    if isinstance(value, list):
        return [_numeric(v) for v in value]
    if isinstance(value, str):
        try:
            return float(value)
        except ValueError:
            return value
    return value


def _native_line_type(spec: dict, name: str) -> dict:
    obj = next(lt for lt in spec["generic"]["line_types"] if lt["name"] == name)
    merged = dict(obj.get("properties", {}))
    for field, native in (("bending_stiffness", "EI"),):
        if obj.get(field) is not None:
            merged[native] = obj[field]
    return merged


@pytest.mark.parametrize("name", SINGLE_LINE_RISERS)
def test_public_example_riser_upgrades_with_native_cells(name):
    spec = _spec(name)
    upgraded = upgrade_spec(spec, DetectedModelType.RISER)
    for typed in upgraded["riser"]["line_types"]:
        native = _native_line_type(spec, typed["name"])
        if isinstance(native.get("EI"), list):
            assert typed["bending_stiffness"] == native["EI"][0]
        if isinstance(native.get("Cd"), list):
            assert typed["drag_coefficient"] == native["Cd"][0]
            assert typed["axial_drag_coefficient"] == native["Cd"][2]
        if isinstance(native.get("Ca"), list):
            assert typed["added_mass_coefficient"] == native["Ca"][0]


@pytest.mark.parametrize("name", SINGLE_LINE_RISERS)
def test_public_example_riser_upgrade_is_lossless(name, tmp_path):
    spec = _spec(name)
    before = _generated(spec, tmp_path / "before")
    upgraded = yaml.safe_load(yaml.safe_dump(upgrade_spec(spec, DetectedModelType.RISER)))
    assert _numeric(_generated(upgraded, tmp_path / "after")) == _numeric(before)


def test_typed_coefficient_edits_land_in_native_cells(tmp_path):
    upgraded = upgrade_spec(_spec("a01_catenary_riser"), DetectedModelType.RISER)
    line_type = upgraded["riser"]["line_types"][0]
    line_type.update(bending_stiffness=150.0, drag_coefficient=1.05,
                     axial_drag_coefficient=0.01, added_mass_coefficient=0.9)
    objects = _generated(upgraded, tmp_path / "edited")["20_generic_objects.yml"]
    emitted = next(lt for lt in objects["LineTypes"] if lt["Name"] == line_type["name"])
    assert emitted["EI"][0] == 150.0
    assert emitted["Cd"][0] == 1.05 and emitted["Cd"][2] == 0.01
    assert emitted["Ca"][0] == 0.9
    assert "CdAxial" not in emitted


def test_axial_drag_on_scalar_cd_writes_native_list(tmp_path):
    """A scalar fallback Cd plus an authored axial value becomes [n, ~, a]."""
    spec = deepcopy(_spec("a01_catenary_riser"))
    spec["generic"]["line_types"][0].setdefault("properties", {})["Cd"] = 1.1
    upgraded = upgrade_spec(spec, DetectedModelType.RISER)
    upgraded["riser"]["line_types"][0]["axial_drag_coefficient"] = 0.02
    objects = _generated(upgraded, tmp_path / "axial")["20_generic_objects.yml"]
    emitted = objects["LineTypes"][0]
    assert emitted["Cd"] == [1.1, None, 0.02]
    assert "CdAxial" not in emitted


def test_anisotropic_ei_rejected():
    spec = _spec("a01_catenary_riser")
    spec = deepcopy(spec)
    lt = spec["generic"]["line_types"][0]
    if lt.get("bending_stiffness") is not None:
        lt["bending_stiffness"] = [100.0, 200.0]
    else:
        lt["properties"]["EI"] = [100.0, 200.0]
    with pytest.raises(ValueError, match="EI"):
        upgrade_spec(spec, DetectedModelType.RISER)
