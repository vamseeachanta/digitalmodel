# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic single-zone calculations, independently derived below.

All design numbers are arbitrary test inputs, not standard or catalogue data.
Hand calculation: 120 m2 * 0.05 A/m2 * 0.2 = 1.2 A;
10 years * (365 days/year * 24 hours/day) = 87,600 hours;
1.2 A * 87,600 hours = 105,120 Ah;
105,120 Ah / (1,500 Ah/kg * 0.8) = 87.6 kg.
The time convention matches the existing kernel's non-leap design year.
Zero and one in domain tests are the definitions of fraction bounds.
Other extreme values are synthetic validation probes, not design guidance.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest
import yaml
from pydantic import BaseModel, ValidationError

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.design_spec import CPDesignSpec, design_from_yaml


@pytest.fixture
def design_data():
    """Return explicit synthetic inputs with no material or environment implied."""
    return {
        "area_m2": 120.0,
        "current_density_A_m2": 0.05,
        "coating_fraction": 0.2,
        "design_life_years": 10.0,
        "electrochemical_capacity_Ah_kg": 1500.0,
        "utilisation": 0.8,
        "basis_description": (
            "Synthetic mean demand over the design life; all inputs are assumed "
            "for arithmetic demonstration only."
        ),
        "units": {
            "area_m2": "m2",
            "current_density_A_m2": "A/m2",
            "coating_fraction": "1",
            "design_life_years": "year",
            "electrochemical_capacity_Ah_kg": "Ah/kg",
            "utilisation": "1",
        },
    }


def write_yaml(tmp_path, data):
    """Write only into the temporary test folder."""
    path = tmp_path / "design.yml"
    path.write_text(yaml.safe_dump(data), encoding="utf-8")
    return str(path)


def test_yaml_design_matches_hand_calculation(tmp_path, design_data):
    path = write_yaml(tmp_path, design_data)
    spec = CPDesignSpec.from_yaml(path)
    assert isinstance(spec, BaseModel)
    assert spec.model_dump() == design_data
    result = design_from_yaml(path)
    assert result["input_basis"] == design_data
    assert result["current_demand_A"] == pytest.approx(1.2)
    assert result["required_charge_Ah"] == pytest.approx(105120.0)
    assert result["required_anode_mass_kg"] == pytest.approx(87.6)
    assert result["units"] == {
        **design_data["units"],
        "current_demand_A": "A",
        "required_charge_Ah": "Ah",
        "required_anode_mass_kg": "kg",
    }
    assert "preliminary screening" in result["limitation"].lower()
    assert "distribution" in result["limitation"].lower()
    assert "potential" in result["limitation"].lower()
    assert "resistance" in result["limitation"].lower()


@pytest.mark.parametrize("nested", [False, True], ids=["design", "units"])
def test_unknown_yaml_keys_rejected(tmp_path, design_data, nested):
    target = design_data["units"] if nested else design_data
    target["unexpected"] = "unsupported"
    with pytest.raises(ValidationError, match="extra_forbidden"):
        design_from_yaml(write_yaml(tmp_path, design_data))


NUMERIC_FIELDS = (
    "area_m2",
    "current_density_A_m2",
    "coating_fraction",
    "design_life_years",
    "electrochemical_capacity_Ah_kg",
    "utilisation",
)


@pytest.mark.parametrize("field", NUMERIC_FIELDS)
@pytest.mark.parametrize(
    "value",
    [-1.0, float("nan"), float("inf"), -float("inf"), True, False, "0.5", None, []],
    ids=["negative", "nan", "inf", "minus-inf", "true", "false", "text", "null", "list"],
)
def test_invalid_design_inputs(tmp_path, design_data, field, value):
    design_data[field] = value
    with pytest.raises(ValidationError, match=field):
        design_from_yaml(write_yaml(tmp_path, design_data))


@pytest.mark.parametrize("field", [f for f in NUMERIC_FIELDS if f != "coating_fraction"])
def test_zero_positive_inputs_rejected(design_data, field):
    design_data[field] = 0.0
    with pytest.raises(ValidationError, match=field):
        CPDesignSpec.model_validate(design_data)


@pytest.mark.parametrize("field", ["coating_fraction", "utilisation"])
def test_fraction_above_one_rejected(design_data, field):
    design_data[field] = 1.01  # Synthetic value just outside a fraction's range.
    with pytest.raises(ValidationError, match=field):
        CPDesignSpec.model_validate(design_data)


@pytest.mark.parametrize("fraction", [0.0, 1.0])
def test_coating_fraction_endpoints(tmp_path, design_data, fraction):
    design_data["coating_fraction"] = fraction
    design_data["utilisation"] = 1.0
    result = design_from_yaml(write_yaml(tmp_path, design_data))
    # Independent endpoint arithmetic: 120 * 0.05 = 6 A on bare area;
    # 6 * 10 * 365 * 24 / 1500 = 350.4 kg at full utilisation.
    assert result["current_demand_A"] == pytest.approx(6.0 * fraction)
    assert result["required_anode_mass_kg"] == pytest.approx(350.4 * fraction)


@pytest.mark.parametrize("field", [*NUMERIC_FIELDS, "basis_description", "units"])
def test_every_input_is_required(design_data, field):
    del design_data[field]
    with pytest.raises(ValidationError, match=field):
        CPDesignSpec.model_validate(design_data)


@pytest.mark.parametrize("field", NUMERIC_FIELDS)
def test_every_unit_is_required(design_data, field):
    del design_data["units"][field]
    with pytest.raises(ValidationError, match=field):
        CPDesignSpec.model_validate(design_data)


@pytest.mark.parametrize("field", NUMERIC_FIELDS)
def test_unit_mismatch_rejected(tmp_path, design_data, field):
    design_data["units"][field] = "unsupported-unit"
    with pytest.raises(ValidationError, match=field):
        design_from_yaml(write_yaml(tmp_path, design_data))


@pytest.mark.parametrize("basis", ["", "  \n\t", None, 1, []])
def test_basis_description_required(design_data, basis):
    design_data["basis_description"] = basis
    with pytest.raises(ValidationError, match="basis_description"):
        CPDesignSpec.model_validate(design_data)


@pytest.mark.parametrize("document", ["", "null", "[]", "42", "false", "basis_description: ["])
def test_invalid_yaml_document_rejected(tmp_path, document):
    path = tmp_path / "invalid.yml"
    path.write_text(document, encoding="utf-8")
    with pytest.raises((ValidationError, yaml.YAMLError)):
        design_from_yaml(str(path))


def test_yaml_python_tag_rejected_without_execution(tmp_path):
    marker = tmp_path / "must-not-exist"
    path = tmp_path / "unsafe.yml"
    path.write_text(
        "!!python/object/apply:pathlib.Path.write_text\n"
        + yaml.safe_dump([str(marker), "unexpected"]),
        encoding="utf-8",
    )
    with pytest.raises(yaml.constructor.ConstructorError):
        design_from_yaml(str(path))
    assert not marker.exists()


@pytest.mark.parametrize("nested", [False, True], ids=["design", "units"])
def test_duplicate_yaml_keys_rejected(tmp_path, design_data, nested):
    text = yaml.safe_dump(design_data)
    if nested:
        text = text.replace("units:\n", "units:\n  area_m2: m2\n")
    else:
        text += "area_m2: 240.0\n"  # Conflicting synthetic area.
    path = tmp_path / "duplicate.yml"
    path.write_text(text, encoding="utf-8")
    with pytest.raises(yaml.constructor.ConstructorError, match="duplicate"):
        design_from_yaml(str(path))


@pytest.mark.parametrize(
    "updates",
    [
        {"area_m2": 1e308, "current_density_A_m2": 10.0},
        {"design_life_years": 1e308},
        {"electrochemical_capacity_Ah_kg": 1e-300, "utilisation": 1e-300},
        {"area_m2": 1e-300, "current_density_A_m2": 1e-300},
    ],
    ids=["current-overflow", "charge-overflow", "denominator-underflow", "current-underflow"],
)
def test_unrepresentable_calculation_rejected(tmp_path, design_data, updates):
    design_data.update(updates)
    with pytest.raises(ValueError):
        design_from_yaml(write_yaml(tmp_path, design_data))


def test_workflow_delegates_to_shared_kernels(tmp_path, design_data, monkeypatch):
    calls = []
    original_current = kernel.current_demand
    original_mass = kernel.anode_mass

    def current(*args):
        calls.append(("current", args))
        return original_current(*args)

    def mass(*args):
        calls.append(("mass", args))
        return original_mass(*args)

    monkeypatch.setattr(kernel, "current_demand", current)
    monkeypatch.setattr(kernel, "anode_mass", mass)
    design_from_yaml(write_yaml(tmp_path, design_data))
    assert calls == [
        ("current", (120.0, 0.05, 0.2)),
        ("mass", (pytest.approx(1.2), 10.0, 1500.0, 0.8)),
    ]


def test_synthetic_example_runs(tmp_path, monkeypatch):
    example = (
        Path(__file__).resolve().parents[2]
        / "examples/cathodic_protection/synthetic_design.yml"
    )
    # An absolute input remains usable when the output folder is the cwd.
    monkeypatch.chdir(tmp_path)
    result = design_from_yaml(str(example))
    output = tmp_path / "synthetic_design_result.json"
    output.write_text(json.dumps(result, allow_nan=False), encoding="utf-8")
    saved = json.loads(output.read_text(encoding="utf-8"))
    assert saved["current_demand_A"] == pytest.approx(1.2)
    assert saved["required_charge_Ah"] == pytest.approx(105120.0)
    assert saved["required_anode_mass_kg"] == pytest.approx(87.6)
    assert saved["input_basis"]["basis_description"].startswith("Synthetic")
    assert sorted(path.name for path in tmp_path.iterdir()) == [output.name]
