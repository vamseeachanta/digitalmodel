"""Production validator parity for the B3 serialization defects."""

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema.environment import WaveTrain


@pytest.mark.parametrize("selector", ["No waves", " no waves ", "NO WAVES"])
def test_invalid_selector_rejected_by_schema_and_validator(tmp_path, selector):
    with pytest.raises(ValueError, match="No waves"):
        WaveTrain(type=selector)
    data = {"Environment": {"WaveTrains": [
        {"Name": "still water", "WaveType": selector},
    ]}}
    findings = _both_entry_points(tmp_path, data)
    assert len(findings) == 1
    assert findings[0].level == "error"
    assert findings[0].category == "wave_type"


@pytest.mark.parametrize("selector", ["Airy", "JONSWAP", "User defined"])
def test_existing_selectors_remain_supported(tmp_path, selector):
    WaveTrain(type=selector, height=0, period=8)
    data = {"Environment": {"WaveTrains": [{"WaveType": selector}]}}
    assert _both_entry_points(tmp_path, data) == []


def test_numeric_and_string_object_names_collide(tmp_path):
    data = {"LineTypes": [{"Name": 1}], "Lines": [{"Name": "1"}]}
    findings = _both_entry_points(tmp_path, data)
    assert len(findings) == 1
    assert findings[0].category == "duplicate"
    assert findings[0].level == "error"
    assert findings[0].object_name == "1"


def _both_entry_points(tmp_path, data):
    (tmp_path / "model.yml").write_text(yaml.safe_dump(data), encoding="utf-8")
    validator = PostGenerationValidator()
    findings = validator.validate_data({"model.yml": data})
    assert validator.validate_directory(tmp_path) == findings
    return findings


def test_case_variant_names_collide(tmp_path):
    data = {"LineTypes": [{"Name": "Riser"}], "Lines": [{"Name": "RISER"}]}
    findings = _both_entry_points(tmp_path, data)
    assert len(findings) == 1
    assert findings[0].category == "duplicate"
    assert findings[0].level == "error"


def test_invalid_second_raw_wave_train_rejected(tmp_path):
    data = {"Environment": {"WaveTrains": [
        {"Name": "Wave 1", "WaveType": "Airy", "WaveHeight": 0, "WavePeriod": 8},
        {"Name": "Wave 2", "WaveType": "No waves"},
    ]}}
    findings = _both_entry_points(tmp_path, data)
    assert len(findings) == 1
    assert findings[0].category == "wave_type"
    assert findings[0].object_name == "Wave 2"
