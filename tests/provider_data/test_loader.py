"""Exercise YAML loading even before the companion library is available."""

from dataclasses import asdict
import os
import subprocess
import sys

import pytest
import yaml

from digitalmodel.provider_data import ProviderDataRecord, SpecValue, load_provider_yaml


def _write_payload(tmp_path, payload):
    path = tmp_path / "parameters.yaml"
    path.write_text(yaml.safe_dump(payload), encoding="utf-8")
    return path


def test_loader_numeric_categorical_and_list_round_trip(tmp_path, parameters_payload):
    path = _write_payload(tmp_path, parameters_payload)
    record = load_provider_yaml(path)
    assert isinstance(record, ProviderDataRecord)
    assert asdict(record.provider) == parameters_payload["source"] | {
        "sha256": "",
        "note": "",
    }
    assert record.system_name == parameters_payload["system"]["name"]
    assert record.system_slug == "irs-15k"
    assert record.specifications["mwd_m"] == SpecValue(3048.0, "m")
    assert record.specifications["control_system"].value.startswith("MUX")
    assert record.specifications["bore_variants"] == [
        SpecValue(6.375, "in", "Production bore"),
        SpecValue(2.0625, "in", "Annulus bore"),
    ]
    assert record.orcaflex_mapping == parameters_payload["orcaflex_mapping"]
    record.validate()
    parameters_payload.pop("orcaflex_mapping")
    other = load_provider_yaml(_write_payload(tmp_path, parameters_payload))
    assert other.orcaflex_mapping == {}
    record.orcaflex_mapping["extra"] = "metadata"
    assert other.orcaflex_mapping == {}


@pytest.mark.parametrize("content", ["", "[]", "null", "42", "source: {}"])
def test_loader_rejects_missing_record_structure(tmp_path, content):
    path = tmp_path / "invalid.yaml"
    path.write_text(content, encoding="utf-8")
    with pytest.raises(ValueError):
        load_provider_yaml(path)


@pytest.mark.parametrize("section", ["source", "system", "specifications"])
def test_loader_rejects_missing_sections(tmp_path, parameters_payload, section):
    parameters_payload.pop(section)
    with pytest.raises(ValueError, match=section):
        load_provider_yaml(_write_payload(tmp_path, parameters_payload))


@pytest.mark.parametrize("field", ["vendor", "name", "slug"])
def test_loader_rejects_missing_source_or_system_fields(
    tmp_path,
    parameters_payload,
    field,
):
    section = "source" if field == "vendor" else "system"
    parameters_payload[section].pop(field)
    with pytest.raises(ValueError, match=field):
        load_provider_yaml(_write_payload(tmp_path, parameters_payload))


@pytest.mark.parametrize(
    "spec",
    [
        {"value": -1, "unit": "psi"},
        {"value": float("nan"), "unit": "psi"},
        {"value": float("inf"), "unit": "psi"},
        {"value": True, "unit": "psi"},
        {"value": "15000", "unit": "psi"},
        {"value": "", "unit": "categorical"},
        {"value": 1},
        [],
        [{"value": -1, "unit": "psi"}],
        [{"value": 1}],
        None,
    ],
)
def test_loader_rejects_invalid_specs(tmp_path, parameters_payload, spec):
    parameters_payload["specifications"]["mwp_psi"] = spec
    with pytest.raises(ValueError, match="mwp_psi"):
        load_provider_yaml(_write_payload(tmp_path, parameters_payload))


def test_loader_rejects_missing_required_spec(tmp_path, parameters_payload):
    parameters_payload["specifications"].pop("mwd_m")
    with pytest.raises(ValueError, match="mwd_m"):
        load_provider_yaml(_write_payload(tmp_path, parameters_payload))


def test_loader_keeps_yaml_and_file_errors(tmp_path):
    path = tmp_path / "missing.yaml"
    with pytest.raises(FileNotFoundError):
        load_provider_yaml(path)
    path.write_text("source: [", encoding="utf-8")
    with pytest.raises(yaml.YAMLError):
        load_provider_yaml(path)


def test_provider_package_does_not_import_citations_or_solver():
    script = """
import sys
class RejectUnrelatedImports:
    def find_spec(self, fullname, path=None, target=None):
        if fullname == 'OrcFxAPI' or fullname.startswith('digitalmodel.citations'):
            raise AssertionError('Provider data attempted an unrelated import')
sys.meta_path.insert(0, RejectUnrelatedImports())
from digitalmodel.provider_data import (
    ProviderDataRecord, ProviderDataSource, SpecValue, load_provider_yaml,
)
assert SpecValue(1, 'count').value == 1
"""
    result = subprocess.run(
        [sys.executable, "-c", script],
        env=os.environ.copy(),
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
