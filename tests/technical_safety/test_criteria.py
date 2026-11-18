"""Fail-closed citation, edition, unit and library checks."""

from pathlib import Path
import tomllib

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.technical_safety.criteria import (
    LIBRARY_PATH,
    CitedValue,
    EditionCriteria,
    get_criteria,
)


def test_bundled_metadata_and_packaging():
    criteria = get_criteria("s_dp_001_1996_rev2")
    assert criteria.standard == "NORSOK S-DP-001"
    assert criteria.edition == "Rev. 2 (1996)"
    assert criteria.status == "superseded"
    assert criteria.values["jet_peak"].value == 250
    assert criteria.values["jet_peak"].citation.clause == "11.3, step 3"
    assert "pool_fuel_controlled_peak" not in criteria.values
    config = tomllib.loads(
        (Path(__file__).resolve().parents[2] / "pyproject.toml").read_text()
    )
    assert (
        "technical_safety/data/*.yml"
        in config["tool"]["setuptools"]["package-data"]["digitalmodel"]
    )


@pytest.mark.parametrize(
    "field,value",
    [("standard", ""), ("edition", " "), ("clause", None), ("standard", 123)],
)
def test_uncited_value_refused(field, value):
    citation = dict(standard="Synthetic", edition="test", clause="fixture")
    citation[field] = value
    with pytest.raises((ValidationError, ValueError)):
        CitedValue(value=10.0, unit="MJ", citation=citation)


def test_missing_citation_refused():
    with pytest.raises(ValidationError):
        CitedValue(value=14.0, unit="MJ")


@pytest.mark.parametrize("value", [0, -1, True, "14", float("inf"), float("nan")])
def test_bad_numeric_criteria(value):
    with pytest.raises((ValueError, ValidationError)):
        CitedValue(
            value=value,
            unit="MJ",
            citation=dict(standard="Synthetic", edition="test", clause="fixture"),
        )


def test_units_editions_and_unknown_keys_checked(project_criteria):
    data = project_criteria().model_dump()
    data["values"]["jet_peak"]["unit"] = "MJ"
    with pytest.raises(ValueError, match="unit"):
        EditionCriteria.model_validate(data)
    data = project_criteria().model_dump()
    data["values"]["jet_peak"]["citation"]["edition"] = "other"
    with pytest.raises(ValueError, match="edition"):
        EditionCriteria.model_validate(data)
    data = project_criteria().model_dump()
    data["values"]["typo"] = data["values"].pop("jet_peak")
    with pytest.raises(ValueError):
        EditionCriteria.model_validate(data)


def test_missing_limit_and_unknown_edition_refused(project_criteria):
    with pytest.raises(ValueError, match="missing"):
        get_criteria("s_dp_001_1996_rev2").require(
            "jet_average", allow_test_criteria=True
        )
    with pytest.raises(ValueError, match="edition"):
        get_criteria("s_001_2020")


def test_loader_rejects_uncited_and_malformed_files(tmp_path):
    data = yaml.safe_load(LIBRARY_PATH.read_text())
    del data["editions"]["s_dp_001_1996_rev2"]["values"]["jet_peak"]["citation"]
    path = tmp_path / "criteria.yml"
    path.write_text(yaml.safe_dump(data))
    with pytest.raises(ValueError):
        get_criteria("s_dp_001_1996_rev2", path)
    for content in (
        "[]",
        "schema_version: 2\neditions: {}",
        "schema_version: 1\neditions: []",
    ):
        path.write_text(content)
        with pytest.raises(ValueError):
            get_criteria("test", path)


def test_cannot_mutate_criteria_mapping(project_criteria):
    criteria = project_criteria()
    values = criteria.values
    with pytest.raises(TypeError):
        values["jet_peak"] = None
    # The immutable mapping cannot invalidate retained results.
    assert criteria.require("jet_peak").value == 250


def test_duplicate_yaml_key_refused(tmp_path):
    path = tmp_path / "duplicate.yml"
    original = LIBRARY_PATH.read_text(encoding="utf-8")
    path.write_text(
        original.replace("value: 250.0", "value: 250.0\n        value: 25.0")
    )
    with pytest.raises(ValueError, match="duplicate"):
        get_criteria("s_dp_001_1996_rev2", path)


def test_external_edition_file_changes_selected_limits(tmp_path, project_criteria):
    path = tmp_path / "project.yml"
    path.write_text(
        yaml.safe_dump(
            dict(
                schema_version=1,
                editions={
                    "a": project_criteria().model_dump(),
                    "b": project_criteria(
                        "test-B", energy=18.0, jet=300.0
                    ).model_dump(),
                },
            )
        )
    )
    assert get_criteria("a", path).require("jet_peak").value == 250
    assert get_criteria("b", path).require("jet_peak").value == 300
    assert get_criteria("b", path).require("supply_vessel_collision_energy").value == 18
