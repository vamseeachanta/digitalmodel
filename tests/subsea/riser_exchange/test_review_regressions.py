# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regressions for consumer provenance and supplied summary consistency."""

from copy import deepcopy
import hashlib
import json
from pathlib import Path
import re

import pytest
from jsonschema import Draft202012Validator, ValidationError

from digitalmodel.riser_study.exchange import (
    CASE_IDS, exchange_schema, load_json, load_result, load_token_registry,
    validate_campaign_run, validate_result,
)

FIXTURES = Path(__file__).parent / "fixtures"
DATA = Path(__file__).parents[3] / "src/digitalmodel/riser_study/exchange/data"


def record(kind):
    name = "strength.json" if kind == "strength" else "fatigue-F03.json"
    return deepcopy(load_json(FIXTURES / "exports-v1.json")["exports"][
        f"results/R08-W1-SCR/{name}"])


@pytest.mark.parametrize("kind", ["strength", "fatigue"])
def test_decision_eligible_full_run(kind):
    """#3519 _eligible validates the full run, then compares four ledger fields."""
    data = record(kind)
    ledger_run = {"id": "synthetic", "phase": "A", "status": "completed", "qualified": True}
    data["run"].update(ledger_run)
    validate_result(data, kind)
    run = data["run"]
    validate_campaign_run(run)
    assert all(run[key] == ledger_run[key] for key in ("id", "phase", "status", "qualified"))
    assert run["phase"] == "A" and run["status"] == "completed" and run["qualified"]


def test_campaign_run_data_basis():
    run = record("fatigue")["run"]
    run.update(id="synthetic", phase="A", status="completed", qualified=True,
               data_basis="ASSUMED synthetic")
    validate_campaign_run(run)


@pytest.mark.parametrize("field,value", [
    ("input_sha256", "invalid"), ("input_sha256", "a" * 64 + "\n"),
    ("date", "2026-02-30"), ("date", "2026-10-25T12:00:00+24:00"),
    ("result_file", "../synthetic.json"), ("result_file", "synthetic.json\n"),
    ("data_basis", ""), ("unexpected", "rejected"),
])
def test_campaign_run_provenance_rejected(field, value):
    run = record("fatigue")["run"]
    run.update(id="synthetic", phase="A", status="completed", qualified=True)
    run[field] = value
    with pytest.raises(ValueError):
        validate_campaign_run(run)


@pytest.mark.parametrize("defect", ["stations_above_root", "zero_root", "root_above_max", "partial_above"])
def test_station_damage_summary_rejected(defect):
    data = record("fatigue")
    if defect == "stations_above_root":
        for spot in data["hot_spots"]:
            spot["damage_per_year"] = 999
    elif defect == "zero_root":
        data.update(damage_per_year=0, life_years=None)
    elif defect == "root_above_max":
        data.update(damage_per_year=0.4, life_years=2.5)
    else:
        del data["hot_spots"][1]["damage_per_year"]
        data["hot_spots"][0]["damage_per_year"] = 999
    with pytest.raises(ValueError, match="station damage"):
        validate_result(data, "fatigue")


@pytest.mark.parametrize("coverage", ["all", "partial", "absent", "zero"])
def test_station_damage_summary_supported(coverage):
    data = record("fatigue")
    if coverage == "partial":
        del data["hot_spots"][0]["damage_per_year"]
    elif coverage == "absent":
        for spot in data["hot_spots"]:
            del spot["damage_per_year"]
    elif coverage == "zero":
        data.update(damage_per_year=0, life_years=None)
        for spot in data["hot_spots"]:
            spot["damage_per_year"] = 0
    # Supplied curve values are deliberately not used to recalculate damage.
    data["sn_curve"]["log_a"] = 15
    assert validate_result(data, "fatigue") is data


def comparison_record(cases):
    return {"comparison_cases": cases,
            "mesh_step": {"wall_delta_pct": 1, "tolerance_pct": 2, "source_sha256": "a" * 64},
            "half_hour": {"wall_delta_pct": 1, "tolerance_pct": 2, "source_sha256": "b" * 64}}


@pytest.mark.parametrize("reader", ["schema", "validator"])
def test_comparison_case_newline_rejected(reader):
    data = comparison_record(["R08-W1-SCR", "R08-W1-SCR\n"])
    if reader == "schema":
        with pytest.raises(ValidationError):
            Draft202012Validator(exchange_schema("g4_checks")).validate(data)
    else:
        with pytest.raises(ValueError):
            validate_result(data, "g4_checks")


def test_comparison_uses_result_case_enum():
    schema = exchange_schema("fatigue")
    cases = schema["$defs"]["g4_checks"]["properties"]["comparison_cases"]["items"]["enum"]
    assert cases == schema["$defs"]["fatigue"]["properties"]["case_id"]["enum"]
    assert set(cases) == set(CASE_IDS)


def test_large_integer_file_reader(tmp_path):
    data = record("strength")
    data["von_mises_utilisation"][1] = 10**400
    data["max_utilisation"]["value"] = 10**400
    path = tmp_path / "large-integer.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    with pytest.raises(ValueError, match="numeric range"):
        load_result(path, "strength")


def test_registry_source_provenance():
    source_bytes = (DATA / "token-registry-source-d3.txt").read_bytes()
    assert b"\r" not in source_bytes
    rows = [line for line in source_bytes.decode("utf-8").splitlines()
            if line.startswith("| ")][1:]
    tokens = []
    for row in rows:
        token_id, kind, caption, producer = [cell.strip() for cell in row.strip("|").split("|")]
        # The registry drops the analysis group and parenthesized production context.
        module = re.fullmatch(r"RIS-[A-Z]+/([\w-]+\.(?:py|md|yml))(?: \([^)]+\))?", producer)
        assert module is not None, producer
        tokens.append({"id": token_id, "kind": kind, "caption": caption,
                       "producing_module": module.group(1)})
    assert len(tokens) == len({token["id"] for token in tokens}) == 58
    registry = load_json(DATA / "token-registry-v1.json")
    manifest = load_json(DATA / "manifest-v1.json")
    expected = {
        "source": "maintainer-supplied analysis list, draft 3",
        "revision": "d3",
        "sha256": hashlib.sha256(source_bytes).hexdigest(),
        "public": True,
    }
    assert registry["source"] == expected["source"]
    assert registry["source_provenance"] == expected
    assert manifest["registry_source"] == expected
    assert registry["tokens"] == tokens
    assert load_token_registry() == tokens


@pytest.mark.parametrize("scale", [1, 10])
@pytest.mark.parametrize("factor,accepted", [(0.99, True), (1.01, False)])
def test_summary_tolerance_boundary(scale, factor, accepted):
    data = record("strength")
    data["von_mises_utilisation"] = [scale, scale, scale]
    data["touchdown"]["utilisation"] = scale
    data["max_utilisation"]["value"] = scale + scale * factor * 1e-9
    if accepted:
        validate_result(data, "strength")
    else:
        with pytest.raises(ValueError, match="max_utilisation"):
            validate_result(data, "strength")


@pytest.mark.parametrize("factor,accepted", [(0.99, True), (1.01, False)])
@pytest.mark.parametrize("direction", [-1, 1])
def test_station_tolerance_boundary(factor, accepted, direction):
    data = record("fatigue")
    data["hot_spots"][0]["damage_per_year"] += direction * factor * 1e-9
    if accepted:
        validate_result(data, "fatigue")
    else:
        with pytest.raises(ValueError, match="station damage"):
            validate_result(data, "fatigue")
