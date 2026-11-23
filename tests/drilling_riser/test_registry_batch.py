"""In-context registry batch coverage checks (#1470).

The batch runner lives under examples/ and depends on wiki-side values. These
tests intentionally assert status/schema only; private workbook values stay in
llm-wiki calc contracts and generated CSV output.
"""

from __future__ import annotations

import importlib.util
import os
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.drilling_riser.calc_contracts import find_contract, resolve_wiki_root


def _load_batch_module():
    path = (
        Path(__file__).resolve().parents[2]
        / "examples"
        / "workflows"
        / "riser-stackup-registry-batch"
        / "run.py"
    )
    spec = importlib.util.spec_from_file_location("registry_batch_run", path)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _explicit_wiki_root() -> Path:
    raw = os.environ.get("LLM_WIKI_PATH")
    if raw is None:
        pytest.skip("set LLM_WIKI_PATH to the reviewed llm-wiki checkout")
    root = resolve_wiki_root()
    expected = Path(raw).resolve()
    assert root == expected
    return expected


def test_rsu0077_registry_batch_is_runnable_source(tmp_path):
    root = _explicit_wiki_root()
    module = _load_batch_module()
    assert find_contract("RSU-0077", module.SOURCE_TENSION_CALC, root) is not None

    rows = module.run(tmp_path / "registry_batch_16q.csv")
    rsu0077 = next(row for row in rows if row.rsu_id == "RSU-0077")
    by_status = {
        status: sum(1 for row in rows if row.status == status)
        for status in sorted({row.status for row in rows})
    }

    assert rsu0077.status.startswith("runnable")
    assert rsu0077.status == "runnable-source"
    assert rsu0077.units == "kips"
    assert rsu0077.tmin_vert
    assert "shallow-water tension-to-rotate" in rsu0077.reason
    assert by_status == {
        "missing-inputs": 12,
        "runnable": 5,
        "runnable-schedule": 3,
        "runnable-source": 1,
    }
    assert all(
        next(row for row in rows if row.rsu_id == rsu_id).status == "missing-inputs"
        for rsu_id in ("RSU-0030", "RSU-0031", "RSU-0032", "RSU-0033")
    )


def test_source_tension_contract_malformed_cases_fail_closed():
    module = _load_batch_module()
    stackup = SimpleNamespace(
        rsu_id="RSU-TEST",
        water_depth_band="0-500m",
        topology_class="sbop",
    )
    row = module._source_tension_row(
        stackup,
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": 1.0,
            "cases": "case-a",
        },
    )

    assert row.status == "missing-inputs"
    assert row.reason == module.REASON_SOURCE_SCHEMA
    assert row.tmin_vert == ""


@pytest.mark.parametrize(
    "contract",
    [
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": 1.0,
            "cases": [{"case": "case-a"}],
        },
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": 1.0,
            "cases": [
                {"case": "case-a", "op_tension_to_apply_kips": 1.0},
                {"case": "case-a", "op_tension_to_apply_kips": 1.0},
            ],
        },
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": 1.0,
            "cases": [{"case": "case-a", "op_tension_to_apply_kips": 2.0}],
        },
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": 1.0,
            "cases": [{"case": ["case-a"], "op_tension_to_apply_kips": 1.0}],
        },
        {
            "units": "kips",
            "governing_case": ["case-a"],
            "governing_tension_to_apply_kips": 1.0,
            "cases": [{"case": "case-a", "op_tension_to_apply_kips": 1.0}],
        },
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": 0.0,
            "cases": [{"case": "case-a", "op_tension_to_apply_kips": 0.0}],
        },
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": "nan",
            "cases": [{"case": "case-a", "op_tension_to_apply_kips": "nan"}],
        },
        {
            "units": "kips",
            "governing_case": "case-a",
            "governing_tension_to_apply_kips": "inf",
            "cases": [{"case": "case-a", "op_tension_to_apply_kips": "inf"}],
        },
    ],
)
def test_source_tension_contract_inconsistent_cases_fail_closed(contract):
    module = _load_batch_module()
    stackup = SimpleNamespace(
        rsu_id="RSU-TEST",
        water_depth_band="0-500m",
        topology_class="sbop",
    )
    row = module._source_tension_row(stackup, contract)

    assert row.status == "missing-inputs"
    assert row.reason == module.REASON_SOURCE_SCHEMA
    assert row.tmin_vert == ""
