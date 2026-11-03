# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Component lookups using synthetic tables, without workbook or solver access.

Synthetic reference: sling breaking load 50,000 lb / factor 5 = 10,000 lb;
choker = 80% of vertical = 8,000 lb; basket = twice vertical = 20,000 lb.
The inclined basket capacities (17,000 and 14,000 lb), widths (1.5 and 3 in),
and weight (1.25 lb/ft) are independently assigned test cells, not ratings.
Shackle breaking load 125 te / factor 5 = 25 te; weight 2.5 lb and size
0.75 in are independently assigned cells. Identifiers 0, 101, 202 and 999
are arbitrary synthetic keys. These definitions supply all expected numbers.

Minimal test extras: pytest, pytest-asyncio and pandas (including numpy).
"""

import importlib.util
import json
from pathlib import Path
import pkgutil
import sys
from types import ModuleType, SimpleNamespace

import numpy as np
import pandas as pd
import pytest


@pytest.fixture
def components(monkeypatch):
    """Load only the lookup module; reject workbook and line-adapter access."""
    def reject_external_access(*args, **kwargs):
        pytest.fail("Component lookup must not load workbook or solver data")

    # These adapters are outside the lookup contract. Stubs avoid importing
    # their dependency trees; actual table filtering and numeric conversion run.
    adapters = {
        "assetutilities.common.data": {
            "ReadFromExcel": lambda: SimpleNamespace(from_xlsx=reject_external_access),
        },
        "digitalmodel.solvers.fea_model.LineType_components": {
            "LineType": reject_external_access,
        },
        "digitalmodel.solvers.fea_model.line_components": {
            "Line": reject_external_access,
        },
    }
    for name, attributes in adapters.items():
        module = ModuleType(name)
        module.__dict__.update(attributes)
        monkeypatch.setitem(sys.modules, name, module)
    monkeypatch.setattr(pkgutil, "get_data", reject_external_access)
    monkeypatch.setattr(pd, "read_excel", reject_external_access)

    # Importing the rigging package normally instantiates workbook-backed
    # objects in rigging.py. Load this source file without that package startup.
    source = (
        Path(__file__).resolve().parents[3]
        / "src/digitalmodel/specialized/rigging/rigging_components.py"
    )
    spec = importlib.util.spec_from_file_location("_rigging_component_lookup", source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def slings(components):
    """Inject the synthetic reference without calling the workbook constructor."""
    instance = object.__new__(components.Slings)
    row = {
        "part_number": np.int64(101),
        "vertical_lb": "10000",
        "choker_lb": "8000",
        "vertical_basket_90_deg_lb": "20000",
        "basket_hitnges_60_deg_lb": "17000",
        "basket_hitnges_45_deg_lb": "14000",
        "w_lb_per_ft": np.float64(1.25),
        "min_width_in": np.float64(1.5),
        "max_width_in": "3.0",
        "design_factor": "5",
    }
    instance.sling_data = {
        "twinpathslings": pd.DataFrame([{**row, "part_number": 0}, row]),
    }
    return instance


@pytest.fixture
def shackles(components):
    """Inject the synthetic reference for each supported shackle grade."""
    instance = object.__new__(components.Shackles)
    row = {
        "part_number": np.int64(202),
        "wll_te": "25.0",
        "w_lb": np.float64(2.5),
        "size_in": np.float64(0.75),
        "design_factor": "5",
        "description": "synthetic shackle",
    }
    instance.shackle_data = {
        grade: pd.DataFrame([{**row, "part_number": 0}, row])
        for grade in ("G2100", "G2110", "G2130")
    }
    return instance


@pytest.mark.parametrize("part_number", ["missing-part", np.int64(999)])
@pytest.mark.parametrize("empty_table", [False, True])
def test_unknown_sling_part_is_reported(slings, part_number, empty_table):
    if empty_table:
        slings.sling_data["twinpathslings"] = slings.sling_data["twinpathslings"].iloc[:0]
    with pytest.raises(ValueError, match=rf"(?i)sling.*{part_number}"):
        slings.get_sling({"subcategory": "twinpathslings", "part_number": part_number})


@pytest.mark.parametrize("part_number", ["missing-part", np.int64(999)])
@pytest.mark.parametrize("empty_table", [False, True])
def test_unknown_shackle_part_is_reported(shackles, part_number, empty_table):
    if empty_table:
        shackles.shackle_data["G2100"] = shackles.shackle_data["G2100"].iloc[:0]
    with pytest.raises(ValueError, match=rf"(?i)shackle.*{part_number}"):
        shackles.get_shackle({"subcategory": "G2100", "part_number": part_number})


@pytest.mark.parametrize("subcategory", ["twinpathslings", "polyester_endless_round"])
def test_known_sling_preserves_capacities_and_numeric_types(slings, subcategory):
    record, model = slings.get_sling({"subcategory": subcategory, "part_number": 101})
    expected = {
        "part_number": 101,
        "wll_vertical_lb": 10000,
        "wll_choker_lb": 8000,
        "wll_vertical_basket_90_deg_lb": 20000,
        "wll_basket_60_deg_lb": 17000,
        "wll_basket_45_deg_lb": 14000,
        "weight_lb_per_ft": 1.25,
        "min_width_in": 1.5,
        "max_width_in": 3,
        "design_factor": 5,
    }
    assert record["part_number"] == 101
    assert record["vertical_lb"] == "10000"
    assert record["w_lb_per_ft"] == 1.25
    assert model == expected
    for key, value in expected.items():
        assert type(model[key]) is type(value)
    assert json.loads(json.dumps((record, model), allow_nan=False)) == [record, expected]


@pytest.mark.parametrize("subcategory", ["G2100", "G2110", "G2130"])
def test_known_shackle_preserves_capacities_and_numeric_types(shackles, subcategory):
    record = shackles.get_shackle({"subcategory": subcategory, "part_number": 202})
    expected = {
        "part_number": 202,
        "wll_te": 25,
        "w_lb": 2.5,
        "size_in": 0.75,
        "design_factor": 5,
        "description": "synthetic shackle",
    }
    assert record == expected
    for key, value in expected.items():
        assert type(record[key]) is type(value)
    assert json.loads(json.dumps(record, allow_nan=False)) == expected
