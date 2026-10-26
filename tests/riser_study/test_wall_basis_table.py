# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent frozen-matrix and arithmetic checks for the sizing adapter."""

import csv
import hashlib
import importlib
import json
from pathlib import Path

import pytest

# Frozen sizing matrix: NPS, OD, W1, W2, checked W1, checked W2 (mm).
SECTIONS = [
    (8, 219.1, 21.2, 25.7, 15.55, 19.4875),
    (10, 273.1, 25.6, 31.2, 19.4, 24.3),
    (12, 323.9, 29.7, 36.3, 22.9875, 28.7625),
]
# Frozen sizing readback, in RP2RD/RP1111/STD2RD/F101/guard order.
MAXIMA = [
    (8, 21.2, (0.447779, 0.998184, 0.422672, 0.933631, 0.491240)),
    (8, 25.7, (0.345532, 0.580696, 0.323593, 0.531020, 0.383925)),
    (10, 25.6, (0.447308, 0.996024, 0.422189, 0.931527, 0.490761)),
    (10, 31.2, (0.345382, 0.580148, 0.323451, 0.530498, 0.383758)),
    (12, 29.7, (0.447788, 0.998224, 0.422681, 0.933670, 0.491249)),
    (12, 36.3, (0.346146, 0.582941, 0.324175, 0.533158, 0.384607)),
]


@pytest.fixture(scope="module")
def adapter():
    return importlib.import_module("scripts.riser_study.wall_basis_table")


@pytest.fixture(scope="module")
def selected(adapter):
    basis = adapter.load_basis()
    return {nps: adapter.select_walls(
        next(row for row in adapter.read_matrix() if row["nps_in"] == nps), basis
    ) for nps, *_ in SECTIONS}


@pytest.mark.parametrize("nps,od,w1,w2,c1,c2", SECTIONS)
@pytest.mark.parametrize("option", [0, 1])
def test_selected_walls_reproduce_matrix(adapter, selected, nps, od, w1, w2, c1, c2, option):
    result = selected[nps][option]
    assert result["wall_mm"] == pytest.approx((w1, w2)[option], abs=0.099)
    assert result["checked_wall_mm"] == pytest.approx((c1, c2)[option], abs=1e-10)
    assert result["outside_diameter_mm"] == od
    assert max(row["utilisation"] for row in result["checks"]) <= 1
    if option == 0:
        section = next(row for row in adapter.read_matrix() if row["nps_in"] == nps)
        preceding = adapter.evaluate(section, w1 - 0.1, adapter.load_basis())
        assert max(row["utilisation"] for row in preceding) > 1
    else:
        assert c2 >= 1.25 * c1
        assert (w2 - 0.1) * 0.875 - 3 < 1.25 * c1


@pytest.mark.parametrize("case_id", [
    f"R{nps:02d}-W{option}-{configuration}"
    for nps in (8, 10, 12) for option in (1, 2) for configuration in ("SCR", "SLWR")
])
def test_governing_matches_each_matrix_row(adapter, selected, case_id):
    matrix = next(row for row in adapter.read_matrix() if row["case_id"] == case_id)
    result = selected[matrix["nps_in"]][int(case_id.split("-")[1][1:]) - 1]
    governing = max(result["checks"], key=lambda row: row["utilisation"])
    assert governing["check_id"] == matrix["governing_check"]
    assert governing["check_id"] == "API_RP1111_propagation_empty_1500m"


@pytest.mark.parametrize("nps,wall,expected", MAXIMA)
def test_all_code_maxima_match_frozen_readback(adapter, nps, wall, expected):
    section = next(row for row in adapter.read_matrix() if row["nps_in"] == nps)
    rows = adapter.evaluate(section, wall, adapter.load_basis())
    for key, value in zip(("API RP 2RD", "API RP 1111", "API STD 2RD", "DNV-ST-F101", "pressure guard"), expected):
        assert max(row["utilisation"] for row in rows if row["code"] == key) == pytest.approx(value, abs=0.0000005)


def test_hand_worked_propagation(adapter):
    # Repository api_rp_1111.py::_check_propagation, independently substituted:
    # p_ext = 1025 * 9.80665 * 1500 = 15077724.375 Pa.
    # t = 0.875 * 21.2 - 3 = 15.55 mm; p_pr = 24*450e6*(15.55/219.1)^2.4.
    # p_pr = 18881434.807117353 Pa; f_p*p_pr = 15105147.845693882 Pa.
    # U = 15077724.375 / 15105147.845693882 = 0.9981844950493682.
    section = adapter.read_matrix()[0]
    rows = adapter.evaluate(section, 21.2, adapter.load_basis())
    row = next(row for row in rows if row["check_id"] == "API_RP1111_propagation_empty_1500m")
    assert row["details"]["p_pr"] == pytest.approx(18881434.807117353, abs=0.01)
    assert row["utilisation"] == pytest.approx(0.9981844950493682, abs=1e-12)


def test_common_wall_is_deducted_exactly_once(adapter):
    geometry = adapter.checked_geometry(219.1, 21.2, 3.0, adapter.load_basis())
    assert geometry.wall_thickness == pytest.approx(0.01555)
    assert geometry.t1 == geometry.t2 == geometry.wall_thickness
    assert geometry.corrosion_allowance == geometry.fabrication_tolerance == 0
    with pytest.raises(ValueError, match="positive"):
        adapter.checked_geometry(219.1, 3.0, 3.0, adapter.load_basis())


def test_all_returned_checks_and_load_conditions_retained(adapter):
    rows = adapter.evaluate(adapter.read_matrix()[0], 21.2, adapter.load_basis())
    expected = {"API RP 1111": {"burst", "collapse", "propagation"},
                "API RP 2RD": {"burst", "collapse", "hoop"},
                "API STD 2RD": {"burst", "collapse", "combined"},
                "DNV-ST-F101": {"pressure_containment", "collapse", "propagation_buckling", "combined_loading"}}
    for code, checks in expected.items():
        for condition in ("pressure_only", "empty_1500m"):
            assert {row["check"] for row in rows if row["code"] == code and row["condition"] == condition} == checks
    assert len(rows) == 27  # (3+3+3+4)*2 plus the separate pressure guard.
    assert {row["code"] for row in rows} == set(expected) | {"pressure guard"}
    guard = next(row for row in rows if row["code"] == "pressure guard")
    # Independent pressure arithmetic using the frozen material and checked wall.
    import math
    pb = 0.45 * (450e6 + 535e6) * math.log(219.1 / (219.1 - 2 * 15.55))
    assert guard["utilisation"] == pytest.approx(20e6 / (0.60 * pb))


@pytest.mark.parametrize("mutation", ["duplicate", "omit", "checked_wall", "grade"])
def test_matrix_rejects_invalid_scope(adapter, tmp_path, mutation):
    rows = adapter.read_matrix()
    if mutation == "duplicate":
        rows[-1] = rows[0]
    elif mutation == "omit":
        rows.pop()
    elif mutation == "checked_wall":
        rows[0]["checked_wall_mm"] = 16.0
    else:
        rows[0]["material_grade"] = "X52"
    path = tmp_path / "matrix.csv"
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=rows[0])
        writer.writeheader()
        writer.writerows(rows)
    with pytest.raises(ValueError):
        adapter.read_matrix(path)


def test_build_reproducibility_and_manifest(adapter, tmp_path):
    first, second = tmp_path / "first", tmp_path / "second"
    manifest = adapter.build(first)
    adapter.build(second)
    for name in ("tab-wt.csv", "tab-wt.manifest.json"):
        assert (first / name).read_bytes() == (second / name).read_bytes()
    assert manifest == json.loads((first / "tab-wt.manifest.json").read_text())
    assert manifest["table"]["sha256"] == hashlib.sha256((first / "tab-wt.csv").read_bytes()).hexdigest()
    for source in manifest["sources"]:
        assert hashlib.sha256((adapter.ROOT / source["path"]).read_bytes()).hexdigest() == source["sha256"]
        assert not Path(source["path"]).is_absolute()
    with (first / "tab-wt.csv").open() as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 12 * 27
    assert {row["case_id"] for row in rows} == {row["case_id"] for row in adapter.read_matrix()}
    for case_id in {row["case_id"] for row in rows}:
        governing = [row for row in rows if row["case_id"] == case_id and row["governing"] == "true"]
        assert len(governing) == 1
        assert governing[0]["check_id"] == "API_RP1111_propagation_empty_1500m"
    assert all(row["commit"] == manifest["commit"] for row in rows)
    assert manifest["limits"]
