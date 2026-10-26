# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic input and source-exclusion contracts; no engineering reference data."""
from pathlib import Path
from dataclasses import replace
import ast
import json

import pytest

from digitalmodel.materials.catalog import _STANDARD, load_catalog, use_catalog
from digitalmodel.materials.grades import get


def test_missing_catalog_fails_closed():
    with use_catalog(None):
        with pytest.raises(ValueError, match="caller-supplied"):
            get("X65")


def test_synthetic_catalog_requires_explicit_test_permission():
    catalog = load_catalog(Path(__file__).parent / "fixtures" / "catalog.json")
    with pytest.raises(ValueError, match="synthetic"):
        with use_catalog(catalog):
            get("X65")


def test_sources_sidecar_is_mandatory(tmp_path):
    path = tmp_path / "catalog.json"
    path.write_text('{"grades": [], "dimensions": []}', encoding="utf-8")
    with pytest.raises(FileNotFoundError):
        load_catalog(path)


def _fixture_copy(tmp_path):
    fixture = Path(__file__).parent / "fixtures"
    data = json.loads((fixture / "catalog.json").read_text())
    sources = json.loads((fixture / "sources.yml").read_text())
    return data, sources


def _load_changed(tmp_path, data, sources):
    path = tmp_path / "catalog.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    path.with_name("sources.yml").write_text(json.dumps(sources), encoding="utf-8")
    return load_catalog(path)


@pytest.mark.parametrize("label", [
    "API 5L strength table", "ASME B36.10M dimensions", "ISO 3183 values",
    "IACS UR W11 strengths", "EN 10025-2 steel", "ASTM A312/A312M strengths",
    "ASTM A790/A790M strengths",
])
def test_standards_attributed_source_labels_are_rejected(tmp_path, label):
    data, sources = _fixture_copy(tmp_path)
    sources["grade:X52"]["title"] = label
    with pytest.raises(ValueError, match="standards-attributed"):
        _load_changed(tmp_path, data, sources)


@pytest.mark.parametrize("publisher", ["API", "ASME", "ISO", "ASTM", "IACS", "CEN"])
def test_standard_publishers_are_rejected(synthetic_catalog, publisher):
    with pytest.raises(ValueError, match="standards-attributed"):
        replace(synthetic_catalog.grades[0].source, publisher=publisher)


@pytest.mark.parametrize("kind", ["manufacturer", "public_paper"])
def test_nonstandard_publication_can_identify_its_product(synthetic_catalog, kind):
    # Synthetic metadata exercises title handling; it establishes no source.
    source = replace(synthetic_catalog.grades[0].source,
                     title="Synthetic publication about API 5L products", kind=kind)
    assert source.title == "Synthetic publication about API 5L products"


@pytest.mark.parametrize("field", ["title", "publisher", "revision", "date", "kind"])
def test_every_source_identity_field_is_required(synthetic_catalog, field):
    with pytest.raises(ValueError, match=field):
        replace(synthetic_catalog.grades[0].source, **{field: ""})


def test_source_needs_a_locator(synthetic_catalog):
    with pytest.raises(ValueError, match="URL or catalogue_number"):
        replace(synthetic_catalog.grades[0].source, url="", catalogue_number="")


@pytest.mark.parametrize("kind", ["standard", "unknown", "licensed_extract"])
def test_source_kind_is_restricted(synthetic_catalog, kind):
    with pytest.raises(ValueError, match="nonstandard"):
        replace(synthetic_catalog.grades[0].source, kind=kind)


@pytest.mark.parametrize("field", ["smys_mpa", "smts_mpa", "E_mpa", "rho_kg_m3"])
@pytest.mark.parametrize("value", [0, -1, True, "300", float("nan"), float("inf")])
def test_material_numeric_validation(synthetic_catalog, field, value):
    with pytest.raises(ValueError, match=field):
        replace(synthetic_catalog.grades[2], **{field: value})


@pytest.mark.parametrize("value", [-1, 0.5, True, "0.3", float("nan"), float("inf")])
def test_poisson_validation(synthetic_catalog, value):
    with pytest.raises(ValueError, match="nu"):
        replace(synthetic_catalog.grades[2], nu=value)


@pytest.mark.parametrize("field", ["nps", "od_in", "wt_in"])
@pytest.mark.parametrize("value", [0, -1, True, "1", float("nan"), float("inf")])
def test_dimension_numeric_validation(synthetic_catalog, field, value):
    with pytest.raises(ValueError, match=field):
        replace(synthetic_catalog.dimensions[0], **{field: value})


@pytest.mark.parametrize("mutation", ["missing", "orphan", "duplicate_row", "ambiguous_alias", "inconsistent_od"])
def test_row_provenance_and_identity_are_exact(tmp_path, mutation):
    data, sources = _fixture_copy(tmp_path)
    if mutation == "missing":
        del sources["grade:X52"]
    elif mutation == "orphan":
        sources["unused"] = sources["grade:X52"]
    elif mutation == "duplicate_row":
        data["grades"].append(data["grades"][0])
    elif mutation == "ambiguous_alias":
        data["grades"][0]["aliases"] = ["x52"]
    else:
        data["dimensions"][1]["od_in"] += 1
    with pytest.raises(ValueError):
        _load_changed(tmp_path, data, sources)


def test_duplicate_source_keys_are_rejected(tmp_path):
    data, sources = _fixture_copy(tmp_path)
    _load_changed(tmp_path, data, sources)
    path = tmp_path / "sources.yml"
    path.write_text('{"same": {}, "same": {}}', encoding="utf-8")
    with pytest.raises(ValueError, match="duplicate"):
        load_catalog(tmp_path / "catalog.json")


def test_catalogue_scope_restores_on_exception(synthetic_catalog):
    original = get("X65")
    with pytest.raises(RuntimeError):
        with use_catalog(None):
            with pytest.raises(ValueError, match="caller-supplied"):
                get("X65")
            raise RuntimeError("synthetic test failure")
    assert get("X65") is original


def test_database_snapshot_survives_catalogue_scope_exit():
    from digitalmodel.materials.database import load_database

    database = load_database()
    original = database.lookup("X65")
    with use_catalog(None):
        assert database.lookup("X65") is original


def test_lazy_consumer_views_follow_the_supplied_catalogue(synthetic_catalog):
    from digitalmodel.materials import legacy_pipe_db_grades_mpa

    view = legacy_pipe_db_grades_mpa()
    original = view["X65"]["yield_strength"]
    rows = tuple(replace(g, smys_mpa=original + 1) if g.name == "X65" else g
                 for g in synthetic_catalog.grades)
    with use_catalog(replace(synthetic_catalog, grades=rows), allow_synthetic=True):
        assert view["X65"]["yield_strength"] == original + 1
    assert view["X65"]["yield_strength"] == original


def test_no_embedded_material_or_dimension_tables_reappear():
    root = Path(__file__).resolve().parents[2] / "src" / "digitalmodel" / "materials"
    for filename in ("grades.py", "line_pipe.py"):
        tree = ast.parse((root / filename).read_text())
        for node in ast.walk(tree):
            if isinstance(node, (ast.List, ast.Tuple, ast.Dict)):
                items = node.keys + node.values if isinstance(node, ast.Dict) else node.elts
                assert not any(isinstance(item, ast.Constant)
                               and isinstance(item.value, (float, int)) for item in items), (
                    f"{filename}:{node.lineno}: embedded numerical table"
                )
            if isinstance(node, ast.Assign) and isinstance(node.value, ast.Constant):
                names = [t.id for t in node.targets if isinstance(t, ast.Name)]
                if any(name.endswith(("_SRC", "_SOURCE")) for name in names):
                    if isinstance(node.value.value, str):
                        assert not _STANDARD.search(node.value.value), (
                            f"{filename}:{node.lineno}: standards-attributed source label"
                        )


def test_explicit_geometry_requires_source():
    from digitalmodel.materials.line_pipe import line_pipe

    with pytest.raises(ValueError, match="geometry_source"):
        line_pipe("X65", od_in=9.7, wt_in=0.2)


def test_schedule_source_is_the_requested_row(synthetic_catalog):
    from digitalmodel.materials.line_pipe import line_pipe

    first, second, *rest = synthetic_catalog.dimensions
    second = replace(second, source=replace(second.source, revision="2"))
    catalog = replace(synthetic_catalog, dimensions=(first, second, *rest))
    with use_catalog(catalog, allow_synthetic=True):
        assert line_pipe("X65", nps=1, schedule="40").geometry_source.revision == "2"
        assert line_pipe("X65", nps=1, schedule="STD").geometry_source.revision == "1"


def test_equivalent_schedule_rows_cannot_overwrite_each_other(synthetic_catalog):
    first, second, *rest = synthetic_catalog.dimensions
    duplicate = replace(first, row_id="duplicate", schedule="sch40")
    with pytest.raises(ValueError, match="duplicate dimension"):
        replace(synthetic_catalog, dimensions=(duplicate, second, *rest))
