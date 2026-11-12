# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise catalogue contracts using invented values, never design inputs."""

import copy
import json
import shutil
from pathlib import Path
from urllib.parse import SplitResult

import pytest
import yaml
from jsonschema import Draft202012Validator, FormatChecker

from digitalmodel.data_systems import engineering_databases
from digitalmodel.data_systems.engineering_databases import (
    CatalogueValidationError,
    load_database,
    load_flexible_pipe,
    load_line_pipe,
    load_soil_stiffness,
    load_structure_properties,
)

ROOT = Path(__file__).resolve().parents[2]
DIRECTORIES = {
    "line_pipe": ROOT / "data/line_pipe",
    "flexible_pipe": ROOT / "data/flexible_pipe",
    "structure_properties": ROOT / "data/riser_database/structure_properties",
    "soil_stiffness": ROOT / "data/soil_stiffness",
}
LOADERS = {
    "line_pipe": load_line_pipe,
    "flexible_pipe": load_flexible_pipe,
    "structure_properties": load_structure_properties,
    "soil_stiffness": load_soil_stiffness,
}
ROWS = {
    "line_pipe": {
        "od_m": 0.4, "wt_m": 0.02, "grade": "synthetic grade",
        "smys_mpa": 200.0, "smts_mpa": 300.0,
    },
    "flexible_pipe": {
        "id_m": 0.1, "od_m": 0.2, "axial_stiffness_n": 100000.0,
        "bending_stiffness_n_m2": 1000.0, "torsional_stiffness_n_m2": 2000.0,
        "dry_mass_kg_per_m": 20.0, "minimum_bend_radius_m": 2.0,
        "design_pressure_pa": 100000.0, "temperature_deg_c": 20.0,
        "stiffness_basis": "initial_tangent", "test_conditions": "invented fixture",
    },
    "structure_properties": {
        "section_type": "custom", "section_label": "synthetic section",
        "area_m2": 0.01, "iy_m4": 0.0001, "iz_m4": 0.0002,
        "torsion_constant_m4": 0.00005, "elastic_modulus_pa": 1000000000.0,
        "density_kg_per_m3": 1000.0, "dry_mass_kg_per_m": 10.0,
    },
    "soil_stiffness": {
        "soil_type": "clay", "drainage": "undrained", "loading": "monotonic",
        "direction": "vertical", "pipe_od_m": 0.4, "embedment_m": 0.1,
        "depth_top_m": 0.0, "depth_bottom_m": 1.0,
        "displacement_m": [0.0, 0.01, 0.02],
        "resistance_n_per_m": [0.0, 10.0, 15.0],
        "initial_secant_stiffness_n_per_m2": 1000.0,
        "test_conditions": "invented fixture; positive resistance magnitudes",
    },
}
CITATION = {
    "title": "Invented test fixture", "publisher": "Synthetic fixture",
    "url": None, "catalogue_number": "synthetic-fixture",
    "revision": "synthetic", "date": "2026-10-26", "page": "fixture 1",
    "table": None, "source_class": "synthetic_fixture",
    "reuse_terms": "Invented test data only; unsuitable for engineering use",
}


@pytest.fixture(params=DIRECTORIES)
def catalogue(request, tmp_path):
    """Copy an empty catalogue and add a clearly marked synthetic row."""
    database = request.param
    directory = tmp_path / database
    shutil.copytree(DIRECTORIES[database], directory)
    data = json.loads((directory / "data.json").read_text(encoding="utf-8"))
    row = dict(id="fixture-1", status="synthetic", citation_id="fixture-source")
    row.update(copy.deepcopy(ROWS[database]))
    data["records"] = [row]
    sources = yaml.safe_load((directory / "sources.yml").read_text(encoding="utf-8"))
    sources["citations"] = {"fixture-source": copy.deepcopy(CITATION)}
    return database, directory, data, sources


def save_and_load(catalogue, **kwargs):
    """Write test mutations and exercise the public loader."""
    database, directory, data, sources = catalogue
    (directory / "data.json").write_text(json.dumps(data), encoding="utf-8", newline="\n")
    (directory / "sources.yml").write_text(
        yaml.safe_dump(sources, allow_unicode=True), encoding="utf-8", newline="\n"
    )
    return load_database(database, directory, **kwargs)


@pytest.mark.parametrize("database", DIRECTORIES)
def test_empty_catalogue_and_schema(database):
    directory = DIRECTORIES[database]
    schema = json.loads((directory / "schema.json").read_text(encoding="utf-8"))
    Draft202012Validator.check_schema(schema)
    assert LOADERS[database](directory) == []
    assert json.loads((directory / "data.json").read_text(encoding="utf-8"))["records"] == []


def test_synthetic_requires_explicit_opt_in(catalogue):
    with pytest.raises(CatalogueValidationError, match="synthetic"):
        save_and_load(catalogue)
    records = save_and_load(catalogue, allow_synthetic=True)
    assert records[0]["id"] == "fixture-1"
    assert records[0]["citation"] == CITATION


@pytest.mark.parametrize("source_class", [
    "mill_catalogue", "manufacturer_catalogue", "public_paper", "public_thesis",
    "public_agency_report", "own_design_basis",
])
def test_clean_source_class_contract(catalogue, source_class):
    # Invented metadata tests admission syntax, not source authenticity or rights.
    catalogue[2]["records"][0]["status"] = "cited"
    citation = catalogue[3]["citations"]["fixture-source"]
    citation["source_class"] = source_class
    citation["url"] = "https://example.invalid/synthetic-fixture"
    citation["catalogue_number"] = None
    citation["page"] = None
    citation["table"] = "fixture table"
    assert save_and_load(catalogue)[0]["citation"]["source_class"] == source_class


@pytest.mark.parametrize("field", list(CITATION))
def test_every_citation_field_is_required(catalogue, field):
    del catalogue[3]["citations"]["fixture-source"][field]
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("mutation", ["missing", "unresolved", "empty", "orphan"])
def test_row_citation_join_is_required(catalogue, mutation):
    row = catalogue[2]["records"][0]
    citations = catalogue[3]["citations"]
    if mutation == "missing":
        del row["citation_id"]
    elif mutation == "unresolved":
        row["citation_id"] = "absent"
    elif mutation == "empty":
        citations["fixture-source"] = {}
    else:
        citations["orphan"] = copy.deepcopy(CITATION)
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("field,value", [
    ("date", "2026-02-30"), ("title", " "), ("revision", ""),
    ("source_class", "standard"), ("url", "file:fixture"),
    ("url", "https://"),
])
def test_invalid_citation(catalogue, field, value):
    catalogue[3]["citations"]["fixture-source"][field] = value
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("authority", [
    "[broken", ":", ":443", "example.invalid:", "example.invalid:bad",
    "example.invalid:70000", "example.invalid%zz", "example.invalid\\invalid",
    "[not-an-ip]", ":@example.invalid",
    "example.invalid/synthetic%zz", "example.invalid/?part=%zz",
    "example.invalid/#part%zz", "example.invalid/#one#two",
])
def test_malformed_citation_authority_without_optional_checker(
    catalogue, monkeypatch, authority
):
    # jsonschema's URI checker is optional; schema patterns alone are insufficient.
    monkeypatch.delitem(FormatChecker.checkers, "uri", raising=False)
    citation = catalogue[3]["citations"]["fixture-source"]
    citation["url"] = "https://" + authority
    citation["catalogue_number"] = None
    with pytest.raises(CatalogueValidationError, match="authority"):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("url", [
    "http://example.invalid/synthetic-fixture",
    "https://example.invalid:443/synthetic-fixture?part=1#table",
    "https://example.invalid/synthetic%20fixture",
    "https://[2001:db8::1]/synthetic-fixture",
])
def test_valid_citation_authority_without_optional_checker(catalogue, monkeypatch, url):
    monkeypatch.delitem(FormatChecker.checkers, "uri", raising=False)
    citation = catalogue[3]["citations"]["fixture-source"]
    citation["url"] = url
    citation["catalogue_number"] = None
    assert save_and_load(catalogue, allow_synthetic=True)[0]["citation"]["url"] == url


@pytest.mark.parametrize("authority", ["[1.2.3.4]", "[1::2::3]"])
def test_legacy_parser_cannot_admit_invalid_ipv6(catalogue, monkeypatch, authority):
    # Model supported parser versions that split bracketed hosts without checking IPs.
    monkeypatch.delitem(FormatChecker.checkers, "uri", raising=False)
    monkeypatch.setattr(
        engineering_databases, "urlsplit",
        lambda _url: SplitResult("https", authority, "/synthetic-fixture", "", ""),
    )
    citation = catalogue[3]["citations"]["fixture-source"]
    citation["url"] = "https://" + authority + "/synthetic-fixture"
    citation["catalogue_number"] = None
    with pytest.raises(CatalogueValidationError, match="authority"):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("fields", [("url", "catalogue_number"), ("page", "table")])
def test_citation_requires_address_and_locator(catalogue, fields):
    for field in fields:
        catalogue[3]["citations"]["fixture-source"][field] = None
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("mutation", ["wrong", "missing", "extra"])
def test_units_are_exact(catalogue, mutation):
    units = catalogue[2]["units"]
    field = next(iter(units))
    if mutation == "wrong":
        units[field] = "ft"
    elif mutation == "missing":
        del units[field]
    else:
        units["extra"] = "m"
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("mutation", ["duplicate", "unknown", "version", "database"])
def test_document_contract(catalogue, mutation):
    data = catalogue[2]
    if mutation == "duplicate":
        data["records"].append(copy.deepcopy(data["records"][0]))
    elif mutation == "unknown":
        data["records"][0]["unrecognised"] = 1
    elif mutation == "version":
        data["schema_version"] = 2
    else:
        data["database"] = "other"
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -1.0, True, "1.0"])
def test_numeric_values_are_finite_and_typed(catalogue, value):
    field = next(key for key, item in ROWS[catalogue[0]].items() if isinstance(item, float))
    catalogue[2]["records"][0][field] = value
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("status,source_class", [
    ("cited", "synthetic_fixture"), ("synthetic", "manufacturer_catalogue"),
])
def test_synthetic_status_cannot_be_disguised(catalogue, status, source_class):
    catalogue[2]["records"][0]["status"] = status
    catalogue[3]["citations"]["fixture-source"]["source_class"] = source_class
    with pytest.raises(CatalogueValidationError):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("database,changes", [
    ("line_pipe", {"wt_m": 0.2}),
    ("line_pipe", {"smts_mpa": 100.0}),
    ("flexible_pipe", {"id_m": 0.3}),
    ("structure_properties", {"dry_mass_kg_per_m": 11.0}),
    ("soil_stiffness", {"depth_bottom_m": -1.0}),
    ("soil_stiffness", {"depth_top_m": 2.0}),
    ("soil_stiffness", {"displacement_m": [0.0, 0.02, 0.01]}),
    ("soil_stiffness", {"resistance_n_per_m": [0.0, 10.0]}),
    ("soil_stiffness", {"resistance_n_per_m": [1.0, 10.0, 15.0]}),
    ("soil_stiffness", {"initial_secant_stiffness_n_per_m2": 500.0}),
])
def test_engineering_relationships(tmp_path, database, changes):
    directory = tmp_path / "catalogue"
    shutil.copytree(DIRECTORIES[database], directory)
    data = json.loads((directory / "data.json").read_text(encoding="utf-8"))
    row = dict(id="fixture-1", status="synthetic", citation_id="fixture-source")
    row.update(copy.deepcopy(ROWS[database]))
    row.update(changes)
    data["records"] = [row]
    sources = yaml.safe_load((directory / "sources.yml").read_text(encoding="utf-8"))
    sources["citations"] = {"fixture-source": copy.deepcopy(CITATION)}
    with pytest.raises(CatalogueValidationError):
        save_and_load((database, directory, data, sources), allow_synthetic=True)


@pytest.mark.parametrize("catalogue", ["structure_properties"], indirect=True)
@pytest.mark.parametrize("area,mass,accepted", [
    (0.01, 10.0, True),
    (0.01, 9.999991, True), (0.01, 10.000009, True),
    (0.01, 9.999989, False), (0.01, 10.000011, False),
    (1e-14, 1.0000009e-11, True), (1e-14, 1.0000011e-11, False),
])
def test_mass_consistency_precision_boundary(catalogue, area, mass, accepted):
    # Hand basis: 0.01 * 1000 = 10 kg/m; deviations are 9e-6 and 11e-6.
    # One ppm at 10 kg/m is 1e-5; the upper-side max-value correction is < 2e-11.
    # At small scale: 1e-14 * 1000 = 1e-11, so no absolute floor may admit 1.1 ppm.
    row = catalogue[2]["records"][0]
    row.update(area_m2=area, dry_mass_kg_per_m=mass)
    if accepted:
        assert save_and_load(catalogue, allow_synthetic=True)[0]["dry_mass_kg_per_m"] == mass
    else:
        with pytest.raises(CatalogueValidationError, match="dry mass"):
            save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("catalogue", ["soil_stiffness"], indirect=True)
@pytest.mark.parametrize("stiffness,accepted", [
    (1000.0, True), (999.9991, True), (1000.0009, True),
    (999.9989, False), (1000.0011, False),
])
def test_secant_consistency_precision_boundary(catalogue, stiffness, accepted):
    # Hand basis: 10 N/m / 0.01 m = 1000 N/m^2; one ppm is 0.001 N/m^2.
    # Differences 0.0009 and 0.0011 bracket it; max-value correction is < 2e-9.
    catalogue[2]["records"][0]["initial_secant_stiffness_n_per_m2"] = stiffness
    if accepted:
        record = save_and_load(catalogue, allow_synthetic=True)[0]
        assert record["initial_secant_stiffness_n_per_m2"] == stiffness
    else:
        with pytest.raises(CatalogueValidationError, match="soil stiffness"):
            save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("catalogue", ["flexible_pipe"], indirect=True)
@pytest.mark.parametrize("temperature,accepted", [
    (-273.151, False), (-273.15, False), (-273.149, True), (0.0, True),
])
def test_temperature_absolute_zero_boundary(catalogue, temperature, accepted):
    # SI Brochure, 9th ed., v4.01 (2026), section 2.3.1, "The kelvin".
    # Hand conversion T = t + 273.15 gives -0.001, 0, 0.001 and 273.15 K.
    # The catalogue contract requires T > 0; this does not qualify service use.
    catalogue[2]["records"][0]["temperature_deg_c"] = temperature
    if accepted:
        assert save_and_load(catalogue, allow_synthetic=True)[0]["temperature_deg_c"] == temperature
    else:
        with pytest.raises(CatalogueValidationError):
            save_and_load(catalogue, allow_synthetic=True)


def test_utf8_citation_text_is_preserved(catalogue):
    catalogue[3]["citations"]["fixture-source"]["title"] = "Invented \u03bc fixture"
    record = save_and_load(catalogue, allow_synthetic=True)[0]
    assert record["citation"]["title"] == "Invented \u03bc fixture"
    assert "\u03bc".encode("utf-8") in (catalogue[1] / "sources.yml").read_bytes()


@pytest.mark.parametrize("catalogue,field", [
    ("structure_properties", "area_m2"),
    ("soil_stiffness", "initial_secant_stiffness_n_per_m2"),
], indirect=["catalogue"])
def test_unrepresentable_derived_arithmetic_is_a_validation_error(catalogue, field):
    catalogue[2]["records"][0][field] = 10 ** 400
    with pytest.raises(CatalogueValidationError, match="engineering numeric"):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("filename", ["schema.json", "data.json", "sources.yml"])
def test_excessive_document_nesting_is_a_validation_error(catalogue, filename):
    save_and_load(catalogue, allow_synthetic=True)
    (catalogue[1] / filename).write_text(
        "[" * 2000 + "0" + "]" * 2000, encoding="utf-8", newline="\n"
    )
    with pytest.raises(CatalogueValidationError):
        load_database(catalogue[0], catalogue[1], allow_synthetic=True)


@pytest.mark.parametrize("catalogue", ["soil_stiffness"], indirect=True)
def test_zero_resistance_first_segment_is_rejected(catalogue):
    catalogue[2]["records"][0]["resistance_n_per_m"][1] = 0.0
    with pytest.raises(CatalogueValidationError, match="first segment"):
        save_and_load(catalogue, allow_synthetic=True)


@pytest.mark.parametrize("catalogue", ["soil_stiffness"], indirect=True)
def test_equal_depth_bounds_represent_a_sampling_depth(catalogue):
    catalogue[2]["records"][0].update(depth_top_m=1.0, depth_bottom_m=1.0)
    record = save_and_load(catalogue, allow_synthetic=True)[0]
    assert record["depth_top_m"] == record["depth_bottom_m"] == 1.0


def test_unknown_database(tmp_path):
    with pytest.raises(CatalogueValidationError, match="database"):
        load_database("unknown", tmp_path)


def test_missing_catalogue_files(tmp_path):
    with pytest.raises(CatalogueValidationError, match="cannot read"):
        load_line_pipe(tmp_path)


@pytest.mark.parametrize("filename,content", [
    ("data.json", "["), ("data.json", "null"), ("sources.yml", "["),
    ("sources.yml", "null"), ("schema.json", "{}"),
])
def test_malformed_documents(catalogue, filename, content):
    save_and_load(catalogue, allow_synthetic=True)
    (catalogue[1] / filename).write_text(content, encoding="utf-8", newline="\n")
    with pytest.raises(CatalogueValidationError):
        load_database(catalogue[0], catalogue[1], allow_synthetic=True)


@pytest.mark.parametrize("filename", ["data.json", "sources.yml"])
def test_duplicate_mapping_keys_are_rejected(catalogue, filename):
    _, directory, _, _ = catalogue
    save_and_load(catalogue, allow_synthetic=True)
    path = directory / filename
    if filename.endswith("json"):
        path.write_text(
            '{"schema_version": 1, "schema_version": 1}', encoding="utf-8", newline="\n"
        )
    else:
        path.write_text("schema_version: 1\nschema_version: 1\n", encoding="utf-8", newline="\n")
    with pytest.raises(CatalogueValidationError, match="duplicate"):
        load_database(catalogue[0], directory, allow_synthetic=True)
