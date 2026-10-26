# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline database contracts; strength fixtures remain in test_grade_matrix.py."""
from dataclasses import FrozenInstanceError, replace
import json
from pathlib import Path
import re
import subprocess
from unittest.mock import Mock

import pytest

from digitalmodel.materials import grades
from digitalmodel.materials import database as database_module
from digitalmodel.materials.catalog import load_catalog, use_catalog
from digitalmodel.materials.database import MaterialDatabase, load_database
from digitalmodel.materials.database_schema import Strengths


@pytest.fixture
def database():
    return load_database()


def test_all_reconciled_grades_are_loaded_once_per_stored_level(database):
    expected = sum(
        (int(g.smts_psl1_mpa is not None) + int(g.smts_mpa is not None))
        if g.standard == "API 5L" else 1
        for g in grades.all_grades()
    )
    assert len(database) == expected
    assert len(set(database)) == len(database)
    for record in database.values():
        assert database[record.key] is record


# Names and product levels are pinned to test_grade_matrix.py's API cases.
@pytest.mark.parametrize(
    "name", ["A25", "A", "X42", "X52", "X65", "X70", "X80", "X90", "X100", "X120"]
)
@pytest.mark.parametrize("psl", ["PSL1", "PSL2", "psl1", "unknown"])
def test_psl_selection_matches_existing_reconciliation(database, name, psl):
    g = grades.get(name)
    if (psl == "unknown"
            or (psl.upper() == "PSL1" and g.smts_psl1_mpa is None)
            or (psl.upper() == "PSL2" and g.smts_mpa is None)):
        for query in (database.lookup, grades.smts_mpa):
            with pytest.raises(ValueError, match="PSL"):
                query(name, psl=psl)
        return
    record = database.lookup(name, psl=psl)
    assert record.strengths.smys_mpa == grades.smys_mpa(name)
    assert record.strengths.smts_mpa == grades.smts_mpa(name, psl=psl)
    assert record.product_level == psl.upper()


@pytest.mark.parametrize(
    "name", ["L360", "l485", "x52", "ah36", "Grade A", "A", "316L", "2205"]
)
def test_aliases_and_distinct_grade_a_are_preserved(database, name):
    level = "PSL1" if grades.get(name).name == "A" else "PSL2"
    assert database.lookup(name, psl=level).designation == grades.get(name).name
    assert database.lookup("A", psl="PSL1").key != database.lookup("Grade A").key


def test_values_and_citations_equal_the_source_records(database):
    for record in database.values():
        source = grades.get(record.designation)
        assert record.specification == source.standard
        assert record.strengths.units == "MPa"
        assert record.strengths.smys_mpa == source.smys_mpa
        assert record.strengths.smts_mpa == grades.smts_mpa(
            source.name, psl=record.product_level or "PSL2"
        )
        citation = source.source_psl1 if record.product_level == "PSL1" else source.source.citation
        assert record.provenance.citation == citation
        assert record.notes == source.notes


def test_provenance_pins_the_supplied_data(database, synthetic_catalog):
    for record in database.values():
        provenance = record.provenance
        assert provenance.repository_path == "catalog.json"
        assert provenance.sha256 == synthetic_catalog.digest
        assert provenance.repository_revision is None
        assert provenance.retrieved_on_utc == "2026-10-26"
        assert provenance.original_source_terms == "caller-supplied; qualification remains required"


def test_software_history_is_not_presented_as_material_provenance(database):
    assert all(record.provenance.repository_revision is None for record in database.values())
    assert all(record.provenance.repository_path != "src/digitalmodel/materials/grades.py"
               for record in database.values())


def test_data_provenance_needs_no_git_history(monkeypatch, synthetic_catalog):
    def unexpected_git(*args, **kwargs):
        pytest.fail("data provenance must not require Git history")
    monkeypatch.setattr(subprocess, "run", unexpected_git)
    database = load_database()
    assert all(record.provenance.sha256 == synthetic_catalog.digest for record in database.values())
    assert database.source_digest_matches


def test_input_digest_covers_data_and_sidecar(database):
    from hashlib import sha256
    fixture = Path(__file__).parent / "fixtures"
    expected = sha256((fixture / "catalog.json").read_bytes() + b"\0" +
                      (fixture / "sources.yml").read_bytes()).hexdigest()
    assert database.lookup("X52").provenance.sha256 == expected


@pytest.fixture
def configured_revision_database(synthetic_catalog):
    return load_database()


def assert_no_git_show(run):
    run.assert_not_called()


@pytest.mark.parametrize("line_ending", ["\n", "\r\n", "\r"])
def test_software_line_endings_do_not_change_data_provenance(
    monkeypatch, configured_revision_database, line_ending,
):
    source = Path(grades.__file__).read_text(encoding="utf-8")
    source = source.replace("\n", line_ending).encode("utf-8")
    run = Mock(return_value=subprocess.CompletedProcess([], 0, stdout=source))
    monkeypatch.setattr(subprocess, "run", run)
    test_input_digest_covers_data_and_sidecar(configured_revision_database)
    assert_no_git_show(run)


def test_altered_input_changes_data_provenance(tmp_path):
    fixture = Path(__file__).parent / "fixtures"
    original = load_catalog(fixture / "catalog.json")
    data = json.loads((fixture / "catalog.json").read_text(encoding="utf-8"))
    data["grades"][0]["smys_mpa"] += 1
    (tmp_path / "catalog.json").write_text(json.dumps(data), encoding="utf-8", newline="\n")
    (tmp_path / "sources.yml").write_bytes((fixture / "sources.yml").read_bytes())
    assert load_catalog(tmp_path / "catalog.json").digest != original.digest


def test_missing_history_does_not_prevent_data_provenance(monkeypatch):
    run = Mock(side_effect=subprocess.CalledProcessError(128, ["git", "show"]))
    monkeypatch.setattr(subprocess, "run", run)
    database = load_database()
    test_input_digest_covers_data_and_sidecar(database)
    assert_no_git_show(run)


@pytest.mark.parametrize("path", [
    "docs/domains/materials/database-gaps.html",
    "src/digitalmodel/materials/docs/agent-pack.md",
])
def test_shipped_docs_expose_superseded_provenance(database, path):
    document = (Path(__file__).resolve().parents[2] / path).read_text(encoding="utf-8")
    assert "caller-supplied" in document
    assert "superseded" in document.lower()
    assert database.lookup("X52").provenance.repository_revision is None


def test_unknown_specification_editions_remain_null(database):
    for record in database.values():
        assert record.edition is None
        assert "|edition-unspecified|" in record.key
        assert grades.get(record.designation).source.revision == "1"


@pytest.mark.parametrize("kind", ["manufacturer", "public_paper", "design_basis", "synthetic"])
def test_publication_revision_is_provenance_only(synthetic_catalog, kind):
    rows = tuple(replace(g, source=replace(g.source, revision="catalogue-r7", kind=kind))
                 for g in synthetic_catalog.grades)
    with use_catalog(replace(synthetic_catalog, grades=rows), allow_synthetic=True):
        database = load_database()
    for record in database.values():
        assert record.edition is None
        assert "catalogue-r7" in record.provenance.citation
        assert "catalogue-r7" not in record.key


def test_explicit_specification_edition_loads_independently(tmp_path):
    fixture = Path(__file__).parent / "fixtures"
    data = json.loads((fixture / "catalog.json").read_text(encoding="utf-8"))
    sources = json.loads((fixture / "sources.yml").read_text(encoding="utf-8"))
    next(g for g in data["grades"] if g["name"] == "X52")["specification_edition"] = "test-edition"
    sources["grade:X52"]["revision"] = "catalogue-r7"
    path = tmp_path / "catalog.json"
    path.write_text(json.dumps(data), encoding="utf-8", newline="\n")
    path.with_name("sources.yml").write_text(json.dumps(sources), encoding="utf-8", newline="\n")
    with use_catalog(load_catalog(path), allow_synthetic=True):
        database = load_database()
    for level in ("PSL1", "PSL2"):
        record = database.lookup("X52", psl=level)
        assert record.edition == "test-edition"
        assert "|test-edition|" in record.key
        assert "catalogue-r7" in record.provenance.citation
    assert database.lookup("X65").edition is None


@pytest.mark.parametrize("edition", [
    "", " ", " untrimmed", "bad|edition", "edition-unspecified", 1, True, float("nan"),
])
def test_invalid_specification_edition_is_rejected(edition):
    with pytest.raises(ValueError, match="specification_edition"):
        replace(grades.get("X52"), specification_edition=edition)


def test_missing_associations_are_explicitly_null(database):
    # No general grade-to-detail-class mapping is established by grades.py.
    for record in database.values():
        assert record.sn_curve_ids is None
        assert record.standards_registry_id is None
        assert record.structural_shape_id is None


def test_duplicate_keys_are_rejected(database):
    record = database.lookup("X52")
    with pytest.raises(ValueError, match="duplicate material key"):
        MaterialDatabase([record, record])


@pytest.mark.parametrize("field", ["designation", "specification", "edition"])
def test_blank_identity_metadata_is_rejected(database, field):
    with pytest.raises(ValueError, match=field):
        replace(database.lookup("X52"), **{field: " "})


def test_explicit_null_edition_token_is_rejected(database):
    with pytest.raises(ValueError, match="edition"):
        replace(database.lookup("X52"), edition="edition-unspecified")


@pytest.mark.parametrize("value", [True, "MPa", float("nan"), float("inf")])
def test_invalid_strength_is_rejected(value):
    g = grades.get("X52")
    with pytest.raises(ValueError, match="smys_mpa.*finite positive"):
        Strengths(smys_mpa=value, smts_mpa=g.smts_mpa)


def test_nonpositive_and_inverted_strengths_are_rejected():
    g = grades.get("X52")
    zero = g.smys_mpa - g.smys_mpa
    with pytest.raises(ValueError, match="smys_mpa.*finite positive"):
        Strengths(smys_mpa=zero, smts_mpa=g.smts_mpa)
    with pytest.raises(ValueError, match="smts_mpa.*smys_mpa"):
        Strengths(smys_mpa=g.smts_mpa, smts_mpa=g.smys_mpa)


def test_units_are_required_and_consistent():
    g = grades.get("X52")
    with pytest.raises(ValueError, match="units.*MPa"):
        Strengths(smys_mpa=g.smys_mpa, smts_mpa=g.smts_mpa, units="psi")


def test_missing_provenance_is_rejected(database):
    with pytest.raises(ValueError, match="provenance"):
        replace(database.lookup("X52"), provenance=None)
    with pytest.raises(ValueError, match="citation"):
        replace(database.lookup("X52").provenance, citation="")


@pytest.mark.parametrize(
    "field,value",
    [("repository_revision", "HEAD"), ("repository_revision", ""),
     ("repository_revision", " "), ("repository_revision", 123),
     ("sha256", "unestablished"),
     ("retrieved_on_utc", "unestablished"), ("repository_path", "../grades.py")],
)
def test_malformed_provenance_is_rejected(database, field, value):
    with pytest.raises(ValueError, match=field):
        replace(database.lookup("X52").provenance, **{field: value})


def test_missing_strengths_are_rejected(database):
    with pytest.raises(ValueError, match="strengths"):
        replace(database.lookup("X52"), strengths=None)


@pytest.mark.parametrize("line_ending", ["\r\n", "\r"])
def test_source_digest_normalises_line_endings(line_ending):
    source = Path(grades.__file__).read_text(encoding="utf-8")
    converted = source.replace("\n", line_ending).encode("utf-8")
    assert database_module.normalised_source_digest(converted) == (
        database_module.normalised_source_digest(source.encode("utf-8"))
    )


def test_source_digest_detects_changed_text():
    source = Path(grades.__file__).read_text(encoding="utf-8")
    changed = source.replace("MaterialGrade", "materialGrade", 1)
    assert changed != source
    assert database_module.normalised_source_digest(changed.encode("utf-8")) != (
        database_module.normalised_source_digest(source.encode("utf-8"))
    )


def test_source_sidecar_drift_changes_provenance(tmp_path):
    fixture = Path(__file__).parent / "fixtures"
    original = load_catalog(fixture / "catalog.json")
    sources = json.loads((fixture / "sources.yml").read_text(encoding="utf-8"))
    sources["grade:X52"]["revision"] = "2"
    (tmp_path / "catalog.json").write_bytes((fixture / "catalog.json").read_bytes())
    (tmp_path / "sources.yml").write_text(json.dumps(sources), encoding="utf-8", newline="\n")
    changed = load_catalog(tmp_path / "catalog.json")
    assert changed.digest != original.digest
    assert next(g for g in changed.grades if g.name == "X52").source.revision == "2"


def test_source_drift_requires_refreshed_provenance():
    assert load_database().source_digest_matches, (
        "the grade module's text changed since the pinned digest was recorded: "
        "inspect the change, then refresh the pinned revision, digest and date "
        "in the database module"
    )


def test_distinct_editions_cannot_silently_overwrite_grade_lookup(database):
    record = database.lookup("X52")
    other = replace(record, edition="test-edition")
    assert record.key != other.key
    with pytest.raises(ValueError, match="ambiguous grade lookup"):
        MaterialDatabase([record, other])


def test_invalid_product_level_is_rejected(database):
    with pytest.raises(ValueError, match="product_level"):
        replace(database.lookup("X52"), product_level=None)
    with pytest.raises(ValueError, match="product_level"):
        replace(database.lookup("S355"), product_level="PSL1")


@pytest.mark.parametrize("field", ["sn_curve_ids", "standards_registry_id", "structural_shape_id"])
def test_unestablished_foreign_keys_are_rejected(database, field):
    value = ("unestablished:curve:air",) if field == "sn_curve_ids" else "unestablished"
    with pytest.raises(ValueError, match=field):
        MaterialDatabase([replace(database.lookup("X52"), **{field: value})])


def test_record_and_database_cannot_be_mutated(database):
    record = database.lookup("X52")
    with pytest.raises(FrozenInstanceError):
        record.designation = "changed"
    with pytest.raises(TypeError):
        database[record.key] = record
    with pytest.raises(AttributeError):
        database.source_digest_matches = False


def test_unknown_stable_key_and_grade_are_rejected(database):
    with pytest.raises(KeyError):
        database["unestablished"]
    with pytest.raises(KeyError):
        database.lookup("ZZ999")


def test_key_lookup_does_not_reenter_grade_reconciliation(database, monkeypatch):
    record = database.lookup("X52")

    def forbidden(*args, **kwargs):
        raise AssertionError("stable-key lookup must use the index")

    monkeypatch.setattr(grades, "get", forbidden)
    monkeypatch.setattr(grades, "all_grades", forbidden)
    assert database[record.key] is record


def test_historical_pack_citations_still_resolve():
    root = Path(__file__).resolve().parents[2]
    pack = (root / "src/digitalmodel/materials/docs/agent-pack.md").read_text(encoding="utf-8")
    # Historical locators remain historical; they do not qualify current inputs.
    revision = "b4c10284fa7d28398e6fa02c262c219c0fec8fef"
    citations = re.findall(r"\[([^\]\s]+):L(\d+)-L(\d+)\]", pack)
    assert citations
    sources = {}
    for path, first, last in citations:
        if path not in sources:
            sources[path] = subprocess.run(
                ["git", "show", f"{revision}:{path}"], cwd=root,
                check=True, capture_output=True,
            ).stdout.decode("utf-8").splitlines()
        assert 1 <= int(first) <= int(last) <= len(sources[path])
        assert any(line.strip() for line in sources[path][int(first)-1:int(last)])


def test_materials_ci_checkout_includes_pinned_history():
    """Issue #2778: the existing materials domain gate needs complete history."""
    import yaml

    root = Path(__file__).resolve().parents[2]
    workflow = yaml.safe_load(
        (root / ".github/workflows/quality-gates-by-domain.yml").read_text(
            encoding="utf-8"
        )
    )
    steps = workflow["jobs"]["domain-tests"]["steps"]
    checkout, = (step for step in steps
                 if step.get("uses", "").startswith("actions/checkout@"))
    # The existing domain workflow uses zero to retain history for citations;
    # this is a workflow contract, not an engineering limit.
    assert checkout["with"]["fetch-depth"] == 0
