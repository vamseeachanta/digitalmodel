"""Offline database contracts; strength fixtures remain in test_grade_matrix.py."""
from dataclasses import FrozenInstanceError, replace
from pathlib import Path

import pytest

from digitalmodel.materials import grades
from digitalmodel.materials import database as database_module
from digitalmodel.materials.database import MaterialDatabase, load_database
from digitalmodel.materials.database_schema import Strengths


@pytest.fixture
def database():
    return load_database()


def test_all_reconciled_grades_are_loaded_once_per_stored_level(database):
    expected = sum(
        2 if g.standard == "API 5L" and g.smts_psl1_mpa is not None else 1
        for g in grades.all_grades()
    )
    assert len(database) == expected
    assert len(set(database)) == len(database)
    for record in database.values():
        assert database[record.key] is record


# Names and product levels are pinned to test_grade_matrix.py's API cases.
@pytest.mark.parametrize(
    "name", ["A25", "X42", "X52", "X65", "X70", "X80", "X90", "X100", "X120"]
)
@pytest.mark.parametrize("psl", ["PSL1", "PSL2", "psl1", "unknown"])
def test_psl_selection_matches_existing_reconciliation(database, name, psl):
    record = database.lookup(name, psl=psl)
    assert record.strengths.smys_mpa == grades.smys_mpa(name)
    assert record.strengths.smts_mpa == grades.smts_mpa(name, psl=psl)
    g = grades.get(name)
    expected_level = (
        "PSL1" if psl.upper() == "PSL1" and g.smts_psl1_mpa is not None else "PSL2"
    )
    assert record.product_level == expected_level


@pytest.mark.parametrize(
    "name", ["L360", "l485", "x52", "ah36", "Grade A", "A", "316L", "2205"]
)
def test_aliases_and_distinct_grade_a_are_preserved(database, name):
    assert database.lookup(name).designation == grades.get(name).name
    assert database.lookup("A").key != database.lookup("Grade A").key


def test_values_and_citations_equal_the_source_records(database):
    for record in database.values():
        source = grades.get(record.designation)
        assert record.specification == source.standard
        assert record.strengths.units == "MPa"
        assert record.strengths.smys_mpa == source.smys_mpa
        assert record.strengths.smts_mpa == grades.smts_mpa(
            source.name, psl=record.product_level or "PSL2"
        )
        assert record.provenance.citation == source.source
        assert record.notes == source.notes


def test_provenance_pins_the_repository_source(database):
    digest = database_module.normalised_source_digest(Path(grades.__file__).read_bytes())
    for record in database.values():
        provenance = record.provenance
        assert provenance.repository_path == "src/digitalmodel/materials/grades.py"
        assert provenance.sha256 == digest
        assert provenance.repository_revision
        assert provenance.retrieved_on_utc
        assert provenance.repository_terms == "MIT License"
        assert provenance.original_source_terms == "not re-verified in this job"


def test_edition_metadata_is_not_inferred(database):
    for record in database.values():
        if record.specification == "API 5L":
            assert record.edition is not None, f"missing edition for {record.key}"
            assert record.edition in record.provenance.citation
        else:
            assert record.edition is None


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
    [("repository_revision", "HEAD"), ("sha256", "unestablished"),
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


def test_source_drift_does_not_block_loading(monkeypatch, database):
    changed_digest = database_module._SOURCE_SHA256[::-1]
    monkeypatch.setattr(database_module, "_SOURCE_SHA256", changed_digest)
    drifted = load_database()
    assert set(drifted) == set(database)
    assert drifted.source_digest_matches is False


def test_source_drift_requires_refreshed_provenance():
    assert load_database().source_digest_matches, (
        "the grade module's text changed since the pinned digest was recorded: "
        "inspect the change, then refresh the pinned revision, digest and date "
        "in the database module"
    )


def test_distinct_editions_cannot_silently_overwrite_grade_lookup(database):
    record = database.lookup("X52")
    other = replace(record, edition=None)
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
