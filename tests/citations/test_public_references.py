"""Public reference metadata must not bypass wiki calculation validation."""

from dataclasses import asdict

import pytest

from digitalmodel.citations import public_references
from digitalmodel.citations.schema import (
    Citation,
    CitedValue,
    CitationResolutionError,
    CitationValidationError,
    validate_citation,
)


def reference(**overrides):
    fields = dict(
        code_id="PUBLIC-REFERENCE",
        publisher="Example publisher",
        revision="Public web edition",
        section="Method description",
        source_url="https://example.org/method",
        retrieved_on="2026-10-20",
    )
    fields.update(overrides)
    return Citation(**fields)


@pytest.mark.parametrize(
    "source_id,publisher,locator",
    [
        (
            "NASA-DRAG-EQUATION", "NASA",
            "https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/",
        ),
        (
            "ORCINA-LINKS-DATA", "Orcina",
            "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Links,Data.htm",
        ),
        (
            "PASSING-SHIP-METHOD", "digitalmodel",
            "docs/domains/hydrodynamics/wang-passing-ship-forces.md",
        ),
    ],
)
def test_registered_reference_preserves_source_and_access_date(
    source_id, publisher, locator
):
    citation = public_references.get_public_reference(source_id)
    assert isinstance(citation, Citation)
    assert citation.code_id == source_id
    assert citation.publisher == publisher
    assert (citation.source_url or citation.repository_path) == locator
    assert citation.retrieved_on == "2026-10-20"
    assert citation.wiki_path == ""
    assert citation.note.strip()


def test_method_note_does_not_claim_retrieval_of_original_papers():
    citation = public_references.get_public_reference("PASSING-SHIP-METHOD")
    assert citation.revision == "01fc9c59728739b7c961a0a7fc77bb62846ef054"
    assert "original papers were not newly retrieved" in citation.note
    assert "access" in citation.note.lower()


def test_unknown_reference_is_not_silently_registered():
    with pytest.raises(ValueError, match="Unknown public reference"):
        public_references.get_public_reference("UNREGISTERED")


@pytest.mark.parametrize(
    "fields",
    [
        {"source_url": ""},
        {"wiki_path": "wikis/method.md"},
        {"repository_path": "docs/method.md"},
        {"source_url": "", "repository_path": "docs/method.md", "wiki_path": "wikis/method.md"},
    ],
)
def test_ambiguous_or_missing_locator_is_rejected(fields):
    with pytest.raises(CitationValidationError, match="exactly one"):
        reference(**fields)


@pytest.mark.parametrize(
    "url",
    [
        "javascript:alert(1)", "data:text/html,test", "http://example.org/",
        "https:///method", "https://user:password@example.org/",
        "https://user@example.org/", "https://example.org/\nmethod",
        "https://example.org/ method", "https://example.org/\tmethod",
        "https://example.org/\x00method", "https://example.org/\x7fmethod",
        "https://example.org\\method", "https://example.org:bad/method",
        "https://example.org:99999/method", "//example.org/method",
        "HTTPS://example.org/method", "https://example.org/<script>",
        'https://example.org/"method', "https://example.org/`method",
        "https://exämple.org/method", "https://example.org/" + "x" * 2048,
    ],
)
def test_unsafe_public_url_is_rejected(url):
    with pytest.raises(CitationValidationError, match="source_url"):
        reference(source_url=url)


@pytest.mark.parametrize(
    "path",
    [
        "/docs/method.md", "C:/docs/method.md", "docs/../method.md",
        "docs/./method.md", "docs//method.md", "docs\\method.md",
        "src/method.py", "docs/method.md\n", "docs/method.md\x00",
        "docs/<script>.md",
    ],
)
def test_repository_locator_cannot_escape_document_namespace(path):
    with pytest.raises(CitationValidationError, match="repository_path"):
        reference(source_url="", repository_path=path)


@pytest.mark.parametrize("date", ["", "2026-02-30", "20261020", "20-10-2026", "2026-10-20T00:00:00Z", "2026-1-5", "2026-10-20\n", "２０２６-１０-２０", None])
@pytest.mark.parametrize("repository", [False, True])
def test_public_reference_requires_real_iso_access_date(date, repository):
    fields = {"source_url": "", "repository_path": "docs/method.md"} if repository else {}
    with pytest.raises(CitationValidationError, match="retrieved_on"):
        reference(retrieved_on=date, **fields)


@pytest.mark.parametrize("source_id", ["NASA-DRAG-EQUATION", "ORCINA-LINKS-DATA", "PASSING-SHIP-METHOD"])
def test_public_reference_is_not_a_validated_calculation_source(source_id, tmp_path):
    citation = public_references.get_public_reference(source_id)
    with pytest.raises(CitationResolutionError) as error:
        validate_citation(citation, repo_root=tmp_path)
    assert error.value.reason == "reference_only"
    assert error.value.code_id == source_id


def test_reference_dictionary_can_be_reconstructed_without_metadata_loss():
    citation = reference()
    assert Citation(**asdict(citation)) == citation


def test_reference_only_citation_cannot_be_attached_to_a_numeric_value():
    with pytest.raises(CitationValidationError, match="reference-only"):
        CitedValue(value=1.0, citation=reference(), units="dimensionless")


@pytest.mark.parametrize("fields", [{"source_sibling": "other"}, {"source_project": "example"}])
def test_public_reference_cannot_claim_wiki_project_routing(fields):
    with pytest.raises(CitationValidationError, match="wiki routing"):
        reference(**fields)


@pytest.mark.parametrize("control", ["\x80", "\x85", "\x9f", "\u202e", "\u200b", "\u2066"])
def test_url_rejects_unicode_controls_and_format_characters(control):
    with pytest.raises(CitationValidationError, match="source_url"):
        reference(source_url="https://example.org/a" + control + "b")


@pytest.mark.parametrize("host", [".", "-", "%65xample.org", "example..org", "-example.org", "example-.org"])
def test_url_rejects_malformed_host(host):
    with pytest.raises(CitationValidationError, match="source_url"):
        reference(source_url="https://" + host + "/method")


@pytest.mark.parametrize("invalid_date", [None, 0, [], False, "2026-02-30"])
def test_wiki_reference_rejects_invalid_supplied_date(invalid_date):
    with pytest.raises(CitationValidationError, match="retrieved_on"):
        reference(source_url="", wiki_path="wikis/method.md", retrieved_on=invalid_date)


def test_legacy_positional_constructor_order_is_preserved():
    citation = Citation("CODE", "Publisher", "Edition", "Section", "wikis/method.md", "Note", "generic", None)
    assert citation.wiki_path == "wikis/method.md"
    assert citation.note == "Note"
    assert citation.source_sibling == "generic"
    assert citation.source_project is None


def test_missing_citation_can_reach_downstream_validation():
    assert CitedValue(value=1.0, citation=None).citation is None


def test_link_reference_distinguishes_tethers_from_compression_springs():
    note = public_references.get_public_reference("ORCINA-LINKS-DATA").note
    assert "Tethers are tension-only" in note
    assert "spring/dampers can carry compression" in note


@pytest.mark.parametrize("source_id", [None, [], {}])
def test_invalid_source_id_is_rejected_with_value_error(source_id):
    with pytest.raises(ValueError, match="Unknown public reference"):
        public_references.get_public_reference(source_id)


def test_registered_method_note_matches_its_pinned_revision():
    import os
    from pathlib import Path
    import subprocess

    citation = public_references.get_public_reference("PASSING-SHIP-METHOD")
    root = Path(__file__).resolve().parents[2]
    note = root / citation.repository_path
    assert note.is_file()
    env = dict(os.environ)
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(name, None)
    result = subprocess.run(["git", "rev-parse", f"{citation.revision}:{citation.repository_path}"], cwd=root, env=env, capture_output=True)
    if result.returncode:
        shallow = subprocess.check_output(["git", "rev-parse", "--is-shallow-repository"], cwd=root, env=env, text=True).strip()
        if shallow == "true":
            pytest.skip("Pinned method-note revision is absent from this shallow checkout")
    assert result.returncode == 0, "Pinned method-note revision cannot be read"
    working_blob = subprocess.check_output(
        ["git", "hash-object", f"--path={citation.repository_path}", str(note)],
        cwd=root, env=env,
    )
    assert result.stdout.strip() == working_blob.strip()
