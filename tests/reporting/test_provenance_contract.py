"""Fail-closed metadata and declared-read regressions for #1019."""

import pytest
from pydantic import ValidationError

import digitalmodel.reporting as reporting


def complete_provenance(**overrides):
    fields = dict(
        code_edition="digitalmodel test / standard 2026",
        units_system="SI",
        validated_path="screening",
    )
    fields.update(overrides)
    return reporting.Provenance(**fields).add(
        "workflow_results", "examples/workflows/demo/results/a.json"
    )


@pytest.mark.parametrize("field", ["code_edition", "validated_path"])
@pytest.mark.parametrize("missing", [None, ""])
def test_metadata_is_mandatory_at_require(field, missing):
    # Empty validated_path is rejected by the Literal; absent values by require.
    if field == "validated_path" and missing == "":
        with pytest.raises(ValidationError):
            complete_provenance(**{field: missing})
        return
    prov = complete_provenance(**{field: missing})
    with pytest.raises(reporting.ProvenanceError, match=field):
        prov.require()
    assert not prov.is_complete()


def test_whitespace_edition_is_incomplete():
    prov = complete_provenance(code_edition="  ")
    with pytest.raises(reporting.ProvenanceError, match="code_edition"):
        prov.require()
    assert not prov.is_complete()


@pytest.mark.parametrize("status", ["validated", "screening", "unvalidated"])
def test_complete_metadata_requires_sources_and_accepts_status(status):
    prov = complete_provenance(validated_path=status)
    assert prov.is_complete() and prov.require() is prov
    prov.sources.clear()
    assert not prov.is_complete()
    with pytest.raises(reporting.ProvenanceError):
        prov.require()


def test_units_are_optional_as_approved():
    assert complete_provenance(units_system=None).is_complete()


def test_metadata_summary_is_escaped_above_sources():
    block = reporting.provenance_block(
        complete_provenance(code_edition="<edition>", units_system="<SI>")
    )
    assert "&lt;edition&gt;" in block and "&lt;SI&gt;" in block
    assert "<edition>" not in block and "<SI>" not in block
    assert "screening" in block
    assert block.index("&lt;edition&gt;") < block.index("<table>")


def section(reads=None, builder=None, mode=reporting.SectionMode.ALWAYS):
    return reporting.ReportSection(
        "results",
        "Results",
        mode,
        builder or (lambda data, **kw: "RESULT"),
        reads=reads,
    )


@pytest.mark.parametrize(
    "reads",
    [None, [], ["workflow_results"], ["examples/workflows/*/results/*.json"]],
)
def test_declared_reads_match_identifiers_or_exact_kind(reads):
    assert "RESULT" in reporting.assemble_report(
        "R", provenance=complete_provenance(), sections=[section(reads)]
    )


@pytest.mark.parametrize(
    "identifier", ["results/run[1].json", "results/a?.json", "results/a*.json"]
)
def test_literal_read_matches_its_exact_identifier(identifier):
    prov = reporting.Provenance(code_edition="test", validated_path="screening").add(
        "file", identifier
    )
    assert "RESULT" in reporting.assemble_report(
        "R", provenance=prov, sections=[section([identifier])]
    )


@pytest.mark.parametrize(
    "reads",
    [["elsewhere/*.json"], ["workflow_*"], ["workflow_results", "missing"]],
)
def test_unmatched_read_fails_before_any_builder_or_write(tmp_path, reads):
    called = []
    out = tmp_path / "report.html"
    error = getattr(reporting, "ContractError")
    assert issubclass(error, reporting.ProvenanceError)
    with pytest.raises(error, match=f"results.*{reads[-1].replace('*', '.*')}"):
        reporting.assemble_report(
            "R",
            provenance=complete_provenance(),
            sections=[section(reads, lambda *a, **kw: called.append(True))],
            output_path=out,
        )
    assert not called and not out.exists()


def test_skipped_sections_still_have_their_contract_checked():
    error = getattr(reporting, "ContractError")
    with pytest.raises(error):
        reporting.assemble_report(
            "R",
            provenance=complete_provenance(),
            mode="compact",
            sections=[section(["missing"], mode=reporting.SectionMode.COMPACT_SKIP)],
        )


def test_bare_backbone_treats_reads_as_metadata():
    assert reporting.ReportBackbone("R", [section(["unmatched"])]).render({}) == [
        "RESULT"
    ]


def test_skeleton_checks_metadata_but_remains_kickoff_renderable():
    sk = reporting.ReportSkeleton(title="Draft", sections=[])
    partial = reporting.Provenance().add("spec", "spec.yml")
    for prov in (None, reporting.Provenance(), partial):
        status = sk.completeness(provenance=prov)
        assert not status.provenance_ok and not status.complete
        assert "<!DOCTYPE html>" in sk.build_html(provenance=prov)
    assert sk.completeness(provenance=complete_provenance()).complete


def test_package_edition_fallback_is_explicit(monkeypatch):
    from importlib.metadata import PackageNotFoundError

    from digitalmodel.reporting import _metadata

    monkeypatch.setattr(_metadata, "version", lambda name: "test-package-version")
    assert _metadata.package_code_edition() == "test-package-version"

    def absent(name):
        raise PackageNotFoundError(name)

    monkeypatch.setattr(_metadata, "version", absent)
    assert _metadata.package_code_edition() == "unknown"
