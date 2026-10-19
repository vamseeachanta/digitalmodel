"""Standard report bundle destination audit (issue #2315).

Expected paths are hand-enumerated using the existing bundle naming contract.
Symlinks place artifacts in separate fixture directories; each manifest link
must resolve to the intended artifact and the retained input hashes must be
unchanged on rejection. No engineering coefficients or limits are introduced.
"""

from hashlib import sha256
import json

import pytest

from digitalmodel.reporting import (
    DocumentMeta, Provenance, ReportSpec, Section, TextBlock, write_report,
)


def _spec():
    return ReportSpec(
        document=DocumentMeta(
            number="B0000-RPT-001-00", revision="00", title="Example",
            project="Example", client="Example", date="2026-10-19",
        ),
        sections=[Section(key="example", title="Example", blocks=[TextBlock(markdown="Example")])],
        provenance=Provenance().add("fixture", "inputs.json", digest="synthetic"),
    )


@pytest.mark.parametrize("pair", [
    ("analysis.html", "analysis_citations.json"),
    ("analysis_citations.json", "analysis_manifest.json"),
    ("analysis.html", "analysis.pdf"),
])
@pytest.mark.parametrize("alias", ["symlink", "hardlink"])
def test_colliding_bundle_outputs_preserve_all_files(tmp_path, monkeypatch, pair, alias):
    import digitalmodel.reporting.engine as engine

    def unexpected_render(*args, **kwargs):
        pytest.fail("Rendering must not start before destination preflight")

    monkeypatch.setattr(engine, "render_html", unexpected_render)
    monkeypatch.setattr(engine, "render_pdf", unexpected_render)
    retained = tmp_path / "retained.data"
    retained.write_bytes(b"retained input")
    before = sha256(retained.read_bytes()).digest()
    for name in pair:
        destination = tmp_path / name
        if alias == "hardlink":
            destination.hardlink_to(retained)
        else:
            try:
                destination.symlink_to(retained.name)
            except OSError:
                pytest.skip("Symlink creation is unavailable")

    with pytest.raises(ValueError, match="distinct"):
        write_report(_spec(), tmp_path, "analysis", pdf="auto")

    assert sha256(retained.read_bytes()).digest() == before
    for name in ("analysis.html", "analysis.pdf", "analysis_citations.json", "analysis_manifest.json"):
        path = tmp_path / name
        if name in pair:
            assert sha256(path.read_bytes()).digest() == before
        else:
            assert not path.exists()


def test_disabled_pdf_is_not_a_destination(tmp_path):
    try:
        (tmp_path / "analysis.pdf").symlink_to("analysis.html")
    except OSError:
        pytest.skip("Symlink creation is unavailable")
    artifacts = write_report(_spec(), tmp_path, "analysis", pdf="off")
    assert artifacts.html_path.is_file()
    assert artifacts.pdf_path is None
    assert (tmp_path / "analysis.pdf").is_symlink()


def test_cross_directory_artifacts_keep_resolvable_manifest_links(tmp_path):
    summary = tmp_path / "summary.html.json"
    details = tmp_path / "details"
    summary.mkdir()
    details.mkdir()
    try:
        (summary / "analysis.html").symlink_to(details / "report.html")
    except OSError:
        pytest.skip("Symlink creation is unavailable")
    try:
        (summary / "analysis_citations.json").symlink_to(details / "citations.json")
    except OSError:
        pytest.skip("Symlink creation is unavailable")

    artifacts = write_report(_spec(), summary, "analysis", pdf="off")
    manifest = json.loads(artifacts.manifest_path.read_text(encoding="utf-8"))
    expected = {
        "html": details / "report.html",
        "citations": details / "citations.json",
        "manifest": summary / "analysis_manifest.json",
    }
    for key, target in expected.items():
        link = artifacts.manifest_path.parent / manifest["artifacts"][key]
        assert link.resolve() == target
        assert link.read_bytes() == target.read_bytes()
    assert manifest["artifacts"]["pdf"] is None
