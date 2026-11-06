"""Path expectations for issue #2315, calculated without the writer.

The output names are the existing report-pack contract. Resolving an artifact
from a relocated manifest must reach the same fixture file. SHA-256 compares
bytes retained on rejection; there are no engineering acceptance limits.
"""

from hashlib import sha256
import json
from pathlib import PureWindowsPath
import re
from types import SimpleNamespace
from urllib.parse import unquote

import pytest

from digitalmodel.report_pack import workflow


def _cfg(root):
    return {
        "_config_dir_path": str(root),
        "_config_file_path": str(root / "analysis"),
        "report_pack": {
            "document": {
                "number": "B0000-RPT-001-00", "revision": "00",
                "title": "Example", "project": "Example", "client": "Example",
            },
            "sections": [{"title": "Results", "content": "Synthetic content."}],
            "manifest": {
                "issue": "https://example.invalid/issues/1", "project": "Example",
                "artifact_class": "test", "privacy_classification": "public",
                "publishability_decision": "test", "input_source_ids": ["fixture"],
                "raw_output_path": "inputs", "final_output_path": "results",
            },
            "output_dir": "results.html.json", "pdf": "off",
        },
    }


OUTPUTS = (
    "analysis_report.md", "analysis_report.html", "analysis_report.pdf",
    "analysis_citations.json", "analysis_manifest.json", "report-layer-manifest.json",
)


@pytest.mark.parametrize("artifact,expected", [
    ("C:/repo/reports/pages/analysis.html", "reports/pages/analysis.html"),
    ("D:/exports/pages/analysis.html", "D:/exports/pages/analysis.html"),
])
def test_display_paths_use_forward_slashes(monkeypatch, artifact, expected):
    root = SimpleNamespace(resolve=lambda: PureWindowsPath("C:/repo"))
    path = SimpleNamespace(resolve=lambda: PureWindowsPath(artifact))
    monkeypatch.setattr(workflow, "REPO_ROOT", root)
    assert workflow._display_path(path) == expected


def test_incompatible_figure_roots_leave_no_outputs(tmp_path, monkeypatch):
    import digitalmodel._output_paths as paths

    cfg = _cfg(tmp_path)
    figure = tmp_path / "figure.svg"
    figure.write_text(
        '<svg xmlns="http://www.w3.org/2000/svg"></svg>',
        encoding="utf-8", newline="\n",
    )
    before = sha256(figure.read_bytes()).digest()
    cfg["report_pack"]["results"] = {
        "figures": [{"title": "Fixture", "path": str(figure)}],
    }
    original = paths.os.path.relpath

    def incompatible_figure(artifact, document_dir):
        if artifact == figure.resolve():
            raise ValueError("Synthetic incompatible roots")
        return original(artifact, document_dir)

    monkeypatch.setattr(paths.os.path, "relpath", incompatible_figure)
    with pytest.raises(ValueError, match="incompatible roots") as caught:
        workflow.router(cfg)
    assert isinstance(caught.value, workflow.ReportPackConfigError)
    message = str(caught.value)
    assert "report_pack.results.figures[1]" in message
    assert "Fixture" in message
    assert figure.as_posix() in message
    assert isinstance(caught.value.__cause__, ValueError)
    assert not (tmp_path / "results.html.json").exists()
    assert sha256(figure.read_bytes()).digest() == before


def test_unsupported_figure_error_uses_forward_slashes(monkeypatch):
    figure = PureWindowsPath("D:/exports/figure.tiff")
    monkeypatch.setattr(workflow, "_resolve_file", lambda *args: figure)
    settings = {"results": {"figures": [{"title": "Fixture", "path": str(figure)}]}}
    with pytest.raises(workflow.ReportPackConfigError, match="unsupported figure format") as caught:
        workflow._load_results(settings, PureWindowsPath("C:/repo"))
    assert figure.as_posix() in str(caught.value)
    assert "\\" not in str(caught.value)


@pytest.mark.parametrize("alias", ["symlink", "hardlink"])
@pytest.mark.parametrize("pair", [
    ("analysis_report.md", "analysis_report.html"),
    ("analysis_citations.json", "analysis_manifest.json"),
    ("analysis_manifest.json", "report-layer-manifest.json"),
    ("analysis_report.pdf", "report-layer-manifest.json"),
])
def test_collision_preserves_all_bytes_before_rendering(tmp_path, monkeypatch, alias, pair):
    cfg = _cfg(tmp_path)
    cfg["report_pack"]["pdf"] = "auto"
    directory = tmp_path / "results.html.json"
    directory.mkdir()
    retained = tmp_path / "retained.data"
    retained.write_bytes(b"retained input")
    before = sha256(retained.read_bytes()).digest()
    for name in pair:
        path = directory / name
        if alias == "symlink":
            path.symlink_to(retained)
        else:
            path.hardlink_to(retained)

    def unexpected(*args, **kwargs):
        pytest.fail("No rendering before preflight")

    monkeypatch.setattr(workflow, "render_markdown", unexpected)
    monkeypatch.setattr(workflow, "render_html", unexpected)
    monkeypatch.setattr(workflow, "render_pdf", unexpected)
    with pytest.raises(ValueError, match="distinct"):
        workflow.router(cfg)
    assert sha256(retained.read_bytes()).digest() == before
    assert {path.name for path in directory.iterdir()} == set(pair)


def test_disabled_pdf_is_excluded_and_suffix_text_directory_is_preserved(tmp_path):
    cfg = _cfg(tmp_path)
    directory = tmp_path / "results.html.json"
    directory.mkdir()
    (directory / "analysis_report.pdf").symlink_to("analysis_report.html")
    workflow.router(cfg)
    manifest = json.loads((directory / "analysis_manifest.json").read_text(encoding="utf-8"))
    assert manifest["pdf_report"] is None
    assert (directory / "analysis_report.pdf").is_symlink()
    assert (directory / "analysis_report.md").is_file()


def test_relocated_manifests_and_markdown_figure_links_resolve(tmp_path):
    cfg = _cfg(tmp_path)
    figure = tmp_path / "inputs" / "figure #1.svg"
    figure.parent.mkdir()
    figure.write_text('<svg xmlns="http://www.w3.org/2000/svg"></svg>', encoding="utf-8", newline="\n")
    cfg["report_pack"]["results"] = {
        "figures": [{"title": "Fixture", "path": str(figure)}],
    }
    before = sha256(figure.read_bytes()).digest()
    directory = tmp_path / "results.html.json"
    directory.mkdir()
    targets = {}
    for name in OUTPUTS:
        if name.endswith(".pdf"):
            continue
        target = tmp_path / name / "artifact"
        target.parent.mkdir()
        (directory / name).symlink_to(target)
        targets[name] = target
    workflow.router(cfg)

    manifest_path = targets["analysis_manifest.json"]
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    for key, name in (
        ("markdown_report", "analysis_report.md"),
        ("html_report", "analysis_report.html"),
        ("citation_sidecar", "analysis_citations.json"),
        ("report_layer_manifest", "report-layer-manifest.json"),
    ):
        assert (manifest_path.parent / manifest[key]).resolve() == targets[name]
    layer_path = targets["report-layer-manifest.json"]
    layer = json.loads(layer_path.read_text(encoding="utf-8"))
    assert {(layer_path.parent / name).resolve() for name in layer["files"]} == set(targets.values())
    assert (layer_path.parent / layer["generated_manifest"]).resolve() == manifest_path
    assert (layer_path.parent / layer["citation_evidence_manifest"]).resolve() == targets["analysis_citations.json"]
    markdown = targets["analysis_report.md"]
    link = re.search(r"!\[Fixture\]\(([^)]+)\)", markdown.read_text(encoding="utf-8")).group(1)
    assert (markdown.parent / unquote(link)).resolve() == figure
    assert sha256(figure.read_bytes()).digest() == before
