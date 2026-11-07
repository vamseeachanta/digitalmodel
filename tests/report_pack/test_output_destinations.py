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


@pytest.mark.parametrize("suffix", [".svg", ".png"])
def test_incompatible_figure_roots_use_file_uri_and_keep_pack(tmp_path, monkeypatch, suffix):

    cfg = _cfg(tmp_path)
    figure = tmp_path / f"figure #50% ü{suffix}"
    if suffix == ".svg":
        figure.write_text(
            '<svg xmlns="http://www.w3.org/2000/svg"><title>Fixture</title></svg>',
            encoding="utf-8", newline="\n",
        )
    else:
        from base64 import b64decode

        figure.write_bytes(b64decode(
            "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aM1cAAAAASUVORK5CYII="
        ))
    before = sha256(figure.read_bytes()).digest()
    cfg["report_pack"]["results"] = {
        "figures": [{"title": "Fixture", "path": str(figure)}],
    }
    original = workflow.relative_artifact_path

    def incompatible_figure(artifact, document):
        if artifact == figure.resolve():
            raise ValueError("Synthetic incompatible roots")
        return original(artifact, document)

    monkeypatch.setattr(workflow, "relative_artifact_path", incompatible_figure)
    workflow.router(cfg)
    directory = tmp_path / "results.html.json"
    assert {path.name for path in directory.iterdir()} == set(OUTPUTS) - {"analysis_report.pdf"}
    markdown = (directory / "analysis_report.md").read_text(encoding="utf-8")
    link = re.search(r"!\[Fixture\]\(([^)]+)\)", markdown).group(1)
    assert link == figure.resolve().as_uri()
    assert "%23" in link and "%25" in link and "%C3%BC" in link
    html = (directory / "analysis_report.html").read_text(encoding="utf-8")
    assert ("<svg" if suffix == ".svg" else "data:image/png;base64,") in html
    manifest = json.loads((directory / "analysis_manifest.json").read_text(encoding="utf-8"))
    assert manifest["html_report"] == "analysis_report.html"
    assert manifest["markdown_report"] == "analysis_report.md"
    assert sha256(figure.read_bytes()).digest() == before


def test_cross_drive_figure_uri_preserves_drive_and_encoding(monkeypatch):
    def incompatible_roots(*args):
        raise ValueError("Synthetic incompatible roots")

    monkeypatch.setattr(workflow, "relative_artifact_path", incompatible_roots)
    results = {"tables": [], "figures": [{
        "title": "Fixture", "path": PureWindowsPath("D:/figures/figure #50% ü.svg"),
    }]}
    assert workflow._markdown_results(results, PureWindowsPath("C:/reports/analysis.md")) == [
        "![Fixture](file:///D:/figures/figure%20%2350%25%20%C3%BC.svg)", "",
    ]


def test_unsupported_figure_error_uses_forward_slashes(monkeypatch):
    figure = PureWindowsPath("D:/exports/figure.tiff")
    monkeypatch.setattr(workflow, "_resolve_file", lambda *args: figure)
    settings = {"results": {"figures": [{"title": "Fixture", "path": str(figure)}]}}
    with pytest.raises(workflow.ReportPackConfigError, match="unsupported figure format") as caught:
        workflow._load_results(settings, PureWindowsPath("C:/repo"))
    assert str(caught.value).startswith("report_pack.results.figures[1]: ")
    assert figure.as_posix() in str(caught.value)
    assert "\\" not in str(caught.value)


def test_unsupported_second_figure_names_index_and_preserves_inputs(tmp_path):
    cfg = _cfg(tmp_path)
    supported = tmp_path / "first.svg"
    unsupported = tmp_path / "second.tiff"
    supported.write_text(
        '<svg xmlns="http://www.w3.org/2000/svg"></svg>',
        encoding="utf-8", newline="\n",
    )
    unsupported.write_bytes(b"unsupported fixture")
    figures = (supported, unsupported)
    before = [sha256(path.read_bytes()).digest() for path in figures]
    cfg["report_pack"]["results"] = {
        "figures": [{"path": str(path)} for path in figures],
    }
    with pytest.raises(workflow.ReportPackConfigError) as caught:
        workflow.router(cfg)
    message = str(caught.value)
    assert message.startswith("report_pack.results.figures[2]: unsupported figure format")
    assert unsupported.as_posix() in message
    assert not (tmp_path / "results.html.json").exists()
    assert [sha256(path.read_bytes()).digest() for path in figures] == before


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
