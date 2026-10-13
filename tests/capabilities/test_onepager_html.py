# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Kept one-pager HTML, metadata, and command-line behaviour."""

import html
import re
import shutil
import subprocess
from html.parser import HTMLParser

import pytest

from scripts.capabilities import build_onepagers as builder


class Page(HTMLParser):
    """Collect titles and resource-bearing attributes, excluding ordinary links."""

    def __init__(self, text):
        super().__init__()
        self.titles = []
        self.resources = []
        self.in_title = False
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        self.in_title = tag == "title"
        if self.in_title:
            self.titles.append("")
        for key, value in attrs:
            if key in {"src", "srcset", "poster", "data"} or (
                key in {"href", "xlink:href"} and tag not in {"a"}
            ):
                self.resources.append(value)

    def handle_endtag(self, tag):
        if tag == "title":
            self.in_title = False

    def handle_data(self, data):
        if self.in_title:
            self.titles[-1] += data


@pytest.fixture
def output(tmp_path, monkeypatch):
    monkeypatch.setattr(builder, "_OUT", tmp_path / "pages")
    monkeypatch.setattr(builder, "_REPO", tmp_path)
    return tmp_path / "pages"


def fake_pdf(source, destination):
    """Record real input bytes in a stand-in output without a browser."""
    destination.write_bytes(source.read_bytes())


def test_html_written_and_renderer_receives_kept_file(output):
    calls = []

    def render(source, destination):
        calls.append((source, destination))
        fake_pdf(source, destination)

    spec = builder.SPECS[0]
    builder.build_onepager(spec, output, renderer=render)
    kept = output / f"{spec['id']}.html"
    pdf = kept.with_suffix(".pdf")
    assert calls == [(kept, pdf)]
    assert kept.read_bytes() == pdf.read_bytes()


def test_title_is_unique_and_escaped(output):
    spec = dict(builder.SPECS[0], title='Strength & <limits> "quoted"')
    builder.build_onepager(spec, output, renderer=fake_pdf)
    text = (output / f"{spec['id']}.html").read_text(encoding="utf-8")
    assert Page(text).titles == [spec["title"]]
    assert f"<title>{html.escape(spec['title'])}</title>" in text


def test_build_leaves_only_named_outputs_and_no_temporary_paths(output, tmp_path):
    before = set(tmp_path.iterdir())
    spec = builder.SPECS[0]
    builder.build_onepager(spec, output, renderer=fake_pdf)
    kept = output / f"{spec['id']}.html"
    assert {p.name for p in output.iterdir()} == {
        kept.name,
        kept.with_suffix(".pdf").name,
    }
    text = kept.read_text(encoding="utf-8")
    assert str(tmp_path) not in text
    assert not re.search(r"tmp[\w-]+\.html|file://|/(?:home|mnt)/", text)
    assert set(tmp_path.iterdir()) - before == {output}


@pytest.mark.parametrize("brand", ["", "body { color: purple; }"])
def test_kept_page_is_self_contained(output, brand):
    spec = builder.SPECS[0]
    builder.build_onepager(spec, output, brand_css=brand, renderer=fake_pdf)
    text = (output / f"{spec['id']}.html").read_text(encoding="utf-8")
    assert Page(text).resources == []
    assert "<svg" in text
    assert not re.search(r"@import|url\s*\(", text, re.I)
    assert "Explore live:" in text


def test_brand_cli_inlines_styles_after_defaults(output, tmp_path):
    css = tmp_path / "brand.css"
    content = "body { color: purple; }"
    css.write_text(content, encoding="utf-8", newline="\n")
    spec = builder.SPECS[0]
    builder.main(["--brand", str(css), spec["id"]], renderer=fake_pdf)
    text = (output / f"{spec['id']}.html").read_text(encoding="utf-8")
    assert text.index(content) > text.index(".cta")
    assert str(css) not in text


@pytest.mark.parametrize("source", ["missing.css", "."])
def test_missing_or_unreadable_brand_writes_nothing(output, tmp_path, source):
    with pytest.raises(SystemExit, match="Cannot read brand stylesheet"):
        builder.main(["--brand", str(tmp_path / source)], renderer=fake_pdf)
    assert not output.exists()


def test_known_id_builds_only_selected_page(output):
    spec = builder.SPECS[0]
    builder.main([spec["id"]], renderer=fake_pdf)
    assert {p.stem for p in output.iterdir()} == {spec["id"]}


def test_unknown_id_retains_error_and_writes_nothing(output):
    with pytest.raises(SystemExit, match=r"^Unknown spec id\(s\): \['unknown'\]$"):
        builder.main(["unknown"], renderer=fake_pdf)
    assert not output.exists()


def test_help_describes_ids_and_brand(capsys):
    with pytest.raises(SystemExit) as result:
        builder.main(["--help"], renderer=fake_pdf)
    assert result.value.code == 0
    text = capsys.readouterr().out
    assert "--brand" in text
    assert "specification" in text.lower()


def test_every_specification_has_a_nonempty_document_title():
    for spec in builder.SPECS:
        assert Page(builder._render_html(spec)).titles == [spec["title"]]
        assert spec["title"].strip()


def test_browser_pdf_document_title(output):
    if not builder._CHROME or not shutil.which("pdfinfo"):
        pytest.skip("not applicable: Chrome or pdfinfo is not available")
    spec = builder.SPECS[0]
    builder.build_onepager(spec, output)
    result = subprocess.run(
        ["pdfinfo", str(output / f"{spec['id']}.pdf")],
        check=True,
        capture_output=True,
        text=True,
    )
    title = next(
        line.partition(":")[2].strip()
        for line in result.stdout.splitlines()
        if line.startswith("Title:")
    )
    assert title == spec["title"]
    assert not re.search(r"tmp[\w-]+", title)


@pytest.mark.parametrize(
    "css",
    [
        '@import "asset.css";',
        "body { background: url(asset.png); }",
        "body { background: u\\72l(asset.png); }",
        "</style><script src=asset.js></script>",
    ],
)
def test_brand_resource_loads_and_markup_are_rejected_before_writing(output, css):
    with pytest.raises(ValueError, match="self-contained"):
        builder.build_onepager(
            builder.SPECS[0], output, brand_css=css, renderer=fake_pdf
        )
    assert not output.exists()


def test_kept_page_is_utf8_with_explicit_lf(output, monkeypatch):
    original = type(output).write_text
    writes = []

    def write_text(path, text, **kwargs):
        writes.append(kwargs)
        # Model a platform default whose text files use CRLF.
        options = dict(kwargs)
        options.setdefault("newline", "\r\n")
        return original(path, text, **options)

    monkeypatch.setattr(type(output), "write_text", write_text)
    spec = dict(builder.SPECS[0], title="Strength – café")
    page = builder.build_onepager(spec, output, renderer=fake_pdf)
    data = page.read_bytes()
    assert "\r" not in data.decode("utf-8")
    assert spec["title"] in data.decode("utf-8")
    assert writes == [{"encoding": "utf-8", "newline": "\n"}]


@pytest.mark.parametrize(
    "css",
    [
        'body { background: image-set("x.png" 1x); }',
        "body { background: image-set('x.png' 1x); }",
        'body { background: -webkit-image-set("x.png" 1x); }',
        "body { background: -WEBKIT-IMAGE-SET ( 'x.png' 1x); }",
        'body { background: image-set(linear-gradient(red, blue) 1x, "x.png" 2x); }',
    ],
)
def test_brand_string_image_set_is_rejected(output, css):
    with pytest.raises(ValueError, match="self-contained"):
        builder.build_onepager(builder.SPECS[0], output, brand_css=css, renderer=fake_pdf)
    assert not output.exists()


def test_brand_image_setting_class_is_accepted(output):
    builder.build_onepager(
        builder.SPECS[0], output,
        brand_css=".image-setting { color: purple; }", renderer=fake_pdf,
    )
    assert output.is_dir()


def test_closing_message_counts_kept_pages_and_pdfs(output, capsys):
    builder.main([builder.SPECS[0]["id"]], renderer=fake_pdf)
    assert "1 kept HTML page(s) and 1 one-pager PDF(s)" in capsys.readouterr().out


def test_census_classifies_only_specification_print_sources(tmp_path, monkeypatch):
    from scripts import check_generated_html as checker

    script = tmp_path / "scripts/capabilities/build_onepagers.py"
    script.parent.mkdir(parents=True)
    script.write_bytes((builder._REPO / "scripts/capabilities/build_onepagers.py").read_bytes())
    output = tmp_path / "docs/api/capabilities/pdf"
    for spec in builder.SPECS:
        builder.build_onepager(spec, output, renderer=fake_pdf)
    monkeypatch.setattr(checker, "MANUAL_PAGES", {})
    assert checker.validate_page_census(tmp_path) == []
    unknown = output / "unknown.html"
    unknown.write_text("<html></html>", encoding="utf-8", newline="\n")
    assert checker.validate_page_census(tmp_path) == [
        "unclassified docs/api HTML page(s): docs/api/capabilities/pdf/unknown.html"
    ]


def test_kept_print_sources_are_git_ignored_and_pdf_siblings_are_not():
    # git check-ignore also reads per-user and per-clone exclude files;
    # checkout-specific excludes can therefore change its answer.
    import os
    from scripts.generated_html_ownership import build_output_pages

    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    pages = build_output_pages(builder._REPO)
    assert pages
    for page in sorted(pages):
        for path, expected in ((page, 0), (page.removesuffix(".html") + ".pdf", 1)):
            result = subprocess.run(
                ["git", "check-ignore", "--no-index", "--quiet", "--", path],
                cwd=builder._REPO, env=env,
            )
            assert result.returncode == expected, path


def test_print_source_ignore_does_not_cover_other_html():
    # git check-ignore also reads per-user and per-clone exclude files;
    # checkout-specific excludes can therefore change its answer.
    import os

    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    for path in ("docs/api/capabilities/example.html", "docs/api/other/pdf/example.html",
                 "docs/api/capabilities/pdf/nested/example.html"):
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", "--quiet", "--", path],
            cwd=builder._REPO, env=env,
        )
        assert result.returncode == 1, path


def test_docstring_identifies_html_as_ignored_build_output():
    assert "git-ignored and not committed" in builder.__doc__


def test_build_rejects_missing_pdf(output):
    spec = builder.SPECS[0]
    with pytest.raises(RuntimeError) as error:
        builder.build_onepager(spec, output, renderer=lambda source, pdf: None)
    message = str(error.value)
    assert spec["id"] in message
    assert f"{spec['id']}.pdf" in message
    assert "missing" in message


def test_build_rejects_empty_pdf(output):
    spec = builder.SPECS[0]

    def render(source, pdf):
        pdf.write_bytes(b"")

    with pytest.raises(RuntimeError) as error:
        builder.build_onepager(spec, output, renderer=render)
    message = str(error.value)
    assert spec["id"] in message
    assert f"{spec['id']}.pdf" in message
    assert "empty" in message


def test_build_rejects_rewritten_title(output):
    spec = builder.SPECS[0]

    def render(source, pdf):
        pdf.write_bytes(b"pdf")
        source.write_text("<title>Other title</title>", encoding="utf-8")

    with pytest.raises(RuntimeError) as error:
        builder.build_onepager(spec, output, renderer=render)
    message = str(error.value)
    assert spec["id"] in message
    assert f"{spec['id']}.html" in message
    assert "Other title" in message
    assert spec["title"] in message
    assert "expected" in message


@pytest.mark.parametrize("fault", ["missing", "empty"])
def test_build_rechecks_html_after_renderer(output, fault):
    spec = builder.SPECS[0]

    def render(source, pdf):
        pdf.write_bytes(b"pdf")
        if fault == "missing":
            source.unlink()
        else:
            source.write_bytes(b"")

    with pytest.raises(RuntimeError) as error:
        builder.build_onepager(spec, output, renderer=render)
    assert spec["id"] in str(error.value)
    assert f"{spec['id']}.html" in str(error.value)
    assert fault in str(error.value)


def test_closing_message_counts_two_validated_output_pairs(output, capsys):
    specs = builder.SPECS[:2]
    builder.main([spec["id"] for spec in specs], renderer=fake_pdf)
    assert "2 kept HTML page(s) and 2 one-pager PDF(s)" in capsys.readouterr().out
    assert len(list(output.glob("*.html"))) == 2
    assert len(list(output.glob("*.pdf"))) == 2


def test_main_stops_after_first_failed_build(output, capsys):
    calls = []

    def render(source, pdf):
        calls.append(source.stem)

    with pytest.raises(RuntimeError, match="missing"):
        builder.main([spec["id"] for spec in builder.SPECS[:2]], renderer=render)
    assert calls == [builder.SPECS[0]["id"]]
    assert not (output / f"{builder.SPECS[1]['id']}.html").exists()
    assert "Wrote" not in capsys.readouterr().out
