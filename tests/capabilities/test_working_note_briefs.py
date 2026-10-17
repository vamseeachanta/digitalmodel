# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Provenance labels for public practice answers and the hand-entered study."""

import importlib.util
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
NOTE = "Internal working note: not a diagnostic report"


def load_script(relative):
    spec = importlib.util.spec_from_file_location("report_label_script", ROOT / relative)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class VisibleText(HTMLParser):
    """Collect body text while excluding scripts, style sheets and metadata."""

    def __init__(self, text):
        super().__init__()
        self.in_body = False
        self.hidden = 0
        self.text = []
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        if tag == "body":
            self.in_body = True
        if tag in ("script", "style"):
            self.hidden += 1

    def handle_endtag(self, tag):
        if tag == "body":
            self.in_body = False
        if tag in ("script", "style"):
            self.hidden -= 1

    def handle_data(self, text):
        if self.in_body and not self.hidden and text.strip():
            self.text.append(text.strip())


@pytest.fixture
def renderer():
    return load_script("docs/collide_pe/render_brief.py")


@pytest.mark.parametrize(
    "problem, expected",
    [
        ({"solution": {}}, NOTE),
        ({"solution": {"answer": "Recalled answer", "solved_by": "Concept recall"}}, NOTE),
        ({"solution": {"answer": {"result": 1800},
                       "domain_function": "digitalmodel.production_engineering.ipr_models.LinearIpr",
                       "method": "Repository calculation", "steps": ["Recorded result"]}}, ""),
        ({"solution": {"answer": {"result": 1800}, "solved_by": "Model service"}}, NOTE),
        ({"solution": {"answer": "Pending solver run.",
                       "domain_function": "digitalmodel.production_engineering.ipr_models.LinearIpr"}}, NOTE),
        ({"solution": {"answer": {"result": 1800}, "domain_function": "external.example"}}, NOTE),
    ],
    ids=["pending", "recalled", "repository-computed", "service-only", "pending-function", "external-function"],
)
def test_problem_note_from_recorded_provenance(renderer, problem, expected):
    assert callable(getattr(renderer, "working_note_label", None))
    assert renderer.working_note_label(problem) == expected


def test_rendered_pending_note_is_first_visible_text(renderer, tmp_path, monkeypatch):
    problem = {"id": "practice", "title": "Practice answer", "solution": {}}
    source = tmp_path / "practice.yml"
    source.write_text(yaml.safe_dump(problem), encoding="utf-8")
    monkeypatch.setattr(renderer, "BRIEFS", tmp_path / "briefs")
    page = renderer.render_file(source)
    assert VisibleText(page.read_text(encoding="utf-8")).text[0] == NOTE


def test_rendered_note_rejects_template_without_marker(renderer, tmp_path, monkeypatch):
    template = tmp_path / "missing-marker.html"
    template.write_text("<html><body>Practice answer</body></html>\n", encoding="utf-8")
    monkeypatch.setattr(renderer, "TEMPLATE", template)
    with pytest.raises(ValueError, match="Working-note label marker is missing"):
        renderer.render({"solution": {}})


PROBLEMS = tuple(sorted((ROOT / "docs/collide_pe/problems").glob("pe-*.yml")))


@pytest.mark.parametrize("path", PROBLEMS, ids=lambda path: path.stem)
def test_committed_pending_brief_label(path):
    page = ROOT / "docs/collide_pe/briefs" / f"{path.stem}.html"
    text = page.read_text(encoding="utf-8")
    assert "Pending solver run" not in text or NOTE in text


@pytest.mark.parametrize("path", PROBLEMS, ids=lambda path: path.stem)
def test_committed_brief_matches_provenance_label(renderer, path):
    problem = yaml.safe_load(path.read_text(encoding="utf-8"))
    page = ROOT / "docs/collide_pe/briefs" / f"{path.stem}.html"
    texts = VisibleText(page.read_text(encoding="utf-8")).text
    label = renderer.working_note_label(problem)
    assert (texts[0] == NOTE) == bool(label)
