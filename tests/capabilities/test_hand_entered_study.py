# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand-entry disclosure in the committed and assembled study page."""

from tests.capabilities.test_working_note_briefs import ROOT, VisibleText, load_script

STUDY_NOTE = (
    "Values in this study are entered by hand from the cited source; "
    "they are not computed by this repository"
)


def assert_study_note(text):
    texts = VisibleText(text).text
    title = texts.index("Rod-pump surface-card analysis")
    assert texts[title + 1] == STUDY_NOTE


def test_committed_study_note_follows_title():
    page = ROOT / "docs/collide_pe/dynacard-undulations/dynacard-rod-pump-report.html"
    assert_study_note(page.read_text(encoding="utf-8"))


def test_assembled_study_note_follows_title(tmp_path):
    builder = load_script("docs/collide_pe/dynacard-undulations/build_report.py")
    page = tmp_path / "study.html"
    builder.report.write(page)
    assert_study_note(page.read_text(encoding="utf-8"))
