# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public wording, unchanged calculations and release integrity for four examples."""

import importlib
import json
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORTS = ROOT / "docs/domains/cathodic_protection/reports"
sys.path.insert(0, str(ROOT / "scripts/reporting"))


def builder():
    return importlib.import_module("build_cp_public_examples")


class VisibleText(HTMLParser):
    """Read rendered strings while excluding bundled scripts and styles."""

    def __init__(self):
        super().__init__()
        self.hidden = 0
        self.parts = []

    def handle_starttag(self, tag, attrs):
        if tag in {"script", "style"}:
            self.hidden += 1

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.hidden -= 1

    def handle_data(self, data):
        if not self.hidden:
            self.parts.append(data)


def test_repository_citation_target_is_metadata_only():
    from digitalmodel.citations import Citation, validate_citation

    records = json.loads((REPORTS / "cases/S02/report_citations.json").read_text(encoding="utf-8"))
    citation = Citation(**records["citations"][0])
    validate_citation(citation, repo_root=ROOT)
    page = (ROOT / "knowledge" / citation.wiki_path).read_text(encoding="utf-8")
    assert "status:" in page and "https://www.dnv.com/" in page
    assert "Table" not in page and "A/m" not in page


@pytest.mark.parametrize("case_id", ["S02", "S03", "S06"])
def test_rebuilt_examples_keep_results_and_bind_release(case_id, tmp_path):
    module = builder()
    folder = module.build_case(case_id, tmp_path)
    saved = json.loads((REPORTS / "cases" / case_id / "results.json").read_text(encoding="utf-8"))
    rebuilt = json.loads((folder / "results.json").read_text(encoding="utf-8"))
    module.compare_results(saved, rebuilt)
    source_folder = REPORTS / "cases" / case_id
    assert module.read_json(folder / "verification.json") == module.read_json(source_folder / "verification.json")
    release = module.read_json(folder / "release.json")
    assert release["input_fixture"] == module.CASES[case_id][1]
    assert release["data_basis"] == "synthetic data"
    module.verify_pack(folder)
    page = (folder / "report.html").read_text(encoding="utf-8")
    reader = VisibleText()
    reader.feed(page)
    text = " ".join(reader.parts)
    first_screen = page.split('<main class="main">')[0]
    assert "Worked example on synthetic data" in first_screen
    assert "internal review draft" not in first_screen.casefold()
    for phrase in ("client", "approved for", "Draft for review"):
        assert phrase.casefold() not in text.casefold()
    # The preceding issue row is retained verbatim as history, below the new summary.
    assert "Internal review draft" in text
    assert text.index("Executive Summary") < text.index("Revision history")
    assert "Criterion" in text and "NOT_EVALUATED" in text
    assert (folder / "changes.json").is_file()
    product = module.PIPELINE_PRODUCT if case_id == "S06" else module.PRODUCT
    assert product in page
    if case_id in {"S02", "S03"}:
        assert "FAIL" in text and "Final current output" in text
        assert "[4.8.1]" in text and "May 2021" in text
        assert "increase the arrangement" in text and "Required input" in text


def test_result_comparison_rejects_numeric_and_structural_changes():
    module = builder()
    module.compare_results({"x": [1, 2.0]}, {"x": [1.0, 2]})
    for changed in ({"x": [1, 2.01]}, {"x": [1]}, {"y": [1, 2]}, {"x": [True, 2]}):
        with pytest.raises(ValueError, match="results changed"):
            module.compare_results({"x": [1, 2]}, changed)


@pytest.mark.parametrize("missing", [None, float("nan"), float("inf"), "not applicable"])
def test_advice_fails_closed_on_invalid_final_demand(missing):
    module = builder()
    results = json.loads((REPORTS / "cases/S02/results.json").read_text(encoding="utf-8"))
    results["current_output_verification"]["final_current_demand_A"] = missing
    with pytest.raises(ValueError, match="NOT_EVALUATED"):
        module.b401_checks(results)


@pytest.mark.parametrize("key", ["anode_requirements", "current_output_verification", "status"])
def test_advice_fails_closed_on_missing_record(key):
    module = builder()
    results = json.loads((REPORTS / "cases/S02/results.json").read_text(encoding="utf-8"))
    results[key] = None
    with pytest.raises(ValueError, match="NOT_EVALUATED"):
        module.b401_checks(results)


@pytest.mark.parametrize("case_id, expected, added", [("S02", 48, 23), ("S03", 64, 36)])
def test_three_hand_calculation_anchors(case_id, expected, added):
    module = builder()
    results = json.loads((REPORTS / "cases" / case_id / "results.json").read_text(encoding="utf-8"))
    checks = module.b401_checks(results)
    # Hand calculation: 25 * 150 = 3750 kg; 28 * 80 = 2240 kg.
    assert checks[0]["provided"] == (3750.0 if case_id == "S02" else 2240.0)
    # Hand calculation: ceil(59.28 / 1.2489) = 48; ceil(46.24 / 0.7276) = 64.
    assert module.design_count(results) == expected
    # Hand calculation: 48 - 25 = 23; 64 - 28 = 36 additional anodes.
    assert module.design_count(results) - results["current_output_verification"]["anode_count"] == added


def test_reference_rebuild_keeps_all_saved_numerical_evidence(tmp_path):
    module = builder()
    folder = module.build_reference(tmp_path)
    old = json.loads((REPORTS / "reference-jacket/cathodic-protection-analysis.citations.json").read_text(encoding="utf-8"))
    new = json.loads((folder / "results.json").read_text(encoding="utf-8"))
    module.compare_results(module.reference_results(old), new)
    module.verify_pack(folder)
    page = (folder / "cathodic-protection-analysis.html").read_text(encoding="utf-8")
    assert "Worked example on published geometry" in page.split('<main class="main">')[0]
    assert "synthetic" in page and "CC BY 4.0" in page


def test_unrelated_case_is_rejected_before_writing(tmp_path):
    with pytest.raises(ValueError, match="scope"):
        builder().build_case("S01", tmp_path)
    assert not list(tmp_path.iterdir())


def test_install_rejects_symlink_before_any_output_changes(tmp_path, monkeypatch):
    module = builder()
    destination, candidate = tmp_path / "reports", tmp_path / "candidate"
    relative_folders = [Path("cases") / key for key in module.CASES] + [Path("reference-jacket")]
    external = tmp_path / "preserved.html"
    external.write_text("preserved\n", encoding="utf-8", newline="\n")
    for relative in relative_folders:
        for parent in (candidate, destination):
            folder = parent / relative
            folder.mkdir(parents=True)
            module.write_json(folder / "results.json", {"quantity": 1})
            (folder / "report.html").write_text("candidate\n", encoding="utf-8", newline="\n")
            module.write_receipt(folder)
    target = destination / "cases/S02/report.html"
    target.unlink()
    target.symlink_to(external)
    module.write_receipt(target.parent)
    module.write_json(destination / "portfolio-receipt.json", {})
    monkeypatch.setattr(module, "REPORTS", destination)
    with pytest.raises(ValueError, match="unsafe"):
        module.install(candidate)
    assert external.read_text(encoding="utf-8") == "preserved\n"
