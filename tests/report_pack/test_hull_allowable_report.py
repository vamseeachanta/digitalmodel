# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Evidence qualification and reproducibility of the hull-girder report."""

import copy
import hashlib
import importlib.util
import json
import shutil
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
DEMO = ROOT / "examples/workflows/hull-girder-report-pack"


def builder():
    spec = importlib.util.spec_from_file_location(
        "hull_allowable_report", DEMO / "build_report.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_every_strength_input_has_explicit_qualification_and_sign_coverage():
    module = builder()
    config = yaml.safe_load((DEMO / "screening_input.yml").read_text())
    basis = json.loads((DEMO / "allowable-basis.json").read_text())
    rows = module.allowable_inventory(config["hull_girder_screening"], basis)
    assert len(rows) == 8
    assert all(row["label"] == "not established" for row in rows)
    curves = [row for row in rows if row["key"].startswith("allowables.")]
    assert len(curves) == 4
    assert all(row["signs"] == ["positive", "negative"] for row in curves)
    assert all(row["negative_inherits_positive"] for row in curves)
    assert all(row["reason"] and row["required_evidence"] for row in rows)
    assert basis["sources"]["iacs_catalogue"]["http_status"] == 200
    assert basis["sources"]["iacs_terms"]["http_status"] == 200
    assert {r["label"] for r in rows} <= {"cited", "derived", "assumed", "not established"}


def test_missing_extra_or_falsely_sourced_strength_inputs_are_rejected():
    module = builder()
    settings = yaml.safe_load((DEMO / "screening_input.yml").read_text())[
        "hull_girder_screening"
    ]
    basis = json.loads((DEMO / "allowable-basis.json").read_text())
    missing = copy.deepcopy(basis)
    missing["allowables"].pop()
    with pytest.raises(ValueError, match="inventory"):
        module.allowable_inventory(settings, missing)
    extra = copy.deepcopy(settings)
    extra["allowables"]["additional_strength_limit"] = {"value": 1}
    with pytest.raises(ValueError, match="inventory"):
        module.allowable_inventory(extra, basis)
    forged = copy.deepcopy(basis)
    forged["allowables"][0]["label"] = "sourced"
    with pytest.raises(ValueError, match="qualification"):
        module.allowable_inventory(settings, forged)


def test_unsourced_strength_cannot_produce_a_strength_acceptance(tmp_path):
    module = builder()
    module.build(tmp_path)
    results = json.loads((tmp_path / "results.json").read_text())
    assert results["qualified_allowables"] == 0
    assert results["strength_disposition"] == "not evaluated"
    assert results["finite_element_performed"] is False
    assert len(results["frames"]) == 5
    assert all(r["allowable"] is None and r["utilization"] is None
               for r in results["frames"])
    assert results["max_sagging_t_m"] == pytest.approx(-62500, rel=1e-3)
    assert abs(results["max_shear_t"]) == pytest.approx(2500, rel=1e-2)
    assert results["closure_ok"] is True
    assert all(r["satisfies_criterion"] for r in results["analytical_comparisons"])


def test_report_is_reproducible_and_all_manifest_digests_match(tmp_path):
    module = builder()
    first, second = tmp_path / "first", tmp_path / "second"
    module.build(first)
    module.build(second)
    assert sorted(p.name for p in first.iterdir()) == [
        "manifest.json", "report.html", "results.json"
    ]
    for file in first.iterdir():
        assert file.read_bytes() == (second / file.name).read_bytes()
    manifest = json.loads((first / "manifest.json").read_text())
    for name, record in manifest["outputs"].items():
        data = (first / name).read_bytes()
        assert record == {"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()}
    for name, record in manifest["inputs"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == record["sha256"]
    module.verify_manifest(first)
    committed = ROOT / "docs/domains/ship-design/hull-girder-report"
    module.verify_manifest(committed)
    for file in first.iterdir():
        assert file.read_bytes() == (committed / file.name).read_bytes()
    assert "pyproject.toml" not in manifest["inputs"]
    assert "tests/report_pack/test_hull_allowable_report.py" not in manifest["inputs"]
    (first / "report.html").write_text("changed\n")
    with pytest.raises(ValueError, match="digest"):
        module.verify_manifest(first)


def test_revision_record_is_rendered_directly_after_summary(tmp_path):
    module = builder()
    module.build(tmp_path)
    text = (tmp_path / "report.html").read_text()
    record = json.loads((DEMO / "revisions.json").read_text())
    assert record["history"][-1]["revision"] == record["revision"]
    assert f'<meta name="report-revision" content="{record["revision"]}">' in text
    assert text.index("1 Executive Summary") < text.index("Revision history")
    assert text.index("Revision history") < text.index("2 Introduction")
    assert text.count('data-revision="') == len(record["history"])
    for row in record["history"]:
        assert f'data-revision="{row["revision"]}"' in text
        assert row["date"] in text and row["change"] in text
    invalid = copy.deepcopy(record)
    invalid["revision"] = "unrecorded"
    path = tmp_path / "invalid-revisions.json"
    path.write_text(json.dumps(invalid))
    with pytest.raises(ValueError, match="revision"):
        module.build(tmp_path / "invalid", revisions_path=path)


def test_report_contains_no_external_resources_or_unqualified_strength_verdict(tmp_path):
    class Page(HTMLParser):
        resources = []
        headings = []

        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if tag in {"script", "link", "img", "iframe", "object", "embed",
                       "audio", "video", "source"}:
                self.resources.append((tag, attrs))
            if tag == "section":
                self.headings.append(attrs.get("id"))

    module = builder()
    module.build(tmp_path)
    text = (tmp_path / "report.html").read_text()
    page = Page()
    page.feed(text)
    assert page.resources == []
    assert page.headings[:3] == ["summary", "revision-history", "introduction"]
    assert len([h for h in page.headings if h != "revision-history"]) == 11
    assert "not evaluated" in text
    assert "based on previous experience" not in text
    assert "synthetic textbook" in text
    assert "assumed" not in text.lower()
    assert "Internal still-water demand analysis" not in text
    assert "This internal analysis" not in text
    for word in ["permissible_mpa", "Prepared by", "Checked by", "Approved by"]:
        assert word not in text
    assert '<svg ' in text and '<caption>' in text


def test_changed_loading_geometry_cannot_reuse_fixed_fixture_validation(tmp_path):
    module = builder()
    config = yaml.safe_load((DEMO / "screening_input.yml").read_text())
    config["hull_girder_screening"]["weights"]["lightship"][0]["x_start_m"] = 10
    path = tmp_path / "changed-input.yml"
    path.write_text(yaml.safe_dump(config))
    with pytest.raises(ValueError, match="fixture"):
        module.build(tmp_path / "invalid", settings_path=path)


def test_alternate_input_cannot_be_reported_as_the_canonical_digest(tmp_path):
    module = builder()
    basis = json.loads((DEMO / "allowable-basis.json").read_text())
    basis["allowables"][0]["reason"] = "Different evidence remains unqualified."
    alternative = tmp_path / "alternative-basis.json"
    alternative.write_text(json.dumps(basis))
    with pytest.raises(ValueError, match="input digest"):
        module.build(tmp_path / "output", basis_path=alternative)
    assert not (tmp_path / "output").exists()


def test_output_symlink_cannot_overwrite_a_caller_file(tmp_path):
    module = builder()
    caller = tmp_path / "caller.txt"
    caller.write_text("preserve these bytes\n")
    before = hashlib.sha256(caller.read_bytes()).hexdigest()
    output = tmp_path / "output"
    output.mkdir()
    (output / "report.html").symlink_to(caller)
    with pytest.raises(ValueError, match="output"):
        module.build(output)
    assert hashlib.sha256(caller.read_bytes()).hexdigest() == before


def test_manifest_ignores_unrelated_metadata_but_checks_version(tmp_path, monkeypatch):
    module = builder()
    output = tmp_path / "output"
    module.build(output)
    isolated = tmp_path / "repository"
    for name in module.INPUTS:
        target = isolated / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / name, target)
    metadata = isolated / "pyproject.toml"
    original = (ROOT / "pyproject.toml").read_text()
    metadata.write_text(original + "\n# Unrelated metadata change.\n")
    monkeypatch.setattr(module, "ROOT", isolated)
    module.verify_manifest(output)
    version = json.loads((output / "manifest.json").read_text())["project_version"]
    metadata.write_text('[project]\nversion = "' + version + '.changed"\n')
    with pytest.raises(ValueError, match="version"):
        module.verify_manifest(output)


@pytest.mark.parametrize("label", ["cited", "derived", "assumed", "invalid"])
def test_missing_evidence_cannot_be_relabelled(label):
    module = builder()
    settings = yaml.safe_load((DEMO / "screening_input.yml").read_text())[
        "hull_girder_screening"
    ]
    basis = json.loads((DEMO / "allowable-basis.json").read_text())
    basis["allowables"][0]["label"] = label
    with pytest.raises(ValueError, match="qualification"):
        module.allowable_inventory(settings, basis)
