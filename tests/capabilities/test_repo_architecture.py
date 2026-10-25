# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Architecture acceptance checks against repository evidence, without solvers.

Expected sets are independently enumerated from directories, card declarations,
and the existing page provenance registry. No engineering constants are used.
"""

import ast
import importlib.util
import json
import re
from pathlib import Path

import pytest

from scripts import check_generated_html as ownership

ROOT = Path(__file__).resolve().parents[2]
ARTIFACTS = ROOT / "docs/capability-map"
SPEC = importlib.util.spec_from_file_location(
    "architecture_cards", ROOT / "scripts/capabilities/build_onepagers.py"
)
CARDS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CARDS)


@pytest.fixture(autouse=True)
def legacy_default_encoding(monkeypatch):
    """Exercise evidence reads with the legacy Windows default encoding."""
    original = Path.read_text

    def read_text(path, encoding=None, errors=None):
        return original(path, encoding=encoding or "cp1252", errors=errors)

    monkeypatch.setattr(Path, "read_text", read_text)


@pytest.fixture(scope="module")
def inventory():
    return json.loads(
        (ARTIFACTS / "repo-architecture.json").read_text(encoding="utf-8")
    )


def test_every_observed_domain_page_is_inventoried(inventory):
    expected = {
        p.relative_to(ROOT).as_posix()
        for p in (ROOT / "docs/api").rglob("*.html")
        if p.relative_to(ROOT / "docs/api").parts[0] not in {"_assets", "capabilities"}
    }
    rows = inventory["pages"]
    assert {row["path"] for row in rows} == expected
    assert len({row["path"] for row in rows}) == len(rows)
    for row in rows:
        assert row["group"] == Path(row["path"]).parts[2]


def test_all_capability_declarations_are_preserved(inventory):
    expected = {c["id"] for c in CARDS.SPECS if c["kind"] == "work"}
    actual = [c["id"] for c in inventory["capabilities"]]
    assert set(actual) == expected
    assert len(actual) == len(expected)
    for row in inventory["capabilities"]:
        source = next(c for c in CARDS.SPECS if c["id"] == row["id"])
        target = source["path"]
        if not target.startswith("https:"):
            target = "docs/api/" + target
        else:
            target = target.split("/main/", 1)[1]
        resolved = target.rstrip("/")
        if (ROOT / resolved).is_dir() and resolved.startswith("docs/api/"):
            resolved += "/index.html"
        assert row["target"] == resolved
        assert row["exists"] == (ROOT / resolved).exists()


def test_retired_wrapper_census_remains_truthful(inventory):
    wrappers = sorted(
        p.relative_to(ROOT).as_posix()
        for p in (ROOT / "docs/api/capabilities/api").glob("*.html")
    )
    assert inventory["wrapper_pages"] == wrappers
    assert inventory["wrapper_status"] == ("present" if wrappers else "absent")


def test_provenance_agrees_with_existing_registry(inventory):
    generators = {
        output: generator
        for generator in ownership.GENERATORS
        for output in generator.outputs
    }
    for row in inventory["pages"]:
        path = row["path"]
        if path in generators:
            generator = generators[path]
            assert row["producer"] == generator.script
            assert row["prerequisites"] == list(generator.prerequisites)
            assert row["provenance"] == "registered generator"
            assert (
                row["limitation"] == "Upstream analysis not executed by this inventory."
            )
        elif path == "docs/api/cfd/kcs-calm-water-resistance-verification.html":
            assert row["provenance"] == "external input or non-reproducible copy"
            assert row["limitation"] == ownership.MANUAL_PAGES[path]
        elif path in ownership.PAGE_EXCLUSIONS:
            assert row["provenance"] == "external input or non-reproducible copy"
            assert row["limitation"] == ownership.PAGE_EXCLUSIONS[path]
            assert row["producer"] is None
        elif path in ownership.MANUAL_PAGES:
            assert row["provenance"] == "inline, no generator"
            assert row["limitation"] == ownership.MANUAL_PAGES[path]
        else:
            pytest.fail(f"Unclassified page: {path}")


def test_evidence_paths_and_direct_calculation_imports_exist(inventory):
    for row in inventory["pages"]:
        for path in row["evidence"] + row["calculations"]:
            assert (ROOT / path).is_file(), path
        if row["producer"]:
            assert (ROOT / row["producer"]).is_file()
        for artifact in row["artifacts"]:
            assert artifact["state"] in {"committed", "generated prerequisite"}
            if artifact["state"] == "committed":
                assert (ROOT / artifact["path"]).is_file()
            assert artifact["writer"] in [row["producer"], *row["prerequisites"]]
            text = (ROOT / artifact["writer"]).read_text(encoding="utf-8")
            name = Path(artifact["path"]).name
            suffix_derived = (
                Path(name).with_suffix(".html").name in text
                and 'with_suffix(".json")' in text
            )
            assert name in text or suffix_derived


def test_execution_boundaries_do_not_confuse_screening_with_licensed_runs(inventory):
    rows = {r["path"]: r for r in inventory["pages"]}
    assert (
        rows["docs/api/drilling/drilling-riser-operability-explorer.html"]["execution"]
        == "analytical screening; no licensed solve"
    )
    for path in ("riser-mesh-sensitivity.html", "riser-validation-report.html"):
        assert "OrcaFlex" in rows["docs/api/orcaflex/" + path]["execution"]
    for row in inventory["pages"]:
        assert row["execution"]
        assert row["execution_evidence"]
        assert (ROOT / row["execution_evidence"]).is_file()


def test_one_diagram_covers_every_page_and_capability(inventory):
    drawing = (ARTIFACTS / "repo-architecture.mmd").read_text(encoding="utf-8")
    assert drawing.startswith("flowchart LR\n")
    for row in inventory["pages"]:
        assert f'{row["node"]}["' in drawing
        assert row["path"] in drawing
        assert f"{row['node']} --> pages" in drawing
        assert row["provenance"] in drawing
    for row in inventory["capabilities"]:
        assert row["id"] in drawing
        if row["page_node"]:
            assert f"{row['node']} --> {row['page_node']}" in drawing
        else:
            assert row["target"] in drawing
    for row in inventory["pages"]:
        if row["producer"]:
            assert row["producer"] in drawing
        for artifact in row["artifacts"]:
            assert artifact["path"] in drawing
    assert "OrcaFlex" in drawing and "OrcaWave" in drawing and "AQWA" in drawing
    assert "Hugging Face" in drawing and "aceengineer.com" in drawing


def test_readme_embeds_exact_committed_mermaid_source():
    drawing = (ARTIFACTS / "repo-architecture.mmd").read_text(encoding="utf-8")
    readme = (ROOT / "README.md").read_text(encoding="utf-8")
    assert readme.count("<!-- repo-architecture:start -->") == 1
    block = readme.split("<!-- repo-architecture:start -->")[1].split(
        "<!-- repo-architecture:end -->"
    )[0]
    assert block.count("```mermaid") == 1
    assert f"```mermaid\n{drawing}```" in block


def test_publication_is_evidence_limited(inventory):
    publication = inventory["publication"]
    assert publication["pages"]["evidence"] == ".github/workflows/pages.yml"
    assert publication["pages"]["root_evidence"] == "mkdocs.yml"
    assert (
        publication["hugging_face"]["evidence"]
        == "scripts/hf/build_capability_datasets.py"
    )
    assert publication["website"]["evidence"] == "docs/api/capabilities/index.html"
    for row in publication.values():
        assert row["remote_status"] == "NOT_EVALUATED"
    assert "NOT_EVALUATED" in (ARTIFACTS / "repo-architecture.html").read_text(
        encoding="utf-8"
    )


def test_documentation_has_worked_example_and_revision_history():
    report = (ARTIFACTS / "repo-architecture.html").read_text(encoding="utf-8")
    assert (
        report.index("Summary")
        < report.index("Revision history")
        < report.index("Method")
    )
    assert "Worked example" in report
    assert "wall-thickness-explorer.json" in report
    assert "No engineering equations" in report
    assert "repo-architecture.mmd" in report


def test_diagram_node_identifiers_are_unique():
    """Independent graph check: one declaration per id, all edges resolve.

    For example, declarations a,b,c and edges a→b,b→c yield 3 distinct nodes
    and zero missing endpoints by set subtraction; no rendering engine is used.
    """
    drawing = (ARTIFACTS / "repo-architecture.mmd").read_text(encoding="utf-8")
    declarations = re.findall(r'^\s*(\w+)\["', drawing, re.M)
    assert len(declarations) == len(set(declarations))
    edges = re.findall(r"^\s*(\w+) (?:-->|-\.->) (\w+)\s*$", drawing, re.M)
    assert edges
    assert {node for edge in edges for node in edge} <= set(declarations)


def test_direct_import_pointers_match_current_producer_source(inventory):
    for row in inventory["pages"]:
        if not row["producer"]:
            continue
        tree = ast.parse((ROOT / row["producer"]).read_text(encoding="utf-8"))
        modules = {
            node.module
            for node in ast.walk(tree)
            if isinstance(node, ast.ImportFrom) and node.module
        }
        for path in row["calculations"]:
            module = path.removeprefix("src/").removesuffix(".py").replace("/", ".")
            module = module.removesuffix(".__init__")
            assert module in modules


def test_drawing_preserves_sibling_outputs_and_generated_prerequisite(inventory):
    drawing = (ARTIFACTS / "repo-architecture.mmd").read_text(encoding="utf-8")
    for row in inventory["pages"]:
        for index, artifact in enumerate(row["artifacts"]):
            node = f"a{row['node']}_{index}"
            if artifact["state"] == "generated prerequisite":
                assert f"{node} --> s{row['node']}" in drawing
            else:
                assert f"s{row['node']} --> {node}" in drawing
                assert f"{node} --> {row['node']}\n" not in drawing


def test_every_implemented_dataset_projection_has_a_source_edge(inventory):
    """The independent expected inputs are literal _load arguments in each
    dataset row function, including the generated full-resolution study.
    """
    source = ROOT / "scripts/hf/build_capability_datasets.py"
    tree = ast.parse(source.read_text(encoding="utf-8"))
    inputs = {
        "docs/api/" + call.args[0].value
        for call in ast.walk(tree)
        if isinstance(call, ast.Call)
        and isinstance(call.func, ast.Name)
        and call.func.id == "_load"
        and call.args
        and isinstance(call.args[0], ast.Constant)
    }
    artifacts = {
        artifact["path"]: f"a{row['node']}_{index}"
        for row in inventory["pages"]
        for index, artifact in enumerate(row["artifacts"])
    }
    assert inputs <= artifacts.keys()
    drawing = (ARTIFACTS / "repo-architecture.mmd").read_text(encoding="utf-8")
    for path in inputs:
        assert f"{artifacts[path]} --> tidy" in drawing


def test_registered_inputs_remain_visible_before_page_generation(inventory):
    drawing = (ARTIFACTS / "repo-architecture.mmd").read_text(encoding="utf-8")
    for generator in ownership.GENERATORS:
        expected = set(generator.inputs)
        expected.update(arg for arg in generator.arguments if (ROOT / arg).is_file())
        for row in inventory["pages"]:
            if row["producer"] != generator.script:
                continue
            assert set(row["inputs"]) == expected
            for index, path in enumerate(row["inputs"]):
                assert (ROOT / path).is_file()
                assert path in drawing
                assert f"i{row['node']}_{index} --> s{row['node']}" in drawing
