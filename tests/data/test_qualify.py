# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Disposable qualification cases with independently specified dependency edges.

No solver runs. Expected dispositions follow the literal fixture references:
an input or its sidecar is retained; a proven write is an output; a missing
or dynamic dependency prevents a removal decision. Counts are fixture sizes,
not engineering acceptance limits.
"""
import importlib.util
import json
from pathlib import Path
import zipfile

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/data/qualify.py"


@pytest.fixture
def qualifier():
    spec = importlib.util.spec_from_file_location("qualify", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def put(root, name, text="fixture"):
    target = root / name
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(text, encoding="utf-8", newline="\n")
    return target


@pytest.mark.parametrize("name,text,kind", [
    ("docs/run.py", "print('example')", "executable"),
    ("docs/run", "#!/bin/sh\necho example", "executable"),
    ("docs/input.yml", "model: sample", "configuration"),
    ("docs/map.qgz", "fixture", "configuration"),
    ("docs/page.md", "example", "documentation"),
])
def test_content_precedes_documentation_directory(qualifier, tmp_path, name, text, kind):
    put(tmp_path, name, text)
    row = qualifier.qualify(tmp_path, [name], [])[0]
    assert row["kind"] == kind
    if kind != "documentation":
        assert row["disposition"] == "RETAIN_REQUIRED"


@pytest.mark.parametrize("reference", ["models/sample", "models/{case}",
                                       "models/$CASE", "models/", "models/*.dat"])
def test_extensionless_parameterized_and_directory_consumers(qualifier, tmp_path, reference):
    put(tmp_path, "models/sample.dat")
    put(tmp_path, "run.sh", f'solver "{reference}"\n')
    row = qualifier.qualify(tmp_path, ["models/sample.dat"], ["run.sh"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"
    assert row["consumers"] == ["run.sh"]


def test_qgz_datasource_retains_dataset_components(qualifier, tmp_path):
    """QGIS 3.40 file-format docs: QGZ wraps QGS XML, not inline layer bytes.

    The XML names coast.shp; all coast.* siblings are conservatively retained,
    including its DBF, SHX, PRJ and compound SHP.XML sidecar. Other stems differ.
    """
    names = [f"gis/coast.{ext}" for ext in ("shp", "dbf", "shx", "prj", "shp.xml")]
    for name in names + ["gis/other.dbf"]:
        put(tmp_path, name)
    with zipfile.ZipFile(tmp_path / "gis/map.qgz", "w") as archive:
        archive.writestr("map.qgs", '<qgis><datasource>./coast.shp|layerid=0</datasource></qgis>')
    rows = qualifier.qualify(tmp_path, names + ["gis/other.dbf"], ["gis/map.qgz"])
    assert all(row["disposition"] == "RETAIN_REQUIRED" for row in rows[:-1])
    assert rows[-1]["disposition"] == "REVIEW_REQUIRED"


def test_csv_data_flow_separates_inputs_from_outputs(qualifier, tmp_path):
    """Literal flow: read_csv(source) reads raw.csv; to_csv(target) writes out.csv.

    No expected value comes from executing the generator or qualification code.
    """
    put(tmp_path, "docs/raw.csv")
    put(tmp_path, "docs/out.csv")
    put(tmp_path, "docs/generate.py", 'from pathlib import Path\n'
        'source = Path("raw.csv")\ntarget = Path("out.csv")\n'
        'frame = pd.read_csv(source)\nframe.to_csv(target)\n')
    rows = qualifier.qualify(tmp_path, ["docs/raw.csv", "docs/out.csv"], ["docs/generate.py"])
    assert rows[0]["disposition"] == "RETAIN_REQUIRED"
    assert rows[0]["role"] == "input"
    assert rows[1]["role"] == "generated-output"
    assert rows[1]["disposition"] == "REVIEW_REQUIRED"


def test_output_read_by_another_consumer_stays_required(qualifier, tmp_path):
    put(tmp_path, "docs/out.csv")
    put(tmp_path, "docs/generate.py", 'df.to_csv("out.csv")\n')
    put(tmp_path, "docs/read.py", 'df = pd.read_csv("out.csv")\n')
    row = qualifier.qualify(tmp_path, ["docs/out.csv"], ["docs/generate.py", "docs/read.py"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"
    assert row["role"] == "input"


@pytest.mark.parametrize("method,mode", [
    ("to_csv", "a"), ("to_csv", "a+"), ("to_csv", "r+"),
    ("to_csv", "w+"), ("to_json", "a"), ("open", "a"),
])
def test_append_and_update_destinations_are_inputs(qualifier, tmp_path, method, mode):
    put(tmp_path, "asset.csv")
    call = "open" if method == "open" else f"frame.{method}"
    put(tmp_path, "run.py", f'{call}("asset.csv", mode="{mode}")\n')
    row = qualifier.qualify(tmp_path, ["asset.csv"], ["run.py"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"
    assert row["role"] == "input"
    assert row["consumers"] == ["run.py"]
    assert not row["unresolved"]


@pytest.mark.parametrize("source", [
    'frame.to_csv("asset.csv", mode=unknown_mode)\n',
    'frame.to_json("asset.csv", mode="invalid")\n',
    'frame.to_csv("asset.csv", **options)\n',
    'open("asset.csv", mode=unknown_mode)\n',
])
def test_unknown_write_modes_remain_unresolved(qualifier, tmp_path, source):
    put(tmp_path, "asset.csv")
    put(tmp_path, "other.csv")
    put(tmp_path, "run.py", source)
    rows = qualifier.qualify(tmp_path, ["asset.csv", "other.csv"], ["run.py"])
    assert rows[0]["disposition"] == "RETAIN_REQUIRED"
    assert rows[0]["unresolved"]
    assert rows[1]["disposition"] == "RETAIN_UNRESOLVED"


@pytest.mark.parametrize("mode,disposition", [("a", "RETAIN_REQUIRED"),
                                               ("w", "REVIEW_REQUIRED")])
def test_bound_write_mode_is_resolved(qualifier, tmp_path, mode, disposition):
    put(tmp_path, "asset.csv")
    put(tmp_path, "run.py", f'mode = "{mode}"\nframe.to_csv("asset.csv", mode=mode)\n')
    row = qualifier.qualify(tmp_path, ["asset.csv"], ["run.py"])[0]
    assert row["disposition"] == disposition
    assert not row["unresolved"]


def test_annotated_reassignment_replaces_stale_path(qualifier, tmp_path):
    put(tmp_path, "unused.csv")
    put(tmp_path, "asset.csv")
    put(tmp_path, "run.py", 'p = "unused.csv"; p: str = "asset.csv"; pd.read_csv(p)\n')
    rows = qualifier.qualify(tmp_path, ["unused.csv", "asset.csv"], ["run.py"])
    assert rows[0]["disposition"] == "REVIEW_REQUIRED"
    assert rows[1]["disposition"] == "RETAIN_REQUIRED"
    assert rows[1]["consumers"] == ["run.py"]
    assert not rows[1]["unresolved"]


def test_dynamic_annotated_reassignment_invalidates_stale_path(qualifier, tmp_path):
    put(tmp_path, "unused.csv")
    put(tmp_path, "asset.csv")
    put(tmp_path, "run.py", 'p = "unused.csv"; p: str = resolve_path(); pd.read_csv(p)\n')
    rows = qualifier.qualify(tmp_path, ["unused.csv", "asset.csv"], ["run.py"])
    assert all(row["disposition"] == "RETAIN_UNRESOLVED" for row in rows)
    assert all(row["unresolved"] for row in rows)


def test_annotation_without_assignment_preserves_path(qualifier, tmp_path):
    put(tmp_path, "asset.csv")
    put(tmp_path, "run.py", 'p = "asset.csv"; p: str; pd.read_csv(p)\n')
    row = qualifier.qualify(tmp_path, ["asset.csv"], ["run.py"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"
    assert not row["unresolved"]


def test_utf8_bom_consumer_retains_input(qualifier, tmp_path):
    put(tmp_path, "asset.csv")
    put(tmp_path, "run.py", '\ufeffpd.read_csv("asset.csv")\n')
    row = qualifier.qualify(tmp_path, ["asset.csv"], ["run.py"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"
    assert not row["unresolved"]


@pytest.mark.parametrize("name,text", [
    ("run.py", "df = pd.read_csv(resolve_input())"),
    ("run.py", "invalid python :"),
    ("run.yml", "input: missing.dat"),
    ("run.qgz", "not a zip archive"),
])
def test_unknown_dependencies_retain_candidates(qualifier, tmp_path, name, text):
    put(tmp_path, "docs/asset.csv")
    put(tmp_path, name, text)
    row = qualifier.qualify(tmp_path, ["docs/asset.csv"], [name])[0]
    assert row["disposition"] == "RETAIN_UNRESOLVED"
    assert row["unresolved"]


def test_empty_consumer_scope_is_explicit(qualifier, tmp_path):
    put(tmp_path, "docs/page.md")
    row = qualifier.qualify(tmp_path, ["docs/page.md"], [])[0]
    assert row["disposition"] == "RETAIN_UNRESOLVED"
    assert row["unresolved"] == ["Consumer scope is empty"]


def test_cli_reports_every_candidate_without_modifying_it(qualifier, tmp_path, capsys):
    put(tmp_path, "docs/page.md")
    put(tmp_path, "run.sh", "echo example\n")
    assert qualifier.main(["--root", str(tmp_path), "--candidate", "docs/page.md",
                           "--consumer", "run.sh"]) == 0
    rows = json.loads(capsys.readouterr().out)
    assert [r["path"] for r in rows] == ["docs/page.md"]
    assert rows[0]["disposition"] == "REVIEW_REQUIRED"
    assert (tmp_path / "docs/page.md").read_text(encoding="utf-8") == "fixture"


@pytest.mark.parametrize("text", [
    'solver "sample"\n',
    'solver --model=sample\n',
])
def test_local_extensionless_shell_model(qualifier, tmp_path, text):
    put(tmp_path, "docs/sample.dat")
    put(tmp_path, "docs/run.sh", text)
    row = qualifier.qualify(tmp_path, ["docs/sample.dat"], ["docs/run.sh"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"


def test_local_extensionless_python_model(qualifier, tmp_path):
    put(tmp_path, "docs/sample.dat")
    put(tmp_path, "docs/run.py", 'model.LoadData("sample")\n')
    row = qualifier.qualify(tmp_path, ["docs/sample.dat"], ["docs/run.py"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"


def test_unknown_generic_call_retains_candidate(qualifier, tmp_path):
    put(tmp_path, "docs/sample.dat")
    put(tmp_path, "docs/run.py", 'load(resolve_input())\n')
    row = qualifier.qualify(tmp_path, ["docs/sample.dat"], ["docs/run.py"])[0]
    assert row["disposition"] == "RETAIN_UNRESOLVED"


def test_compound_sidecar_without_shape_candidate(qualifier, tmp_path):
    put(tmp_path, "gis/coast.shp")
    put(tmp_path, "gis/coast.shp.xml")
    put(tmp_path, "gis/map.qgs", '<qgis><datasource>coast.shp</datasource></qgis>')
    row = qualifier.qualify(tmp_path, ["gis/coast.shp.xml"], ["gis/map.qgs"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"


def test_documented_example(capsys, monkeypatch):
    """The expected edges are literal read/write statements in the example."""
    root = SCRIPT.parents[2]
    monkeypatch.chdir(root)
    text = (root / "docs/data/content-qualification.md").read_text(encoding="utf-8")
    code = text.split("```python\n", maxsplit=1)[1].split("```", maxsplit=1)[0]
    expected = text.split("```text\n", maxsplit=1)[1].split("```", maxsplit=1)[0]
    exec(compile(code, "documented-example", "exec"), {})
    assert capsys.readouterr().out == expected


@pytest.mark.parametrize("source", [
    'source = "models/" + "sample.dat"\nmodel.LoadData(source)\n',
    'source = Path("models") / f"{case}.dat"\nmodel.LoadData(source)\n',
])
def test_constructed_python_paths(qualifier, tmp_path, source):
    put(tmp_path, "models/sample.dat")
    put(tmp_path, "run.py", source)
    row = qualifier.qualify(tmp_path, ["models/sample.dat"], ["run.py"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"


def test_control_flow_does_not_prove_output_only(qualifier, tmp_path):
    put(tmp_path, "out.csv")
    put(tmp_path, "run.py", 'if condition:\n    path = "out.csv"\ndf.to_csv(path)\n')
    row = qualifier.qualify(tmp_path, ["out.csv"], ["run.py"])[0]
    assert row["disposition"] == "RETAIN_UNRESOLVED"


def test_focused_ci_versions_match_project_test_constraints():
    """The repository's test extra, not a separately chosen pin, is the oracle."""
    import tomllib

    root = SCRIPT.parents[2]
    project = tomllib.loads((root / "pyproject.toml").read_text(encoding="utf-8"))
    dependencies = project["project"]["optional-dependencies"]["test"]
    workflow = (root / ".github/workflows/store-data-tests.yml").read_text(encoding="utf-8")
    for name in ("pytest", "pytest-asyncio"):
        constraint = next(dep for dep in dependencies
                          if dep.startswith(name + ">") or dep.startswith(name + "="))
        assert f"'{constraint}'" in workflow


def test_current_directory_consumer_preserves_tree(qualifier, tmp_path):
    put(tmp_path, "docs/asset.csv")
    put(tmp_path, "run.sh", 'scanner "."\n')
    row = qualifier.qualify(tmp_path, ["docs/asset.csv"], ["run.sh"])[0]
    assert row["disposition"] == "RETAIN_REQUIRED"


def test_unknown_receiver_call_is_unresolved(qualifier, tmp_path):
    put(tmp_path, "asset.csv")
    put(tmp_path, "run.py", "reader.load()\n")
    row = qualifier.qualify(tmp_path, ["asset.csv"], ["run.py"])[0]
    assert row["disposition"] == "RETAIN_UNRESOLVED"
