"""Section 9: wrapper source and referenced constants are part of the basis."""
from pathlib import Path
import os
import subprocess

import pytest
import yaml

from digitalmodel.parametric import refresh


@pytest.mark.parametrize("basename, atlas_id", [
    ("anchor_capacity", "99065b072fad"),
    ("code_check", "19b5ee839115"),
    ("esp_pump_hydraulics", "8a1b02cd3a84"),
    ("fpso_mooring_full", "025638a3b525"),
    ("free_span", "d42fe3756340"),
    ("inspection_planning", "21c3aff58a82"),
    ("lifting_lug", "c223190a77d6"),
    ("mooring_fatigue", "56461296c27d"),
    ("pile_capacity", "d50b356d2a2d"),
    ("rao_tabulation", "7bfb648a16ac"),
    ("riser_fatigue", "843f439a67d3"),
    ("span_rectification", "95183104ef3e"),
    ("synthetic_rope_mooring_fatigue", "59b7de8abc08"),
    ("weather_window", "3387e71f659b"),
    ("viv_analysis", "dd607056d686"),
    ("viv_analysis", None),
])
def test_code_locator_reproduces_recorded_fingerprint(
    tmp_path, monkeypatch, basename, atlas_id,
):
    """Reconstruct file inputs from a revision retained in main history.

    Registry spec, input and response sources are read from the revision;
    declared standards come from the manifest. The fingerprint algorithm,
    source-file list and RESPONSE_FUNCS use the working tree.
    This check requires Git history; it does not qualify a source archive.
    """
    from digitalmodel.parametric.atlas import Atlas

    atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, basename, atlas_id)
    revision = atlas.provenance["code_version"]
    git_env = {key: value for key, value in os.environ.items()
               if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    for reference in ("HEAD", "origin/main"):
        ancestry = subprocess.run(
            ["git", "merge-base", "--is-ancestor", revision, reference],
            cwd=refresh.REPO_ROOT, capture_output=True, text=True, env=git_env,
        )
        assert ancestry.returncode == 0, (
            f"recorded code revision must be an ancestor of {reference}; "
            "use a revision retained in main history and fetch its history"
        )
    workflow_id = atlas.provenance["workflow_id"]
    registry = yaml.safe_load(subprocess.check_output(
        ["git", "show", f"{revision}:docs/registry/workflows.yaml"],
        cwd=refresh.REPO_ROOT, env=git_env,
    ))
    row = next(row for row in registry["workflows"] if row["id"] == workflow_id)
    monkeypatch.setattr(refresh, "_row", lambda _: row)
    monkeypatch.setattr(refresh, "STANDARDS", {basename: atlas.provenance["standards"]})
    paths = [row["input"], *refresh.SOURCE_FILES[atlas.basename],
             "src/digitalmodel/parametric/generate.py"]
    for relative in paths:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(subprocess.check_output(
            ["git", "show", f"{revision}:{relative}"], cwd=refresh.REPO_ROOT,
            env=git_env,
        ))
    assert refresh.content_fingerprint(workflow_id, tmp_path) == (
        atlas.provenance["content_fingerprint"]
    ), "recorded code revision does not reproduce the atlas response basis"


def test_wrapper_edit_invalidates_only_its_atlas(tmp_path, monkeypatch):
    source = Path(refresh.REPO_ROOT) / "src/digitalmodel/parametric/generate.py"
    target = tmp_path / "src/digitalmodel/parametric/generate.py"
    target.parent.mkdir(parents=True)
    original = source.read_text(encoding="utf-8")
    target.write_text(original, encoding="utf-8")
    (tmp_path / "input.yml").write_text("synthetic: true\n")
    monkeypatch.setattr(refresh, "_row", lambda name: {
        "basename": name, "input": "input.yml",
        "parametric": {"physics": "linear", "response": "v", "axes": []}})
    monkeypatch.setattr(refresh, "SOURCE_FILES", {})
    names = ["mudmat_bearing_capacity", "mooring_fatigue"]
    before = {n: refresh.content_fingerprint(n, tmp_path) for n in names}
    start = original.index("def _mudmat_bearing_utilisation")
    changed = original[:start] + original[start:].replace("fos = 2.0", "fos = 2.1", 1)
    assert changed != original
    target.write_text(changed, encoding="utf-8")
    assert refresh.content_fingerprint(names[0], tmp_path) != before[names[0]]
    assert refresh.content_fingerprint(names[1], tmp_path) == before[names[1]]


def test_source_hash_is_stable_across_git_line_endings(tmp_path):
    path = tmp_path / "calculator.py"
    path.write_bytes(b"VALUE = 2\n")
    expected = refresh._sha_files([path.name], tmp_path)
    path.write_bytes(b"VALUE = 2\r\n")
    assert refresh._sha_files([path.name], tmp_path) == expected


def test_input_hash_is_stable_across_git_line_endings(tmp_path, monkeypatch):
    from digitalmodel.parametric import fingerprint
    monkeypatch.setattr(fingerprint, "response_source", lambda *args: "wrapper")
    monkeypatch.setattr(refresh, "SOURCE_FILES", {})
    monkeypatch.setattr(refresh, "_row", lambda _: {
        "basename": "audit", "input": "input.yml",
        "parametric": {"physics": "linear", "response": "v", "axes": []}})
    path = tmp_path / "input.yml"
    path.write_bytes(b"value: 2\n")
    expected = refresh.content_fingerprint("audit", tmp_path)
    path.write_bytes(b"value: 2\r\n")
    assert refresh.content_fingerprint("audit", tmp_path) == expected
