"""Section 9: wrapper source and referenced constants are part of the basis."""
from pathlib import Path

from digitalmodel.parametric import refresh


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
