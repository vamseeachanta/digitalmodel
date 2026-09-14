"""Verify canonical Git-byte inputs for planning register evidence."""
import importlib.util
import json
from pathlib import Path
import subprocess


def load_helper():
    path = Path(__file__).parents[2] / "docs/plans/evidence/register_json_prose.py"
    spec = importlib.util.spec_from_file_location("register_json_prose", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_recursive_string_values_preserve_order_and_unicode():
    helper = load_helper()
    value = {"excluded_key": ["alpha", 1, {"nested_key": "σr"}], "tail": None}
    assert helper.string_values(value) == ["alpha", "σr"]


def test_receipt_uses_staged_bytes_instead_of_modified_working_file(tmp_path):
    helper = load_helper()
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    subprocess.run(["git", "-C", str(tmp_path), "config", "core.autocrlf", "false"], check=True)
    path = tmp_path / "fixture.json"
    staged = b'{"value":"staged", "nested":["second",false]}\n'
    path.write_bytes(staged)
    subprocess.run(["git", "-C", str(tmp_path), "add", "--", "fixture.json"], check=True)
    path.write_text(json.dumps({"value": "uncommitted"}), encoding="utf-8")
    receipt = helper.extract(tmp_path, "", ["fixture.json"])
    assert receipt["prose"] == "staged\n\nsecond"
    assert receipt["inputs"][0]["bytes"] == len(staged)
    assert receipt["inputs"][0]["sha256"] == helper.digest(staged)
    assert receipt["extracted_bytes"] == len(b"staged\n\nsecond")


def test_commit_tree_extraction_and_digest_are_reproducible(tmp_path):
    """Regression-only coverage of existing committed-tree extraction behavior."""
    helper = load_helper()
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    subprocess.run(["git", "-C", str(tmp_path), "config", "core.autocrlf", "false"], check=True)
    path = tmp_path / "fixture.json"
    path.write_bytes(b'{"first":"alpha","nested":["beta",2]}\n')
    subprocess.run(["git", "-C", str(tmp_path), "add", "--", "fixture.json"], check=True)
    subprocess.run(["git", "-C", str(tmp_path), "-c", "user.name=Fixture",
                    "-c", "user.email=fixture@example.invalid", "commit", "-qm", "fixture"], check=True)
    tree = subprocess.check_output(
        ["git", "-C", str(tmp_path), "rev-parse", "HEAD^{tree}"], text=True
    ).strip()
    path.write_bytes(b'{"first":"changed working file"}\n')
    receipt = helper.extract(tmp_path, tree, ["fixture.json"])
    assert receipt["prose"] == "alpha\n\nbeta"
    assert receipt["extracted_prose_sha256"] == helper.digest(b"alpha\n\nbeta")
    assert receipt["extracted_bytes"] == len(b"alpha\n\nbeta")
