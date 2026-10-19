"""Independent counts for the licence-free corpus inventory.

Synthetic examples test syntax, not engineering coefficients. Hand counts are
written beside assertions. The real example counts are a direct enumeration of
the named records in the cited repository YAML, independent of the inventory.
"""

import hashlib
import json
import runpy
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
# Exercise the standalone tool without importing unrelated solver integrations.
TOOL = runpy.run_path(
    str(ROOT / "src/digitalmodel/solvers/orcaflex/corpus_inventory.py")
)
build_inventory = TOOL["build_inventory"]
inspect_file = TOOL["inspect_file"]
main = TOOL["main"]
write_inventory = TOOL["write_inventory"]


def inspect_text(tmp_path, text, suffix=".yml"):
    path = tmp_path / ("sample" + suffix)
    path.write_text(text, encoding="utf-8")
    return inspect_file(path, tmp_path)


def test_named_objects_and_unclassified_sections(tmp_path):
    """Two Lines plus one new object type; no hardcoded type whitelist."""
    row = inspect_text(
        tmp_path,
        """# Program: OrcaFlex 11.6b
General: {UnitsSystem: SI}
Lines: [{Name: first}, {Name: second}]
FutureObjects: [{Name: third}]
Groups: {Structure: {first: Model}}
""",
    )
    assert row["object_counts"] == {"Lines": 2, "FutureObjects": 1}
    assert row["unclassified_sections"] == ["Groups"]
    assert row["version"] == "11.6b"
    assert row["status"] == "local_census"
    assert row["round_trip"] == "NOT_EVALUATED"


@pytest.mark.parametrize("suffix", [".yml", ".yaml", ".dat"])
def test_text_format_uses_content_not_extension(tmp_path, suffix):
    row = inspect_text(tmp_path, "\ufeff%YAML 1.1\n---\nLines: [{Name: a}]\n", suffix)
    assert row["format"] == "yaml"
    assert row["object_counts"] == {"Lines": 1}  # One named record.
    assert row["version"] is None


@pytest.mark.parametrize("suffix", [".dat", ".sim"])
def test_prefixed_binary_signature(tmp_path, suffix):
    """Issue #2078: signature is behind a length prefix in the header."""
    path = tmp_path / ("binary" + suffix)
    data = b"\x09OrcaFlex\x00\xff"
    path.write_bytes(data)
    row = inspect_file(path, tmp_path)
    assert row["status"] == "signature_candidate"
    assert row["format"] == "binary"
    assert row["object_counts"] is None
    assert row["version"] is None
    assert row["sha256"] == hashlib.sha256(data).hexdigest()
    assert row["issues"] == ["binary_requires_conversion"]


def test_marker_outside_header_does_not_confirm_binary(tmp_path):
    """Issue #2078 specifies a 64-byte header window, not a full-file search."""
    path = tmp_path / "other.dat"
    path.write_bytes(b"\xff" * 64 + b"OrcaFlex")
    assert inspect_file(path, tmp_path)["status"] == "not_recognised"


@pytest.mark.parametrize(
    "text,issue",
    [
        ("Lines: [", "invalid_yaml"),
        ("Lines: []\nLines: [{Name: hidden}]", "duplicate_key"),
        ("Lines: [{Name: a, Name: b}]", "duplicate_key"),
        ("---\nLines: [{Name: a}]\n---\nLines: [{Name: b}]", "multiple_documents"),
        ("Lines: [*missing]", "invalid_yaml"),
        ("Lines: &loop [*loop]", "recursive_alias"),
        ("!!python/object:example {}", "unsupported_tag"),
    ],
)
def test_ambiguous_yaml_is_not_silently_reduced(tmp_path, text, issue):
    row = inspect_text(tmp_path, text)
    assert row["status"] == "NOT_EVALUATED"
    assert issue in row["issues"]
    assert row["object_counts"] is None


@pytest.mark.parametrize(
    "dependency",
    [
        "BaseFile: absent.yml\n",
        "IncludeFile: absent.yml\n",
        "VesselTypes: [{Name: type, IncludeFile: absent.yml}]\n",
    ],
)
def test_dependencies_are_reported_without_opening_them(tmp_path, dependency):
    row = inspect_text(tmp_path, "Lines: [{Name: a}]\n" + dependency)
    assert row["status"] == "NOT_EVALUATED"
    assert row["object_counts"] == {
        "Lines": 1,
        **({"VesselTypes": 1} if dependency.startswith("VesselTypes") else {}),
    }
    assert row["issues"] == ["unresolved_dependency"]


def test_mixed_collection_is_not_partially_counted(tmp_path):
    row = inspect_text(tmp_path, "General: {}\nLines: [{Name: a}, broken]\n")
    assert row["object_counts"] == {}
    assert row["unclassified_sections"] == ["Lines"]


@pytest.mark.parametrize("text", ["", "[]", "spec: {name: input}", "name: input"])
def test_nonmodel_yaml_is_explicit(tmp_path, text):
    row = inspect_text(tmp_path, text)
    assert row["status"] == "not_recognised"


def test_symlink_does_not_read_external_content(tmp_path):
    external = tmp_path / "outside.yml"
    external.write_text("General: {}")
    root = tmp_path / "root"
    root.mkdir()
    (root / "link.yml").symlink_to(external)
    row = build_inventory(root)[0]
    assert row["sha256"] is None
    assert row["issues"] == ["symlink_not_followed"]


def test_deterministic_table_and_manifest(tmp_path):
    """Digest is SHA-256 of exact table bytes, independently calculated here."""
    root = tmp_path / "inputs"
    root.mkdir()
    (root / "z.yml").write_text("General: {}\n")
    (root / "a.yaml").write_text("Lines: [{Name: a}]\n")
    (root / "ignored.txt").write_text("not a model")
    rows = build_inventory(root)
    assert [row["path"] for row in rows] == ["a.yaml", "z.yml"]
    out = tmp_path / "out"
    write_inventory(rows, out)
    table = (out / "inventory.jsonl").read_bytes()
    manifest = json.loads((out / "manifest.json").read_text())
    assert manifest["table_sha256"] == hashlib.sha256(table).hexdigest()
    assert manifest["files"] == 2  # a.yaml and z.yml only.
    write_inventory(build_inventory(root), out)
    assert (out / "inventory.jsonl").read_bytes() == table
    (root / "a.yaml").write_text("Lines: [{Name: changed}]\n")
    assert build_inventory(root)[0]["sha256"] != rows[0]["sha256"]


def test_cli_selection(tmp_path):
    (tmp_path / "a.yml").write_text("General: {}\n")
    (tmp_path / "b.yml").write_text("General: {}\n")
    out = tmp_path / "output"
    assert main([str(tmp_path), "--select", "a.yml", "--output", str(out)]) == 0
    rows = [
        json.loads(line) for line in (out / "inventory.jsonl").read_text().splitlines()
    ]
    assert [row["path"] for row in rows] == ["a.yml"]


def test_public_corpus_example():
    """Hand census of A01 Pliant wave riser monolithic YAML in model_library.

    The top-level named lists have: four LineTypes (product, two buoyed
    variants, tether), four Lines (simple, detailed up/down, tether), and one
    each of VesselTypes, ClumpTypes, Vessels, 6DBuoys and Links. VariableData
    and Groups are mappings and remain explicitly unclassified.
    """
    root = Path(__file__).resolve().parents[3]
    path = root / (
        "docs/domains/orcaflex/library/model_library/"
        "a01_pliant_wave_riser/monolithic/A01 Pliant wave riser.yml"
    )
    row = inspect_file(path, root)
    assert row["object_counts"] == {
        "LineTypes": 4,
        "Lines": 4,
        "VesselTypes": 1,
        "ClumpTypes": 1,
        "Vessels": 1,
        "6DBuoys": 1,
        "Links": 1,
    }
    assert row["unclassified_sections"] == ["Groups", "VariableData"]


def test_public_mooring_example():
    """Hand census of C06 CALM buoy monolithic YAML in model_library.

    Six moorings, one load hose and one hawser give eight Lines. Chain, hose
    and hawser give three LineTypes. Base and top give two 6DBuoys. There is
    one each of VesselTypes, Vessels and Constraints. These counts enumerate
    top-level records in the source, without using the tool's output.
    """
    path = ROOT / (
        "docs/domains/orcaflex/library/model_library/"
        "c06_calm_buoy/monolithic/C06 CALM buoy.yml"
    )
    assert inspect_file(path, ROOT)["object_counts"] == {
        "Lines": 8,
        "LineTypes": 3,
        "6DBuoys": 2,
        "VesselTypes": 1,
        "Vessels": 1,
        "Constraints": 1,
    }


def test_committed_inventory_is_regenerable(tmp_path):
    """Check exact artifacts and independently digest each selected source."""
    committed = ROOT / "docs/domains/orcaflex/corpus_inventory"
    rows = [
        json.loads(line)
        for line in (committed / "inventory.jsonl").read_text().splitlines()
    ]
    patterns = [row["path"] for row in rows]
    generated = build_inventory(ROOT, patterns)
    assert len(generated) == len(rows)
    for row in generated:
        assert (
            row["sha256"]
            == hashlib.sha256((ROOT / row["path"]).read_bytes()).hexdigest()
        )
    write_inventory(generated, tmp_path)
    for name in ("inventory.jsonl", "manifest.json"):
        assert (tmp_path / name).read_bytes() == (committed / name).read_bytes()


def test_binary_extension_with_text_header(tmp_path):
    row = inspect_text(tmp_path, "# Program: OrcaFlex 11.6b\nGeneral: {}\n", ".dat")
    assert row["format"] == "yaml"
    assert row["version"] == "11.6b"


@pytest.mark.parametrize("pattern", ["../*.yml", "/absolute/*.yml"])
def test_selection_cannot_escape_root(tmp_path, pattern):
    with pytest.raises(ValueError, match="relative"):
        build_inventory(tmp_path, [pattern])


def test_directory_symlink_selection_is_not_followed(tmp_path):
    external = tmp_path / "outside"
    external.mkdir()
    (external / "sample.yml").write_text("General: {}")
    root = tmp_path / "root"
    root.mkdir()
    (root / "linked").symlink_to(external, target_is_directory=True)
    assert build_inventory(root, ["linked/*.yml"]) == []


def test_deep_yaml_does_not_abort_inventory(tmp_path):
    """Depth is an adversarial input, not an engineering acceptance limit."""
    (tmp_path / "deep.yml").write_text("Lines: " + "[" * 2000 + "]" * 2000)
    (tmp_path / "simple.yml").write_text("General: {}")
    rows = build_inventory(tmp_path)
    assert rows[0]["status"] == "NOT_EVALUATED"
    assert rows[0]["object_counts"] is None
    assert rows[1]["status"] == "local_census"


def test_merge_keys_are_explicit(tmp_path):
    row = inspect_text(tmp_path, "General: {<<: {UnitsSystem: SI}}")
    assert row["status"] == "NOT_EVALUATED"
    assert row["issues"] == ["unsupported_merge"]


def test_conflicting_versions_are_explicit(tmp_path):
    row = inspect_text(
        tmp_path, ("# Program: OrcaFlex 11.6b\n# Program: OrcaFlex 9.3a\nGeneral: {}")
    )
    assert row["version"] is None
    assert row["status"] == "NOT_EVALUATED"
    assert row["issues"] == ["conflicting_versions"]


def test_read_failure_is_a_row(tmp_path):
    row = inspect_file(tmp_path / "missing.yml", tmp_path)
    assert row["status"] == "NOT_EVALUATED"
    assert row["issues"] == ["read_error"]


def test_standalone_does_not_probe_solver(monkeypatch):
    import builtins

    original = builtins.__import__

    def guard(name, *args, **kwargs):
        if "orcfxapi" in name.casefold():
            pytest.fail("Solver import attempted")
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", guard)
    runpy.run_path(str(ROOT / "src/digitalmodel/solvers/orcaflex/corpus_inventory.py"))
