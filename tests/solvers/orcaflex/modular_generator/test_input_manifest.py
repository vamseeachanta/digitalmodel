# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Input fingerprint tests; no solver binding is imported.

Synthetic files are byte fixtures, not engineering acceptance values. Graph
expectations are enumerated by hand in each test. SHA-256 reference: RFC 6234
(2011), section 8.5, SHA256 TEST1 (the message ``abc``).
"""

import hashlib
import json

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.input_manifest import (
    InputManifestError,
    write_input_manifest,
    verify_input_manifest,
)


ABC_SHA256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"


def put(root, name, content):
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")
    return path


def load(path):
    return json.loads(path.read_text(encoding="utf-8"))


def test_known_digest_and_independent_aggregate(tmp_path):
    """Hand construction: one record is 'model.yml' + NUL + H(abc) + LF.

    H(abc) is RFC 6234 section 8.5 TEST1. The aggregate is computed directly
    with hashlib over these literal record bytes, independently of the writer.
    """
    put(tmp_path, "model.yml", "abc")
    path = write_input_manifest(tmp_path, ["model.yml"])
    manifest = load(path)
    assert manifest["files"] == {"model.yml": ABC_SHA256}
    expected_record = f"model.yml\0{ABC_SHA256}\n".encode("utf-8")
    assert manifest["fingerprint"] == hashlib.sha256(expected_record).hexdigest()
    assert verify_input_manifest(tmp_path).status == "PASS"
    assert set(manifest["revisions"]) == {"generator", "template", "design_basis"}
    assert {r["status"] for r in manifest["revisions"].values()} == {"NOT_EVALUATED"}


def test_transitive_repeated_keys_and_nested_basefile(tmp_path):
    """Hand graph: master -> a, b; a -> base; b -> base; base -> leaf.

    parameters is an additional seed; the expected set is the six named files.
    Repeated YAML keys must survive parsing for dependency discovery.
    """
    put(tmp_path, "master.yml", "includefile: a.yml\nincludefile: sub/b.yml\n")
    put(tmp_path, "a.yml", "BaseFile: base.yml\n")
    put(tmp_path, "sub/b.yml", "Environment:\n  includefile: ../base.yml\n")
    put(tmp_path, "base.yml", "- includefile: leaf.yml\n")
    put(tmp_path, "leaf.yml", "{}\n")
    put(tmp_path, "inputs/parameters.yml", "{}\n")
    path = write_input_manifest(tmp_path, ["master.yml", "inputs/parameters.yml"])
    assert set(load(path)["files"]) == {
        "master.yml",
        "a.yml",
        "sub/b.yml",
        "base.yml",
        "leaf.yml",
        "inputs/parameters.yml",
    }
    assert verify_input_manifest(tmp_path).status == "PASS"
    put(tmp_path, "leaf.yml", "changed\n")
    result = verify_input_manifest(tmp_path)
    assert result.status == "FAIL"
    assert "Digest mismatch: leaf.yml" in result.findings
    assert "Aggregate fingerprint mismatch" in result.findings


@pytest.mark.parametrize("mutation", ["delete", "omit", "aggregate", "metadata"])
def test_independent_verification_detects_incomplete_or_tampered_manifest(
    tmp_path, mutation
):
    """Hand graph has exactly master and leaf; any deletion loses one input."""
    put(tmp_path, "master.yml", "BaseFile: leaf.yml\n")
    put(tmp_path, "leaf.yml", "{}\n")
    path = write_input_manifest(tmp_path, ["master.yml"])
    data = load(path)
    if mutation == "delete":
        # Rename retains the fixture data while making its old name unavailable.
        (tmp_path / "leaf.yml").rename(tmp_path / "retained.yml")
    elif mutation == "omit":
        del data["files"]["leaf.yml"]
    elif mutation == "aggregate":
        data["fingerprint"] = "changed"
    else:
        data["revisions"]["generator"]["status"] = "PASS"
    path.write_text(json.dumps(data), encoding="utf-8")
    assert verify_input_manifest(tmp_path).status == "FAIL"


@pytest.mark.parametrize("reference", ["missing.yml", "../outside.yml", "", "[]"])
def test_invalid_references_fail_without_writing_manifest(tmp_path, reference):
    """Missing, escaping, empty and sequence references cannot give coverage."""
    put(tmp_path, "master.yml", f"BaseFile: {reference}\n")
    with pytest.raises(InputManifestError):
        write_input_manifest(tmp_path, ["master.yml"])
    assert not (tmp_path / "input-manifest.json").exists()


def test_absolute_reference_is_rejected(tmp_path):
    """Build the absolute fixture path at runtime; portability requires rejection."""
    target = put(tmp_path, "leaf.yml", "{}\n")
    put(tmp_path, "master.yml", f"BaseFile: {target.as_posix()}\n")
    with pytest.raises(InputManifestError, match="relative"):
        write_input_manifest(tmp_path, ["master.yml"])


def test_cycle_is_rejected(tmp_path):
    """Hand graph a -> b -> a has a cycle, so no finite composition is claimed."""
    put(tmp_path, "a.yml", "includefile: b.yml\n")
    put(tmp_path, "b.yml", "BaseFile: a.yml\n")
    with pytest.raises(InputManifestError, match="cycle"):
        write_input_manifest(tmp_path, ["a.yml"])


def test_opaque_base_is_hashed_but_coverage_is_not_evaluated(tmp_path):
    """Hand graph: master -> binary base; its bytes are covered, its graph is unknown."""
    put(tmp_path, "master.yml", "BaseFile: base.dat\n")
    (tmp_path / "base.dat").write_bytes(b"\x00synthetic")
    path = write_input_manifest(tmp_path, ["master.yml"])
    assert "base.dat" in load(path)["files"]
    result = verify_input_manifest(tmp_path)
    assert result.status == "NOT_EVALUATED"
    assert "Opaque dependency graph: base.dat" in result.findings


def test_deterministic_relocation_and_seed_order(tmp_path):
    """Hand sets {a,b} and {b,a} have identical sorted path/digest records."""
    for folder in ("first", "second"):
        root = tmp_path / folder
        put(root, "a.yml", "abc")
        put(root, "b.yml", "{}\n")
    first = write_input_manifest(tmp_path / "first", ["a.yml", "b.yml"])
    second = write_input_manifest(tmp_path / "second", ["b.yml", "a.yml", "a.yml"])
    assert first.read_bytes() == second.read_bytes()


def test_invalid_yaml_and_symlink_escape_fail(tmp_path):
    """Unparseable text and links outside the bundle cannot establish coverage."""
    root = tmp_path / "bundle"
    put(root, "master.yml", "[")
    with pytest.raises(InputManifestError, match="YAML"):
        write_input_manifest(root, ["master.yml"])
    target = put(tmp_path, "external.yml", "{}\n")
    (root / "link.yml").symlink_to(target)
    put(root, "master.yml", "BaseFile: link.yml\n")
    with pytest.raises(InputManifestError, match="outside"):
        write_input_manifest(root, ["master.yml"])


def test_missing_or_invalid_manifest_fails(tmp_path):
    """Absent and malformed evidence never passes."""
    assert verify_input_manifest(tmp_path).status == "FAIL"
    put(tmp_path, "input-manifest.json", "[]")
    assert verify_input_manifest(tmp_path).status == "FAIL"


@pytest.mark.parametrize("overrides", [False, True])
def test_generation_publishes_complete_manifest(validated_spec, tmp_path, overrides):
    """Disk oracle: every generated YAML file is a seed and has its byte digest.

    No engineering value is evaluated; the existing validated fixture drives
    the builders, and hashlib independently fingerprints the published bytes.
    """
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

    generator = ModularModelGenerator.from_spec(validated_spec)
    output = tmp_path / "output"
    if overrides:
        result = generator.generate_with_overrides(output)
        assert result.manifest_file == output / "input-manifest.json"
    else:
        generator.generate(output)
    data = load(output / "input-manifest.json")
    expected = {
        path.relative_to(output).as_posix(): hashlib.sha256(
            path.read_bytes()
        ).hexdigest()
        for path in output.rglob("*.yml")
    }
    assert "inputs/parameters.yml" in expected
    assert "master.yml" in expected
    assert data["files"] == expected
    assert verify_input_manifest(output).status == "PASS"


@pytest.mark.parametrize("overrides", [False, True])
def test_manifest_failure_preserves_existing_output(
    validated_spec, tmp_path, monkeypatch, overrides
):
    """A deliberately missing include prevents publication of either entry point."""
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

    generator = ModularModelGenerator.from_spec(validated_spec)
    original = generator._write_master

    def dangling_master(path, generated_files):
        original(path, generated_files)
        with path.open("a", encoding="utf-8") as stream:
            stream.write("\n- includefile: includes/unavailable.yml\n")

    monkeypatch.setattr(generator, "_write_master", dangling_master)
    output = tmp_path / "output"
    put(output, "master.yml", "previous output\n")
    generate = generator.generate_with_overrides if overrides else generator.generate
    with pytest.raises(InputManifestError, match="Missing input"):
        generate(output)
    assert (output / "master.yml").read_text() == "previous output\n"
    assert not (output / "input-manifest.json").exists()
