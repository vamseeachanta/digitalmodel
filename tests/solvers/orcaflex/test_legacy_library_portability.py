# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep preparation evidence stable across Git and path implementations."""

import importlib.util
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
GENERIC = "docs/domains/orcaflex/mooring/buoy"


def implementation():
    spec = importlib.util.spec_from_file_location(
        "portable_preparation", ROOT / "scripts/orcaflex/prepare_legacy_library.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_inventory_order_uses_case_sensitive_posix_locators(tmp_path):
    class CaseInsensitiveOrderPath(type(tmp_path)):
        """Emulate Windows ordering while accessing the local fixture."""

        def __lt__(self, other):
            return str(self).casefold() < str(other).casefold()

    root = CaseInsensitiveOrderPath(tmp_path)
    library = root / GENERIC
    library.mkdir(parents=True)
    for name in ("alpha.yml", "Zeta.yml"):
        (library / name).write_bytes(b"General: {}\nEnvironment: {}\n")
    module = implementation()
    report = module.inspection.audit(root)
    expected = [GENERIC + "/Zeta.yml", GENERIC + "/alpha.yml"]
    assert [item["path"] for item in report["models"]] == expected
    assert [item["path"] for item in module.inspection.publication_records(report, root)["models"]] == expected


@pytest.mark.parametrize("autocrlf", ["true", "input"])
def test_git_checkout_preserves_all_hash_bound_bytes(tmp_path, autocrlf):
    """Exercise real Git conversion, including pre-existing CRLF library blobs."""
    def git(*args):
        return subprocess.run(["git", "-C", str(tmp_path), *args], check=True,
                              capture_output=True)

    git("init", "--quiet")
    git("config", "core.autocrlf", "false")
    (tmp_path / ".gitattributes").write_bytes((ROOT / ".gitattributes").read_bytes())
    sources = ["src/digitalmodel/solvers/orcaflex/" + name for name in (
        "yaml_validator.py", "yaml_timestep_validation.py",
        "starting_shape_validation.py")]
    sources += ["scripts/orcaflex/check_legacy_library_sanitisation.py",
                "scripts/legal/check_identifiers.py", ".legal-deny-list.yaml"]
    sources += ["docs/domains/orcaflex/library/legacy-round-trip/" + name for name in (
        "inventory.json", "models.json", "validators.json", "comparators.json",
        "criterion.json", "native-manifest.template.json")]
    expected = {name: b"synthetic\nsource\n" for name in sources}
    for group in ("jumper", "mooring", "installation", "training"):
        for suffix in ("yml", "yaml", "dat"):
            for ending, content in (("lf", b"synthetic\nsource\n"),
                                    ("crlf", b"synthetic\r\nsource\r\n")):
                expected[f"docs/domains/orcaflex/{group}/case/{ending}.{suffix}"] = content
    for relative, content in expected.items():
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(content)
    git("add", ".")
    git("config", "core.autocrlf", autocrlf)
    for relative in expected:
        (tmp_path / relative).unlink()
    git("checkout-index", "--all", "--force")
    assert all((tmp_path / relative).read_bytes() == content
               for relative, content in expected.items())


@pytest.mark.parametrize("section", [
    "PyModels: []\n",
    "VesselTypes:\n  - Name: synthetic-type\n    AMDMethod: Frequency dependent\n",
])
def test_preparation_uses_current_validator_rules(tmp_path, section):
    module = implementation()
    library = tmp_path / GENERIC
    library.mkdir(parents=True)
    (library / "model.yml").write_text("General: {}\nEnvironment: {}\n" + section,
                                       encoding="utf-8")
    record = module.build_inventory(tmp_path, "a" * 40, "2026-10-18", 123)
    result = module.validate_models(record, tmp_path)["models"][0]
    assert result["status"] == "fail"
    assert result["errors"] > 0


def test_missing_and_escaped_dependencies_count_independently_and_transitively(tmp_path):
    module = implementation()
    root = tmp_path / "source"
    library = root / GENERIC
    library.mkdir(parents=True)
    (tmp_path / "external.dat").write_bytes(b"synthetic\n")
    (library / "model.yml").write_text(
        "BaseFile: absent.yml\nIncludeFiles: [../../../../../../external.dat]\n",
        encoding="utf-8")
    (library / "dependent.yml").write_text("BaseFile: model.yml\n", encoding="utf-8")
    result = module.inspection.publication_records(module.inspection.audit(root), root)
    assert result["models"] == []
    assert result["inputs"] == []
    assert result["counts"]["excluded_models"] == 2
    assert result["counts"]["models_with_missing_inputs"] == 2
    assert result["counts"]["models_with_escaped_inputs"] == 2
    assert "external.dat" not in str(result)
    assert "absent.yml" not in str(result)
