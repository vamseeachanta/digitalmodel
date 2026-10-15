# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check 52 SLWR result references against tree files and 39 of 40 store rows."""
import importlib.util
from pathlib import Path
import re

import pytest


ROOT = Path(__file__).resolve().parents[2]
SLWR = Path("docs/domains/orcaflex/risers/production/slwr")


def result_references(root, configuration):
    """Return repository-relative result paths from configuration text."""
    references = []
    for line in configuration.read_text(encoding="utf-8-sig").splitlines():
        if line.lstrip().startswith("#"):
            continue
        match = re.search(
            r"\bName:\s*[\"']?(.+?\.sim)(?=[\"']?\s*(?:#.*)?$)",
            line, re.IGNORECASE,
        )
        if match:
            path = configuration.parent / match[1].replace("\\", "/")
            references.append(path.resolve().relative_to(root.resolve()))
    return references


def unresolved_references(root, manifest, configuration):
    """Return results without a tree file, exact store path or stored input."""
    paths = {Path(row["path"]) for row in manifest}
    inputs = {(path.parent, path.stem) for path in paths}
    return [
        path for path in result_references(root, configuration)
        if not (root / path).is_file() and path not in paths
        and (path.parent, path.stem) not in inputs
    ]


@pytest.fixture
def fetch_module():
    """Load the store reader and selector without retrieving data."""
    spec = importlib.util.spec_from_file_location(
        "store_fetch", ROOT / "scripts/data/fetch.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def configurations():
    """Find all configurations, independently of the working directory."""
    files = sorted((ROOT / SLWR / "OrcaFlex_Post_Process").glob("*.yml"))
    assert len(files) == 16
    return files


def test_real_references_resolve(fetch_module, configurations):
    manifest = fetch_module.read_manifest(ROOT)
    unresolved = [
        f"{configuration.relative_to(ROOT)}: {path}"
        for configuration in configurations
        for path in unresolved_references(ROOT, manifest, configuration)
    ]
    assert not unresolved, "Unresolved results:\n" + "\n".join(unresolved)


@pytest.mark.parametrize("resolution", ["missing", "input", "exact", "tree"])
def test_reference_resolution_calibration(tmp_path, resolution):
    configuration = tmp_path / "post" / "case.yml"
    configuration.parent.mkdir()
    configuration.write_text("- Name: ..\\inputs\\sample.SIM\n")
    manifest = []
    if resolution in {"input", "exact"}:
        suffix = ".yml" if resolution == "input" else ".SIM"
        manifest = [{"path": "inputs/sample" + suffix}]
    elif resolution == "tree":
        (tmp_path / "inputs").mkdir()
        (tmp_path / "inputs/sample.SIM").write_text("result text")
    unresolved = unresolved_references(tmp_path, manifest, configuration)
    assert unresolved == ([Path("inputs/sample.SIM")] if resolution == "missing" else [])


def test_configurations_name_fetch_prerequisite(configurations):
    for configuration in configurations:
        comment = "\n".join(configuration.read_text().splitlines()[:2])
        assert comment.startswith("# Prerequisite:"), str(configuration.relative_to(ROOT))
        for required in ("scripts/data/fetch.py", "../README.md", "content store"):
            assert required in comment, str(configuration.relative_to(ROOT))


def test_readme_selector_covers_required_inputs(fetch_module, configurations):
    readme = (ROOT / SLWR / "README.md").read_text()
    commands = re.findall(r"python scripts/data/fetch\.py '([^']+)'", readme)
    assert len(commands) == 1, "README must document one quoted fetch selector"
    manifest = fetch_module.read_manifest(ROOT)
    selected = {row["path"] for row in fetch_module.select_rows(manifest, commands[0])}
    references = {path for configuration in configurations
                  for path in result_references(ROOT, configuration)}
    needed = {row["path"] for row in manifest
              if any(Path(row["path"]).parent == path.parent
                     and Path(row["path"]).stem == path.stem for path in references)}
    assert needed
    assert needed <= selected, "README selector misses: " + str(sorted(needed - selected))
