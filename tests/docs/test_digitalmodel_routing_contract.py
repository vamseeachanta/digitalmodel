from __future__ import annotations

import re
import subprocess
from pathlib import Path

import yaml


ROOT = Path(__file__).resolve().parents[2]
DOCS_README = ROOT / "docs" / "README.md"
OPERATOR_MAP = ROOT / "docs" / "maps" / "digitalmodel-operator-map.md"
REGISTRY = ROOT / "docs" / "registry" / "module-routing.yaml"


def package_domains() -> set[str]:
    source_root = ROOT / "src" / "digitalmodel"
    return {
        path.name
        for path in source_root.iterdir()
        if path.is_dir() and (path / "__init__.py").exists()
    }


def operator_map_domains() -> set[str]:
    rows = set()
    for line in OPERATOR_MAP.read_text(encoding="utf-8").splitlines():
        if not line.startswith("| `"):
            continue
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        rows.add(cells[0].strip("`"))
    return rows


def registry_domains() -> set[str]:
    data = yaml.safe_load(REGISTRY.read_text(encoding="utf-8"))
    return {entry["module"] for entry in data["modules"]}


def test_required_routing_surfaces_exist() -> None:
    assert DOCS_README.is_file()
    assert OPERATOR_MAP.is_file()
    assert REGISTRY.is_file()


def test_docs_readme_links_required_surfaces_and_boundaries() -> None:
    text = DOCS_README.read_text(encoding="utf-8")
    for required in (
        "../AGENTS.md",
        "../README.md",
        "domains/README.md",
        "maps/digitalmodel-operator-map.md",
        "registry/module-routing.yaml",
    ):
        assert required in text

    assert "Curated routing surfaces" in text
    assert "Raw inventory surfaces" in text
    assert "repo-vs-bulk-artifact-store" in text
    assert "/mnt/ace/data" in text
    assert "implementation example" in text


def test_operator_map_rows_match_live_source_tree() -> None:
    assert operator_map_domains() == package_domains()


def test_operator_map_has_required_columns_and_slice_link() -> None:
    header = next(
        line for line in OPERATOR_MAP.read_text(encoding="utf-8").splitlines()
        if line.startswith("| Module |")
    )
    for column in (
        "Module",
        "Source",
        "Tests",
        "Docs",
        "Issue routing",
        "Key dependencies",
    ):
        assert column in header

    assert "digitalmodel-orcawave-orcaflex-operator-map.md" in OPERATOR_MAP.read_text(
        encoding="utf-8"
    )


def test_registry_covers_operator_map_rows_and_required_fields() -> None:
    data = yaml.safe_load(REGISTRY.read_text(encoding="utf-8"))
    assert data["repo"] == "digitalmodel"
    assert data["schema_version"] == 1
    assert registry_domains() == operator_map_domains()

    for entry in data["modules"]:
        assert entry["entry_point"] == f"src/digitalmodel/{entry['module']}/"
        assert entry["operator_map_row"] == f"docs/maps/digitalmodel-operator-map.md#{entry['module']}"
        assert entry["maturity"] in {
            "production",
            "stable",
            "beta",
            "development",
            "stub",
        }
        assert entry["key_tests"]


def test_stale_registry_references_are_removed_from_active_docs() -> None:
    for path in (ROOT / "README.md", ROOT / "ROADMAP.md", DOCS_README):
        text = path.read_text(encoding="utf-8")
        assert "specs/module-registry.yaml" not in text
        assert "stale until restored" not in text


def test_domain_readme_and_agents_link_repo_wide_map() -> None:
    domain_readme = (ROOT / "docs" / "domains" / "README.md").read_text(
        encoding="utf-8"
    )
    agents = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
    assert "docs/maps/digitalmodel-operator-map.md" in domain_readme
    assert "non-OrcaWave/OrcaFlex" in domain_readme
    assert "docs/maps/digitalmodel-operator-map.md" in agents


def test_no_tracked_source_hygiene_violations() -> None:
    result = subprocess.run(
        ["git", "ls-files", "src/digitalmodel"],
        cwd=ROOT,
        check=True,
        text=True,
        capture_output=True,
    )
    tracked = result.stdout.splitlines()
    forbidden_pattern = re.compile(r"(\.bak$|\.orig$|__pycache__/|\.pytest_cache/|\.ruff_cache/)")
    assert not [path for path in tracked if forbidden_pattern.search(path)]
