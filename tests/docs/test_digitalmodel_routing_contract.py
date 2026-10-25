from __future__ import annotations

import re
import subprocess
from pathlib import Path

import pytest
import yaml
from yaml.constructor import ConstructorError


ROOT = Path(__file__).resolve().parents[2]
DOCS_README = ROOT / "docs" / "README.md"
OPERATOR_MAP = ROOT / "docs" / "maps" / "digitalmodel-operator-map.md"
REGISTRY = ROOT / "docs" / "registry" / "module-routing.yaml"


class UniqueKeySafeLoader(yaml.SafeLoader):
    """Reject repeated keys while retaining safe YAML construction."""

    def construct_mapping(self, node, deep=False):
        self.flatten_mapping(node)
        mapping = super().construct_mapping(node, deep=deep)
        seen = set()
        for key_node, _ in node.value:
            key = self.construct_object(key_node, deep=deep)
            if key in seen:
                raise ConstructorError(
                    "while constructing a mapping",
                    node.start_mark,
                    f"found duplicate key {key!r}",
                    key_node.start_mark,
                )
            seen.add(key)
        return mapping


def load_registry(text: str) -> dict:
    """Parse the routing registry without silently shadowing mapping entries."""
    return yaml.load(text, Loader=UniqueKeySafeLoader)


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
    data = load_registry(REGISTRY.read_text(encoding="utf-8"))
    return {entry["module"] for entry in data["modules"]}


def test_required_routing_surfaces_exist() -> None:
    assert DOCS_README.is_file()
    assert OPERATOR_MAP.is_file()
    assert REGISTRY.is_file()


def test_ffs_report_navigation_is_linked_and_qualification_is_explicit() -> None:
    data = load_registry(REGISTRY.read_text(encoding="utf-8"))
    entry = next(row for row in data['modules'] if row['module'] == 'asset_integrity')
    assert entry['screening_study']['status'] == 'unqualified-preparation'
    for path in entry['report_navigation'] + [entry['screening_study']['spec'],
                                             entry['screening_study']['diagnostic']]:
        assert (ROOT / path).is_file(), path


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
    data = load_registry(REGISTRY.read_text(encoding="utf-8"))
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


def test_ffs_preliminary_evidence_has_pinned_owner_and_qualification() -> None:
    data = load_registry(REGISTRY.read_text(encoding="utf-8"))
    entry = next(row for row in data["modules"] if row["module"] == "asset_integrity")
    evidence = entry["screening_study"]["canonical_preliminary_results"]
    assert evidence["owner_repo"] == "vamseeachanta/digitalmodel-data"
    assert re.fullmatch(r"[0-9a-f]{40}", evidence["revision"])
    for field in ("manifest_sha256", "publication_receipt_sha256"):
        assert re.fullmatch(r"[0-9a-f]{64}", evidence[field])
    for field in ("manifest", "plot", "csv", "json"):
        assert f"/blob/{evidence['revision']}/data/{evidence['dataset_id']}/" in evidence[field]
    assert re.search(r"/blob/[0-9a-f]{40}/reports/", evidence["publication_receipt"])
    assert evidence["run_id"]
    assert evidence["engineering_qualified"] is False
    assert evidence["api579_allowable_remaining_wall_in"] is None
    assert evidence["asset_acceptance"] is None


@pytest.mark.parametrize(
    "text",
    [
        "modules: []\nmodules: []\n",
        (
            "modules:\n  - module: solvers\n"
            "    submodules:\n      openfoam: {qa_family: CF}\n"
            "    submodules:\n      top_level: {qa_family: DF}\n"
        ),
        (
            "modules:\n  - module: solvers\n    submodules:\n"
            "      orcaflex:\n        qa_family: MR\n        qa_family: DF\n"
        ),
    ],
    ids=["root", "repeated-solver-submodules", "submodule-metadata"],
)
def test_registry_rejects_duplicate_mapping_keys(text: str) -> None:
    """YAML 1.2.2 (2021), section 3.2.1.1: mapping keys must be unique.

    Each literal repeats a key in the same mapping; each must be rejected.
    The solver case reproduces the shadowing structure reported in issue #2772.
    Source: https://yaml.org/spec/1.2.2/#3211-nodes
    """
    with pytest.raises(ConstructorError):
        load_registry(text)


def test_registry_preserves_solver_submodule_routes() -> None:
    """Expect the manually enumerated routes in the registry at d34bda5cf18d.

    Independent inventory: blender_automation, calculix, gmsh_meshing, ghs,
    mystran, openfoam, orcaflex, seven orcaflex child packages, orcawave and
    top_level make sixteen routes.
    Paths and QA families are transcribed from docs/registry/module-routing.yaml
    at that revision; these are routing metadata, not engineering limits.
    For intentional changes, re-enumerate routes by hand from the registry, not loader output.
    """
    expected_families = {
        "blender_automation": "CF",
        "calculix": "FE",
        "gmsh_meshing": "FE",
        "ghs": "EC",
        "mystran": "FE",
        "openfoam": "CF",
        "orcaflex": "MR",
        "orcaflex/format_converter": "MR",
        "orcaflex/model_generator": "MR",
        "orcaflex/modular_generator": "MR",
        "orcaflex/modular_input_validation": "MR",
        "orcaflex/mooring_analysis": "MR",
        "orcaflex/results_analysis": "MR",
        "orcaflex/universal": "MR",
        "orcawave": "DF",
        "top_level": "DF",
    }
    data = load_registry(REGISTRY.read_text(encoding="utf-8"))
    solver = next(entry for entry in data["modules"] if entry["module"] == "solvers")
    expected = {}
    for name, family in expected_families.items():
        package = "solvers" if name == "top_level" else f"solvers/{name}"
        expected[name] = {
            "agent_pack": f"src/digitalmodel/{package}/docs/agent-pack.md",
            "qa_family": family,
        }
    assert solver["submodules"] == expected
