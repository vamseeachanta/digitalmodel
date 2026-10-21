# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

import importlib.util
import json
import re
import shlex
import subprocess
import sys
import textwrap
from pathlib import Path

import yaml
import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/ci/detect_touched_domains.py"


@pytest.mark.parametrize(
    ("path", "expected"),
    [
        ("docs/registry/module-routing.yaml", {"asset-integrity", "contracts", "workflows", "subsea"}),
        ("docs/maps/example.md", {"contracts"}),
        ("src/digitalmodel/solvers/orcaflex/docs/agent-pack.md",
         {"contracts", "orcaflex-solver"}),
        ("src/digitalmodel/cathodic_protection/docs/agent-pack.md",
         {"contracts", "cathodic-protection"}),
        ("src/digitalmodel/weather_window/docs/x.md", None),
        ("src/digitalmodel/subsea/docs/example.py", None),
        ("src/digitalmodel/unknown/docs/example.md", None),
    ],
)
def test_documentation_routes(path: str, expected: set[str] | None) -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    selected = {domain.name for domain in detector.touched_domains([path], domains)}
    assert selected == (expected if expected is not None else {d.name for d in domains})


def load_detector_module():
    spec = importlib.util.spec_from_file_location("detect_touched_domains", SCRIPT)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("path", [
    "scripts/ci/generate_family_map.py",
    "config/ci/module-families.json",
])
def test_family_inventory_routes_to_workflows(path: str) -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    selected = {domain.name for domain in detector.touched_domains([path], domains)}
    assert selected == {"workflows"}


@pytest.mark.parametrize(("path", "expected"), [
    ("src/digitalmodel/orcaflex/installation_analysis.py",
     {"orcaflex", "marine-ops-other", "workflows"}),
    ("src/digitalmodel/orcaflex/source.py", {"orcaflex"}),
])
def test_shared_skew_basis_routes(path: str, expected: set[str]) -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    selected = {domain.name for domain in detector.touched_domains([path], domains)}
    assert selected == expected


def run_detector(
    repo: Path, domains_file: Path, *args: str
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            sys.executable,
            "-I",
            "-S",
            str(SCRIPT),
            "--domains-file",
            str(domains_file),
            *args,
        ],
        cwd=repo,
        check=False,
        text=True,
        capture_output=True,
    )


def init_repo(path: Path) -> None:
    subprocess.run(["git", "init"], cwd=path, check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", "codex@example.invalid"],
        cwd=path,
        check=True,
    )
    subprocess.run(
        ["git", "config", "user.name", "Codex"],
        cwd=path,
        check=True,
    )


def commit_all(repo: Path, message: str) -> str:
    subprocess.run(["git", "add", "."], cwd=repo, check=True)
    subprocess.run(["git", "commit", "-m", message], cwd=repo, check=True)
    return subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=repo, text=True
    ).strip()


def write_domains(path: Path) -> None:
    path.write_text(
        textwrap.dedent(
            """\
            # Test Domains

            | Domain | Test roots | Purpose/deps/notes |
            | --- | --- | --- |
            | asset-integrity | `tests/asset_integrity/` | FFS and integrity tests. |
            | citations | `tests/citations/` | Citation registry tests. |
            | structural | `tests/structural/`, `tests/test_wall_thickness.py` | Structural tests. |
            | workflows | `tests/workflows/`, `tests/scripts/` | Workflow and CI harness tests. |
            """
        )
    )


def test_full_mode_outputs_all_domains(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "full",
        "--output-format",
        "json-matrix",
    )

    assert result.returncode == 0, result.stderr
    assert json.loads(result.stdout) == {
        "include": [
            {"domain": "asset-integrity", "runner": "ubuntu-latest"},
            {"domain": "citations", "runner": "ubuntu-latest"},
            {"domain": "structural", "runner": "ubuntu-latest"},
            {"domain": "workflows", "runner": "ubuntu-latest"},
        ]
    }


def test_touched_mode_outputs_single_domain(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    test_file = tmp_path / "tests" / "citations" / "test_registry.py"
    test_file.parent.mkdir(parents=True)
    test_file.write_text("def test_old():\n    assert True\n")
    base = commit_all(tmp_path, "base")

    test_file.write_text("def test_new():\n    assert True\n")
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "citations"


def test_known_src_change_outputs_matching_domain(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    source = tmp_path / "src" / "digitalmodel" / "citations" / "registry.py"
    source.parent.mkdir(parents=True)
    source.write_text("VALUE = 1\n")
    base = commit_all(tmp_path, "base")

    source.write_text("VALUE = 2\n")
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == ["citations"]


def test_shared_src_change_escalates_to_full_matrix(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    source = tmp_path / "src" / "digitalmodel" / "engine.py"
    source.parent.mkdir(parents=True)
    source.write_text("VALUE = 1\n")
    base = commit_all(tmp_path, "base")

    source.write_text("VALUE = 2\n")
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == [
        "asset-integrity",
        "citations",
        "structural",
        "workflows",
    ]


def test_no_domain_changes_output_empty_matrix(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    domains_file.write_text(
        textwrap.dedent(
            """\
            # Test Domains

            | Domain | Test roots | Purpose/deps/notes |
            | --- | --- | --- |
            | marine-engineering | `tests/marine_ops/marine_engineering/` | Marine engineering tests. |
            | workflows | `tests/workflows/`, `tests/scripts/` | Workflow and CI harness tests. |
            """
        )
    )
    init_repo(tmp_path)
    changed_paths = (
        tmp_path / "scripts" / "ci" / "detect_touched_domains.py",
        tmp_path
        / "tests"
        / "marine_ops"
        / "marine_engineering"
        / "visualization"
        / "test_no_regression_traces.py",
    )
    for changed_path in changed_paths:
        changed_path.parent.mkdir(parents=True, exist_ok=True)
        changed_path.write_text("VALUE = 1\n")
    base = commit_all(tmp_path, "base")

    for changed_path in changed_paths:
        changed_path.write_text("VALUE = 2\n")
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "json-matrix",
    )

    assert result.returncode == 0, result.stderr
    assert json.loads(result.stdout) == {"include": []}


def test_specific_test_override_wins_over_broad_domain_root(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    domains_file.write_text(
        textwrap.dedent(
            """\
            # Test Domains

            | Domain | Test roots | Purpose/deps/notes |
            | --- | --- | --- |
            | cathodic-protection | `tests/cathodic_protection/` | CP tests. |
            | specialized | `tests/specialized/` | Specialized tests. |
            """
        )
    )
    init_repo(tmp_path)
    test_file = (
        tmp_path / "tests" / "specialized" / "cathodic_protection" / "test_dnv.py"
    )
    test_file.parent.mkdir(parents=True)
    test_file.write_text("def test_old():\n    assert True\n")
    base = commit_all(tmp_path, "base")

    test_file.write_text("def test_new():\n    assert True\n")
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == ["cathodic-protection"]


def test_all_domain_path_mappings_have_regression_coverage(tmp_path: Path) -> None:
    detector = load_detector_module()
    mapped_domains = sorted(
        {domain for _path, domains in detector.DOMAIN_PATHS for domain in domains}
    )
    domains_file = tmp_path / "DOMAINS.md"
    domains_file.write_text(
        "# Test Domains\n\n"
        "| Domain | Test roots | Purpose/deps/notes |\n"
        "| --- | --- | --- |\n"
        + "\n".join(
            f"| {domain} | `tests/{domain}/` | {domain} tests. |"
            for domain in mapped_domains
        )
        + "\n"
    )

    for index, (domain_path, expected_domains) in enumerate(detector.DOMAIN_PATHS):
        repo = tmp_path / f"repo-{index}"
        repo.mkdir()
        init_repo(repo)
        changed_file = repo / domain_path
        if domain_path.endswith("/"):
            changed_file = changed_file / "test_touched.py"
        changed_file.parent.mkdir(parents=True)
        changed_file.write_text("VALUE = 1\n")
        base = commit_all(repo, "base")

        changed_file.write_text("VALUE = 2\n")
        head = commit_all(repo, "head")

        result = run_detector(
            repo,
            domains_file,
            "--mode",
            "touched",
            "--base",
            base,
            "--head",
            head,
            "--output-format",
            "list",
        )

        assert result.returncode == 0, result.stderr
        assert result.stdout.splitlines() == [
            domain for domain in mapped_domains if domain in expected_domains
        ]


def test_all_mapped_domain_names_exist_in_real_domains_file() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(Path("tests/DOMAINS.md"))
    domain_names = {domain.name for domain in domains}
    mapped_domain_names = {
        domain
        for _path, domains_for_path in detector.DOMAIN_PATHS
        for domain in domains_for_path
    }

    assert mapped_domain_names <= domain_names


def test_config_change_escalates_to_full_matrix(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    config = tmp_path / ".claude" / "quality-gates.yaml"
    config.parent.mkdir(parents=True)
    config.write_text("gates: {}\n")
    base = commit_all(tmp_path, "base")

    config.write_text("gates:\n  tests: {}\n")
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "json-matrix",
    )

    assert result.returncode == 0, result.stderr
    assert [item["domain"] for item in json.loads(result.stdout)["include"]] == [
        "asset-integrity",
        "citations",
        "structural",
        "workflows",
    ]


def test_pyproject_package_data_change_outputs_matching_domain(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    domains_file.write_text(
        textwrap.dedent(
            """\
            # Test Domains

            | Domain | Test roots | Purpose/deps/notes |
            | --- | --- | --- |
            | orcaflex | `tests/orcaflex/` | OrcaFlex tests. |
            | structural | `tests/structural/` | Structural tests. |
            | workflows | `tests/workflows/`, `tests/scripts/` | Workflow tests. |
            """
        )
    )
    init_repo(tmp_path)
    config = tmp_path / "pyproject.toml"
    config.write_text(
        textwrap.dedent(
            """\
            [tool.setuptools.package-data]
            digitalmodel = ["subsea/cross_sections/fixtures/*.yml"]
            """
        )
    )
    base = commit_all(tmp_path, "base")

    config.write_text(
        textwrap.dedent(
            """\
            [tool.setuptools.package-data]
            digitalmodel = [
                "orcaflex/data/*.yml",
                "subsea/cross_sections/fixtures/*.yml",
            ]
            """
        )
    )
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == ["orcaflex"]


def test_unknown_pyproject_package_data_change_escalates_to_full_matrix(
    tmp_path: Path,
) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    config = tmp_path / "pyproject.toml"
    config.write_text(
        textwrap.dedent(
            """\
            [tool.setuptools.package-data]
            digitalmodel = ["subsea/cross_sections/fixtures/*.yml"]
            """
        )
    )
    base = commit_all(tmp_path, "base")

    config.write_text(
        textwrap.dedent(
            """\
            [tool.setuptools.package-data]
            digitalmodel = [
                "new_domain/data/*.yml",
                "subsea/cross_sections/fixtures/*.yml",
            ]
            """
        )
    )
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == [
        "asset-integrity",
        "citations",
        "structural",
        "workflows",
    ]


def test_non_package_data_pyproject_change_escalates_to_full_matrix(
    tmp_path: Path,
) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    config = tmp_path / "pyproject.toml"
    config.write_text(
        textwrap.dedent(
            """\
            [tool.black]
            line-length = 88
            """
        )
    )
    base = commit_all(tmp_path, "base")

    config.write_text(
        textwrap.dedent(
            """\
            [tool.black]
            line-length = 100
            """
        )
    )
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == [
        "asset-integrity",
        "citations",
        "structural",
        "workflows",
    ]


def test_unmapped_change_outputs_empty_matrix(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    readme = tmp_path / "README.md"
    readme.write_text("old\n")
    base = commit_all(tmp_path, "base")

    readme.write_text("new\n")
    head = commit_all(tmp_path, "head")

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "json-matrix",
    )

    assert result.returncode == 0, result.stderr
    assert json.loads(result.stdout) == {"include": []}


def test_quality_gate_workflows_parse() -> None:
    for workflow in (
        Path(".github/workflows/quality-gates.yml"),
        Path(".github/workflows/quality-gates-by-domain.yml"),
    ):
        assert yaml.safe_load(workflow.read_text())


BOGUS_SHA = "0123456789abcdef0123456789abcdef01234567"
ZERO_SHA = "0000000000000000000000000000000000000000"


def _repo_with_single_domain_change(tmp_path: Path) -> tuple[Path, str, str]:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    test_file = tmp_path / "tests" / "citations" / "test_registry.py"
    test_file.parent.mkdir(parents=True)
    test_file.write_text("def test_old():\n    assert True\n")
    base = commit_all(tmp_path, "base")
    test_file.write_text("def test_new():\n    assert True\n")
    head = commit_all(tmp_path, "head")
    return domains_file, base, head


def test_touched_mode_unreachable_base_errors_by_default(tmp_path: Path) -> None:
    """Without --on-missing-base, an unreachable base is still a hard error (exit 2)."""
    domains_file, _base, head = _repo_with_single_domain_change(tmp_path)

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        BOGUS_SHA,
        "--head",
        head,
        "--output-format",
        "json-matrix",
    )

    assert result.returncode == 2, result.stdout
    assert "detect_touched_domains.py" in result.stderr


def test_touched_mode_unreachable_base_falls_back_to_full(tmp_path: Path) -> None:
    """--on-missing-base full makes an unreachable base fail SAFE to the full matrix."""
    domains_file, _base, head = _repo_with_single_domain_change(tmp_path)

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        BOGUS_SHA,
        "--head",
        head,
        "--on-missing-base",
        "full",
        "--output-format",
        "json-matrix",
    )

    assert result.returncode == 0, result.stderr
    assert [item["domain"] for item in json.loads(result.stdout)["include"]] == [
        "asset-integrity",
        "citations",
        "structural",
        "workflows",
    ]


def test_zero_sha_base_falls_back_to_full(tmp_path: Path) -> None:
    """New-branch zero-SHA base fails SAFE to the full matrix under the flag."""
    domains_file, _base, head = _repo_with_single_domain_change(tmp_path)

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        ZERO_SHA,
        "--head",
        head,
        "--on-missing-base",
        "full",
        "--output-format",
        "json-matrix",
    )

    assert result.returncode == 0, result.stderr
    assert [item["domain"] for item in json.loads(result.stdout)["include"]] == [
        "asset-integrity",
        "citations",
        "structural",
        "workflows",
    ]


def test_on_missing_base_full_does_not_alter_happy_path(tmp_path: Path) -> None:
    """The flag is inert when the base is reachable: still scopes to the touched domain."""
    domains_file, base, head = _repo_with_single_domain_change(tmp_path)

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--on-missing-base",
        "full",
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == ["citations"]


def test_push_like_pr_scopes_single_domain(tmp_path: Path) -> None:
    """A single-domain push diff routes to one leg, mirroring the PR touched path."""
    domains_file, base, head = _repo_with_single_domain_change(tmp_path)

    result = run_detector(
        tmp_path,
        domains_file,
        "--mode",
        "touched",
        "--base",
        base,
        "--head",
        head,
        "--output-format",
        "list",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == ["citations"]


def test_workflow_routes_push_to_touched_mode() -> None:
    """Structural guard: the workflow routes push -> touched (--on-missing-base full),
    and --mode full is reachable only from the schedule/dispatch (else) arm."""
    workflow_text = Path(
        ".github/workflows/quality-gates-by-domain.yml"
    ).read_text()

    # push routes to touched mode with the fail-safe flag
    assert 'EVENT_NAME" = "push"' in workflow_text
    assert "--on-missing-base full" in workflow_text

    # before/after SHAs are wired into the detect step env
    assert "github.event.before" in workflow_text
    assert "github.event.after" in workflow_text

    # the --mode full arm must not be reached from the push branch: it lives in the
    # final else (schedule + workflow_dispatch). Assert the push arm precedes full.
    push_idx = workflow_text.index('EVENT_NAME" = "push"')
    full_idx = workflow_text.index("--mode full")
    assert push_idx < full_idx, "push arm must precede the --mode full (else) arm"


def test_rig_transcription_selects_existing_contracts_shard() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(Path("tests/DOMAINS.md"))
    module_path = Path("tests/contracts/test_udw_rig_fleet_external.py")
    assert module_path.is_file()
    selected = detector.touched_domains([module_path.as_posix()], domains)
    assert [domain.name for domain in selected] == ["contracts"]
    assert any(detector.path_matches_root(module_path.as_posix(), root)
               for root in selected[0].roots)


def test_structure_tests_select_contracts_shard() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(Path("tests/DOMAINS.md"))
    selected = detector.touched_domains(
        ["tests/repo_structure/test_repo_structure_contract.py"], domains
    )
    assert [domain.name for domain in selected] == ["contracts"]


def test_structure_config_selects_contracts_shard() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(Path("tests/DOMAINS.md"))
    selected = detector.touched_domains(["config/repo_structure.yml"], domains)
    assert [domain.name for domain in selected] == ["contracts"]


def test_structure_validator_selects_contracts_shard() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(Path("tests/DOMAINS.md"))
    selected = detector.touched_domains(
        ["scripts/maintenance/verify_repo_structure.py"], domains
    )
    assert [domain.name for domain in selected] == ["contracts"]


def test_package_document_without_pyyaml_selects_full_matrix(tmp_path: Path) -> None:
    """A dependency-free subprocess must retain all shards for package docs."""
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    pack = tmp_path / "src/digitalmodel/cathodic_protection/docs/agent-pack.md"
    pack.parent.mkdir(parents=True)
    pack.write_text("Initial documentation\n")
    base = commit_all(tmp_path, "base")
    pack.write_text("Updated documentation\n")
    head = commit_all(tmp_path, "head")
    result = run_detector(
        tmp_path, domains_file, "--mode", "touched", "--base", base,
        "--head", head, "--output-format", "list",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == [
        "asset-integrity", "citations", "structural", "workflows",
    ]


def registry_reader_shards(root: Path, detector) -> dict[str, set[str]]:
    """Resolve registry references that are covered by actual gate commands."""
    domains = detector.parse_domains(root / "tests/DOMAINS.md")
    gates = yaml.safe_load((root / ".claude/quality-gates.yaml").read_text())["gates"]
    executed = {
        token.split("::", 1)[0]
        for gate in gates.values()
        for token in shlex.split(gate.get("command", ""))
        if token.startswith("tests/")
    }
    executed = {
        item.rstrip("/") + "/" if (root / item).is_dir() else item
        for item in executed
    }
    references = re.compile(
        r"""docs/registry|module-routing|workflows\.yaml|['"]docs['"]\s*/\s*['"]registry['"]"""
    )
    readers = {}
    for path in (root / "tests").rglob("*.py"):
        relative = path.relative_to(root).as_posix()
        if not references.search(path.read_text(encoding="utf-8")):
            continue
        if not any(detector.path_matches_root(relative, item) for item in executed):
            continue
        owners = {
            domain.name for domain in domains for test_root in domain.roots
            if detector.path_matches_root(relative, test_root)
        }
        assert owners, f"Registry reader has no domain: {relative}"
        readers[relative] = owners
    return readers


def assert_registry_readers_routed(readers: dict[str, set[str]], routed: set[str]) -> None:
    """Require every discovered reader's shard in the registry route."""
    for reader, owners in readers.items():
        assert owners <= routed, f"{reader}: missing {sorted(owners - routed)}"


def test_registry_route_covers_gate_readers() -> None:
    """Check registry readers; roots run by no gate command are not asserted."""
    detector = load_detector_module()
    readers = registry_reader_shards(SCRIPT.parents[2], detector)
    assert readers
    routed = detector.mapped_domain_names("docs/registry/workflows.yaml")
    assert_registry_readers_routed(readers, routed)


@pytest.mark.parametrize("gate_root", ["tests/structural/", "tests/structural"])
@pytest.mark.parametrize("reference", [
    'ROOT / "docs/registry/extra.yaml"',
    'ROOT / "docs" / "registry" / "extra.yaml"',
    'ROOT / "workflows.yaml"',
    'ROOT / "module-routing.yaml"',
])
def test_registry_reader_guard_detects_missing_shard(
    tmp_path: Path, reference: str, gate_root: str,
) -> None:
    """Calibrate each spelling against a reader outside the current route."""
    detector = load_detector_module()
    tests = tmp_path / "tests"
    tests.mkdir()
    write_domains(tests / "DOMAINS.md")
    gates = tmp_path / ".claude"
    gates.mkdir()
    (gates / "quality-gates.yaml").write_text(
        f"gates:\n  tests-structural:\n    command: python -m pytest {gate_root}\n"
    )
    reader = tests / "structural/test_registry_reader.py"
    reader.parent.mkdir()
    reader.write_text(f"REGISTRY = {reference}\n")
    readers = registry_reader_shards(tmp_path, detector)
    assert readers == {"tests/structural/test_registry_reader.py": {"structural"}}
    routed = detector.mapped_domain_names("docs/registry/workflows.yaml")
    with pytest.raises(AssertionError, match="missing.*structural"):
        assert_registry_readers_routed(readers, routed)


# Record pins add contract coverage without replacing existing domain routes.
def test_pinned_source_adds_contracts_with_old_route_calibration() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    path = "src/digitalmodel/orcaflex/mooring_design.py"
    assert {d.name for d in detector.touched_domains(
        [path], domains, pinned_paths={path}
    )} == {"contracts", "orcaflex"}
    assert {d.name for d in detector.touched_domains(
        [path], domains, pinned_paths=set()
    )} == {"orcaflex"}


def test_unpinned_source_keeps_own_shard() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    selected = detector.touched_domains(
        ["src/digitalmodel/orcaflex/other.py"], domains,
        pinned_paths={"src/digitalmodel/orcaflex/mooring_design.py"},
    )
    assert {d.name for d in selected} == {"orcaflex"}


def test_replay_pr_2481_changed_paths() -> None:
    """Replay the committed 15-path diff with and without record routing."""
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    paths = json.loads((Path(__file__).parent / "fixtures/pr_2481_changed_paths.json").read_text())
    assert len(paths) == 15
    assert {d.name for d in detector.touched_domains(
        paths, domains, pinned_paths=detector.git_pinned_paths("HEAD"),
        pinned_domains=detector.git_pinned_domains("HEAD", domains),
    )} == {"contracts", "orcaflex", "workflows"}
    assert {d.name for d in detector.touched_domains(
        paths, domains, pinned_paths=set(),
    )} == {"orcaflex", "workflows"}


def test_domain_record_document_selects_contracts() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    selected = detector.touched_domains(["docs/domains/new-record.md"], domains)
    assert {d.name for d in selected} == {"contracts"}


def test_extract_manifest_pins_reads_nested_shapes_and_relative_paths() -> None:
    detector = load_detector_module()
    tracked = {"src/source.py", "inputs/data.json", "records/output.csv",
               "records/local.txt", "local.txt"}
    payload = {
        "source_files": {"src/source.py": "a" * 64},
        "nested": [{"record_inputs": {"inputs/data.json": "B" * 64}}],
        "files": {"records/output.csv": {"sha256": "c" * 64, "bytes": 10}},
        "sha256": {"./output.csv": "d" * 64, "local.txt": "e" * 64},
        "absent.txt": "f" * 64,
        "records/local.txt": "g" * 64,
    }
    assert detector.extract_manifest_pins(
        payload, "records/manifest.json", tracked
    ) == {"src/source.py", "inputs/data.json", "records/output.csv", "local.txt",
          "records/local.txt"}


def test_git_pinned_paths_discovers_manifest_names_and_shapes(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch,
) -> None:
    detector = load_detector_module()
    init_repo(tmp_path)
    for path in ("src/source.py", "inputs/data.json", "records/output.csv"):
        target = tmp_path / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text("retained\n")
    manifests = {
        "manifest.json": {"source_files": {"src/source.py": "a" * 64}},
        "records/basis.json": {
            "nested": [{"record_inputs": {"inputs/data.json": "b" * 64}}],
            "files": {"src/source.py": {"sha256": "c" * 64}},
        },
        "records/new.manifest.json": {"sha256": {"output.csv": "d" * 64}},
        "ignored.json": {"absent.txt": "e" * 64},
    }
    for path, payload in manifests.items():
        (tmp_path / path).write_text(json.dumps(payload))
    ref = commit_all(tmp_path, "record fixtures")
    (tmp_path / "manifest.json").write_text("{}")
    monkeypatch.chdir(tmp_path)
    assert detector.git_pinned_paths(ref) == {
        "src/source.py", "inputs/data.json", "records/output.csv",
    }


def test_git_pinned_paths_skips_invalid_json_with_notice(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture,
) -> None:
    detector = load_detector_module()
    init_repo(tmp_path)
    (tmp_path / "manifest.json").write_text("not JSON\n")
    (tmp_path / "basis.json").write_text(json.dumps({"basis.json": "a" * 64}))
    ref = commit_all(tmp_path, "invalid manifest fixture")
    monkeypatch.chdir(tmp_path)
    assert detector.git_pinned_paths(ref) == {"basis.json"}
    notice = capsys.readouterr().err
    assert len(notice.splitlines()) == 1
    assert "manifest.json" in notice and "JSON" in notice


def test_git_pinned_paths_skips_oversized_manifest_with_notice(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture,
) -> None:
    detector = load_detector_module()
    init_repo(tmp_path)
    payload = {"manifest.json": "a" * 64, "padding": "x" * (5 * 1024 * 1024)}
    (tmp_path / "manifest.json").write_text(json.dumps(payload))
    ref = commit_all(tmp_path, "large manifest fixture")
    monkeypatch.chdir(tmp_path)
    assert detector.git_pinned_paths(ref) == set()
    notice = capsys.readouterr().err
    assert len(notice.splitlines()) == 1
    assert "manifest.json" in notice and "5 MB" in notice


def write_pin_domains(path: Path) -> None:
    path.write_text(
        "| Domain | Test roots |\n| --- | --- |\n"
        "| contracts | `tests/docs/` |\n"
        "| citations | `tests/citations/` |\n"
        "| orcaflex | `tests/orcaflex/` |\n"
    )


def test_new_manifest_pin_routes_without_detector_change(tmp_path: Path) -> None:
    init_repo(tmp_path)
    domains_file = tmp_path / "DOMAINS.md"
    write_pin_domains(domains_file)
    source = tmp_path / "src/digitalmodel/citations/new_source.py"
    source.parent.mkdir(parents=True)
    source.write_text("VALUE = 1\n")
    base = commit_all(tmp_path, "source fixture")
    record = tmp_path / "records/new.manifest.json"
    record.parent.mkdir()
    record.write_text(json.dumps({source.relative_to(tmp_path).as_posix(): "a" * 64}))
    reader = tmp_path / "tests/docs/test_record.py"
    reader.parent.mkdir(parents=True)
    reader.write_text('RECORD = "records/new.manifest.json"\n')
    source.write_text("VALUE = 2\n")
    head = commit_all(tmp_path, "new record fixture")
    result = run_detector(
        tmp_path, domains_file, "--mode", "touched", "--base", base,
        "--head", head, "--output-format", "list",
    )
    assert result.returncode == 0, result.stderr
    assert set(result.stdout.splitlines()) == {"contracts", "citations"}


def test_base_only_pin_still_routes_after_removal(tmp_path: Path) -> None:
    init_repo(tmp_path)
    domains_file = tmp_path / "DOMAINS.md"
    write_pin_domains(domains_file)
    source = tmp_path / "src/digitalmodel/orcaflex/source.py"
    source.parent.mkdir(parents=True)
    source.write_text("VALUE = 1\n")
    record = tmp_path / "basis.json"
    record.write_text(json.dumps({source.relative_to(tmp_path).as_posix(): "a" * 64}))
    reader = tmp_path / "tests/docs/test_record.py"
    reader.parent.mkdir(parents=True)
    reader.write_text('RECORD = "basis.json"\n')
    base = commit_all(tmp_path, "pinned source fixture")
    record.write_text("{}")
    source.write_text("VALUE = 2\n")
    head = commit_all(tmp_path, "removed pin fixture")
    result = run_detector(
        tmp_path, domains_file, "--mode", "touched", "--base", base,
        "--head", head, "--output-format", "list",
    )
    assert result.returncode == 0, result.stderr
    assert set(result.stdout.splitlines()) == {"contracts", "orcaflex"}


def test_pinned_no_domain_path_selects_contracts() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    path = "scripts/ci/detect_touched_domains.py"
    assert detector.is_no_domain_path(path)
    selected = detector.touched_domains([path], domains, pinned_paths={path})
    assert {d.name for d in selected} == {"contracts"}


@pytest.mark.parametrize("path", ["src/digitalmodel/orcaflex/source.py",
                                  "docs/domains/new-record.md"])
def test_record_route_without_contracts_selects_full_matrix(path: str) -> None:
    detector = load_detector_module()
    domains = [detector.Domain("orcaflex", ("tests/orcaflex/",)),
               detector.Domain("structural", ("tests/structural/",))]
    assert detector.touched_domains([path], domains, pinned_paths={path}) == domains


def test_real_manifest_pins_exist_and_select_contracts(monkeypatch: pytest.MonkeyPatch) -> None:
    detector = load_detector_module()
    root = SCRIPT.parents[2]
    monkeypatch.chdir(root)
    tracked = set(subprocess.check_output(
        ["git", "ls-tree", "-r", "--name-only", "-z", "HEAD"], text=True,
    ).rstrip("\0").split("\0"))
    working_pins = set()
    records = {}
    for path in tracked:
        if Path(path).name.lower() in {"manifest.json", "basis.json"} or path.lower().endswith(".manifest.json"):
            pins = detector.extract_manifest_pins(
                json.loads((root / path).read_text()), path, tracked,
            )
            working_pins.update(pins)
            if pins:
                records[path] = pins
    assert working_pins
    assert working_pins == detector.git_pinned_paths("HEAD")
    domains = detector.parse_domains(root / "tests/DOMAINS.md")
    routes = detector.git_pinned_domains("HEAD", domains)
    all_names = {d.name for d in domains}
    readers = {
        path: detector.git_file_text("HEAD", path) for path in tracked
        if path.startswith("tests/") and path.endswith(".py")
        and (Path(path).name.startswith("test_") or path.endswith("_test.py"))
    }
    for manifest, pins in records.items():
        folder = str(Path(manifest).parent) if "/" in manifest else manifest
        owning_tests = [path for path, text in readers.items() if folder in text]
        expected = {"contracts"}
        for test in owning_tests:
            owners = detector.mapped_domain_names(test) or {
                d.name for d in domains for test_root in d.roots
                if detector.path_matches_root(test, test_root)
            }
            expected.update(owners or all_names)
        if not owning_tests:
            expected = all_names
        for path in pins | {manifest}:
            assert expected <= routes[path], (path, expected - routes[path])
            selected = {d.name for d in detector.touched_domains(
                [path], domains, pinned_domains=routes,
            )}
            assert expected <= selected, (path, owning_tests, expected - selected)
    for path in sorted(working_pins):
        assert (root / path).is_file(), path
        selected = detector.touched_domains([path], domains, pinned_paths=working_pins)
        assert "contracts" in {d.name for d in selected}, path


@pytest.mark.parametrize("operation", ["ls-tree", "show", "grep", "cat-file"])
def test_pin_git_failure_selects_full_matrix(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture, operation: str,
) -> None:
    detector = load_detector_module()
    init_repo(tmp_path)
    domains_file = tmp_path / "DOMAINS.md"
    write_pin_domains(domains_file)
    source = tmp_path / "tests/citations/test_source.py"
    source.parent.mkdir(parents=True)
    source.write_text("VALUE = 1\n")
    (tmp_path / "manifest.json").write_text(json.dumps({
        source.relative_to(tmp_path).as_posix(): "a" * 64,
    }))
    base = commit_all(tmp_path, "base fixture")
    source.write_text("VALUE = 2\n")
    head = commit_all(tmp_path, "head fixture")
    real_run = subprocess.run

    def fail_pin_read(args, **kwargs):
        if args[:2] == ["git", operation]:
            raise subprocess.CalledProcessError(1, args)
        return real_run(args, **kwargs)

    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(detector.subprocess, "run", fail_pin_read)
    assert detector.main([
        "--domains-file", str(domains_file), "--mode", "touched",
        "--base", base, "--head", head, "--output-format", "list",
    ]) == 0
    output = capsys.readouterr()
    assert set(output.out.splitlines()) == {"contracts", "citations", "orcaflex"}
    assert "pin" in output.err and "full matrix" in output.err


@pytest.mark.parametrize("rename", [False, True])
def test_pinned_input_routes_to_reader_shard(
    tmp_path: Path, rename: bool,
) -> None:
    """An edit or rename retains both contract and non-contract reader tests."""
    init_repo(tmp_path)
    domains_file = tmp_path / "DOMAINS.md"
    write_pin_domains(domains_file)
    source = tmp_path / "src/digitalmodel/orcaflex/source.py"
    source.parent.mkdir(parents=True)
    source.write_text("VALUE = 1\n")
    record = tmp_path / "records/manifest.json"
    record.parent.mkdir()
    record.write_text(json.dumps({source.relative_to(tmp_path).as_posix(): "a" * 64}))
    reader = tmp_path / "tests/citations/test_record.py"
    reader.parent.mkdir(parents=True)
    reader.write_text('RECORD = "records/manifest.json"\n')
    base = commit_all(tmp_path, "record fixture")
    if rename:
        subprocess.run(
            ["git", "mv", str(source.relative_to(tmp_path)),
             str(source.with_name("moved.py").relative_to(tmp_path))],
            cwd=tmp_path, check=True,
        )
    else:
        source.write_text("VALUE = 2\n")
    head = commit_all(tmp_path, "changed input fixture")
    result = run_detector(
        tmp_path, domains_file, "--mode", "touched", "--base", base,
        "--head", head, "--output-format", "list",
    )
    assert result.returncode == 0, result.stderr
    assert set(result.stdout.splitlines()) == {"contracts", "citations", "orcaflex"}


def test_reliability_input_selects_structural() -> None:
    detector = load_detector_module()
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    routes = detector.git_pinned_domains("HEAD", domains)
    path = "docs/domains/asset-integrity/reliability-benchmark/criteria.json"
    assert {"contracts", "structural"} <= {d.name for d in detector.touched_domains(
        [path], domains, pinned_domains=routes,
    )}


@pytest.mark.parametrize("change_manifest", [False, True])
def test_record_without_reader_selects_full_matrix(
    tmp_path: Path, change_manifest: bool,
) -> None:
    init_repo(tmp_path)
    domains_file = tmp_path / "DOMAINS.md"
    write_pin_domains(domains_file)
    record = tmp_path / "records/manifest.json"
    record.parent.mkdir()
    source = tmp_path / "input.json"
    source.write_text("{}")
    record.write_text(json.dumps({"input.json": "a" * 64}))
    base = commit_all(tmp_path, "unowned record fixture")
    if change_manifest:
        record.write_text(json.dumps({"input.json": "b" * 64}))
    else:
        source.write_text("[]")
    head = commit_all(tmp_path, "changed record fixture")
    result = run_detector(
        tmp_path, domains_file, "--mode", "touched", "--base", base,
        "--head", head, "--output-format", "list",
    )
    assert result.returncode == 0, result.stderr
    assert set(result.stdout.splitlines()) == {"contracts", "citations", "orcaflex"}


def test_extract_manifest_pins_keeps_ambiguous_candidates() -> None:
    detector = load_detector_module()
    tracked = {"output.csv", "records/output.csv"}
    assert detector.extract_manifest_pins(
        {"output.csv": "a" * 64}, "records/manifest.json", tracked,
    ) == tracked


def test_uppercase_path_valued_manifest(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    detector = load_detector_module()
    init_repo(tmp_path)
    record = tmp_path / "records/MANIFEST.json"
    record.parent.mkdir()
    (record.parent / "input.json").write_text("{}")
    record.write_text(json.dumps({"files": [{"path": "input.json", "sha256": "a" * 64}]}))
    ref = commit_all(tmp_path, "path-valued manifest fixture")
    monkeypatch.chdir(tmp_path)
    assert detector.git_pinned_paths(ref) == {"records/input.json"}


def test_git_changed_files_keeps_unicode_and_both_rename_paths(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch,
) -> None:
    detector = load_detector_module()
    init_repo(tmp_path)
    source = tmp_path / "retained-é.txt"
    source.write_text("retained\n")
    base = commit_all(tmp_path, "unicode source fixture")
    destination = source.with_name("moved-é.txt")
    subprocess.run(["git", "mv", source.name, destination.name], cwd=tmp_path, check=True)
    head = commit_all(tmp_path, "unicode rename fixture")
    monkeypatch.chdir(tmp_path)
    assert set(detector.git_changed_files(base, head)) == {source.name, destination.name}



def test_oversized_manifest_is_not_read(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture,
) -> None:
    detector = load_detector_module()
    init_repo(tmp_path)
    (tmp_path / "manifest.json").write_text(" " * (detector.MAX_MANIFEST_BYTES + 1))
    ref = commit_all(tmp_path, "oversized fixture")
    monkeypatch.chdir(tmp_path)

    def reject_read(*args):
        pytest.fail("oversized manifest blob was read")

    monkeypatch.setattr(detector, "git_file_text", reject_read)
    assert detector.git_pinned_paths(ref) == set()
    assert "5 MB" in capsys.readouterr().err


@pytest.mark.parametrize("reader_root", ["citations", "unowned"])
def test_manifest_edit_routes_reader_or_full_matrix(
    tmp_path: Path, reader_root: str,
) -> None:
    init_repo(tmp_path)
    domains_file = tmp_path / "DOMAINS.md"
    write_pin_domains(domains_file)
    source = tmp_path / "input.json"
    source.write_text("{}")
    record = tmp_path / "records/manifest.json"
    record.parent.mkdir()
    record.write_text(json.dumps({source.name: "a" * 64}))
    reader = tmp_path / "tests" / reader_root / "test_record.py"
    reader.parent.mkdir(parents=True)
    reader.write_text('RECORD = "records/manifest.json"\n')
    base = commit_all(tmp_path, "owned manifest fixture")
    record.write_text(json.dumps({source.name: "b" * 64}))
    head = commit_all(tmp_path, "changed digest fixture")
    result = run_detector(
        tmp_path, domains_file, "--mode", "touched", "--base", base,
        "--head", head, "--output-format", "list",
    )
    assert result.returncode == 0, result.stderr
    expected = {"contracts", "citations"}
    if reader_root == "unowned":
        expected.add("orcaflex")
    assert set(result.stdout.splitlines()) == expected


def test_record_reader_routes_union_base_and_head(tmp_path: Path) -> None:
    init_repo(tmp_path)
    domains_file = tmp_path / "DOMAINS.md"
    write_pin_domains(domains_file)
    source = tmp_path / "input.json"
    source.write_text("{}")
    record = tmp_path / "records/manifest.json"
    record.parent.mkdir()
    record.write_text(json.dumps({source.name: "a" * 64}))
    reader = tmp_path / "tests/citations/test_record.py"
    reader.parent.mkdir(parents=True)
    reader.write_text('RECORD = "records/manifest.json"\n')
    base = commit_all(tmp_path, "base reader fixture")
    reader.write_text("# Reader removed\n")
    replacement = tmp_path / "tests/docs/test_record.py"
    replacement.parent.mkdir(parents=True)
    replacement.write_text('RECORD = "records/manifest.json"\n')
    source.write_text("[]")
    head = commit_all(tmp_path, "head reader fixture")
    result = run_detector(
        tmp_path, domains_file, "--mode", "touched", "--base", base,
        "--head", head, "--output-format", "list",
    )
    assert result.returncode == 0, result.stderr
    assert set(result.stdout.splitlines()) == {"contracts", "citations"}


def test_bha_database_owned_and_executed_by_subsea():
    """Ownership alone must not leave the acceptance suite unexecuted."""
    root = SCRIPT.parents[2]
    detector = load_detector_module()
    domains = detector.parse_domains(root / "tests/DOMAINS.md")
    for path in ("tests/bha_database/test_database.py", "data/bha_database/examples.yml",
                 "data/bha_database/manifest.yaml", "data/bha_standards/drill_collars_std.yml",
                 "scripts/bha_database/build_manifest.py",
                 "docs/bha_database.md"):
        selected = detector.touched_domains([path], domains)
        assert {domain.name for domain in selected} == {"subsea"}, path
    selected = detector.touched_domains(["src/digitalmodel/bha_database/schema.py"], domains)
    assert "subsea" in {domain.name for domain in selected}
    subsea = next(domain for domain in domains if domain.name == "subsea")
    assert "tests/bha_database/" in subsea.roots
    gates = yaml.safe_load((root / ".claude/quality-gates.yaml").read_text())["gates"]
    assert "tests/bha_database/" in shlex.split(gates["tests-subsea"]["command"])
    assert "tests/bha_database/" in gates["tests-subsea"]["test_roots"]
