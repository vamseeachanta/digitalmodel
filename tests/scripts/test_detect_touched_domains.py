import importlib.util
import json
import subprocess
import sys
import textwrap
from pathlib import Path

import yaml


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/ci/detect_touched_domains.py"


def load_detector_module():
    spec = importlib.util.spec_from_file_location("detect_touched_domains", SCRIPT)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def run_detector(repo: Path, domains_file: Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            "uv",
            "run",
            "python",
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
    return subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip()


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
    test_file = tmp_path / "tests" / "specialized" / "cathodic_protection" / "test_dnv.py"
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
        domain for _path, domains_for_path in detector.DOMAIN_PATHS for domain in domains_for_path
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
