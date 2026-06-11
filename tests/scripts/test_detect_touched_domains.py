import json
import subprocess
import textwrap
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/ci/detect_touched_domains.py"


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


def test_detector_harness_change_outputs_empty_matrix(tmp_path: Path) -> None:
    domains_file = tmp_path / "DOMAINS.md"
    write_domains(domains_file)
    init_repo(tmp_path)
    script = tmp_path / "scripts" / "ci" / "detect_touched_domains.py"
    script.parent.mkdir(parents=True)
    script.write_text("VALUE = 1\n")
    base = commit_all(tmp_path, "base")

    script.write_text("VALUE = 2\n")
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
