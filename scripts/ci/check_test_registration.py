# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reconcile test-file enrollment without importing or collecting test modules."""

from __future__ import annotations

import argparse
import ast
import configparser
import fnmatch
import glob
import json
import re
import shlex
import sys
from pathlib import Path
from typing import Any

import yaml

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from scripts.ci import detect_touched_domains as detector


def expand_root(
    root: Path,
    pattern: str,
    cache: dict[str, list[tuple[str, bool]]] | None = None,
) -> list[tuple[str, bool]]:
    """Resolve a test root to normalized paths and their directory status."""
    pattern = pattern.removeprefix("./")
    if Path(pattern).is_absolute() or ".." in Path(pattern).parts:
        raise ValueError("Test roots must be repository-relative")
    if not glob.has_magic(pattern):
        pattern = pattern.rstrip("/")
    if any(char in pattern for char in "{}()") or any(
        expression in pattern for expression in ("[^", "[:", "[.", "[=")
    ):
        raise ValueError("Unsupported shell pattern in test root")
    cache = {} if cache is None else cache
    if pattern not in cache:
        candidates = (
            root / item for item in glob.glob(pattern, root_dir=root, recursive=False)
        )
        cache[pattern] = [
            (item.relative_to(root).as_posix(), item.is_dir()) for item in candidates
        ]
    return cache[pattern]


def matches(
    root: Path,
    path: str,
    pattern: str,
    cache: dict[str, list[tuple[str, bool]]] | None = None,
) -> bool:
    """Match a shell-expanded directory or file, without prefix collisions."""
    return any(
        path == item or (is_dir and path.startswith(item + "/"))
        for item, is_dir in expand_root(root, pattern, cache)
    )


def validate_shell_expansion(command: str, tokens: list[str], start: int) -> None:
    """Permit unquoted expansion only for positional pytest test roots."""
    value_options = {
        "--ignore",
        "--ignore-glob",
        "-p",
        "--tb",
        "--junitxml",
        "--import-mode",
    }
    raw_tokens = re.findall(r"""(?:[^\s'"]+|'[^']*'|"[^"]*")+""", command)
    for index, raw in enumerate(raw_tokens):
        is_root = (
            index >= start
            and tokens[index].startswith(("tests/", "./tests/"))
            and (index == start or tokens[index - 1] not in value_options)
        )
        quoted = re.findall(r"'[^']*'|\"[^\"]*\"", raw)
        if is_root and any(char in part for part in quoted for char in "*?["):
            raise ValueError("Quoted wildcard test roots need explicit audit support")
        unquoted = re.sub(r"'[^']*'|\"[^\"]*\"", "", raw)
        if not is_root and any(char in unquoted for char in "*?[~"):
            raise ValueError("Shell expansion outside test roots is unsupported")


def pytest_tokens(command: str) -> list[str]:
    """Validate the executable invocation before reading pytest arguments."""
    command = re.sub(
        r'(?<!\S)--with[ \t]+"assetutilities @ git\+https://github\.com/'
        r'\$\{GITHUB_REPOSITORY%/\*\}/assetutilities\.git@main"(?=[ \t]|$)',
        "--with assetutilities",
        command,
    )
    if any(char in command for char in ";&|`$\n\r\\{}()"):
        raise ValueError("Unsupported shell composition in pytest command")
    tokens = shlex.split(command)
    starts = [
        i + 2 for i in range(len(tokens) - 1) if tokens[i : i + 2] == ["-m", "pytest"]
    ]
    if len(starts) != 1:
        raise ValueError("Expected exactly one python -m pytest invocation")
    validate_shell_expansion(command, tokens, starts[0])
    executable = starts[0] - 3
    if executable < 0 or not re.fullmatch(
        r"python(?:3(?:\.\d+)?)?", tokens[executable]
    ):
        raise ValueError("Expected a Python executable before -m pytest")
    prefix = tokens[:executable]
    if prefix:
        if prefix[:2] != ["uv", "run"]:
            raise ValueError("Only Python or uv run pytest commands are supported")
        options = iter(prefix[2:])
        for option in options:
            if option in {"--with", "--with-editable", "--extra", "--group"}:
                if not next(options, ""):
                    raise ValueError("Missing uv option value")
            elif option not in {
                "--no-project",
                "--no-sources",
                "--frozen",
                "--locked",
                "--offline",
                "--no-sync",
            }:
                raise ValueError(f"Unsupported uv option: {option}")
    return tokens[starts[0] :]


def pytest_arguments(command: str) -> tuple[list[str], list[str], list[str]]:
    """Read roots and ignores; reject unmodelled selection and configuration."""
    roots: list[str] = []
    ignores: list[str] = []
    globs: list[str] = []
    args = iter(pytest_tokens(command))
    for arg in args:
        option = arg.split("=", 1)[0]
        if option in {"--ignore", "--ignore-glob"}:
            value = arg.split("=", 1)[1] if "=" in arg else next(args, "")
            if not value:
                raise ValueError(f"Missing value for {option}")
            (ignores if option == "--ignore" else globs).append(value)
        elif option in {"-o", "--override-ini"}:
            raise ValueError(
                "Pytest configuration overrides need explicit audit support"
            )
        elif option in {"-p", "--tb", "--junitxml", "--import-mode"}:
            value = arg.split("=", 1)[1] if "=" in arg else next(args, "")
            if not value:
                raise ValueError(f"Missing value for {option}")
            if option == "-p" and value not in {
                "no:cacheprovider",
                "no:randomly",
                "no:sugar",
            }:
                raise ValueError("Only supported reporting-plugin disabling is allowed")
        elif arg in {
            "--no-header",
            "--disable-warnings",
            "--strict-markers",
            "-x",
            "-s",
        } or re.fullmatch(r"-[qv]+|-r[a-zA-Z]+", arg):
            continue
        elif (
            arg.startswith("-")
            or "::" in arg
            or not arg.startswith(("tests/", "./tests/"))
        ):
            raise ValueError(f"Unsupported pytest argument: {arg}")
        else:
            roots.append(arg)
    if not roots:
        raise ValueError("Explicit pytest test roots are required")
    return roots, ignores, globs


def validate_collection_policy(tree: ast.Module) -> None:
    """Reject policy mutations that cannot be resolved by literal AST reading."""
    for node in ast.walk(tree):
        if isinstance(node, (ast.AnnAssign, ast.AugAssign)) and (
            isinstance(node.target, ast.Name)
            and node.target.id in {"collect_ignore", "collect_ignore_glob"}
        ):
            raise ValueError("Collection policy mutations need explicit audit support")
        if (
            isinstance(node, ast.Name)
            and isinstance(node.ctx, ast.Load)
            and node.id in {"collect_ignore", "collect_ignore_glob"}
        ):
            raise ValueError("Collection policy mutations need explicit audit support")
        if (
            isinstance(node, ast.Assign)
            and any(
                isinstance(target, ast.Name)
                and target.id in {"collect_ignore", "collect_ignore_glob"}
                for target in node.targets
            )
            and node not in tree.body
        ):
            raise ValueError(
                "Conditional collection policy needs explicit audit support"
            )
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
            isinstance(target, ast.Name) and target.id == "_tests_dir"
            for target in node.targets
        ):
            expected = ast.parse("Path(__file__).parent", mode="eval").body
            if ast.dump(node.value) != ast.dump(expected):
                raise ValueError("Unsupported collection policy base directory")


def hook_binding(node: ast.AST) -> str:
    """Read explicit Python bindings without importing a conftest module."""
    if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
        return node.name
    if isinstance(node, ast.Name) and isinstance(node.ctx, ast.Store):
        return node.id
    if isinstance(node, ast.Attribute) and isinstance(node.ctx, ast.Store):
        return node.attr
    if isinstance(node, ast.alias):
        return node.asname or node.name.split(".")[0]
    return ""


def marker_registration_only(node: ast.AST) -> bool:
    """Recognize the existing literal marker-only pytest_configure functions."""
    if (
        not isinstance(node, ast.FunctionDef)
        or node.decorator_list
        or len(node.args.args) != 1
    ):
        return False
    argument = node.args.args[0].arg
    for statement in node.body:
        if (
            isinstance(statement, ast.Expr)
            and isinstance(statement.value, ast.Constant)
            and isinstance(statement.value.value, str)
        ):
            continue
        if not isinstance(statement, ast.Expr) or not isinstance(
            statement.value, ast.Call
        ):
            return False
        call = statement.value
        if not (
            isinstance(call.func, ast.Attribute)
            and isinstance(call.func.value, ast.Name)
            and call.func.value.id == argument
            and call.func.attr == "addinivalue_line"
            and not call.keywords
            and len(call.args) == 2
            and all(
                isinstance(arg, ast.Constant) and isinstance(arg.value, str)
                for arg in call.args
            )
            and isinstance(call.args[0], ast.Constant)
            and call.args[0].value == "markers"
        ):
            return False
    return True


def validate_file_collection_hooks(tree: ast.Module) -> None:
    """Allow runtime item hooks and literal marker registration, not file hooks."""
    runtime_hooks = {
        "pytest_collection_modifyitems",
        "pytest_runtest_makereport",
        "pytest_runtest_setup",
        "pytest_runtest_teardown",
    }
    for node in ast.walk(tree):
        name = hook_binding(node)
        if name == "*":
            raise ValueError("Wildcard conftest imports need explicit audit support")
        if not name.startswith("pytest_") or name in runtime_hooks:
            continue
        if name == "pytest_configure" and marker_registration_only(node):
            continue
        raise ValueError("Dynamic file collection hooks need explicit audit support")


def collection_rules(root: Path) -> list[tuple[str, str]]:
    """Read declarative ignores with AST; never execute a conftest file."""
    config = configparser.ConfigParser(interpolation=None)
    config.read(root / "pytest.ini", encoding="utf-8")
    if not config.has_option("pytest", "norecursedirs"):
        raise ValueError("An explicit norecursedirs collection policy is required")
    rules = [
        ("norecurse", item)
        for item in config["pytest"].get("norecursedirs", "").split()
    ]
    addopts = config["pytest"].get("addopts", "")
    _, ignores, globs = pytest_arguments(
        "python -m pytest tests/ " + shlex.join(shlex.split(addopts))
    )
    rules.extend(("path", item) for item in ignores)
    rules.extend(("glob", item) for item in globs)
    paths = [root / "conftest.py", *(root / "tests").rglob("conftest.py")]
    for path in paths:
        if not path.exists():
            continue
        tree = ast.parse(path.read_text(encoding="utf-8"))
        validate_collection_policy(tree)
        validate_file_collection_hooks(tree)
        for node in tree.body:
            if not isinstance(node, ast.Assign):
                continue
            names = {
                target.id for target in node.targets if isinstance(target, ast.Name)
            }
            for name in names & {"collect_ignore", "collect_ignore_glob"}:
                if not isinstance(node.value, (ast.List, ast.Tuple)):
                    raise ValueError("Collection exclusions must be declarative lists")
                for item in node.value.elts:
                    relative = ignore_literal(item)
                    pattern = (path.parent.relative_to(root) / relative).as_posix()
                    rules.append(
                        ("glob" if name.endswith("_glob") else "path", pattern)
                    )
    return rules


def harness_roots(root: Path) -> list[str]:
    """Read unconditional test arguments from the existing independent harness."""
    path = root / ".github/workflows/quality-gates-by-domain.yml"
    if not path.exists():
        return []
    workflow = yaml.safe_load(path.read_text(encoding="utf-8"))
    job = workflow["jobs"]["ci-harness-tests"]
    if "if" in job or job.get("continue-on-error"):
        return []
    step = next(
        item for item in job["steps"] if item.get("name") == "Run harness tests"
    )
    if "if" in step or step.get("continue-on-error"):
        return []
    command = step["run"].replace("\\\n", " ").strip()
    roots, ignores, globs = pytest_arguments(command)
    if ignores or globs:
        raise ValueError("Harness exclusions need explicit audit support")
    return roots


def ignore_literal(node: ast.AST) -> str:
    """Support literal strings and the existing str(_tests_dir / 'file') form."""
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return node.value
    if (
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id == "str"
        and len(node.args) == 1
    ):
        node = node.args[0]
        if (
            isinstance(node, ast.BinOp)
            and isinstance(node.op, ast.Div)
            and isinstance(node.left, ast.Name)
            and node.left.id == "_tests_dir"
        ):
            return ignore_literal(node.right)
    raise ValueError("Dynamic collection exclusions need explicit audit support")


def glob_targets(path: str, pattern: str) -> list[str]:
    """Match a file and every ancestor that pytest traverses during collection."""
    return [
        candidate.as_posix()
        for candidate in [Path(path), *Path(path).parents]
        if candidate.as_posix() != "."
        and fnmatch.fnmatchcase(candidate.as_posix(), pattern)
    ]


def collection_excluded(
    root: Path,
    path: str,
    roots: list[str],
    rules: list[tuple[str, str]],
    cache: dict[str, list[tuple[str, bool]]] | None = None,
) -> bool:
    """Account for recursive ignores; explicitly named paths bypass recursion."""
    explicit_roots = [
        item for pattern in roots for item, _ in expand_root(root, pattern, cache)
    ]
    for kind, pattern in rules:
        if kind == "norecurse":
            targets = [
                parent.as_posix()
                for parent in Path(path).parents
                if parent.as_posix() != "."
            ]
            targets = [
                item
                for item in targets
                if fnmatch.fnmatchcase(
                    (root.absolute() / item).as_posix()
                    if "/" in pattern
                    else Path(item).name,
                    "*/" + pattern if "/" in pattern else pattern,
                )
            ]
        elif kind == "glob":
            targets = glob_targets(path, pattern)
        else:
            targets = [pattern] if matches(root, path, pattern) else []
        for target in targets:
            explicit = any(
                item == target or item.startswith(target + "/")
                for item in explicit_roots
            )
            if not explicit:
                return True
    return False


def discovered_tests(root: Path) -> list[str]:
    """Use the repository's pytest filename patterns, including ignored files."""
    config = configparser.ConfigParser(interpolation=None)
    config.read(root / "pytest.ini", encoding="utf-8")
    patterns = config["pytest"]["python_files"].split()
    return sorted(
        path.relative_to(root).as_posix()
        for path in (root / "tests").rglob("*.py")
        if any(fnmatch.fnmatchcase(path.name, pattern) for pattern in patterns)
    )


def missing_roots(
    root: Path,
    domains: list[detector.Domain],
    gates: dict[str, dict[str, Any]],
    commands: dict[str, tuple[list[str], list[str], list[str]]],
) -> set[str]:
    """Detect stale directory, file and glob entries, including empty roots."""
    findings = set()
    for domain in domains:
        gate = gates.get(f"tests-{domain.name}", {})
        roots = [
            *domain.roots,
            *gate.get("test_roots", []),
            *commands.get(domain.name, ([], [], []))[0],
        ]
        for item in roots:
            if not any((root / path).exists() for path, _ in expand_root(root, item)):
                findings.add(f"missing-root|{domain.name}|{item}")
    return findings


def documented_owner(
    root: Path,
    path: str,
    domains: list[detector.Domain],
    cache: dict[str, list[tuple[str, bool]]],
) -> tuple[detector.Domain | None, str]:
    """Resolve specific ownership and overrides without silently choosing ties."""
    owners = [
        domain
        for domain in domains
        if any(matches(root, path, item, cache) for item in domain.roots)
    ]
    if not owners:
        return None, "undocumented"
    overrides = detector.mapped_domain_names(path)
    if overrides:
        owners = [domain for domain in owners if domain.name in overrides]
    else:
        specificity = {
            domain.name: max(
                len(item) for item in domain.roots if matches(root, path, item, cache)
            )
            for domain in owners
        }
        owners = [
            domain
            for domain in owners
            if specificity[domain.name] == max(specificity.values())
        ]
    return (owners[0], "") if len(owners) == 1 else (None, "ambiguous-owner")


def audit(root: Path) -> set[str]:
    """Return exact enrollment defects as kind|domain|repository-relative-path."""
    domains = detector.parse_domains(root / "tests/DOMAINS.md")
    gates = yaml.safe_load(
        (root / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )["gates"]
    commands = {
        domain.name: pytest_arguments(
            gates.get(f"tests-{domain.name}", {}).get("command", "")
        )
        for domain in domains
        if f"tests-{domain.name}" in gates
    }
    rules = collection_rules(root)
    harness = harness_roots(root)
    cache: dict[str, list[tuple[str, bool]]] = {}
    findings = missing_roots(root, domains, gates, commands)
    for path in discovered_tests(root):
        owner, problem = documented_owner(root, path, domains, cache)
        if owner is None:
            findings.add(f"{problem}|-|{path}")
            continue
        name = owner.name
        gate = gates.get(f"tests-{name}", {})
        if gate.get("enabled") is not True or gate.get("domain") != name:
            findings.add(f"disabled-gate|{name}|{path}")
            continue
        if gate.get("failure_action") != "block":
            findings.add(f"advisory-gate|{name}|{path}")
        if not any(
            matches(root, path, item, cache) for item in gate.get("test_roots", [])
        ):
            findings.add(f"missing-metadata|{name}|{path}")
        roots, ignores, globs = commands[name]
        if not any(matches(root, path, item, cache) for item in roots):
            findings.add(f"missing-command|{name}|{path}")
        elif any(matches(root, path, item) for item in ignores) or any(
            glob_targets(path, pattern) for pattern in globs
        ):
            findings.add(f"command-excluded|{name}|{path}")
        if collection_excluded(root, path, roots, rules, cache):
            findings.add(f"collection-excluded|{name}|{path}")
        selected = {domain.name for domain in detector.touched_domains([path], domains)}
        if name not in selected and not (
            detector.is_no_domain_path(path)
            and any(matches(root, path, item, cache) for item in harness)
        ):
            findings.add(f"not-selected|{name}|{path}")
    return findings


def reconcile(findings: set[str], exceptions: dict[str, str]) -> set[str]:
    """Reject new defects, unreasoned exceptions and stale debt entries."""
    errors = findings - exceptions.keys()
    for key, reason in exceptions.items():
        if not isinstance(reason, str) or not reason.strip():
            errors.add(f"unreasoned-exception|{key}")
        if key not in findings:
            errors.add(f"stale-exception|{key}")
    return errors


def main(argv: list[str] | None = None) -> int:
    """Run the registration invariant with the exact, reviewable debt ledger."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[2]
    )
    args = parser.parse_args(argv)
    try:
        findings = audit(args.root)
        ledger = args.root / "config/ci/test-registration-exceptions.json"
        exceptions = (
            json.loads(ledger.read_text(encoding="utf-8")) if ledger.exists() else {}
        )
        errors = reconcile(findings, exceptions)
    except (
        OSError,
        ValueError,
        KeyError,
        TypeError,
        SyntaxError,
        StopIteration,
        yaml.YAMLError,
    ) as error:
        print(f"Test registration audit failed: {error}", file=sys.stderr)
        return 1
    for finding in sorted(errors):
        print(finding)
    print(
        f"Test registration: {len(errors)} new/stale defects; {len(findings & exceptions.keys())} acknowledged findings"
    )
    return int(bool(errors))


if __name__ == "__main__":
    raise SystemExit(main())
