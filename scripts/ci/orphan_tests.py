"""Inventory pytest file selectors across automatic and manually dispatched workflows."""

import argparse
import ast
from collections import defaultdict
import configparser
import hashlib
from fnmatch import fnmatchcase
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tomllib

# Fingerprints of reviewed opaque setup, reporting, build and action steps.
# Any change needs inspection for indirect test commands before repinning.
OPAQUE_STEPS = {
    "capability-card-links": "aba52666a5a1423219cd",
    "docs": "22ca61497dc0caf90ef7",
    "full-matrix-sweep": "e58d973116f575848e8c",
    "gmsh-meshing-tests": "da204575908f50e169f7",
    "pages": "50d2a08a6b213216900a",
    "parametric-atlas-drift": "dedd79cbd17f2dd5519a",
    "publish": "9f7edf6e4675003a16d6",
    "quality-gates-by-domain": "f5904058b3ea1a973de6",
    "quality-gates": "6c54e8199a63b844a968",
    "repo-structure": "3b681ebd99f2e79b5ac7",
    "store-data-tests": "8c18f9916cd01e22f8ef",
    "text-encoding": "2a4e3a6534e6a70424d5",
    "workflow-automation-tests": "aaab07a811401ff74ee9",
}


def settings(root):
    ini = configparser.ConfigParser(interpolation=None)
    if ini.read(root / "pytest.ini"):
        return dict(ini["pytest"])
    path = root / "pyproject.toml"
    if not path.is_file():
        return {}
    return tomllib.loads(path.read_text()).get("tool", {}).get("pytest", {}).get("ini_options", {})


def patterns(config, key, default):
    value = config.get(key, default)
    return shlex.split(value) if isinstance(value, str) else value


def conftest_ignores(path):
    """Read supported collect_ignore literals without importing a test environment."""
    if not path.is_file():
        return set()
    paths = set()
    tree = ast.parse(path.read_bytes())
    assignments = {target for stmt in tree.body if isinstance(stmt, ast.Assign) for target in stmt.targets
                   if isinstance(target, ast.Name) and target.id == "collect_ignore"}
    for node in ast.walk(tree):
        if getattr(node, "id", None) == "collect_ignore" and (node not in assignments or len(assignments) != 1):
            raise ValueError(f"dynamic collect_ignore in {path.name}")
        if getattr(node, "name", None) == "pytest_ignore_collect" or getattr(node, "id", None) == "collect_ignore_glob":
            raise ValueError(f"unsupported collection hook in {path.name}")
    for stmt in tree.body:
        if not isinstance(stmt, ast.Assign) or not any(
            isinstance(target, ast.Name) and target.id == "collect_ignore" for target in stmt.targets
        ):
            continue
        if not isinstance(stmt.value, (ast.List, ast.Tuple)):
            raise ValueError(f"unresolved collect_ignore in {path.name}")
        for expression in stmt.value.elts:
            if isinstance(expression, ast.Call) and ast.unparse(expression.func) == "str":
                expression = expression.args[0]
                if isinstance(expression, ast.BinOp) and ast.unparse(expression.left) == "_tests_dir":
                    expression = expression.right
            value = ast.literal_eval(expression)
            paths.add(path.parent / value)
    return paths


def candidates(root):
    config = settings(root)
    options = patterns(config, "addopts", "") + shlex.split(os.environ.get("PYTEST_ADDOPTS", ""))
    for option in options:
        if not re.fullmatch(r"-[qv]+|--tb=(?:short|long|line|no|auto|native)|--strict-markers|--import-mode=importlib", option):
            raise ValueError(f"unreviewed pytest addopts option {option!r}")
    names = patterns(config, "python_files", "test_*.py *_test.py")
    excluded = patterns(config, "norecursedirs", "*.egg .* _darcs build CVS dist node_modules venv {arch}")
    files, ignored = set(), conftest_ignores(root / "conftest.py")
    for pattern in patterns(config, "testpaths", "."):
        for start in root.glob(pattern) if pattern != "." else [root]:
            if start.is_file():
                files.add(start)
                continue
            for directory, dirs, leaves in os.walk(start):
                parent = Path(directory)
                ignored.update(conftest_ignores(parent / "conftest.py"))
                dirs[:] = [d for d in dirs if not any(
                    fnmatchcase(d, pat) if "/" not in pat else fnmatchcase((parent / d).as_posix(), "*/" + pat)
                    for pat in excluded
                )]
                files.update(parent / f for f in leaves if any(fnmatchcase(f, pat) for pat in names))
    if not files:
        raise ValueError("no matching files in configured testpaths")
    return {p.relative_to(root).as_posix() for p in files}, {
        p.relative_to(root).as_posix() for p in ignored
    }


def scalars(text, key):
    """Read indented literal/folded and single-line scalar values."""
    lines = text.splitlines()
    for index, line in enumerate(lines):
        match = re.fullmatch(r"( *)(?:- )?" + key + r":\s*(.*)", line)
        if not match:
            continue
        value = match[2].strip()
        header = re.fullmatch(r"([>|])[-+]?(?:\s+#.*)?", value)
        indent = line.index(key)
        block = []
        for next_line in lines[index + 1:]:
            if next_line.strip() and len(next_line) - len(next_line.lstrip()) <= indent:
                break
            block.append(next_line.strip())
        if header:
            yield (" " if header[1] == ">" else "\n").join(block)
        else:
            yield " ".join([value, *(v for v in block if v and not v.startswith("#"))]).strip()


def at_or_below(path, prefix):
    return prefix == "." or path == prefix or path.startswith(prefix + "/")


def selected_paths(command, files, ignored, root):
    tokens = shlex.split(command, comments=True)
    if not tokens or "pytest" not in tokens:
        raise ValueError("expected pytest invocation")
    start = len(tokens) - tokens[::-1].index("pytest")
    prefix = tokens[:start - 1]
    if prefix[:1] == ["PYTHONPATH=src"]:
        prefix = prefix[1:]
    if prefix[-2:] == ["python", "-m"]:
        prefix = prefix[:-2]
    if prefix and prefix[:2] != ["uv", "run"] or any(t in {"&&", "||", ";", "|"} for t in prefix):
        raise ValueError("unresolved pytest launcher")
    args = iter(tokens[start:])
    includes, excludes = [], []
    for arg in args:
        if arg in {"-p", "-m", "-k"}:
            next(args)
        elif arg == "--ignore" or arg.startswith("--ignore="):
            operand = next(args) if arg == "--ignore" else arg.partition("=")[2]
            if not re.fullmatch(r"[./\w-]+", operand):
                raise ValueError(f"unresolved pytest exclusion {operand!r}")
            excludes.append(Path(operand).as_posix())
        elif re.fullmatch(r"-(?:[qvx]+|r\w+)|--no-header|--(?:tb|basetemp|cov(?:-[\w-]+)?)=.+", arg):
            continue
        elif arg.startswith("-") or re.search(r"[$;&|`<>]", arg):
            raise ValueError(f"unresolved pytest argument {arg!r}")
        else:
            operand = arg.partition("::")[0]
            matches = [root] if operand == "." else list(root.glob(operand))
            if not matches:
                raise ValueError(f"missing pytest operand {operand!r}")
            includes.extend(path.relative_to(root).as_posix() for path in matches)
    return {f for f in files if any(at_or_below(f, p) for p in includes or ["."])
            and not any(at_or_below(f, p) for p in excludes)
            and (f in includes or not any(at_or_below(f, p) for p in ignored))}


def domain_commands(root, text):
    table = (root / ".claude/quality-gates.yaml").read_text()
    gates = dict(re.findall(r"^  (tests-[\w-]+): *\n((?:(?:    | *#| *$).*\n)*)", table, re.M))
    extensions = dict(re.findall(
        r'if os\.environ\["GATE_NAME"\] == "(tests-[\w-]+)":\s*\n\s*command \+= "([^"]*)"', text))
    if text.count("command +=") != len(extensions) or 'gates[os.environ["GATE_NAME"]]["command"]' not in text:
        raise ValueError("unresolved domain gate reader")
    detector = subprocess.run(
        [sys.executable, "scripts/ci/detect_touched_domains.py", "--mode", "full", "--output-format", "list"],
        cwd=root, check=True, capture_output=True, text=True,
    )
    if not detector.stdout.strip():
        raise ValueError("domain reader returned an empty matrix")
    for domain in detector.stdout.splitlines():
        gate = "tests-" + domain
        commands = list(scalars(gates[gate], "command"))
        if len(commands) != 1:
            raise ValueError(f"missing or ambiguous command for {gate}")
        yield domain, json.loads(commands[0]) + extensions.get(gate, "")


def workflow_commands(root, workflow, text):
    """Keep opaque steps distinct from parsed pytest operands in the audit."""
    commands = list(domain_commands(root, text)) if "steps.gate.outputs.command" in text else []
    if re.search(r"\bPYTEST_ADDOPTS\b", text):
        raise ValueError("unreviewed workflow pytest environment")
    if re.search(r"""[{,]\s*["']?(?:run|uses)["']?\s*:|^\s*["'](?:run|uses)["']:""", text, re.M):
        raise ValueError("unresolved workflow mapping")
    blocks = list(scalars(text, "run"))
    uses = list(scalars(text, "uses"))
    if not blocks and not uses:
        raise ValueError("unresolved workflow steps")
    opaque = uses + list(scalars(text, "script")) + list(scalars(text, "working-directory"))
    guards = list(scalars(text, "if"))
    trigger = trigger_block(text)
    if guards or re.search(r"^ {4,}\S|^  [\w-]+:\s+[^#\s]", trigger, re.M):
        opaque.extend([trigger, *guards])
    for block in blocks:
        for line in block.replace("\\\n", " ").splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if re.search(r"(?:^|\s)pytest(?:\s|$)", line) and not re.search(r"\bpip install\b", line):
                commands.append(("", line))
            else:
                # Subject the reader itself to review, but resolve supported path extensions live.
                line = re.sub(r'^(command \+= )"[^"]*"$', r'\1"<operands>"', line)
                opaque.append(line)
    if opaque:
        digest = hashlib.sha256("\n".join(opaque).encode()).hexdigest()[:20]
        if OPAQUE_STEPS.get(workflow.stem) != digest:
            raise ValueError(f"unreviewed indirect commands, conditions or action steps ({digest})")
    return commands


def trigger_block(text):
    text = "\n".join(line for line in text.splitlines() if not line.lstrip().startswith("#"))
    match = re.search(r"""^["']?on["']?:[ \t]*([^\n]*)(.*?)(?=^\S|\Z)""", text, re.M | re.S)
    if not match:
        raise ValueError("unresolved workflow triggers")
    return match[0].rstrip()


def manual_only(text):
    block = trigger_block(text)
    value = block.splitlines()[0].partition(":")[2].split("#", 1)[0].strip()
    if value:
        if not re.fullmatch(r"\w+|\[\s*\w+(?:\s*,\s*\w+)*\s*\]", value):
            raise ValueError("unresolved workflow triggers")
        events = re.findall(r"\w+", value)
    else:
        events = re.findall(r"^  ([\w-]+):", block, re.M)
    if not events or set(events) - {"workflow_dispatch", "push", "schedule", "pull_request"}:
        raise ValueError(f"unresolved workflow triggers {events}")
    return events == ["workflow_dispatch"]


def inventory(root):
    selected, automatic, unknown, manual = defaultdict(list), set(), [], []
    try:
        files, ignored = candidates(root)
    except (SyntaxError, KeyError, OSError, ValueError, TypeError, configparser.Error) as error:
        files, ignored = set(), set()
        unknown.append(f"discovery: {error}")
    workflows = sorted((root / ".github/workflows").glob("*.y*ml"))
    if not workflows:
        unknown.append("missing workflow definitions")
    for workflow in workflows:
        try:
            text = workflow.read_text()
            if not re.search(r"^jobs:\s*(?:#.*)?$", text, re.M):
                raise ValueError("unresolved workflow jobs")
            dispatch = manual_only(text)
            if dispatch:
                manual.append(workflow.stem)
            for domain, command in workflow_commands(root, workflow, text):
                paths = selected_paths(command, files, ignored, root)
                source = workflow.stem + ("/" + domain if domain else "")
                for path in paths:
                    if source not in selected[path]:
                        selected[path].append(source)
                if not dispatch:
                    automatic.update(paths)
        except (KeyError, OSError, ValueError, StopIteration, subprocess.CalledProcessError) as error:
            unknown.append(f"{workflow.name}: {error}")
    eligible = {p for p in files if not any(at_or_below(p, i) for i in ignored)} | set(selected)
    missing = sorted(eligible - automatic)
    folders = defaultdict(lambda: dict(selected=0, orphans=0, automatic_orphans=0))
    functions = {}
    for path in sorted(eligible):
        folder = "/".join(Path(path).parts[:-1][:2]) or "."
        folders[folder]["selected" if path in selected else "orphans"] += 1
        folders[folder]["automatic_orphans"] += path not in automatic
        if path not in automatic:
            try:
                tree = ast.parse((root / path).read_bytes())
                functions[path] = sum(isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
                                      and node.name.startswith("test_") for node in ast.walk(tree))
            except (SyntaxError, OSError) as error:
                functions[path] = None
                unknown.append(f"{path}: {error}")
    env = {k: v for k, v in os.environ.items() if k not in {"GIT_DIR", "GIT_COMMON_DIR", "GIT_WORK_TREE"}}
    revision = subprocess.run(["git", "rev-parse", "--show-toplevel", "HEAD"], cwd=root,
                              capture_output=True, text=True, env=env)
    lines = revision.stdout.splitlines()
    sha = lines[-1] if revision.returncode == 0 and len(lines) == 2 and Path(lines[0]) == root else None
    if sha is None:
        unknown.append("test root has no Git revision")
    else:
        from orphan_measure import tree_changes
        if changes := tree_changes(root):
            unknown.append(f"checkout differs from base: {changes}")
    return dict(base_commit=sha, selected=dict(selected),
                orphans=sorted(eligible - set(selected)), automatic_orphans=missing, functions=functions,
                manual_workflows=manual, ignored=sorted(files - eligible), unknown=unknown, folders=folders)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("list", "collect", "run"))
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    root, out = args.root.resolve(), args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=True)
    if args.mode != "list":
        from orphan_measure import measure
        return measure(root, out, args.mode)
    data = inventory(root)
    (out / "list.json").write_text(json.dumps(data, sort_keys=True, indent=2) + "\n")
    return int(bool(data["unknown"]))


if __name__ == "__main__":
    raise SystemExit(main())
