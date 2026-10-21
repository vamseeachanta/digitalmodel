# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regenerate the advisory family inventory without importing application code.

Run ``python scripts/ci/generate_family_map.py --write`` after adding files.
``--check`` reports drift and exits zero while family selection is inactive.
Dependency measurement covers static absolute and relative imports, including
imports inside functions and TYPE_CHECKING blocks; dynamic imports are excluded.
"""

import argparse
import ast
import copy
import json
import os
import warnings
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CONTRACT = Path("config/ci/module-families.json")


def python_files(root):
    """Include new files but never descend into environments or checkouts."""
    for directory, subdirs, names in os.walk(root, followlinks=False):
        parent = Path(directory)
        subdirs[:] = sorted(
            name
            for name in subdirs
            if name not in {".git", ".venv", "__pycache__", "node_modules"}
            and not (parent / name).is_symlink()
            and not getattr(parent / name, "is_junction", lambda: False)()
            and not (parent / name / "pyvenv.cfg").exists()
            and not (parent / name / ".git").exists()
        )
        for name in sorted(names):
            path = parent / name
            if name.endswith(".py") and not path.is_symlink():
                yield path


def module_name(path):
    parts = path.with_suffix("").parts
    return ".".join(parts[:-1] if parts[-1] == "__init__" else parts)


def source_modules(root):
    modules = set()
    for path in python_files(root):
        relative = path.relative_to(root)
        if relative.parts[:2] != ("src", "digitalmodel"):
            continue
        parts = module_name(relative.relative_to("src")).split(".")
        modules.update(".".join(parts[:i]) for i in range(1, len(parts) + 1))
    return modules


def test_files(root):
    return {
        path.relative_to(root).as_posix()
        for path in python_files(root)
        if path.name.startswith("test_") or path.name.endswith("_test.py")
    }


def prefix_family(name, rules, separator, default="platform"):
    matches = [p for p in rules if name == p or name.startswith(p + separator)]
    return rules[max(matches, key=len)] if matches else default


def measured_dependencies(root, modules, families):
    dependencies = {family: set() for family in families if family != "ENGINE"}
    for path in python_files(root):
        relative = path.relative_to(root)
        if relative.parts[:2] != ("src", "digitalmodel"):
            continue
        module = module_name(relative.relative_to("src"))
        family = modules[module]
        if family == "ENGINE":
            continue
        # Legacy escape sequences warn on newer Python versions. Keep those
        # diagnostics visible without inheriting pytest's warning-as-error rule.
        with warnings.catch_warnings():
            warnings.simplefilter("default", SyntaxWarning)
            tree = ast.parse(
                path.read_text(encoding="utf-8-sig"), filename=relative.as_posix()
            )
        package = module if path.name == "__init__.py" else module.rpartition(".")[0]
        for node in ast.walk(tree):
            targets = []
            if isinstance(node, ast.Import):
                targets = [alias.name for alias in node.names]
            elif isinstance(node, ast.ImportFrom):
                base = node.module or ""
                if node.level:
                    parts = package.split(".")
                    base = ".".join(
                        parts[: len(parts) - node.level + 1] + ([base] if base else [])
                    )
                targets = [base] + [base + "." + alias.name for alias in node.names]
            dependencies[family].update(
                t for t in targets if modules.get(t) == "ENGINE"
            )
    return {family: sorted(values) for family, values in sorted(dependencies.items())}


def regenerate(root, contract):
    result = copy.deepcopy(contract)
    rules = result["assignment_rules"]
    boundaries = result["engine"]["boundaries"]
    modules = {}
    for module in sorted(source_modules(root)):
        engine = any(
            module == b or (b != "digitalmodel" and module.startswith(b + "."))
            for b in boundaries
        )
        modules[module] = (
            "ENGINE" if engine else prefix_family(module, rules["module_prefixes"], ".")
        )
    result["module_to_family"] = modules
    result["engine"]["modules"] = [m for m, f in modules.items() if f == "ENGINE"]
    result["test_file_to_family"] = {
        name: contract["test_file_to_family"].get(
            name, prefix_family(name, rules["test_prefixes"], "/")
        )
        for name in sorted(test_files(root))
    }
    result["family_to_engine_deps"] = measured_dependencies(
        root, modules, result["families"]
    )
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--write", action="store_true")
    mode.add_argument("--check", action="store_true")
    args = parser.parse_args()
    path = ROOT / CONTRACT
    contract = json.loads(path.read_text(encoding="utf-8"))
    result = regenerate(ROOT, contract)
    text = json.dumps(result, indent=2) + "\n"
    if args.write:
        path.write_text(text, encoding="utf-8", newline="\n")
    elif result != contract:
        print("Family inventory drift (report-only); run with --write to refresh.")
    else:
        print("Family inventory current (report-only; no family gate activated).")
    print(
        f"{len(result['module_to_family'])} modules; {len(result['test_file_to_family'])} tests; {len(result['engine']['modules'])} ENGINE modules"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
