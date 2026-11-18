"""Check section-2 public-symbol tables against package ASTs, without imports.

The table header starts with ``Public symbol``. Each row has one backticked
name (``Class.method`` for public members) and one ``[path:Lstart-Lend]``
locator. Paths are repository-relative. Inventory includes definitions in all
package Python files, public class members, and private definitions explicitly
exported by a literal __all__. Imported third-party names and constants are not
function/class definitions. Local re-exports need no duplicate row. Legacy packs
without this table fail explicitly; they are never silently declared consistent.
"""

from __future__ import annotations

import argparse
import ast
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
LOCATOR = re.compile(r"\[([^\]\n]+):L(\d+)-L(\d+)\]")
NAME = re.compile(r"`([A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*)`")
DEFINITIONS = (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)


def local_imports(trees: dict, module_paths: dict) -> dict:
    """Resolve explicit relative and absolute imports within the package."""
    imports = {path: {} for path in trees}
    for module, path in module_paths.items():
        context = module.split(".")
        if path.name != "__init__.py":
            context = context[:-1]
        for node in trees[path].body:
            if not isinstance(node, ast.ImportFrom):
                continue
            parts = node.module.split(".") if node.module else []
            if node.level:
                if node.level > len(context):
                    continue
                parts = context[:len(context) - node.level + 1] + parts
            source = module_paths.get(".".join(parts))
            if source is not None:
                for alias in node.names:
                    imports[path][alias.asname or alias.name] = (source, alias.name)
    return imports


def public_exports(trees: dict, module_paths: dict) -> dict:
    """Propagate module exports through local bindings to their definitions."""
    exports = {path: set() for path in trees}
    for path, tree in trees.items():
        for node in tree.body:
            if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id == "__all__"
                for target in node.targets
            ):
                try:
                    value = ast.literal_eval(node.value)
                except (ValueError, TypeError):
                    continue
                if isinstance(value, (list, tuple)):
                    exports[path].update(name for name in value if isinstance(name, str))
    imports = local_imports(trees, module_paths)
    for path, bindings in imports.items():
        exports[path].update(name for name in bindings if not name.startswith("_"))
    changed = True
    while changed:
        changed = False
        for path, bindings in imports.items():
            for name, (source, original) in bindings.items():
                if name in exports[path] and original not in exports[source]:
                    exports[source].add(original)
                    changed = True
    return exports


def public_definitions(package: Path, root: Path) -> dict[tuple[str, str], int]:
    """Map (source path, qualified symbol) to its definition-name line."""
    definitions = {}
    trees = {}
    module_paths = {}
    package_parts = package.relative_to(root).parts
    if package_parts[0] == "src":
        package_parts = package_parts[1:]
    for path in sorted(package.rglob("*.py")):
        if any(part in {"docs", "__pycache__"} for part in path.relative_to(package).parts):
            continue
        trees[path] = ast.parse(path.read_text(encoding="utf-8"), filename=path.name)
        parts = path.relative_to(package).with_suffix("").parts
        if parts[-1] == "__init__":
            parts = parts[:-1]
        module_paths[".".join((*package_parts, *parts))] = path
    exports = public_exports(trees, module_paths)

    for path, tree in trees.items():
        def visit(nodes, prefix=""):
            for node in nodes:
                if isinstance(node, DEFINITIONS):
                    if node.name.startswith("_") and (prefix or node.name not in exports[path]):
                        continue
                    name = prefix + node.name
                    definitions[(path.relative_to(root).as_posix(), name)] = node.lineno
                    if isinstance(node, ast.ClassDef):
                        visit(node.body, name + ".")
                elif isinstance(node, (ast.If, ast.Try)):
                    # Definitions under optional-import/version guards are still API.
                    visit(node.body, prefix)
                    visit(node.orelse, prefix)
                    if isinstance(node, ast.Try):
                        visit(node.finalbody, prefix)
                        for handler in node.handlers:
                            visit(handler.body, prefix)

        visit(tree.body)
    return definitions


def check_pack(pack: Path, root: Path = ROOT) -> list[str]:
    """Return directional errors, including a diagnostic for unsupported tables."""
    root, pack = root.resolve(), pack.resolve()
    label = pack.relative_to(root).as_posix()
    errors = []

    def error(direction, name, detail):
        errors.append(f"{label}: {direction}: {name} {detail}")

    text = pack.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 2\..*?(?=^## 3\.|\Z)", text)
    lines = section[0].splitlines() if section else []
    header = next((i for i, line in enumerate(lines)
                   if re.match(r"\|\s*Public symbol\s*\|", line)), None)
    if header is None:
        error("pack-to-code", "<section-2>", "missing Public symbol table")
        return errors
    package = pack.parent.parent
    definitions = public_definitions(package, root)
    documented = set()
    for line in lines[header + 1:]:
        if not line.strip().startswith("|"):
            break
        if re.match(r"\|\s*:?-", line):
            continue
        first = line.split("|")[1].strip()
        name_match = NAME.fullmatch(first)
        if not name_match:
            error("pack-to-code", first or "<empty>", "expected one backticked symbol")
            continue
        name = name_match[1]
        locators = LOCATOR.findall(line)
        if len(locators) != 1:
            error("locator", name, "expected exactly one source locator")
            continue
        relative, start_text, end_text = locators[0]
        start, end = int(start_text), int(end_text)
        path = (root / relative).resolve()
        if not path.is_relative_to(package) or not path.is_file():
            error("locator", name, "source is absent or outside package")
            continue
        relative = path.relative_to(root).as_posix()
        key = (relative, name)
        if key in documented:
            error("pack-to-code", name, f"duplicate row ({relative})")
        documented.add(key)
        count = len(path.read_text(encoding="utf-8").splitlines())
        if not 1 <= start <= end <= count:
            error("locator", name, f"range outside file ({relative})")
        if key not in definitions:
            error("pack-to-code", name, f"not defined ({relative})")
        elif not start <= definitions[key] <= end:
            error("locator", name, f"range does not contain definition name ({relative})")
    for relative, name in sorted(definitions.keys() - documented):
        error("code-to-pack", name, f"missing ({relative})")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pack", type=Path, action="append", required=True)
    parser.add_argument("--root", type=Path, default=ROOT)
    args = parser.parse_args()
    failed = False
    for pack in args.pack:
        try:
            errors = check_pack(args.root / pack, args.root)
        except (OSError, ValueError, SyntaxError) as exc:
            # Do not include system paths or source contents in diagnostics.
            errors = [f"{pack.name}: pack-to-code: <inventory> {type(exc).__name__}"]
        for error in errors:
            print(error)
        failed |= bool(errors)
    return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
