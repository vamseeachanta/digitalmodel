"""Build an import map of all digitalmodel.modules.* references across src/.

Recursively scans all .py files under src/, finds import statements that
reference digitalmodel.modules.*, and outputs a structured JSON report.
"""

from __future__ import annotations

import json
import re
import sys
from collections import defaultdict
from pathlib import Path

# Pattern matches both forms:
#   from digitalmodel.modules.X import Y
#   import digitalmodel.modules.X
IMPORT_RE = re.compile(
    r"^\s*(?:from|import)\s+"  # starts with from or import
    r"(digitalmodel\.modules\.\S+)"  # capture the dotted path
)

MODULE_NAME_RE = re.compile(
    r"^digitalmodel\.modules\.([^.\s]+)"  # first component after modules.
)


def extract_module_name(dotted_path: str) -> str:
    """Extract the top-level module name from a dotted import path.

    Example: 'digitalmodel.orcaflex.orcaflex' -> 'orcaflex'
    """
    match = MODULE_NAME_RE.match(dotted_path)
    if match:
        return match.group(1)
    return dotted_path


def scan_file(filepath: Path) -> list[dict]:
    """Scan a single Python file for digitalmodel.modules imports."""
    results = []
    try:
        text = filepath.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return results

    for lineno, line in enumerate(text.splitlines(), start=1):
        # Skip comments-only lines (but keep inline imports)
        stripped = line.lstrip()
        if stripped.startswith("#"):
            continue

        match = IMPORT_RE.match(line)
        if match:
            full_import = line.strip()
            dotted_path = match.group(1)
            module_name = extract_module_name(dotted_path)
            results.append(
                {
                    "line": lineno,
                    "module": module_name,
                    "full_import": full_import,
                }
            )
    return results


def build_import_map(src_dir: Path) -> dict:
    """Build the complete import map from all .py files under src_dir."""
    by_file: dict[str, list[dict]] = {}
    by_module: dict[str, dict] = defaultdict(lambda: {"count": 0, "files": []})
    total_imports = 0
    total_files = 0

    py_files = sorted(src_dir.rglob("*.py"))

    for filepath in py_files:
        rel_path = str(filepath.relative_to(src_dir.parent))
        imports = scan_file(filepath)
        if not imports:
            continue

        total_files += 1
        total_imports += len(imports)
        by_file[rel_path] = imports

        for imp in imports:
            mod = imp["module"]
            by_module[mod]["count"] += 1
            by_module[mod]["files"].append(f"{rel_path}:{imp['line']}")

    # Sort by_module by count descending
    sorted_modules = dict(
        sorted(by_module.items(), key=lambda x: x[1]["count"], reverse=True)
    )

    return {
        "total_imports": total_imports,
        "total_files": total_files,
        "by_module": sorted_modules,
        "by_file": dict(sorted(by_file.items())),
    }


def main() -> None:
    project_root = Path(__file__).resolve().parent.parent
    src_dir = project_root / "src"

    if not src_dir.is_dir():
        print(f"ERROR: src/ directory not found at {src_dir}", file=sys.stderr)
        sys.exit(1)

    print(f"Scanning {src_dir} for digitalmodel.modules.* imports...")
    import_map = build_import_map(src_dir)

    # Write JSON output
    output_path = Path(__file__).resolve().parent / "import_map.json"
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(import_map, f, indent=2)

    # Print summary
    print(f"\nImport Map Summary")
    print(f"{'=' * 50}")
    print(f"Total imports:  {import_map['total_imports']}")
    print(f"Total files:    {import_map['total_files']}")
    print(f"Total modules:  {len(import_map['by_module'])}")
    print(f"\nTop modules by import count:")
    print(f"{'-' * 50}")
    for module, data in list(import_map["by_module"].items())[:15]:
        print(f"  {module:<35} {data['count']:>4} imports")

    print(f"\nJSON written to: {output_path}")


if __name__ == "__main__":
    main()
