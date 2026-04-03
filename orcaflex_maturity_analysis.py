#!/usr/bin/env python3
"""AST-based maturity analysis for the OrcaFlex public API package."""
import ast
import os
import json
import sys

BASE = "/mnt/local-analysis/workspace-hub/digitalmodel"

# Source files (public API layer)
SRC_DIR = os.path.join(BASE, "src/digitalmodel/orcaflex")
# Test directories
TEST_DIRS = [
    os.path.join(BASE, "tests/orcaflex"),
    os.path.join(BASE, "tests/solvers/orcaflex"),
]


def get_py_files(directory):
    """Get all .py files recursively."""
    result = []
    for root, dirs, files in os.walk(directory):
        for f in files:
            if f.endswith(".py"):
                result.append(os.path.join(root, f))
    return sorted(result)


def analyze_source_file(filepath):
    """Analyze a source file using AST."""
    with open(filepath, "r", errors="replace") as fh:
        source = fh.read()
    lines = source.count("\n") + 1

    try:
        tree = ast.parse(source, filename=filepath)
    except SyntaxError:
        return {
            "lines": lines,
            "classes": [],
            "functions": [],
            "public_classes": 0,
            "public_functions": 0,
            "classes_with_docstring": 0,
            "functions_with_docstring": 0,
            "parse_error": True,
        }

    classes = []
    functions = []

    for node in ast.walk(tree):
        if isinstance(node, ast.ClassDef):
            has_doc = (
                isinstance(node.body[0], ast.Expr)
                and isinstance(node.body[0].value, (ast.Str, ast.Constant))
                if node.body
                else False
            )
            is_public = not node.name.startswith("_")
            classes.append(
                {"name": node.name, "public": is_public, "has_docstring": has_doc}
            )
        elif isinstance(node, ast.FunctionDef) or isinstance(
            node, ast.AsyncFunctionDef
        ):
            # Only top-level and class-level functions
            has_doc = (
                isinstance(node.body[0], ast.Expr)
                and isinstance(node.body[0].value, (ast.Str, ast.Constant))
                if node.body
                else False
            )
            is_public = not node.name.startswith("_")
            functions.append(
                {"name": node.name, "public": is_public, "has_docstring": has_doc}
            )

    public_classes = [c for c in classes if c["public"]]
    public_functions = [f for f in functions if f["public"]]

    return {
        "lines": lines,
        "classes": classes,
        "functions": functions,
        "class_count": len(classes),
        "function_count": len(functions),
        "public_classes": len(public_classes),
        "public_functions": len(public_functions),
        "classes_with_docstring": sum(1 for c in public_classes if c["has_docstring"]),
        "functions_with_docstring": sum(
            1 for f in public_functions if f["has_docstring"]
        ),
    }


def analyze_test_file(filepath):
    """Count test functions in a test file."""
    with open(filepath, "r", errors="replace") as fh:
        source = fh.read()
    lines = source.count("\n") + 1

    try:
        tree = ast.parse(source, filename=filepath)
    except SyntaxError:
        return {"lines": lines, "test_count": 0, "test_names": [], "parse_error": True}

    test_names = []
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            if node.name.startswith("test_"):
                test_names.append(node.name)

    return {"lines": lines, "test_count": len(test_names), "test_names": test_names}


def rel(path, base):
    return os.path.relpath(path, base)


# ---- ANALYSIS ----
print("=" * 80)
print("  ORCAFLEX PUBLIC API PACKAGE - MATURITY ASSESSMENT (#1656)")
print("=" * 80)

# (a) Source Module Inventory
print("\n(a) SOURCE MODULE INVENTORY")
print("-" * 80)
src_files = get_py_files(SRC_DIR)
total_lines = 0
total_classes = 0
total_functions = 0
total_public_classes = 0
total_public_functions = 0
total_classes_docstring = 0
total_functions_docstring = 0
src_analysis = {}

print(f"{'Module':<55} {'Lines':>6} {'Class':>6} {'Func':>6}")
print("-" * 80)

for f in src_files:
    info = analyze_source_file(f)
    rp = rel(f, SRC_DIR)
    src_analysis[rp] = info
    total_lines += info["lines"]
    total_classes += info.get("class_count", 0)
    total_functions += info.get("function_count", 0)
    total_public_classes += info["public_classes"]
    total_public_functions += info["public_functions"]
    total_classes_docstring += info["classes_with_docstring"]
    total_functions_docstring += info["functions_with_docstring"]
    print(
        f"  {rp:<53} {info['lines']:>6} {info.get('class_count',0):>6} {info.get('function_count',0):>6}"
    )

print("-" * 80)
print(
    f"  {'TOTAL':<53} {total_lines:>6} {total_classes:>6} {total_functions:>6}"
)
print(f"\n  Source files: {len(src_files)}")
print(f"  Total lines: {total_lines}")
print(f"  Total classes: {total_classes}  (public: {total_public_classes})")
print(f"  Total functions: {total_functions}  (public: {total_public_functions})")

# (b) Test File Inventory
print("\n\n(b) TEST FILE INVENTORY")
print("-" * 80)
print(f"{'Test File':<65} {'Tests':>6} {'Lines':>6}")
print("-" * 80)

all_test_files = []
total_tests = 0
total_test_lines = 0

# Public API tests
print("\n  --- tests/orcaflex/ (public API tests) ---")
pub_test_dir = os.path.join(BASE, "tests/orcaflex")
pub_test_files = get_py_files(pub_test_dir)
pub_tests_total = 0
for f in pub_test_files:
    info = analyze_test_file(f)
    rp = rel(f, BASE)
    all_test_files.append((rp, info))
    total_tests += info["test_count"]
    total_test_lines += info["lines"]
    pub_tests_total += info["test_count"]
    if info["test_count"] > 0 or "test_" in os.path.basename(f):
        print(f"  {rp:<63} {info['test_count']:>6} {info['lines']:>6}")

print(f"\n  Subtotal: {pub_tests_total} test functions")

# Solver layer tests
print("\n  --- tests/solvers/orcaflex/ (solver layer tests) ---")
solver_test_dir = os.path.join(BASE, "tests/solvers/orcaflex")
solver_test_files = get_py_files(solver_test_dir)
solver_tests_total = 0
for f in solver_test_files:
    info = analyze_test_file(f)
    rp = rel(f, BASE)
    all_test_files.append((rp, info))
    total_tests += info["test_count"]
    total_test_lines += info["lines"]
    solver_tests_total += info["test_count"]
    if info["test_count"] > 0 or "test_" in os.path.basename(f):
        print(f"  {rp:<63} {info['test_count']:>6} {info['lines']:>6}")

print(f"\n  Subtotal: {solver_tests_total} test functions")
print(f"\n  TOTAL: {total_tests} test functions across {len([t for t in all_test_files if t[1]['test_count'] > 0])} test files")
print(f"  Total test lines: {total_test_lines}")

# (c) Estimated Coverage by Module
print("\n\n(c) ESTIMATED COVERAGE BY MODULE (public API layer)")
print("-" * 80)
print(f"{'Source Module':<40} {'Funcs':>6} {'Matching Test File':>28} {'Tests':>6} {'Status':<10}")
print("-" * 80)

# Build map of test files to their test counts
test_map = {}
for rp, info in all_test_files:
    basename = os.path.basename(rp)
    test_map[basename] = info["test_count"]

covered_modules = 0
total_src_modules = 0

for f in src_files:
    rp = rel(f, SRC_DIR)
    basename = os.path.basename(f).replace(".py", "")
    if basename == "__init__":
        continue
    total_src_modules += 1
    info = src_analysis[rp]
    func_count = info.get("function_count", 0)

    # Find matching test file
    test_name = f"test_{basename}.py"
    test_count = test_map.get(test_name, 0)

    # Also check for partial matches
    if test_count == 0:
        for tname, tcount in test_map.items():
            if basename in tname and tcount > 0:
                test_name = tname
                test_count = tcount
                break

    if test_count > 0:
        covered_modules += 1
        status = "COVERED"
    else:
        status = "NO TESTS"

    print(
        f"  {rp:<38} {func_count:>6} {test_name:>28} {test_count:>6} {status:<10}"
    )

coverage_pct = (covered_modules / total_src_modules * 100) if total_src_modules > 0 else 0
print(f"\n  Modules with tests: {covered_modules}/{total_src_modules} ({coverage_pct:.0f}%)")

# (d) Docstring Coverage
print("\n\n(d) DOCSTRING COVERAGE (public classes and functions)")
print("-" * 80)

total_pub_items = total_public_classes + total_public_functions
total_documented = total_classes_docstring + total_functions_docstring
doc_pct = (total_documented / total_pub_items * 100) if total_pub_items > 0 else 0

print(f"  Public classes:   {total_public_classes:>4}  with docstrings: {total_classes_docstring:>4}  ({(total_classes_docstring/total_public_classes*100) if total_public_classes else 0:.0f}%)")
print(f"  Public functions: {total_public_functions:>4}  with docstrings: {total_functions_docstring:>4}  ({(total_functions_docstring/total_public_functions*100) if total_public_functions else 0:.0f}%)")
print(f"  OVERALL:          {total_pub_items:>4}  with docstrings: {total_documented:>4}  ({doc_pct:.0f}%)")

# Per-file docstring details
print("\n  Per-file breakdown:")
for f in src_files:
    rp = rel(f, SRC_DIR)
    info = src_analysis[rp]
    pub_c = info["public_classes"]
    pub_f = info["public_functions"]
    doc_c = info["classes_with_docstring"]
    doc_f = info["functions_with_docstring"]
    total = pub_c + pub_f
    documented = doc_c + doc_f
    if total > 0:
        pct = documented / total * 100
        print(f"    {rp:<50} {documented}/{total} ({pct:.0f}%)")

# (e) Maturity Verdict
print("\n\n(e) MATURITY VERDICT")
print("=" * 80)

# Criteria:
# - Module test coverage >= 80%
# - Docstring coverage >= 50%
# - Test-to-function ratio >= 0.5
# - At least some tests per significant module

test_ratio = total_tests / total_functions if total_functions else 0
print(f"  Module test coverage:  {coverage_pct:.0f}% (threshold: 80%)")
print(f"  Docstring coverage:    {doc_pct:.0f}% (threshold: 50%)")
print(f"  Test/function ratio:   {test_ratio:.2f} (threshold: 0.5)")
print(f"  Public API tests:      {pub_tests_total}")
print(f"  Solver layer tests:    {solver_tests_total}")
print(f"  Total tests:           {total_tests}")

if coverage_pct >= 80 and doc_pct >= 50 and test_ratio >= 0.5:
    verdict = "TESTED"
    detail = "The OrcaFlex public API package meets maturity thresholds."
elif coverage_pct >= 70:
    verdict = "TESTED (with caveats)"
    detail = "Most modules covered but some gaps remain."
else:
    verdict = "NEEDS MORE WORK"
    detail = "Insufficient test coverage or documentation."

print(f"\n  VERDICT: {verdict}")
print(f"  {detail}")
print("=" * 80)
