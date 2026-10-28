# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Literal fixtures define locator bytes and bounds independently of the check."""

from __future__ import annotations

import json
import os
import re
import runpy
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

# tests/docs is two directory levels below the repository root.
ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/docs/source_locators.py"
# The fixture below has exactly two physical source lines, numbered from one.
LOCATOR = b"[src/example/__init__.py:L1-L2]"


@pytest.fixture
def document(tmp_path):
    source = tmp_path / "src/example/__init__.py"
    source.parent.mkdir(parents=True)
    source.write_bytes(b"# first source line\n# second source line\n")
    markdown = tmp_path / "pack.md"
    markdown.write_bytes(
        b"## 3. Method\n\nInitializer: `" + LOCATOR + b"`.\n\n"
        b"| Evidence | Meaning |\n|---|---|\n| `" + LOCATOR + b"` | initializer |\n"
    )
    return tmp_path, source, markdown


@pytest.fixture
def check():
    return runpy.run_path(str(SCRIPT))["check_markdown"]


def test_initializer_locator_survives_configured_formatter(document):
    _, source, markdown = document
    # Literal source text establishes L1-L2; the helper supplies no expectation.
    assert source.read_bytes() == b"# first source line\n# second source line\n"
    config = (ROOT / ".pre-commit-config.yaml").read_text(encoding="utf-8")
    pin = re.search(r"mirrors-prettier\s+rev: v([^\s]+)", config)
    assert pin is not None, "Configured Markdown formatter pin not found"
    executable = shutil.which(os.environ.get("SOURCE_LOCATORS_PRETTIER", "prettier"))
    assert executable is not None, (
        "Configured formatter unavailable; set SOURCE_LOCATORS_PRETTIER to its executable"
    )
    # The executable must belong to the pinned package, including prereleases.
    manifest = Path(executable).resolve().parent.parent / "package.json"
    package = json.loads(manifest.read_text(encoding="utf-8"))
    assert package["name"] == "prettier"
    assert package["version"] == pin[1]
    version = subprocess.run(
        [executable, "--version"], capture_output=True, text=True, check=True,
    ).stdout.strip()
    print(f"Formatter package {pin[1]}; CLI reports {version}")
    subprocess.run(
        [executable, "--write", str(markdown)], cwd=markdown.parent,
        capture_output=True, text=True, check=True,
    )
    formatted = markdown.read_bytes()
    assert b"Initializer: `" + LOCATOR + b"`." in formatted
    # Check the cell content while permitting surrounding table padding.
    cells = [cell.strip() for line in formatted.splitlines() if line.startswith(b"|")
             for cell in line.split(b"|")]
    assert b"`" + LOCATOR + b"`" in cells
    # One paragraph occurrence plus one table occurrence equals two.
    assert formatted.count(LOCATOR) == 2


def test_locator_outside_inventory_resolves(document, check):
    root, _, markdown = document
    # Both paragraph and table are in section three, with no inventory table.
    assert check(markdown, root) == []


@pytest.mark.parametrize(
    ("locator", "diagnostic"),
    [
        ("[src/example/absent.py:L1-L2]", "missing Python source"),
        ("[src/example/__init__.py:L2-L1]", "reversed line bounds"),
        # The two-line fixture ends at L2; L3 is one line beyond its end.
        ("[src/example/__init__.py:L1-L3]", "line bounds outside source"),
        # Line numbering begins at one, so zero is outside the source.
        ("[src/example/__init__.py:L0-L1]", "line bounds outside source"),
    ],
)
def test_missing_source_and_invalid_line_bounds_are_diagnosed(
    document, check, locator, diagnostic,
):
    root, _, markdown = document
    markdown.write_text(f"Evidence: `{locator}`.\n", encoding="utf-8")
    assert check(markdown, root) == [f"pack.md:1: {locator}: {diagnostic}"]


@pytest.mark.parametrize("bounds", ["L1-L1", "L2-L2", "L1-L2"])
def test_bounds_are_inclusive(document, check, bounds):
    root, source, markdown = document
    # Two literal lines, with no final newline: both endpoints still exist.
    source.write_bytes(b"# first source line\n# second source line")
    markdown.write_text(f"`[src/example/__init__.py:{bounds}]`\n", encoding="utf-8")
    assert check(markdown, root) == []


def test_source_locator_check_does_not_import_source(document, check):
    root, source, markdown = document
    source.write_bytes(b"raise RuntimeError('source must not execute')\n# second line\n")
    assert check(markdown, root) == []
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, check=False,
    )
    # CLI convention: zero means all supplied documents passed.
    assert result.returncode == 0, result.stdout + result.stderr


def test_external_links_and_ordinary_prose_are_not_local_locators(document, check):
    root, _, markdown = document
    markdown.write_text(
        "[https://example.org/absent.py:L1-L2]\n"
        "[source](https://example.org/absent.py:L1-L2)\n"
        "[src/absent.py:L1-L2](https://example.org/source)\n"
        "[src/absent.py:L1-L2][external]\n"
        "[external]: https://example.org/source\n"
        "Discuss src/absent.py and inclusive L1-L2 in ordinary prose.\n",
        encoding="utf-8",
    )
    assert check(markdown, root) == []


def test_bare_table_locator_is_checked(document, check):
    root, _, markdown = document
    markdown.write_text(
        "| Evidence |\n|---|\n| [src/absent.py:L1-L2] |\n", encoding="utf-8",
    )
    # Header, separator, then the missing-source row: the diagnostic is on line three.
    assert check(markdown, root) == [
        "pack.md:3: [src/absent.py:L1-L2]: missing Python source"
    ]


def test_source_must_resolve_inside_root(document, check, tmp_path):
    root, source, markdown = document
    outside = tmp_path.parent / (tmp_path.name + "-outside.py")
    outside.write_text("# source outside root\n", encoding="utf-8")
    source.unlink()
    source.symlink_to(outside)
    assert check(markdown, root) == [
        f"pack.md:3: {LOCATOR.decode()}: source outside repository",
        f"pack.md:7: {LOCATOR.decode()}: source outside repository",
    ]


def test_cli_checks_only_supplied_markdown_and_reports_failure(document):
    root, _, markdown = document
    other = root / "other.md"
    other.write_text("[src/absent.py:L1-L2]\n", encoding="utf-8")
    command = [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)]
    good = subprocess.run(command, capture_output=True, text=True, check=False)
    assert good.returncode == 0, good.stdout + good.stderr
    bad = subprocess.run(command + [str(other)], capture_output=True, text=True, check=False)
    # CLI convention: one means at least one supplied document failed.
    assert bad.returncode == 1
    assert "other.md:1: [src/absent.py:L1-L2]: missing Python source" in bad.stdout
