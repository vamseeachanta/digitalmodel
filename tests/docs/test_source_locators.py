# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Literal fixtures define locator bytes and bounds independently of the check."""

from __future__ import annotations

import ast
import json
import os
import re
import runpy
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

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


@pytest.mark.parametrize(
    ("workflow_name", "job_name"),
    [
        ("quality-gates-by-domain.yml", "domain-tests"),
        ("full-matrix-sweep.yml", "sweep-tests"),
    ],
)
def test_contracts_workflows_provision_configured_formatter(workflow_name, job_name):
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows" / workflow_name).read_text(encoding="utf-8")
    )
    steps = workflow["jobs"][job_name]["steps"]
    setup = next((step for step in steps if step.get("uses") == "actions/setup-node@v4"), None)
    assert setup is not None, "Contracts tests require a provisioned Node runtime"
    assert setup["if"] == "matrix.domain == 'contracts'"
    assert setup["with"]["node-version"] == "22"
    provision = next(
        (step for step in steps if step["name"] == "Provision source locator formatter"), None,
    )
    assert provision is not None, "Contracts tests require the pinned formatter"
    assert provision["if"] == "matrix.domain == 'contracts'"
    config = (ROOT / ".pre-commit-config.yaml").read_text(encoding="utf-8")
    pin = re.search(r"mirrors-prettier\s+rev: v([^\s]+)", config)
    assert pin is not None, "Configured Markdown formatter pin not found"
    assert f'"prettier@{pin[1]}"' in provision["run"]
    assert "npm install" in provision["run"]
    assert "SOURCE_LOCATORS_PRETTIER=" in provision["run"]
    assert '>> "$GITHUB_ENV"' in provision["run"]
    gate = next(step for step in steps if step["name"] == "Run domain gate")
    assert steps.index(setup) < steps.index(provision) < steps.index(gate)


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
        [executable, "--version"], capture_output=True, text=True, encoding="utf-8",
        check=True,
    ).stdout.strip()
    print(f"Formatter package {pin[1]}; CLI reports {version}")
    subprocess.run(
        [executable, "--write", str(markdown)], cwd=markdown.parent,
        capture_output=True, text=True, encoding="utf-8", check=True,
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
    markdown.write_text(f"Evidence: `{locator}`.\n", encoding="utf-8", newline="\n")
    assert check(markdown, root) == [f"pack.md:1: {locator}: {diagnostic}"]


@pytest.mark.parametrize("bounds", ["L1-L1", "L2-L2", "L1-L2"])
def test_bounds_are_inclusive(document, check, bounds):
    root, source, markdown = document
    # Two literal lines, with no final newline: both endpoints still exist.
    source.write_bytes(b"# first source line\n# second source line")
    markdown.write_text(
        f"`[src/example/__init__.py:{bounds}]`\n", encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == []


def test_source_locator_check_does_not_import_source(document, check):
    root, source, markdown = document
    source.write_bytes(b"raise RuntimeError('source must not execute')\n# second line\n")
    assert check(markdown, root) == []
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, encoding="utf-8", check=False,
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
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == []


@pytest.mark.parametrize("spacing", [" ", "\t", "   "])
def test_bare_locator_before_colon_prose_is_checked(document, check, spacing):
    root, _, markdown = document
    markdown.write_text(
        f"[src/absent.py:L1-L2]{spacing}: explanation\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == [
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source"
    ]


@pytest.mark.parametrize("indent", ["", " ", "   "])
def test_adjacent_colon_reference_definitions_are_excluded(document, check, indent):
    root, _, markdown = document
    markdown.write_text(
        f"{indent}[src/absent.py:L1-L2]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == []


@pytest.mark.parametrize("spacing", ["", " ", "\t", "   "])
def test_reference_link_spacing_remains_excluded(document, check, spacing):
    root, _, markdown = document
    markdown.write_text(
        f"[src/absent.py:L1-L2]{spacing}[reference]\n[reference]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == []


@pytest.mark.parametrize("definition_first", [False, True])
@pytest.mark.parametrize(
    "definition",
    [
        "[src/absent.py:L1-L2]: https://example.org/source\n",
        '   [SRC/ABSENT.PY:l1-l2]: reference.md "Source"\n',
        "[src/absent.py:L1-L2]: <reference.md>\n",
        "[src/absent.py:L1-L2]:\n  reference.md\n",
    ],
)
def test_defined_shortcut_reference_is_excluded(
    document, check, definition, definition_first,
):
    root, _, markdown = document
    shortcut = "[src/absent.py:L1-L2]\n"
    text = definition + "\n" + shortcut if definition_first else shortcut + "\n" + definition
    markdown.write_text(text, encoding="utf-8", newline="\n")
    assert check(markdown, root) == []


@pytest.mark.parametrize("delimiter", ["`", "``"])
def test_defined_shortcut_inside_inline_code_is_checked(document, check, delimiter):
    root, _, markdown = document
    markdown.write_text(
        f"{delimiter}[src/absent.py:L1-L2]{delimiter}\n\n"
        "[src/absent.py:L1-L2]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == [
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source"
    ]


@pytest.mark.parametrize("delimiter", ["`", "``"])
@pytest.mark.parametrize(
    ("prefix", "suffix"),
    [
        ("", ": explanation"),
        ("", "(reference.md)"),
        ("", "[reference]"),
        ("", " [reference]"),
        ("!", "(reference.md)"),
        ("\\", ": explanation"),
    ],
)
def test_inline_literal_with_link_syntax_is_checked(
    document, check, delimiter, prefix, suffix,
):
    root, _, markdown = document
    markdown.write_text(
        f"{delimiter}{prefix}[src/absent.py:L1-L2]{suffix}{delimiter}\n",
        encoding="utf-8", newline="\n",
    )
    errors = check(markdown, root)
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    expected = "pack.md:1: [src/absent.py:L1-L2]: missing Python source"
    assert (errors, result.returncode, result.stdout) == ([expected], 1, expected + "\n")
    assert result.stderr == ""


@pytest.mark.parametrize("suffix", [": explanation", "(reference.md)", "[reference]"])
def test_inline_literal_with_link_syntax_accepts_existing_source(document, check, suffix):
    root, _, markdown = document
    markdown.write_text(
        f"`[src/example/__init__.py:L1-L2]{suffix}`\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == []


@pytest.mark.parametrize("blank", ["", " ", "\t"])
@pytest.mark.parametrize("newline", [b"\n", b"\r\n", b"\r"])
def test_unmatched_backticks_in_separate_paragraphs_preserve_reference(
    document, check, blank, newline,
):
    root, _, markdown = document
    text = (
        f"`Unmatched\n{blank}\n[src/absent.py:L1-L2]: reference.md\n"
        f"{blank}\n[src/absent.py:L1-L2]`\n"
    )
    markdown.write_bytes(text.encode("utf-8").replace(b"\n", newline))
    errors = check(markdown, root)
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    # The definition occupies physical line three; neither backtick has a mate
    # in its paragraph, so the shortcut on physical line five is a link.
    assert (errors, result.returncode, result.stdout, result.stderr) == ([], 0, "", "")


@pytest.mark.parametrize(
    "boundary",
    ["# Heading", "## Heading", "Heading\n---", "***", "- item", "+ item",
     "* item", "1. item", "1) item", "> quote"],
)
def test_unmatched_backticks_cannot_cross_markdown_blocks(document, check, boundary):
    root, _, markdown = document
    markdown.write_text(
        f"`Unmatched\n{boundary}\n[src/absent.py:L1-L2]`\n\n"
        "[src/absent.py:L1-L2]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    # The heading, break or container ends the opening backtick's paragraph.
    assert (check(markdown, root), result.returncode, result.stdout, result.stderr) == (
        [], 0, "", "",
    )


@pytest.mark.parametrize(
    ("opening", "continuation", "blank"),
    [("> ", "> ", ">"), ("> > ", "> > ", "> >"),
     ("- ", "  ", ""), ("1. ", "   ", ""), ("> - ", ">   ", ">")],
)
def test_reference_definitions_in_containers_define_shortcuts(
    document, check, opening, continuation, blank,
):
    root, _, markdown = document
    markdown.write_text(
        f"{opening}[src/absent.py:L1-L2]\n{blank}\n"
        f"{continuation}[src/absent.py:L1-L2]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert (check(markdown, root), result.returncode, result.stdout, result.stderr) == (
        [], 0, "", "",
    )


@pytest.mark.parametrize(
    ("opening", "continuation"), [("", ""), ("> ", "> "), ("- ", "  ")],
)
def test_multiline_code_within_one_block_still_checks_literal_locators(
    document, check, opening, continuation,
):
    root, _, markdown = document
    markdown.write_text(
        f"{opening}`code\n{continuation}[src/absent.py:L1-L2]\n{continuation}`\n\n"
        "[src/absent.py:L1-L2]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    # The missing source is literal code on physical line two, despite its definition.
    expected = "pack.md:2: [src/absent.py:L1-L2]: missing Python source"
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert (check(markdown, root), result.returncode, result.stdout, result.stderr) == (
        [expected], 1, expected + "\n", "",
    )


@pytest.mark.parametrize(
    "code",
    ["> ```markdown\n> [src/absent.py:L1-L2]: reference.md\n> ```\n",
     ">     [src/absent.py:L1-L2]: reference.md\n",
     "- ```markdown\n  [src/absent.py:L1-L2]: reference.md\n  ```\n"],
)
def test_code_in_containers_cannot_define_shortcut(document, check, code):
    root, _, markdown = document
    markdown.write_text(
        code + "\n[src/absent.py:L1-L2]\n", encoding="utf-8", newline="\n",
    )
    errors = check(markdown, root)
    # Both the code example and the unresolved prose shortcut must be checked.
    assert len(errors) == 2
    assert all(error.endswith("missing Python source") for error in errors)


def test_unicode_before_code_preserves_locator_offsets(document, check):
    root, _, markdown = document
    markdown.write_text(
        "\U0001f4d6 `\U0001f4d6 [src/absent.py:L1-L2]` [src/absent.py:L1-L2]\n\n"
        "> [src/absent.py:L1-L2]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == [
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source"
    ]


def test_missing_markdown_parser_fails_closed(document, check, monkeypatch):
    root, _, markdown = document
    monkeypatch.setenv("SOURCE_LOCATORS_PRETTIER", "missing-markdown-formatter")
    expected = "pack.md: cannot parse Markdown; pinned formatter and Node.js required"
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert (check(markdown, root), result.returncode, result.stdout, result.stderr) == (
        [expected], 1, expected + "\n", "",
    )


@pytest.mark.parametrize("backslashes", [1, 2, 3, 4])
def test_escaped_backtick_is_not_an_inline_code_delimiter(document, check, backslashes):
    root, _, markdown = document
    prefix = "\\" * backslashes
    markdown.write_text(
        prefix + "`[src/absent.py:L1-L2]`\n\n"
        "[src/absent.py:L1-L2]: reference.md\n",
        encoding="utf-8", newline="\n",
    )
    # Pairs escape each other; an odd remaining backslash escapes the backtick.
    expected = [] if backslashes % 2 else [
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source"
    ]
    assert check(markdown, root) == expected


@pytest.mark.parametrize(
    ("code", "code_line", "line"),
    [
        ("`[src/absent.py:L1-L2]: reference.md`\n", 1, 3),
        ("``[src/absent.py:L1-L2]: reference.md``\n", 1, 3),
        ("`code\n[src/absent.py:L1-L2]: reference.md\n`\n", 2, 5),
        ("```markdown\n[src/absent.py:L1-L2]: reference.md\n```\n", 2, 5),
        ("~~~markdown\n[src/absent.py:L1-L2]: reference.md\n~~~\n", 2, 5),
        ("    [src/absent.py:L1-L2]: reference.md\n", 1, 3),
    ],
)
def test_definition_in_code_cannot_define_shortcut(document, check, code, code_line, line):
    root, _, markdown = document
    markdown.write_text(
        code + "\n[src/absent.py:L1-L2]\n", encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == [
        f"pack.md:{code_line}: [src/absent.py:L1-L2]: missing Python source",
        f"pack.md:{line}: [src/absent.py:L1-L2]: missing Python source"
    ]


@pytest.mark.parametrize("destination", ["", "reference.md trailing prose", "<unclosed"])
def test_invalid_definition_cannot_define_shortcut(document, check, destination):
    root, _, markdown = document
    markdown.write_text(
        f"[src/absent.py:L1-L2]\n\n[src/absent.py:L1-L2]: {destination}\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == [
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source"
    ]


def test_cli_accepts_defined_shortcut_and_rejects_inline_locator(document):
    root, _, markdown = document
    command = [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)]
    definition = "\n\n[src/absent.py:L1-L2]: reference.md\n"
    markdown.write_text(
        "[src/absent.py:L1-L2]" + definition, encoding="utf-8", newline="\n",
    )
    linked = subprocess.run(command, capture_output=True, text=True, encoding="utf-8")
    assert linked.returncode == 0, linked.stdout + linked.stderr
    assert linked.stdout == ""
    markdown.write_text(
        "`[src/absent.py:L1-L2]`" + definition, encoding="utf-8", newline="\n",
    )
    literal = subprocess.run(command, capture_output=True, text=True, encoding="utf-8")
    assert literal.returncode == 1
    assert literal.stdout == (
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source\n"
    )


@pytest.mark.parametrize("spacing", [" ", "\t", "   "])
def test_bare_locator_before_parenthetical_prose_is_checked(document, check, spacing):
    root, _, markdown = document
    markdown.write_text(
        f"[src/absent.py:L1-L2]{spacing}(explanation)\n",
        encoding="utf-8", newline="\n",
    )
    assert check(markdown, root) == [
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source"
    ]


@pytest.mark.parametrize(
    "separator", ["\v", "\f", "\x1c", "\x1d", "\x1e", "\x85", "\u2028", "\u2029"],
)
def test_control_characters_do_not_add_source_lines(document, check, separator):
    root, source, markdown = document
    source.write_text(
        f"value = 'first{separator}last'\n", encoding="utf-8", newline="\n",
    )
    # One assignment on physical line one; its string contains no CR or LF.
    parsed = ast.parse(source.read_text(encoding="utf-8"))
    assert parsed.body[0].lineno == parsed.body[0].end_lineno == 1
    markdown.write_bytes(b"[src/example/__init__.py:L1-L1]\n")
    assert check(markdown, root) == []
    # Line two does not exist, independently of the checker's line counter.
    markdown.write_bytes(b"[src/example/__init__.py:L2-L2]\n")
    assert check(markdown, root) == [
        "pack.md:1: [src/example/__init__.py:L2-L2]: line bounds outside source"
    ]


@pytest.mark.parametrize(
    "separator", ["\v", "\f", "\x1c", "\x1d", "\x1e", "\x85", "\u2028", "\u2029"],
)
def test_control_characters_do_not_add_document_lines(document, check, separator):
    root, _, markdown = document
    markdown.write_text(
        f"Prose{separator} [src/absent.py:L1-L2]\n[src/absent.py:L1-L2]\n",
        encoding="utf-8", newline="\n",
    )
    # Only the LF advances the document: the two locators are on lines one and two.
    assert check(markdown, root) == [
        "pack.md:1: [src/absent.py:L1-L2]: missing Python source",
        "pack.md:2: [src/absent.py:L1-L2]: missing Python source",
    ]


@pytest.mark.parametrize("newline", [b"\n", b"\r\n", b"\r"])
def test_physical_newlines_define_source_bounds_and_document_lines(
    document, check, newline,
):
    root, source, markdown = document
    source.write_bytes(b"# first" + newline + b"# second" + newline)
    markdown.write_bytes(
        b"[src/example/__init__.py:L1-L2]" + newline
        + b"[src/example/__init__.py:L3-L3]" + newline
    )
    # Two physical source lines validate the first range and reject line three.
    assert check(markdown, root) == [
        "pack.md:2: [src/example/__init__.py:L3-L3]: line bounds outside source"
    ]


def test_bare_table_locator_is_checked(document, check):
    root, _, markdown = document
    markdown.write_text(
        "| Evidence |\n|---|\n| [src/absent.py:L1-L2] |\n",
        encoding="utf-8", newline="\n",
    )
    # Header, separator, then the missing-source row: the diagnostic is on line three.
    assert check(markdown, root) == [
        "pack.md:3: [src/absent.py:L1-L2]: missing Python source"
    ]


def test_source_must_resolve_inside_root(document, check, tmp_path):
    root, source, markdown = document
    outside = tmp_path.parent / (tmp_path.name + "-outside.py")
    outside.write_text("# source outside root\n", encoding="utf-8", newline="\n")
    source.unlink()
    source.symlink_to(outside)
    assert check(markdown, root) == [
        f"pack.md:3: {LOCATOR.decode()}: source outside repository",
        f"pack.md:7: {LOCATOR.decode()}: source outside repository",
    ]


def test_cli_checks_only_supplied_markdown_and_reports_failure(document):
    root, _, markdown = document
    other = root / "other.md"
    other.write_text("[src/absent.py:L1-L2]\n", encoding="utf-8", newline="\n")
    command = [sys.executable, str(SCRIPT), "--root", str(root), str(markdown)]
    good = subprocess.run(
        command, capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert good.returncode == 0, good.stdout + good.stderr
    bad = subprocess.run(
        command + [str(other)], capture_output=True, text=True, encoding="utf-8",
        check=False,
    )
    # CLI convention: one means at least one supplied document failed.
    assert bad.returncode == 1
    assert "other.md:1: [src/absent.py:L1-L2]: missing Python source" in bad.stdout
