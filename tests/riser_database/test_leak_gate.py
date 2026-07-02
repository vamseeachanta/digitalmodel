"""Suite 6 (#1245): the leak gate — hard merge gate for every public-route row.

Two layers (plan D4):

* Layer A (always runs, public CI included): regex structural checks over the
  actual loaded rows AND the raw data files. Patterns here are generic and
  public-safe — no client tokens, no internal share names.
* Layer B (in-context only): matches every public row against the PRIVATE
  workspace-hub deny list, with case/separator normalization to close
  fixed-string evasion gaps. Skips loudly when workspace-hub is absent; a
  green in-context run BEFORE the first push is part of the merge gate.

Failure output never echoes a private pattern — only its group and index.
"""
from __future__ import annotations

import os
import re
from pathlib import Path

import pytest
import yaml

from digitalmodel.riser_database.loader import DEFAULT_DB_ROOT, RiserDatabase

# -- Layer A: structural patterns (public-safe) ----------------------------------

STRUCTURAL_PATTERNS = (
    (re.compile(r"/mnt/"), "absolute mount path"),
    (re.compile(r"/home/"), "home directory path"),
    (re.compile(r"(?i)\b[a-z]:\\"), "windows drive path"),
    (re.compile(r"\\\\"), "UNC path"),
    (re.compile(r"[A-Za-z0-9_.+-]+@[A-Za-z0-9-]+\.[A-Za-z]"), "email address"),
    (re.compile(r"(?i)https?://"), "URL"),
)

_DATA_FILES = (
    "config_catalog.csv",
    "standards_crosswalk.csv",
    "material_sn_scf_dff.csv",
    "manifest.yaml",
)


def test_layer_a_public_rows_structurally_clean():
    violations = []
    for table, key, flat in RiserDatabase.load().iter_public_rows():
        for pattern, label in STRUCTURAL_PATTERNS:
            if pattern.search(flat):
                violations.append((table, key, label))
    for name in _DATA_FILES:
        text = (DEFAULT_DB_ROOT / name).read_text()
        for pattern, label in STRUCTURAL_PATTERNS:
            if pattern.search(text):
                violations.append((name, "<raw file>", label))
    assert not violations, violations


# -- Layer B: private deny list (in-context hard gate) -----------------------------

_KNOWN_HUB_ROOTS = (
    Path("/mnt/local-analysis/workspace-hub"),  # abs-path-allowed
    Path.home() / "workspace-hub",
)


def _deny_list_path() -> Path | None:
    env = os.environ.get("WORKSPACE_HUB_ROOT")
    candidates = ([Path(env)] if env else []) + list(_KNOWN_HUB_ROOTS)
    for base in candidates:
        path = base / ".legal-deny-list.yaml"
        if path.is_file():
            return path
    return None


def _normalize(text: str) -> str:
    """Case- and separator-insensitive form: closes the evasion gap where a
    deny pattern differs from row text only by ``_``/``-``/whitespace."""
    return re.sub(r"[\s_\-]+", "", text).casefold()


def _deny_patterns(path: Path) -> list[tuple[str, str]]:
    """Yield (group, normalized_pattern) for every deny entry, walking the
    whole YAML so new groups are picked up automatically."""
    doc = yaml.safe_load(path.read_text())
    found: list[tuple[str, str]] = []

    def walk(node, group):
        if isinstance(node, dict):
            if "pattern" in node and isinstance(node["pattern"], str):
                normalized = _normalize(node["pattern"])
                if normalized:
                    found.append((group, normalized))
                return
            for key, child in node.items():
                if key == "exclusions":
                    continue
                walk(child, key if group == "<root>" else group)
        elif isinstance(node, list):
            for child in node:
                walk(child, group)

    walk(doc, "<root>")
    return found


def test_layer_b_private_deny_list_scan():
    deny_path = _deny_list_path()
    if deny_path is None:
        pytest.skip(
            "workspace-hub .legal-deny-list.yaml not available (standalone "
            "mode) — this test is the in-context HARD MERGE GATE and must run "
            "green in a workspace-hub checkout before the branch is pushed"
        )
    patterns = _deny_patterns(deny_path)
    assert patterns, "deny list parsed to zero patterns — parser drift?"
    violations = []
    for table, key, flat in RiserDatabase.load().iter_public_rows():
        normalized_row = _normalize(flat)
        for index, (group, pattern) in enumerate(patterns):
            if pattern in normalized_row:
                # Never echo the private pattern itself.
                violations.append((table, key, f"{group}[{index}]"))
    assert not violations, violations
