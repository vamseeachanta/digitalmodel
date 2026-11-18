# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Tug pack consistency and independently calculated synthetic arithmetic.

Numerical basis: agent-pack.md section 4 specifies a synthetic reference of
2 knots, exponent 2 and cap 4. Squaring speed/reference by hand gives
0, 1/4, 1, 9/4, 4 and 9; the unity floor and upper cap give
1, 1, 1, 9/4, 4 and 4. These are exact binary fractions, not design criteria.
Other integers below are file locators, section numbers and CLI exit statuses.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
from fractions import Fraction
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PACK_PATH = Path("src/digitalmodel/tug/docs/agent-pack.md")
PACK = ROOT / PACK_PATH
CHECKER = ROOT / "scripts/docs/check_agent_pack_functions.py"


def _run_checker(root):
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    return subprocess.run(
        [sys.executable, str(CHECKER), "--root", str(root), "--pack", str(PACK_PATH)],
        cwd=root,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )


def test_pack_matches_public_definitions():
    """The shipped checker accepts the real pack and its registry pointer."""
    assert PACK.is_file(), "tug knowledge pack is missing"
    result = _run_checker(ROOT)
    assert result.returncode == 0, result.stdout + result.stderr
    registry = (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    entry = re.search(r"(?ms)^  - module: tug\n(.*?)(?=^  - module:|\Z)", registry)
    assert entry is not None
    pointers = re.findall(r"^\s+agent_pack: (\S+)\s*$", entry[1], re.MULTILINE)
    assert pointers == [PACK_PATH.as_posix()]
    assert (ROOT / pointers[0]).resolve() == PACK.resolve()


def test_added_and_stale_symbols_are_rejected(tmp_path):
    """Copied synthetic packages calibrate both directions without source edits."""
    seed = tmp_path / "seed"
    package = seed / "src/digitalmodel/tug"
    (package / "docs").mkdir(parents=True)
    source = (
        "def documented():\n    return None\n\n\n"
        "class Result:\n    def area(self):\n        return None\n"
    )
    (package / "model.py").write_text(source, encoding="utf-8")
    (seed / PACK_PATH).write_text(
        "## 2. Inputs and outputs\n"
        "| Public symbol | Evidence |\n|---|---|\n"
        "| `documented` | [src/digitalmodel/tug/model.py:L1-L2] |\n"
        "| `Result` | [src/digitalmodel/tug/model.py:L5-L7] |\n"
        "| `Result.area` | [src/digitalmodel/tug/model.py:L6-L7] |\n"
        "## 3. Method\n",
        encoding="utf-8",
    )
    for case, changed_source, diagnostic in (
        ("added", source + "\ndef added():\n    pass\n", "code-to-pack: added missing"),
        ("stale", source.replace("documented", "_retired"), "pack-to-code: documented not defined"),
        ("member", source.replace("def area", "def added_member"), "code-to-pack: Result.added_member missing"),
    ):
        copied = tmp_path / case
        shutil.copytree(seed, copied)
        assert _run_checker(copied).returncode == 0
        (copied / "src/digitalmodel/tug/model.py").write_text(
            changed_source, encoding="utf-8"
        )
        result = _run_checker(copied)
        assert result.returncode == 1, result.stdout + result.stderr
        assert diagnostic in result.stdout
    assert (package / "model.py").read_text(encoding="utf-8") == source


def test_documented_example_matches_independent_calculation():
    """Execute the documented public call against an independent rational oracle."""
    assert PACK.is_file(), "tug knowledge pack is missing"
    text = PACK.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 4\..*?(?=^## 5\.)", text)
    assert section is not None
    examples = re.findall(r"```python\n(.*?)\n```", section[0], re.DOTALL)
    assert len(examples) == 1
    namespace = {}
    exec(compile(examples[0], "<tug-pack-example>", "exec"), namespace)
    assert namespace["speeds_kn"] == (0, 1, 2, 3, 4, 6)
    assert namespace["reference_speed_kn"] == 2
    assert namespace["exponent"] == 2
    assert namespace["cap"] == 4
    raw = tuple(Fraction(speed, 2) ** 2 for speed in (0, 1, 2, 3, 4, 6))
    assert raw == (0, Fraction(1, 4), 1, Fraction(9, 4), 4, 9)
    expected = (1, 1, 1, Fraction(9, 4), 4, 4)
    for ratio, factor in zip(raw, expected):
        assert 1 <= factor <= 4
        assert factor == (1 if ratio <= 1 else 4 if ratio >= 4 else ratio)
    assert namespace["factors"] == expected


def test_pack_has_ten_sections_and_qualification_limits():
    """The pack retains the existing form and explicit unevaluated disciplines."""
    assert PACK.is_file(), "tug knowledge pack is missing"
    text = PACK.read_text(encoding="utf-8")
    assert re.findall(r"^## (\d+)\.", text, re.MULTILINE) == [
        str(number) for number in range(1, 11)
    ]
    for subject in ("Towline class factors", "Bollard-pull assumptions", "Emission factors", "Stability/girting criteria"):
        assert re.search(rf"\| {re.escape(subject)} \| NOT_EVALUATED \|", text)
    for relative, first, last in re.findall(r"\[([^\]\n]+):L(\d+)-L(\d+)\]", text):
        source = ROOT / relative
        assert source.is_file(), relative
        assert 1 <= int(first) <= int(last) <= len(
            source.read_text(encoding="utf-8").splitlines()
        ), f"invalid locator: {relative}:L{first}-L{last}"
