# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Corrosion documentation contracts; synthetic arithmetic is no design basis.

Numerical example inputs and expected categories are derived independently in
the pack's section 4. Synthetic locator numbers below refer to the fixture's
literal source lines, rather than any engineering quantity.
"""

from __future__ import annotations

import os
import re
import runpy
import shutil
import subprocess
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
PACK = ROOT / "src/digitalmodel/corrosion/docs/agent-pack.md"
CHECKER = ROOT / "scripts/docs/check_agent_pack_functions.py"


def test_pack_matches_public_definitions():
    assert PACK.is_file(), "Corrosion knowledge pack is missing"
    check_pack = runpy.run_path(str(CHECKER))["check_pack"]
    assert check_pack(PACK, ROOT) == []
    registry = yaml.safe_load(
        (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    )
    entries = [entry for entry in registry["modules"] if entry["module"] == "corrosion"]
    assert len(entries) == 1
    assert entries[0]["agent_pack"] == PACK.relative_to(ROOT).as_posix()


def test_added_and_stale_symbols_are_rejected(tmp_path):
    seed = tmp_path / "seed"
    package = seed / "src/digitalmodel/corrosion"
    (package / "docs").mkdir(parents=True)
    (package / "model.py").write_text(
        "class Result:\n    def value(self):\n        return 'synthetic'\n",
        encoding="utf-8",
    )
    relative_pack = "src/digitalmodel/corrosion/docs/agent-pack.md"
    table = (
        "## 2. Inputs and outputs\n"
        "| Public symbol | Evidence |\n|---|---|\n"
        "| `Result` | [src/digitalmodel/corrosion/model.py:L1-L3] |\n"
        "| `Result.value` | [src/digitalmodel/corrosion/model.py:L2-L3] |\n"
        "## 3. Method\n"
    )
    (seed / relative_pack).write_text(table, encoding="utf-8")
    for drift in ("added", "stale"):
        copied = tmp_path / drift
        shutil.copytree(seed, copied)
        pack = copied / relative_pack
        assert _run_checker(pack, copied).returncode == 0
        if drift == "added":
            (pack.parent.parent / "additional.py").write_text(
                "def added():\n    return 'synthetic'\n", encoding="utf-8"
            )
            diagnostic = "code-to-pack: added missing"
        else:
            pack.write_text(
                table.replace(
                    "## 3. Method",
                    "| `removed` | [src/digitalmodel/corrosion/model.py:L1-L3] |\n"
                    "## 3. Method",
                ),
                encoding="utf-8",
            )
            diagnostic = "pack-to-code: removed not defined"
        result = _run_checker(pack, copied)
        assert result.returncode == 1, result.stdout + result.stderr
        assert diagnostic in result.stdout


def _run_checker(pack, root):
    # Repository bindings must not redirect fixture/checker child processes.
    environment = {
        key: value
        for key, value in os.environ.items()
        if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}
    }
    return subprocess.run(
        [sys.executable, str(CHECKER), "--root", str(root), "--pack", str(pack)],
        cwd=root,
        env=environment,
        capture_output=True,
        text=True,
        check=False,
    )


def test_documented_example_matches_independent_calculation():
    assert PACK.is_file(), "Corrosion knowledge pack is missing"
    text = PACK.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 4\..*?(?=^## 5\.)", text)
    assert section is not None
    examples = re.findall(r"```python\n(.*?)```", section[0], re.DOTALL)
    assert len(examples) == 1
    namespace = {}
    exec(compile(examples[0], PACK.name, "exec"), namespace)
    # Independent interval arithmetic: 1 + 0.25 = 1.25 V. Thus 0.5 and 1
    # belong below/on the allowable; 1.125 and 1.25 belong above 1 through
    # 1.25 inclusive; 1.5 exceeds 1.25. These are synthetic software inputs.
    assert namespace["allowable_delta_v"] == 1.0
    assert namespace["marginal_band_v"] == 0.25
    assert namespace["deltas_v"] == (0.5, 1.0, 1.125, 1.25, 1.5)
    assert namespace["verdicts"] == ("OK", "OK", "MARGINAL", "MARGINAL", "PROTECT")


def test_pack_retains_ten_sections_and_qualification_limits():
    assert PACK.is_file(), "Corrosion knowledge pack is missing"
    text = PACK.read_text(encoding="utf-8")
    assert re.findall(r"(?m)^## (\d+)\.", text) == [str(i) for i in range(1, 11)]
    assert "software classification check" in text
    for topic in (
        "Default metal potentials",
        "Environmental thresholds",
        "Corrosion rates",
        "Physical applicability",
    ):
        assert re.search(rf"\|\s+{re.escape(topic)}\s+\|\s+NOT_EVALUATED\s+\|", text)
