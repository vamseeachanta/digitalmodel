# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""ROV pack consistency; synthetic arithmetic is not an engineering design basis.

Input citation: agent-pack.md, section 4, synthetic basis S. Independent
Pythagoras gives sqrt(3**2 + 4**2 + 0**2) = sqrt(25) = 5 m. Thus reaches
5 and 6 m pass, 4 m fails only reach; the oversized axis 0.3 > 0.2 m
fails only envelope at reach 6 m. No expected result is taken from the checker.
"""

from __future__ import annotations

import os
import re
import runpy
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

# Repository root: this file is under tests/docs (two containing directories).
ROOT = Path(__file__).resolve().parents[2]
PACKAGE = Path("src/digitalmodel/subsea/rov")
PACK = PACKAGE / "docs/agent-pack.md"
SCRIPT = ROOT / "scripts/docs/check_agent_pack_functions.py"


def test_pack_matches_public_definitions():
    assert (ROOT / PACK).is_file(), "ROV agent pack is missing"
    checker = runpy.run_path(str(SCRIPT))["check_pack"]
    assert checker(ROOT / PACK, ROOT) == []
    text = (ROOT / PACK).read_text(encoding="utf-8")
    # Ten numbered sections are the existing knowledge-pack contract.
    assert re.findall(r"^## (\d+)\.", text, re.MULTILINE) == [
        str(number) for number in range(1, 11)
    ]


def test_registry_points_to_real_pack():
    registry = yaml.safe_load(
        (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    )
    subsea = next(row for row in registry["modules"] if row["module"] == "subsea")
    assert subsea["submodules"]["rov"]["agent_pack"] == PACK.as_posix()
    assert (ROOT / subsea["submodules"]["rov"]["agent_pack"]).is_file()


def _run_checker(scratch):
    environment = {key: value for key, value in os.environ.items()
                   if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    return subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(scratch), "--pack", str(PACK)],
        capture_output=True, text=True, check=False, env=environment,
    )


@pytest.mark.parametrize("drift", ["added", "stale"])
def test_added_and_stale_symbols_are_rejected(tmp_path, drift):
    """Copy a synthetic package; calibrate both directions without real-source edits."""
    template = tmp_path / "template" / PACKAGE
    (template / "docs").mkdir(parents=True)
    (template / "__init__.py").write_text("", encoding="utf-8")
    # Locators count the literal synthetic source lines below, not API values.
    (template / "model.py").write_text(
        "class Probe:\n    def reach(self):\n        pass\n\n"
        "def screen():\n    pass\n", encoding="utf-8",
    )
    (template / "docs/agent-pack.md").write_text(
        "## 2. Inputs and outputs\n"
        "| Public symbol | Evidence |\n|---|---|\n"
        f"| `Probe` | [{PACKAGE}/model.py:L1-L3] |\n"
        f"| `Probe.reach` | [{PACKAGE}/model.py:L2-L3] |\n"
        f"| `screen` | [{PACKAGE}/model.py:L5-L6] |\n"
        "## 3. Method\n", encoding="utf-8",
    )
    scratch = tmp_path / "candidate"
    copied = scratch / PACKAGE
    shutil.copytree(template, copied)
    control = _run_checker(scratch)
    # CLI convention: zero is success; one is inventory drift (checker main).
    assert control.returncode == 0, control.stdout + control.stderr
    if drift == "added":
        (copied / "added.py").write_text("def added():\n    pass\n", encoding="utf-8")
        expected = f"{PACK}: code-to-pack: added missing ({PACKAGE}/added.py)"
    else:
        source = copied / "model.py"
        source.write_text(
            source.read_text(encoding="utf-8").replace(
                "def screen():\n    pass\n", "# definition removed\n# body removed\n",
            ),
            encoding="utf-8",
        )
        expected = f"{PACK}: pack-to-code: screen not defined ({PACKAGE}/model.py)"
    result = _run_checker(scratch)
    assert result.returncode == 1, result.stdout + result.stderr
    assert result.stdout.splitlines() == [expected]
    assert result.stderr == ""


def test_documented_example_matches_independent_calculation():
    """Execute the real Markdown example; compare with hand calculations above."""
    text = (ROOT / PACK).read_text(encoding="utf-8")
    (example,) = re.findall(r"^```python\n(.*?)^```", text, re.MULTILINE | re.DOTALL)
    namespace = {}
    exec(compile(example, PACK.as_posix(), "exec"), namespace)
    assert namespace["tool"].envelope_m == namespace["target"].envelope_m
    assert namespace["target"].location_m == (3.0, 4.0, 0.0)  # Synthetic basis S.
    assert namespace["at_boundary"].compatible is True
    assert namespace["at_boundary"].reasons == ()
    assert namespace["beyond_boundary"].compatible is True
    assert namespace["beyond_boundary"].reasons == ()
    assert namespace["short_reach"].compatible is False
    assert namespace["short_reach"].reasons == ("reach",)
    assert namespace["oversize"].compatible is False
    assert namespace["oversize"].reasons == ("envelope",)
