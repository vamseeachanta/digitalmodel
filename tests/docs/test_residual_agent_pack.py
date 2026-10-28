# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Residual pack consistency; synthetic arithmetic is not engineering qualification."""

from __future__ import annotations

import hashlib
import math
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = Path("src/digitalmodel/residual")
PACK = PACKAGE / "docs/agent-pack.md"
CHECKER = ROOT / "scripts/docs/check_agent_pack_functions.py"


def _check_pack(root):
    env = os.environ.copy()
    for binding in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(binding, None)
    return subprocess.run(
        [sys.executable, str(CHECKER), "--root", str(root), "--pack", str(PACK)],
        cwd=root, env=env, capture_output=True, text=True, check=False,
    )


def test_pack_matches_public_definitions():
    result = _check_pack(ROOT)
    assert result.returncode == 0, result.stdout + result.stderr
    assert result.stdout == ""


def test_registry_points_to_real_pack():
    registry = yaml.safe_load(
        (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    )
    entry = next(row for row in registry["modules"] if row["module"] == "residual")
    assert entry["submodules"]["residual"]["agent_pack"] == PACK.as_posix()
    assert (ROOT / PACK).is_file()


def test_added_and_stale_symbols_are_rejected(tmp_path):
    # Calibrate the unchanged checker on copies, including nested/new-file API.
    before = {
        path: hashlib.sha256(path.read_bytes()).hexdigest()
        for path in (ROOT / PACKAGE).rglob("*") if path.is_file()
    }
    for drift in ("added", "stale"):
        root = tmp_path / drift
        copied = root / PACKAGE
        shutil.copytree(ROOT / PACKAGE, copied, ignore=shutil.ignore_patterns("__pycache__"))
        result = _check_pack(root)
        assert result.returncode == 0, result.stdout + result.stderr
        if drift == "added":
            nested = copied / "nested"
            nested.mkdir()
            (nested / "extra.py").write_text(
                "class Extra:\n    def added(self):\n        pass\n", encoding="utf-8"
            )
            expected = "code-to-pack: Extra.added missing"
        else:
            source = copied / "skill.py"
            source.write_text(
                source.read_text(encoding="utf-8").replace("def rmse(", "def renamed_rmse("),
                encoding="utf-8",
            )
            expected = "pack-to-code: rmse not defined"
        result = _check_pack(root)
        assert result.returncode == 1, result.stdout + result.stderr
        assert expected in result.stdout
        assert result.stderr == ""
    after = {
        path: hashlib.sha256(path.read_bytes()).hexdigest()
        for path in (ROOT / PACKAGE).rglob("*") if path.is_file()
    }
    assert after == before


def test_documented_example_matches_independent_calculation():
    text = (ROOT / PACK).read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 4\..*?(?=^## 5\.)", text)
    assert section is not None
    examples = re.findall(r"```python\n(.*?)```", section[0], re.DOTALL)
    assert len(examples) == 1
    result = {}
    exec(compile(examples[0], PACK.as_posix(), "exec"), result)
    # Hand calculation, pack section 4: (3² + 4²)/2 = 12.5; improved errors vanish.
    assert result["observed"] == [0, 0]
    assert result["predicted"] == [3, 4]
    assert result["improved"] == [0, 0]
    assert result["before"] == pytest.approx(math.sqrt(12.5), rel=1e-12, abs=0.0)
    assert result["after"] == 0.0
    assert result["gain"] == pytest.approx(math.sqrt(12.5) - 0.0, rel=1e-12, abs=0.0)
    # Hand differences, pack section 4: 5-3=2, 5-2=3, 5-4=1; noise=2.
    assert result["boundary_gains"] == [2.0, 3.0, 1.0]
    assert len(result["significant"]) == 3
    for actual, expected in zip(result["significant"], (False, True, False)):
        assert actual is expected


def test_empty_and_constant_series_limits():
    from digitalmodel.residual import ResidualModel, correlation, rmse

    # Empty/constant branches: skill.py:13-30 and model.py:113-123.
    assert math.isnan(rmse([], []))
    assert math.isnan(correlation([], []))
    assert math.isnan(correlation([0], [0]))
    assert math.isnan(correlation([0, 0], [3, 4]))
    assert math.isnan(correlation([3, 4], [0, 0]))
    with pytest.raises(ValueError, match="shape mismatch"):
        rmse([0], [0, 0])
    assert ResidualModel.fit([], []).status.startswith("escalate:insufficient_pairs")
    # Eight pairs is the default minimum in model.py:29; constants have no slope.
    model = ResidualModel.fit([0] * 8, [0] * 8)
    assert model.status == "escalate:degenerate_predicted(no_variation)"


def test_pack_keeps_ten_numbered_sections():
    text = (ROOT / PACK).read_text(encoding="utf-8")
    # The established pack form uses sections 1 through 10 (task acceptance).
    assert re.findall(r"^## (\d+)\.", text, re.MULTILINE) == [
        str(number) for number in range(1, 11)
    ]
