# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Collect test citations in package documentation without executing tests."""

import os
import re
import subprocess
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
PACKS = sorted((ROOT / "src/digitalmodel").glob("**/docs/agent-pack.md"))


def collect_pack(pack: Path, root: Path, basetemp: Path) -> None:
    text = pack.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## [45]\..*?(?=^## 6\.|\Z)", text)
    ids = list(dict.fromkeys(re.findall(
        r"`(tests/[^`\n]+::[^`\n]+)`", section[0] if section else ""
    )))
    print(f"{pack.relative_to(root).as_posix()}: {len(ids)} ids")
    if not ids:
        return
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    result = subprocess.run(
        [sys.executable, "-m", "pytest", "--collect-only", "-q",
         "-p", "no:cacheprovider", f"--basetemp={basetemp}", *ids],
        cwd=root, env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_missing_citation_fails(tmp_path: Path) -> None:
    tests = tmp_path / "tests"
    tests.mkdir()
    (tests / "test_example.py").write_text("def test_exists():\n    pass\n")
    pack = tmp_path / "agent-pack.md"
    pack.write_text("## 4. Examples\n`tests/test_example.py::test_missing`\n## 5. Limits\n")
    with pytest.raises(AssertionError, match="test_missing"):
        collect_pack(pack, tmp_path, tmp_path / "collection")


def test_missing_gate_citation_fails(tmp_path: Path) -> None:
    tests = tmp_path / "tests"
    tests.mkdir()
    (tests / "test_example.py").write_text("def test_exists():\n    pass\n")
    pack = tmp_path / "agent-pack.md"
    pack.write_text("## 4. Examples\nNone.\n## 5. Gates\n"
                    "`tests/test_example.py::test_missing`\n## 6. Pitfalls\n")
    with pytest.raises(AssertionError, match="test_missing"):
        collect_pack(pack, tmp_path, tmp_path / "collection")


def test_pack_without_citations_passes(tmp_path: Path, capsys) -> None:
    pack = tmp_path / "agent-pack.md"
    pack.write_text("## 4. Examples\nNone.\n## 5. Limits\nNone.\n")
    collect_pack(pack, tmp_path, tmp_path / "collection")
    assert "agent-pack.md: 0 ids" in capsys.readouterr().out


@pytest.mark.parametrize("pack", PACKS, ids=lambda path: path.relative_to(ROOT).as_posix())
def test_agent_pack_node_ids(pack: Path, tmp_path: Path) -> None:
    collect_pack(pack, ROOT, tmp_path / "collection")
