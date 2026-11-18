# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Screening pack inventories and synthetic examples are software contracts."""

from __future__ import annotations

import re
import runpy
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
CHECKER = ROOT / "scripts/docs/check_agent_pack_functions.py"
MODULES = ("foam_system", "lifting_lug", "corrosion")


@pytest.mark.parametrize("module", MODULES)
def test_screening_pack_inventory_and_registration(module):
    pack = ROOT / f"src/digitalmodel/{module}/docs/agent-pack.md"
    assert pack.is_file(), f"Missing knowledge pack: {module}"
    assert runpy.run_path(str(CHECKER))["check_pack"](pack, ROOT) == []
    registry = yaml.safe_load(
        (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    )
    entries = [row for row in registry["modules"] if row["module"] == module]
    assert len(entries) == 1
    assert entries[0]["agent_pack"] == pack.relative_to(ROOT).as_posix()


@pytest.mark.parametrize("module", MODULES)
def test_screening_pack_form_and_source_locators(module):
    pack = ROOT / f"src/digitalmodel/{module}/docs/agent-pack.md"
    assert pack.is_file(), f"Missing knowledge pack: {module}"
    text = pack.read_text(encoding="utf-8")
    assert re.findall(r"(?m)^## (\d+)\.", text) == [str(i) for i in range(1, 11)]
    assert text.index("Summary:") < text.index("### Revision history") < text.index("## 1.")
    assert "NOT_EVALUATED" in text
    for relative, start, end in re.findall(r"\[([^\]\n]+):L(\d+)-L(\d+)\]", text):
        source = ROOT / relative
        assert source.is_file(), relative
        assert 1 <= int(start) <= int(end) <= len(source.read_text(encoding="utf-8").splitlines())


@pytest.mark.parametrize("module", MODULES)
def test_real_pack_drift_fails_cli_in_both_directions(module, tmp_path):
    # Work only on temporary copies; source imports and solver calls are unnecessary.
    relative = Path(f"src/digitalmodel/{module}")
    package = tmp_path / relative
    shutil.copytree(ROOT / relative, package, ignore=shutil.ignore_patterns("__pycache__"))
    pack = relative / "docs/agent-pack.md"
    command = [sys.executable, str(CHECKER), "--root", str(tmp_path), "--pack", str(pack)]

    def run():
        return subprocess.run(command, cwd=tmp_path, capture_output=True, text=True, check=False)

    result = run()
    assert result.returncode == 0, result.stdout + result.stderr
    added = package / "inventory_probe.py"
    added.write_text("def inventory_probe():\n    pass\n", encoding="utf-8")
    result = run()
    assert result.returncode == 1
    assert "code-to-pack: inventory_probe missing" in result.stdout
    # Remove the definition by a plain edit to exercise a stale documentation row.
    added.write_text("# definition removed\n# retained locator line\n", encoding="utf-8")
    table = tmp_path / pack
    text = table.read_text(encoding="utf-8")
    # Place the stale row inside the existing section-2 table.
    lines = text.splitlines()
    header = next(i for i, line in enumerate(lines) if re.match(r"\|\s*Public symbol\s*\|", line))
    lines.insert(header + 2, f"| `inventory_probe` | synthetic | none | [{relative.as_posix()}/inventory_probe.py:L1-L2] |")
    table.write_text("\n".join(lines) + "\n", encoding="utf-8")
    result = run()
    assert result.returncode == 1
    assert "pack-to-code: inventory_probe not defined" in result.stdout


def example_namespace(module):
    pack = ROOT / f"src/digitalmodel/{module}/docs/agent-pack.md"
    assert pack.is_file(), f"Missing knowledge pack: {module}"
    text = pack.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 4\..*?(?=^## 5\.)", text)
    assert section is not None
    examples = re.findall(r"```python\n(.*?)```", section[0], re.DOTALL)
    assert len(examples) == 1
    namespace = {}
    exec(compile(examples[0], pack.name, "exec"), namespace)
    return namespace


def test_foam_example_matches_independent_area_arithmetic():
    namespace = example_namespace("foam_system")
    # Synthetic: 10 m2 * 2 L/min/m2 = 20 L/min; * 5 min = 100 L.
    # Concentrate = 100 L * 5/100 = 5 L, with an equal reserve = 10 L.
    assert namespace["area"].area_m2 == 10.0
    assert namespace["criterion"].application_rate_lpm_per_m2 == 2.0
    assert namespace["criterion"].discharge_time_min == 5.0
    assert namespace["demand"].design_solution_flow_lpm == 20.0
    assert namespace["quantity"].base_concentrate_l == 5.0
    assert namespace["quantity"].total_concentrate_l == 10.0
    assert namespace["quantity"].injection_rate_lpm == 1.0


def test_lifting_example_matches_independent_section_arithmetic():
    namespace = example_namespace("lifting_lug")
    # Synthetic 10 kN = 10000 N. Areas: 20*10, (60-20)*10,
    # and 2*(30-20/2)*10 mm2 give 50, 25, 25 MPa.
    result = namespace["result"]
    assert (result.pin_bearing, result.net_tension, result.shear_tearout) == (50.0, 25.0, 25.0)
