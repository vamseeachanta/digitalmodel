# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solver-free S-lay campaign preparation and independent comparator tests."""

import importlib.util
import math
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts/orcaflex/prepare_slay_campaign.py"
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/slay-campaign"


def load_driver():
    spec = importlib.util.spec_from_file_location("slay_preparation", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_catenary_satisfies_force_geometry_and_curvature():
    driver = load_driver()
    result = driver.catenary(2.0, 40.0, 240.0)
    assert result["horizontal_tension_kn"] == pytest.approx(160.0)
    assert result["suspended_length_m"] == pytest.approx(math.sqrt(40**2 + 2*80*40))
    assert result["top_tension_kn"] == pytest.approx(240.0)
    assert result["touchdown_curvature_per_m"] == pytest.approx(1/80)
    assert 80 * (math.cosh(result["span_m"]/80) - 1) == pytest.approx(40)


@pytest.mark.parametrize("weight,depth,tension", [(0,40,240), (2,0,240), (2,40,80), (2,40,float("nan"))])
def test_catenary_rejects_nonphysical_inputs(weight, depth, tension):
    with pytest.raises(ValueError):
        load_driver().catenary(weight, depth, tension)


def test_preparation_generates_valid_models_without_solver_import(tmp_path):
    code = '''
import importlib.abc, runpy, sys
class DenySolver(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.startswith(("OrcFxAPI", "digitalmodel.solvers.orcaflex.orcaflex_api")):
            raise AssertionError("Solver access during preparation")
sys.meta_path.insert(0, DenySolver())
sys.argv = [sys.argv[1], "--campaign", sys.argv[2], "--output", sys.argv[3], "--dry-run"]
runpy.run_path(sys.argv[0], run_name="__main__")
'''
    result = subprocess.run([sys.executable, "-c", code, str(SCRIPT), str(CAMPAIGN), str(tmp_path / "prepared")],
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    import json
    manifest = json.loads((tmp_path / "prepared/preparation.json").read_text())
    assert len(manifest["cases"]) == 6
    assert manifest["native_completed"] == 0
    assert all(case["validator_errors"] == 0 for case in manifest["cases"])
    assert all(case["native_wall_seconds"] is None for case in manifest["cases"])
    assert all(len(case["files"]) > 3 for case in manifest["cases"])


def test_launch_requires_a_later_execution_adapter(tmp_path):
    result = subprocess.run([sys.executable, str(SCRIPT), "--campaign", str(CAMPAIGN),
                             "--output", str(tmp_path), "--launch"], capture_output=True, text=True)
    assert result.returncode != 0
    assert "execution adapter" in result.stderr


def test_generated_models_match_committed_models(tmp_path):
    driver = load_driver()
    manifest = driver.prepare(CAMPAIGN, tmp_path)
    for case in manifest["cases"]:
        for relative in case["files"]:
            assert (tmp_path / relative).read_bytes() == (CAMPAIGN / relative).read_bytes()


def test_validator_errors_block_preparation(monkeypatch, tmp_path):
    driver = load_driver()
    from digitalmodel.solvers.orcaflex.modular_generator.post_validator import ValidationWarning
    monkeypatch.setattr(driver.PostGenerationValidator, "validate_directory",
                        lambda *args: [ValidationWarning("error", "reference", "Missing object")])
    with pytest.raises(ValueError, match="validation"):
        driver.prepare(CAMPAIGN, tmp_path)


def test_weight_is_independent_of_dimension_key_order():
    import yaml
    basis = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text())["basis"]
    driver = load_driver()
    expected = driver.submerged_weight(basis)
    dims = basis["pipeline"]["dimensions"]
    basis["pipeline"]["dimensions"] = dict(reversed(list(dims.items())))
    assert driver.submerged_weight(basis) == pytest.approx(expected)


@pytest.mark.parametrize("bad_id", ["../outside", "duplicate"])
def test_invalid_case_identity_leaves_output_untouched(tmp_path, bad_id):
    import yaml
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text())
    table["cases"][0]["id"] = bad_id
    if bad_id == "duplicate":
        table["cases"][1]["id"] = bad_id
    source = tmp_path / "source"
    source.mkdir()
    (source / "case-table.yml").write_text(yaml.safe_dump(table))
    output = tmp_path / "output"
    with pytest.raises(ValueError, match="identity"):
        load_driver().prepare(source, output)
    assert not output.exists()
