"""Durable workflow and report contract; synthetic arithmetic basis."""
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.hull_wastage_screen.workflow import router

ROOT = Path(__file__).resolve().parents[3]


@pytest.mark.parametrize("filename", ["test_screen.py", "test_workflow.py"])
def test_focused_file_is_owned_and_in_ci_command(filename):
    """Both focused files must belong to a domain and its executed command."""
    import shlex
    from scripts.ci.detect_touched_domains import (
        parse_domains, path_matches_root, touched_domains,
    )
    path = Path(__file__).with_name(filename).relative_to(ROOT).as_posix()
    domains = parse_domains(ROOT / "tests/DOMAINS.md")
    assert [d.name for d in touched_domains([path], domains)] == ["asset-integrity"]
    gate = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())[
        "gates"]["tests-asset-integrity"]
    assert gate["enabled"] is True
    assert any(path_matches_root(path, root) for root in gate["test_roots"])
    assert any(path_matches_root(path, token) for token in shlex.split(gate["command"])
               if token.startswith("tests/"))


def test_example_workflow(tmp_path):
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    cfg["hull_wastage_screen"]["output_dir"] = str(tmp_path)
    result = router(cfg)["hull_wastage_screen"]["result"]
    assert result["elements"][0]["renewal_mm"] == 8.
    assert result["elements"][0]["status"] == "MONITOR"
    assert result["status"] == "NOT_EVALUATED"
    saved = json.loads((tmp_path / "hull-wastage.json").read_text())
    assert saved == result
    report = (tmp_path / "hull-wastage.html").read_text()
    assert "Hull-structure wastage analysis" in report
    assert "Revision history" in report
    assert "NOT_EVALUATED" in report
    assert "8.000" in report


def test_report_escapes_inputs(tmp_path):
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    cfg["hull_wastage_screen"]["elements"][0]["id"] = "<script>bad()</script>"
    cfg["hull_wastage_screen"]["output_dir"] = str(tmp_path)
    router(cfg)
    report = (tmp_path / "hull-wastage.html").read_text()
    assert "<script>bad()" not in report
    assert "&lt;script&gt;" in report


@pytest.mark.parametrize("nominal,measured,compression,shear", [
    (50., 50., 0., 200.), (12., 10., 10., 20.),
    (24., 20., 100., 0.), (20., 10., 10., 0.),
])
def test_unsupported_plate_capacity_is_retained_as_unevaluated(
    tmp_path, nominal, measured, compression, shear,
):
    """Numeric output and HTML must not present an unsupported plate capacity."""
    from .test_screen import buckling_case
    data = buckling_case()
    data["elements"][0].update(as_built_mm=nominal, measured_mm=measured)
    data["buckling"].update(sigma_x_mpa=compression, tau_mpa=shear)
    data["output_dir"] = str(tmp_path)
    result = router({"hull_wastage_screen": data})["hull_wastage_screen"]["result"]
    saved = json.loads((tmp_path / "hull-wastage.json").read_text())
    assert saved == result
    plate = saved["checks"]["buckling"]
    assert plate["status"] == "NOT_EVALUATED"
    assert "utilization" not in plate
    assert "capacity_retained_fraction" not in plate
    report = (tmp_path / "hull-wastage.html").read_text()
    assert plate["reason"] in report
    assert "utilization:" not in report
    assert "capacity_retained_fraction:" not in report


def test_registry_and_engine_route():
    registry = yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text())
    rows = [r for r in registry["workflows"] if r["id"] == "hull-wastage-screen"]
    assert len(rows) == 1
    assert rows[0]["basename"] == "hull_wastage_screen"
    import ast
    test_path, test_name = rows[0]["test"].split("::")
    registered_test = ROOT / test_path
    assert registered_test.is_file(), test_path
    assert test_name in {
        node.name for node in ast.parse(registered_test.read_text()).body
        if isinstance(node, ast.FunctionDef)
    }, rows[0]["test"]
    # Read routing source without importing unrelated licensed-solver adapters.
    tree = ast.parse((ROOT / "src/digitalmodel/engine.py").read_text())
    arms = [n for n in ast.walk(tree) if isinstance(n, ast.If)
            and 'basename == \'hull_wastage_screen\'' == ast.unparse(n.test)]
    assert len(arms) == 1
    assert any(isinstance(n, ast.ImportFrom)
               and n.module == "digitalmodel.hull_wastage_screen.workflow"
               for n in ast.walk(arms[0].body[0]))


def test_routed_arm_executes_without_solver_imports(tmp_path):
    """Execute the shipped dispatch arm; unrelated solver imports stay unexecuted."""
    import ast
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    cfg["hull_wastage_screen"]["output_dir"] = str(tmp_path)
    tree = ast.parse((ROOT / "src/digitalmodel/engine.py").read_text())
    arm = next(n for n in ast.walk(tree) if isinstance(n, ast.If)
               and ast.unparse(n.test) == "basename == 'hull_wastage_screen'")
    module = ast.Module(body=arm.body, type_ignores=[])
    namespace = {"cfg_base": cfg}
    exec(compile(module, "engine_dispatch_arm", "exec"), namespace)
    assert namespace["cfg_base"]["hull_wastage_screen"]["result"]["status"] == "NOT_EVALUATED"


def test_previous_issue_is_preserved(tmp_path):
    import pytest
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    cfg["hull_wastage_screen"]["output_dir"] = str(tmp_path)
    router(cfg)
    before = {p.name: p.read_bytes() for p in tmp_path.iterdir()}
    with pytest.raises(FileExistsError):
        router(cfg)
    assert {p.name: p.read_bytes() for p in tmp_path.iterdir()} == before


@pytest.mark.parametrize("metadata", ["directory", "file", "both", "none"])
def test_relative_output_uses_config_location(tmp_path, monkeypatch, metadata):
    config_dir = tmp_path / "config"
    config_dir.mkdir()
    other_cwd = tmp_path / "other"
    other_cwd.mkdir()
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    if metadata in {"directory", "both"}:
        cfg["_config_dir_path"] = str(config_dir)
    if metadata in {"file", "both"}:
        parent = other_cwd if metadata == "both" else config_dir
        cfg["_config_file_path"] = str(parent / "input.yml")
    monkeypatch.chdir(other_cwd)
    router(cfg)
    expected = other_cwd if metadata == "none" else config_dir
    assert (expected / "results/hull-wastage.json").exists()
    assert (expected / "results/hull-wastage.html").exists()
    if metadata != "none":
        assert not (other_cwd / "results").exists()


def test_near_boundary_report_retains_governing_precision(tmp_path):
    from .test_screen import section_case, buckling_case
    data = section_case()
    data["global"]["modulus_fraction"] = .9
    data["limits"]["reserve_mm"] = .6
    for item in data["elements"]:
        item["measured_mm"] = 10.75
    data["buckling"] = buckling_case()["buckling"]
    data["buckling"]["element_id"] = "deck"
    data["buckling"]["source"] = "<b>synthetic</b>"
    data["output_dir"] = str(tmp_path)
    result = router({"hull_wastage_screen": data})["hull_wastage_screen"]["result"]
    global_result = result["checks"]["global"]
    assert global_result["modulus_ratios"]["sm_deck_m3"] < .9
    assert global_result["status"] == "RENEW"
    report = (tmp_path / "hull-wastage.html").read_text()
    for text in ("sm_deck_m3: 0.896", "modulus_fraction: 0.900",
                 "remaining_thickness_mm: 10.750", "na_area_m2: 0.01075",
                 "sm_deck_m3: 0.02150", "&lt;b&gt;synthetic&lt;/b&gt;"):
        assert text in report
    assert "code_qualified: False" in report
    assert "age_applicability" in report
    assert "CSR input deviations" in report
    assert "reserve_mm differs from CSR" in report


@pytest.fixture
def configured_engine(monkeypatch):
    """Load the complete engine, stubbing only unrelated workflow adapters."""
    import ast
    import importlib.util
    import sys
    from types import ModuleType

    def unused_adapter(*args, **kwargs):
        pytest.fail("An unrelated workflow adapter was called")

    engine_path = ROOT / "src/digitalmodel/engine.py"
    # Inspect imports only; execute the entire shipped module and entry point.
    for node in ast.parse(engine_path.read_text()).body:
        if not isinstance(node, ast.ImportFrom) or not node.module:
            continue
        if not node.module.startswith("digitalmodel."):
            continue
        if node.module == "digitalmodel.solvers.orcaflex.output_control":
            continue
        stub = ModuleType(node.module)
        for alias in node.names:
            setattr(stub, alias.name, unused_adapter)
        monkeypatch.setitem(sys.modules, node.module, stub)
    spec = importlib.util.spec_from_file_location("_hull_wastage_engine", engine_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.engine


@pytest.mark.parametrize("absolute_output", [False, True])
def test_complete_engine_loads_base_config(
    tmp_path, monkeypatch, configured_engine, absolute_output,
):
    """Real file loading, configuration merging and dispatch produce both outputs."""
    input_dir = tmp_path / "config"
    input_dir.mkdir()
    caller_dir = tmp_path / "caller"
    caller_dir.mkdir()
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    output = tmp_path / "absolute-results" if absolute_output else input_dir / "results"
    if absolute_output:
        cfg["hull_wastage_screen"]["output_dir"] = str(output)
    input_path = input_dir / "input.yml"
    input_path.write_text(yaml.safe_dump(cfg), encoding="utf-8")
    monkeypatch.chdir(caller_dir)
    result = configured_engine(inputfile=str(input_path))
    screen = result["hull_wastage_screen"]["result"]
    assert result["default"]["log_level"] == "INFO"
    assert result["default"]["config"]["overwrite"]["output"] is True
    assert screen["status"] == "NOT_EVALUATED"
    assert screen["elements"][0]["renewal_mm"] == 8.
    assert screen["elements"][0]["status"] == "MONITOR"
    assert json.loads((output / "hull-wastage.json").read_text()) == screen
    assert "Hull-structure wastage analysis" in (output / "hull-wastage.html").read_text()
    assert not (caller_dir / "results").exists()
