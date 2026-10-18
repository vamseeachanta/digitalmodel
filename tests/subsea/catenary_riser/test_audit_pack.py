"""Knowledge-pack audit: named package checks and explicit missing prerequisites."""
import importlib
from pathlib import Path

import pytest
from click.testing import CliRunner

from digitalmodel.subsea.catenary_riser.cli import cli

ROOT = Path(__file__).resolve().parents[3]
PACKAGE = ROOT / "src/digitalmodel/subsea/catenary_riser"


def test_pack_uses_direct_package_validation_nodes():
    pack = (PACKAGE / "docs/agent-pack.md").read_text()
    worked = pack.split("## 4.")[1].split("## 5.")[0]
    assert "Checker self-test" in worked
    assert "test_scr_design_and_general_solver_in_inextensible_limit" in worked
    assert "test_slwr_equilibrium_and_recorded_legacy_force_divergence" in worked
    assert "CatenarySolver.solve" in worked
    assert "LazyWaveSolver.solve" in worked
    assert "checker self-tests" in worked
    s1 = next(row for row in pack.splitlines() if row.startswith("| G2 / S1"))
    assert "test_independent_geometry_closes" not in s1
    assert "do not detect divergent package routes" in s1


@pytest.mark.parametrize("gate,function,criterion", [
    ("G0 / W1", "EffectiveWeightCalculator.calculate", "2257.021972"),
    ("G0 / W2", "LazyWaveAnalyzer.analyze", "1 expected NotImplementedError"),
    ("G1 / L1", "WriteOrcaflexModel", "exports cannot be produced"),
    ("G2 / S1", "CatenarySolver.solve", "converged true"),
    ("G2 / S2", "WriteOrcaflexModel", "0 unconverged"),
    ("G3 / D1", "WriteOrcaflexModel", "0 instability"),
    ("G4 / Q1", "WriteOrcaflexModel", "Provenance-record count equals embedded RAO-dataset count"),
    ("G5 / R1", "render_html", "0 invalid values classified PASS"),
])
def test_every_gate_names_package_contract(gate, function, criterion):
    pack = (PACKAGE / "docs/agent-pack.md").read_text()
    row = next(row for row in pack.splitlines() if row.startswith(f"| {gate} |"))
    assert function in row
    assert criterion in row


def test_pack_has_real_trackers_and_corrected_conventions():
    pack = (PACKAGE / "docs/agent-pack.md").read_text()
    assert "issue pending" not in pack
    for tracker in (2753, 2754, 2755, 2756):
        assert f"#{tracker}" in pack
    assert "degrees from vertical" in pack.split("## 2.")[1].split("## 3.")[0]
    assert "annulus" in pack.split("## 6.")[1].split("## 7.")[0]
    assert "S in m²" in pack
    readme = (PACKAGE / "README.md").read_text()
    assert "Production Ready" not in readme
    assert "acosh(1 + z / a)" in readme
    assert "atan2(H, w×s)" in readme


@pytest.mark.parametrize("analysis", ["Extreme", "Fatigue"])
def test_export_build_fails_for_missing_type_data(monkeypatch, tmp_path, analysis):
    # Test the package's actual resource-loading route, without native loading.
    module = importlib.import_module("digitalmodel.subsea.catenary_riser.legacy.orcaflexModel")
    monkeypatch.setattr(module, "orcaflexModel", lambda *args: ({}, {}))
    cfg = {
        "default": {"Analysis": {"SLWR": False, "SCR": True, analysis: True}},
        "EnvironmentLoad": {analysis: [{"Wave": {"WaveTrains": [{"Name": "generic"}]}}]},
        "Analysis": {"fe_folder": str(tmp_path) + "/", "file_name": "generic"},
    }
    with pytest.raises(FileNotFoundError, match=f"VesselTypes_{analysis}"):
        module.build_model(analysis, cfg)
    assert not list(tmp_path.iterdir())


def test_cli_vertical_angle_and_convergence_output(tmp_path):
    output = tmp_path / "result.json"
    result = CliRunner().invoke(cli, [
        "simple", "--diameter", ".508", "--thickness", ".025",
        "--length", "1500", "--water-depth", "1000", "--offset", "500",
        "--internal-fluid", "oil", "--top-tension", "2757021.971932033",
        "--output", str(output),
    ])
    assert result.exit_code == 0, result.output
    assert "10.45° from vertical" in result.output
    import json
    data = json.loads(output.read_text())["results"]
    assert data["converged"] is True
    assert data["top_tension_N"] == pytest.approx(2757021.97, abs=.001)
    # A requested force that cannot fit must fail before writing an output.
    output.unlink()
    result = CliRunner().invoke(cli, [
        "simple", "--diameter", ".508", "--thickness", ".025",
        "--length", "1500", "--water-depth", "1000", "--offset", "500",
        "--internal-fluid", "oil", "--top-tension", "5000000", "--output", str(output),
    ])
    assert result.exit_code != 0
    assert "exceeds available length" in result.output
    assert not output.exists()
