# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Force balance and preparation contracts for surface shore pull-in."""

import importlib
import importlib.util
import json
import math
from pathlib import Path
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/shore-pull-in"
SCRIPT = ROOT / "scripts/orcaflex/prepare_shore_pull_in.py"


def method():
    return importlib.import_module("digitalmodel.marine_ops.installation.shore_approach")


def driver():
    spec = importlib.util.spec_from_file_location("shore_preparation", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_level_route_force_balance():
    m = method()
    result = m.pull_in([m.RouteSegment(100, 2, 0.32)], tail_tension_kn=10)
    assert result["pipe_head_tension_kn"] == pytest.approx(74)
    assert result["friction_kn"] == pytest.approx(64)
    assert result["gravity_kn"] == 0


def test_incline_and_buoyancy_are_resolved_along_route():
    m = method()
    segment = m.RouteSegment(40, 2, 0.32, rise_m=4, buoyancy_kn_m=0.5)
    r = m.pull_in([segment], efficiency=0.95, capacity_kn=100)
    cosine = math.sqrt(1 - (4 / 40) ** 2)
    expected = 1.5 * 40 * (0.32 * cosine + 4 / 40)
    assert r["pipe_head_tension_kn"] == pytest.approx(expected)
    assert r["winch_demand_kn"] == pytest.approx(expected / 0.95)
    assert r["capacity_utilisation"] == pytest.approx(expected / 0.95 / 100)


def test_ordered_route_reports_peak_tension_and_capacity_exceedance():
    m = method()
    r = m.pull_in([m.RouteSegment(10, 2, 0), m.RouteSegment(10, 2, 0)],
                 tail_tension_kn=10, efficiency=0.5, capacity_kn=15)
    assert r["winch_demand_kn"] == 20
    assert r["capacity_status"] == "exceeded"
    assert r["segment_end_tensions_kn"] == [10, 10]
    assert r["peak_pipe_tension_kn"] == 10


def test_capacity_is_not_evaluated_when_not_supplied():
    m = method()
    r = m.pull_in([m.RouteSegment(10, 2, 0.32)])
    assert r["capacity_utilisation"] is None
    assert r["capacity_status"] == "not_evaluated"


@pytest.mark.parametrize("kwargs", [
    {"length_m": 0}, {"weight_kn_m": float("nan")}, {"friction": -1},
    {"rise_m": 11}, {"buoyancy_kn_m": 2}, {"buoyancy_kn_m": -1},
])
def test_invalid_segment_is_rejected(kwargs):
    m = method()
    values = dict(length_m=10, weight_kn_m=2, friction=0.32)
    values.update(kwargs)
    with pytest.raises(ValueError):
        m.pull_in([m.RouteSegment(**values)])


@pytest.mark.parametrize("kwargs", [
    {"efficiency": 0}, {"efficiency": 1.1}, {"capacity_kn": 0},
    {"tail_tension_kn": -1}, {"tail_tension_kn": float("inf")},
])
def test_invalid_winch_is_rejected(kwargs):
    m = method()
    with pytest.raises(ValueError):
        m.pull_in([m.RouteSegment(10, 2, 0.32)], **kwargs)


def test_downhill_compression_is_outside_tension_only_method():
    m = method()
    with pytest.raises(ValueError, match="compression"):
        m.pull_in([m.RouteSegment(10, 2, 0, rise_m=-1)])


def test_empty_route_is_rejected():
    with pytest.raises(ValueError):
        method().pull_in([])


def test_case_table_has_input_level_provenance():
    d = driver()
    table = json.loads((CAMPAIGN / "case-table.json").read_text())
    leaves = d.numeric_paths(table["basis"]) | d.numeric_paths(table["cases"], "cases")
    assert leaves <= set(table["input_sources"])
    assert table["public_dataset"]["sha256"] == "7147f215e3ab2c4e3890e10daf51ff2f6504cc0782abb5139e23ad529b801884"
    assert table["public_dataset"]["record_count"] == 161


def test_analytical_check_is_independent_and_precedes_models(tmp_path, monkeypatch):
    d = driver()
    checked = []
    original_check = d.analytical_pull
    original_generate = d.ModularModelGenerator.generate
    identities = [c["id"] for c in json.loads((CAMPAIGN / "case-table.json").read_text())["cases"]]
    def check(basis, case):
        checked.append(case["id"])
        return original_check(basis, case)
    def generate(generator, output):
        assert checked == identities
        return original_generate(generator, output)
    monkeypatch.setattr(d, "analytical_pull", check)
    monkeypatch.setattr(d.ModularModelGenerator, "generate", generate)
    result = d.prepare(CAMPAIGN, tmp_path / "out")
    assert result["analytical_before_models"] is True
    for case in result["cases"]:
        assert case["analytical_pull_kn"] == pytest.approx(case["method"]["pipe_head_tension_kn"], rel=1e-12)
        assert case["analytical_relative_error"] <= 1e-12


def test_every_model_validates_and_every_digest_matches(tmp_path):
    import hashlib
    d = driver()
    out = tmp_path / "out"
    manifest = d.prepare(CAMPAIGN, out)
    from digitalmodel.solvers.orcaflex.modular_generator.post_validator import PostGenerationValidator
    for case in manifest["cases"]:
        findings = PostGenerationValidator().validate_directory(out / "models" / case["id"] / "includes")
        assert not [f for f in findings if f.level == "error"]
    for name, digest in manifest["files"].items():
        assert hashlib.sha256((out / name).read_bytes()).hexdigest() == digest


def test_two_builds_are_byte_identical_and_reproduce_committed_outputs(tmp_path):
    d = driver()
    a, b = tmp_path / "a", tmp_path / "b"
    d.prepare(CAMPAIGN, a)
    d.prepare(CAMPAIGN, b)
    for name in d.DELIVERABLES:
        assert (a / name).read_bytes() == (b / name).read_bytes()
        assert (a / name).read_bytes() == (CAMPAIGN / name).read_bytes()


def test_report_revision_matches_data_and_has_no_external_resources(tmp_path):
    from html.parser import HTMLParser
    d = driver()
    out = tmp_path / "out"
    d.prepare(CAMPAIGN, out)
    page = (out / "analysis.html").read_text()
    table = json.loads((CAMPAIGN / "case-table.json").read_text())
    assert page.index('id="summary"') < page.index('id="revision-history"') < page.index('id="method"')
    revision = table["revisions"][-1]
    assert f'<td>{revision["revision"]}</td><td>{revision["date"]}</td>' in page
    assert f'Revision {table["revision"]}' in page
    class Resources(HTMLParser):
        def handle_starttag(self, tag, attrs):
            if tag in {"script", "link", "img", "iframe", "source"}:
                assert not any(k in {"href", "src", "srcset"} for k, v in attrs)
    Resources().feed(page)


def test_launch_is_refused_before_preparation(tmp_path):
    driver()
    r = subprocess.run([sys.executable, str(SCRIPT), "--campaign", str(CAMPAIGN),
                        "--output", str(tmp_path / "out"), "--launch"], capture_output=True, text=True)
    assert r.returncode == 2
    assert "qualified execution adapter" in r.stderr
    assert not (tmp_path / "out").exists()


def test_preparation_does_not_import_solver(tmp_path):
    driver()
    code = '''
import importlib.abc, runpy, sys
class DenySolver(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.startswith(("OrcFxAPI", "digitalmodel.solvers.orcaflex.orcaflex_api")):
            raise AssertionError("Solver import attempted")
sys.meta_path.insert(0, DenySolver())
sys.argv = [sys.argv[1], "--campaign", sys.argv[2], "--output", sys.argv[3], "--dry-run"]
runpy.run_path(sys.argv[0], run_name="__main__")
'''
    r = subprocess.run([sys.executable, "-c", code, str(SCRIPT), str(CAMPAIGN),
                        str(tmp_path / "out")], capture_output=True, text=True)
    assert r.returncode == 0, r.stderr


def test_validator_error_blocks_deliverables(tmp_path, monkeypatch):
    d = driver()
    from digitalmodel.solvers.orcaflex.modular_generator.post_validator import ValidationWarning
    monkeypatch.setattr(d.PostGenerationValidator, "validate_directory",
                        lambda *a: [ValidationWarning("error", "reference", "Missing object")])
    with pytest.raises(ValueError, match="validation"):
        d.prepare(CAMPAIGN, tmp_path / "out")
    assert not (tmp_path / "out/preparation.json").exists()


def test_existing_output_is_preserved(tmp_path):
    d = driver()
    (tmp_path / "existing.txt").write_text("preserve\n")
    with pytest.raises(ValueError, match="empty"):
        d.prepare(CAMPAIGN, tmp_path)
    assert (tmp_path / "existing.txt").read_text() == "preserve\n"


def test_criteria_digest_and_time_precede_first_generation(tmp_path):
    d = driver()
    manifest = d.prepare(CAMPAIGN, tmp_path / "out")
    import hashlib
    evidence = manifest["criteria_evidence"]
    assert evidence["sha256"] == hashlib.sha256((CAMPAIGN / "criteria.json").read_bytes()).hexdigest()
    assert evidence["recorded_utc"] < evidence["first_generation_utc"]


def test_preparation_models_do_not_assign_standard_design_factors(tmp_path):
    d = driver()
    out = tmp_path / "out"
    d.prepare(CAMPAIGN, out)
    bundle = json.loads((out / "models.json").read_text())
    for files in bundle["cases"].values():
        assert "DNVSTF101" not in files["includes/05_line_types.yml"]


@pytest.mark.parametrize("distinct_friction", [False, True])
def test_each_prepared_pipe_model_carries_its_case_friction(tmp_path, distinct_friction):
    import yaml
    table = json.loads((CAMPAIGN / "case-table.json").read_text())
    if distinct_friction:
        # Two study cases share a coefficient; perturb the copied inputs to
        # expose reuse of any one case's coefficient across all three models.
        for index, case in enumerate(table["cases"]):
            case["friction"] *= index + 1
        assert len({case["friction"] for case in table["cases"]}) == 3
    campaign = tmp_path / "campaign"
    campaign.mkdir()
    (campaign / "case-table.json").write_text(json.dumps(table))
    (campaign / "criteria.json").write_bytes((CAMPAIGN / "criteria.json").read_bytes())
    output = tmp_path / "output"
    driver().prepare(campaign, output)
    bundle = json.loads((output / "models.json").read_text())
    pipe_names = {segment["type"] for segment in table["basis"]["pipeline"]["segments"]}
    carried = []
    for case in table["cases"]:
        name = "includes/05_line_types.yml"
        text = bundle["cases"][case["id"]][name]
        assert text == (output / "models" / case["id"] / name).read_text()
        pipe_types = [t for t in yaml.safe_load(text)["LineTypes"] if t["Name"] in pipe_names]
        assert {t["Name"] for t in pipe_types} == pipe_names
        for line_type in pipe_types:
            assert line_type["SeabedAxialFrictionCoefficient"] == case["friction"]
            assert line_type["SeabedLateralFrictionCoefficient"] == case["friction"]
        carried.append(pipe_types[0]["SeabedAxialFrictionCoefficient"])
    assert carried == [case["friction"] for case in table["cases"]]
    assert len(set(carried)) == (3 if distinct_friction else 2)


def test_preparation_pins_surface_seed_independently_of_generator_defaults(tmp_path, monkeypatch):
    import yaml
    d = driver()
    original = d.ModularModelGenerator.generate

    def changed_defaults(generator, folder):
        result = original(generator, folder)
        path = folder / "includes/07_lines.yml"
        data = yaml.safe_load(path.read_text())
        line = data["Lines"][0]
        line["IncludeTorsion"] = "Yes"
        connection = next(key for key in line if key.startswith("Connection, ConnectionX"))
        line[connection] = [["Fixed", 1, 2, 3, 0, 0, 0, None]] * 2
        path.write_text(yaml.safe_dump(data, sort_keys=False))
        return result

    monkeypatch.setattr(d.ModularModelGenerator, "generate", changed_defaults)
    output = tmp_path / "output"
    d.prepare(CAMPAIGN, output)
    bundle = json.loads((output / "models.json").read_text())
    for files in bundle["cases"].values():
        line = yaml.safe_load(files["includes/07_lines.yml"])["Lines"][0]
        connection = next(key for key in line if key.startswith("Connection, ConnectionX"))
        assert line["IncludeTorsion"] == "No"
        assert line[connection] == [
            ["Free", 0, 0, 0, 180, 90, 0, None],
            ["Anchored", -1979, 0, 0.5612, 180, 90, 0, None],
        ]
        assert line["ConnectionxBendingStiffness, ConnectionyBendingStiffness"] == [[], [0, None]]
        assert not any("ConnectionTwistingStiffness" in key for key in line)


def test_preparation_reproduces_across_shared_generator_schema_variants(tmp_path, monkeypatch):
    import yaml
    d = driver()
    original = d.ModularModelGenerator.generate
    d.prepare(CAMPAIGN, tmp_path / "original")

    def updated_schema(generator, folder):
        result = original(generator, folder)
        includes = folder / "includes"
        for path in includes.glob("*.yml"):
            data = yaml.safe_load(path.read_text())
            if path.name == "04_vessel_types.yml":
                for hull in data["VesselTypes"]:
                    if "Displacement" in hull:
                        mass = hull.pop("Displacement")[0]
                        inertia = hull.pop("MomentsOfInertia")[0]
                        for key in ("CalculationMethod", "WaveReferencePoint", "CentreOfGravityRelativeToCoG"):
                            hull.pop(key)
                        hull["Draughts"] = [{
                            "Name": "Draught1", "Mass": mass,
                            "MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ":
                                [[inertia[0], 0, 0], [0, inertia[1], 0], [0, 0, inertia[2]]],
                            "CentreOfMass": [0, 0, 0], "DisplacementRAOs": {"RAOOrigin": [0, 0, 0]},
                        }]
            elif path.name == "06_vessels.yml":
                for hull in data["Vessels"]:
                    hull.pop("WaveCalculationMethod", None)
            elif path.name == "09_shapes.yml":
                data["Shapes"] = []
            elif path.name == "10_groups.yml":
                for name in ("Ramp inclined", "Ramp-curve"):
                    data["Groups"]["Structure"].pop(name, None)
            elif path.name == "11_winches.yml":
                for winch in data["Winches"]:
                    if "StageTension" in winch:
                        tension = winch["StageTension"][-1]
                        winch.clear()
                        winch.update({
                            "Name": "Tensioner", "WinchType": "Simple", "WinchControlType": "By stage",
                            "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo":
                                [["Generic lay hull", 10, 0, 0], ["Generic pipe", 0, 0, 0, "End A"]],
                            "StageMode, StageValue": [["Specified tension", tension]] * 3,
                        })
            path.write_text(yaml.safe_dump(data, sort_keys=True))
        master = folder / "master.yml"
        header, body = master.read_text().split("---\n", 1)
        master.write_text(header + "---\n" + "\n".join(reversed(body.strip().splitlines())) + "\n")
        return result

    monkeypatch.setattr(d.ModularModelGenerator, "generate", updated_schema)
    d.prepare(CAMPAIGN, tmp_path / "updated")
    for name in d.DELIVERABLES:
        assert (tmp_path / "original" / name).read_bytes() == (tmp_path / "updated" / name).read_bytes()
