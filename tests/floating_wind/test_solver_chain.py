"""Offline chain tests; synthetic values are independent arithmetic fixtures.

Fixture values are chosen for arithmetic, not engineering acceptance. Barge
volume = 20*10*2 = 400 m3, seawater mass = 410 t at the screening code's
1025 kg/m3. Steel = 100 t, RNA = 10 t, tower = 20 t, ballast = 280 t.
With steel CG = 1 m, ballast CG = 0.1 m (existing _assemble convention),
RNA CG = 12 m and tower CG = 8 m above keel, total first moment =
100 + 28 + 120 + 160 = 408 t.m. Hull CG above SWL =
(100 + 28)/380 - 2 = -1.6631578947368422 m.
See OpenStax University Physics vol. 1 (2016), §9.6 (mass first moment),
§10.5 (I = sum m*r²); BIPM SI Brochure 9th ed. (2019), Table 8 (t to kg).
"""

import copy
import hashlib
import json
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

from digitalmodel.floating_wind.solver_chain import (
    ChainInput,
    ResultRecord,
    build_chain_report,
    compare_results,
    prepare_chain,
)
from digitalmodel.reporting import DocumentMeta, render_html

EXAMPLE = Path(__file__).parents[2] / "examples/workflows/floating-wind-chain/input.yml"


@pytest.fixture
def inputs():
    return yaml.safe_load(EXAMPLE.read_text())


def test_shared_input_maps_hull_without_topside_double_count(inputs, tmp_path):
    """410-30=380 t hull; Ixx=380*3²=3420 t.m², with explicit hull radius."""
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    p = bundle.variant.properties
    assert p.displacement_t == pytest.approx(410)
    assert p.ballast_mass_t == pytest.approx(280)
    inertia = bundle.diffraction["vessel"]["inertia"]
    assert inertia["mass"] == pytest.approx(380000)
    assert inertia["centre_of_gravity"][2] == pytest.approx(-1.6631578947368422)
    draught = bundle.coupled_specs["operating"]["generic"]["vessel_types"][0][
        "properties"
    ]["Draughts"][0]
    assert draught["Mass"] == pytest.approx(380)
    assert draught[
        "MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ"
    ][0] == [3420, 0, 0]
    assert draught["CentreOfMass"] == inertia["centre_of_gravity"]
    assert bundle.manifest["solver_ready"] is False
    assert bundle.manifest["input_digest"] == bundle.input_digest
    assert (tmp_path / "manifest.json").exists()
    assert not any(
        f["level"] == "error" for f in bundle.manifest["validation_findings"]
    )


def test_each_load_case_drives_screen_and_time_domain(inputs, tmp_path):
    """Shared sea states must survive generation rather than template defaults."""
    inputs["load_cases"].append(dict(name="parked", hs_m=3, tp_s=9, steady_load_kN=0))
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert set(bundle.coupled_specs) == {"operating", "parked"}
    parked = bundle.coupled_specs["parked"]["environment"]["waves"]["trains"][0]
    assert (parked["height"], parked["period"]) == (3, 9)
    turbine = bundle.coupled_specs["operating"]["generic"]["turbines"][0]
    assert turbine == inputs["template"]["generic"]["turbines"][0]
    assert bundle.variant.params == inputs["params"]


@pytest.mark.parametrize(
    "field,value",
    [
        ("hull_radii_m", [0, 2, 3]),
        ("hull_radii_m", [float("nan"), 2, 3]),
        ("water_depth_m", 1),
        ("mesh_file", "../escape.gdf"),
        ("case_id", "../escape"),
        ("frequencies_rad_s", []),
        ("frequencies_rad_s", [0]),
        ("headings_deg", [float("inf")]),
    ],
)
def test_rejects_invalid_required_inputs(inputs, field, value):
    inputs[field] = value
    with pytest.raises(ValueError):
        ChainInput.model_validate(inputs)


def test_missing_explicit_screen_coefficient_rejected(inputs):
    del inputs["params"]["ca_heave"]
    with pytest.raises(ValueError, match="explicit"):
        ChainInput.model_validate(inputs)


def test_missing_turbine_rejected_before_writing(inputs, tmp_path):
    inputs["template"]["generic"]["turbines"] = []
    with pytest.raises(ValueError, match="turbine"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())


def test_disconnected_turbine_rejected(inputs, tmp_path):
    inputs["template"]["generic"]["turbines"][0]["properties"]["Connection"] = "Free"
    with pytest.raises(ValueError, match="connected"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)


def test_failed_screen_preserved_and_prevents_readiness(inputs, tmp_path):
    inputs["criteria"]["gm_min_m"] = 100
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not bundle.variant.passed
    assert bundle.manifest["screening_passed"] is False
    assert bundle.manifest["solver_ready"] is False


def test_infeasible_hull_rejected(inputs, tmp_path):
    inputs["params"]["steel_mass_t"] = 500
    with pytest.raises(ValueError, match="infeasible"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)


def test_rejects_stale_hydrodynamics(inputs, tmp_path):
    inputs["template"]["generic"]["vessel_types"][0]["properties"]["Draughts"] = [
        {"Name": "old", "DisplacementRAOs": {"old": "results"}}
    ]
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    draught = bundle.coupled_specs["operating"]["generic"]["vessel_types"][0][
        "properties"
    ]["Draughts"][0]
    assert "DisplacementRAOs" not in draught
    assert "diffraction_import" in bundle.manifest["not_evaluated"]


def test_deterministic_digests_and_input_not_mutated(inputs, tmp_path):
    original = copy.deepcopy(inputs)
    a = prepare_chain(ChainInput.model_validate(inputs), tmp_path / "a")
    b = prepare_chain(ChainInput.model_validate(inputs), tmp_path / "b")
    assert a.input_digest == b.input_digest
    assert a.manifest == b.manifest
    assert inputs == original
    inputs["topside"]["rna_mass_t"] = 11
    c = prepare_chain(ChainInput.model_validate(inputs), tmp_path / "c")
    assert c.input_digest != a.input_digest


def test_result_comparison_uses_units_version_and_input_digest(inputs, tmp_path):
    """Independent comparison: (12-10)/10*100 = +20.0%, delta=2.0 s."""
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    bundle.variant.properties.heave_natural_period_s = 10
    record = ResultRecord(
        input_digest=bundle.input_digest,
        case_name="operating",
        solver="OrcaFlex",
        version="test-version",
        evidence_kind="synthetic",
        source="independent hand calculation",
        metric="heave_natural_period_s",
        value=12,
        unit="s",
    )
    row = compare_results(bundle, [record])[0]
    assert row["difference"] == pytest.approx(2)
    assert row["difference_percent"] == pytest.approx(20)
    assert row["status"] == "SYNTHETIC"
    assert row["version"] == "test-version"
    with pytest.raises(ValueError, match="digest"):
        compare_results(bundle, [record.model_copy(update={"input_digest": "0" * 64})])
    with pytest.raises(ValueError, match="unit"):
        ResultRecord(**{**record.model_dump(), "unit": "m"})
    with pytest.raises(ValueError, match="solver"):
        ResultRecord(**{**record.model_dump(), "solver": "OrcaWave"})


def test_mixed_solver_versions_and_duplicate_metrics_rejected(inputs, tmp_path):
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    data = dict(
        input_digest=bundle.input_digest,
        case_name="operating",
        solver="OrcaFlex",
        version="one",
        evidence_kind="synthetic",
        source="hand calculation",
        metric="pitch_natural_period_s",
        value=12,
        unit="s",
    )
    record = ResultRecord(**data)
    with pytest.raises(ValueError, match="duplicate"):
        compare_results(bundle, [record, record])
    data.update(version="two", metric="heave_natural_period_s")
    with pytest.raises(ValueError, match="version"):
        compare_results(bundle, [record, ResultRecord(**data)])


def test_report_standard_form_and_no_fabricated_results(inputs, tmp_path):
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    doc = DocumentMeta(
        number="R0000-RPT-001-00",
        revision="00",
        title="Floating wind sizing analysis",
        project="Generic",
        client="Generic",
    )
    spec = build_chain_report(bundle, document=doc)
    titles = [s.title for s in spec.sections]
    assert titles[:2] == ["Summary", "Revision history"]
    report = render_html(spec)
    assert "NOT_EVALUATED" in report
    assert "diffraction_import" in report
    assert "No solver results supplied" in report
    assert bundle.input_digest in report
    assert "OpenStax" in report
    assert report == render_html(build_chain_report(bundle, document=doc))
    # A complete standard report spec can be serialised without non-finite floats.
    json.loads(spec.model_dump_json())


def test_offline_import_and_generation_never_probe_solver(tmp_path):
    """An import finder fails even attempted solver imports, including try/except."""
    script = """
import importlib.abc
import sys
class NoSolver(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, *args):
        if fullname.startswith(("OrcFxAPI", "OrcaWave", "digitalmodel.hydrodynamics.diffraction")):
            raise AssertionError("forbidden solver import")
sys.meta_path.insert(0, NoSolver())
import yaml
from digitalmodel.floating_wind.solver_chain import ChainInput, prepare_chain
from pathlib import Path
inputs = yaml.safe_load(Path(sys.argv[1]).read_text())
prepare_chain(ChainInput.model_validate(inputs), Path(sys.argv[2]))
"""
    result = subprocess.run(
        [sys.executable, "-c", script, str(EXAMPLE), str(tmp_path)],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr


def test_existing_output_is_preserved(inputs, tmp_path):
    """An existing output directory must not be mixed with another case."""
    marker = tmp_path / "existing.txt"
    marker.write_text("existing data")
    with pytest.raises(ValueError, match="empty"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert marker.read_text() == "existing data"
    assert list(tmp_path.iterdir()) == [marker]


def test_repository_validator_errors_are_not_bypassed(inputs, tmp_path):
    inputs["template"]["generic"]["constraints"] = [
        dict(name="Other", in_frame_connection="UndefinedConnection")
    ]
    with pytest.raises(ValueError, match="validation failed"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not (tmp_path / "manifest.json").exists()


def test_typed_connection_takes_precedence(inputs, tmp_path):
    """Match the generic builder: typed Free cannot be overridden by properties."""
    inputs["template"]["generic"]["buoys_6d"] = [
        dict(name="Tower", connection="Free", properties={"Connection": "Floater"})
    ]
    inputs["template"]["generic"]["turbines"][0]["properties"]["Connection"] = "Tower"
    with pytest.raises(ValueError, match="connected"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)


def test_example_runner_and_standard_report(tmp_path):
    """End-to-end example produces the hand-calculated 380.0 t hull mass."""
    result = subprocess.run(
        [sys.executable, str(EXAMPLE.with_name("run.py")), "--out", str(tmp_path)],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    report = (tmp_path / "analysis.html").read_text()
    assert "380.0" in report and "NOT_EVALUATED" in report
    assert report == EXAMPLE.with_name("analysis.html").read_text()
    assert (tmp_path / "shared-input.json").exists()
    manifest = json.loads((tmp_path / "manifest.json").read_text())
    assert "shared-input.json" in manifest["files"]


def test_recorded_evidence_report_does_not_claim_acceptance(inputs, tmp_path):
    """A serialised synthetic period is not a physical validation result."""
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    record_data = dict(
        input_digest=bundle.input_digest,
        case_name="operating",
        solver="OrcaFlex",
        version="fixture",
        evidence_kind="synthetic",
        source="hand calculation: 10 s + 2 s = 12 s",
        metric="heave_natural_period_s",
        value=12,
        unit="s",
    )
    path = tmp_path / "fixture.json"
    path.write_text(json.dumps([record_data]))
    records = [ResultRecord.model_validate(r) for r in json.loads(path.read_text())]
    document = DocumentMeta(
        number="R0000-RPT-001-00",
        revision="00",
        title="Floating wind sizing analysis",
        project="Generic",
        client="Generic",
    )
    report = render_html(build_chain_report(bundle, document=document, records=records))
    assert "SYNTHETIC" in report
    assert "OrcaFlex/fixture" in report
    assert "no acceptance limit" in report


@pytest.mark.parametrize(
    "criteria",
    [
        {"modal_band_s": [16, 5]},
        {"mooring_stiffness_kN_per_m": 1, "max_offset_m": None},
        {"mooring_stiffness_kN_per_m": None, "max_offset_m": 1},
        {"mooring_stiffness_kN_per_m": 1, "max_offset_m": -1},
    ],
)
def test_invalid_screening_criteria_rejected(inputs, criteria):
    inputs["criteria"].update(criteria)
    with pytest.raises(ValueError):
        ChainInput.model_validate(inputs)


def test_nonzero_floater_datum_rejected(inputs, tmp_path):
    """A translated SWL frame must not receive untranslated hull mass properties."""
    inputs["template"]["generic"]["vessels"][0]["initial_position"] = [0, 0, -1]
    with pytest.raises(ValueError, match="datum"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)


@pytest.mark.parametrize("units_system", ["User", "US"])
def test_non_si_template_rejected_before_writing(inputs, tmp_path, units_system):
    inputs["template"]["generic"]["general_properties"] = {
        "UnitsSystem": units_system,
        "MassUnits": "kg",
        "LengthUnits": "ft",
    }
    with pytest.raises(ValueError, match="SI units"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())


@pytest.mark.parametrize("wave_type", ["jonswap", "pierson_moskowitz"])
@pytest.mark.parametrize("raw_periods", [{}, {"WaveTz": 5}, {"WaveTp": 6, "WaveTz": 5}])
def test_spectral_peak_period_survives_generation(
    inputs, tmp_path, wave_type, raw_periods
):
    env = inputs["template"]["environment"]
    env["waves"]["type"] = wave_type
    env["raw_properties"] = {"WaveTrains": [raw_periods]}
    inputs["load_cases"].append(dict(name="parked", hs_m=3, tp_s=9, steady_load_kN=0))
    prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    for case in inputs["load_cases"]:
        output = tmp_path / case["name"] / "includes" / "03_environment.yml"
        train = yaml.safe_load(output.read_text())["Environment"]["WaveTrains"][0]
        assert train["WaveTp"] == case["tp_s"]
        assert train["WaveHs"] == case["hs_m"]
        assert "WaveTz" not in train
        assert "WavePeriod" not in train


@pytest.mark.parametrize("wave_type", ["user_defined", "Torsethaugen", "unknown"])
def test_unsupported_wave_period_convention_rejected(inputs, tmp_path, wave_type):
    inputs["template"]["environment"]["waves"]["type"] = wave_type
    with pytest.raises(ValueError, match="wave type"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())


@pytest.mark.parametrize("name", ["diffraction", "Diffraction", "DIFFRACTION"])
def test_reserved_case_name_rejected_before_writing(inputs, tmp_path, name):
    inputs["load_cases"][0]["name"] = name
    with pytest.raises(ValueError, match="reserved"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())


def test_written_specifications_match_bundle_and_manifest(inputs, tmp_path):
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert (
        yaml.safe_load((tmp_path / "diffraction.yml").read_text()) == bundle.diffraction
    )
    for name, spec in bundle.coupled_specs.items():
        assert yaml.safe_load((tmp_path / f"{name}.yml").read_text()) == spec
    for name, digest in bundle.manifest["files"].items():
        assert hashlib.sha256((tmp_path / name).read_bytes()).hexdigest() == digest


@pytest.mark.parametrize(
    "key,value", [("MassUnits", "kg"), ("LengthUnits", "ft"), ("ForceUnits", "lbf")]
)
def test_si_template_with_unit_override_rejected(inputs, tmp_path, key, value):
    inputs["template"]["generic"]["general_properties"] = {
        "UnitsSystem": "SI",
        key: value,
    }
    with pytest.raises(ValueError, match="SI units"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())
