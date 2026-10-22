"""Offline chain tests; synthetic values are independent arithmetic fixtures.

Fixture values are chosen for arithmetic, not engineering acceptance. Barge
volume = 20*10*2 = 400 m3, seawater mass = 410 t at the explicit assumed
1025 kg/m3 input. Steel = 100 t, RNA = 10 t, tower = 20 t, ballast = 280 t.
With steel CG = 1 m and the explicit assumed ballast CG = 0.1 m,
RNA CG = 12 m and tower CG = 8 m above keel, total first moment =
100 + 28 + 120 + 160 = 408 t.m. Hull CG above SWL =
(100 + 28)/380 - 2 = -1.6631578947368422 m.
See OpenStax University Physics vol. 1 (2016), §9.6 (mass first moment),
§10.5 (I = sum m*r²); BIPM SI Brochure 9th ed. (2019), Table 8 (t to kg).
"""

import ast
import copy
import hashlib
import json
import stat
import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

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
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


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
    result_section = next(
        section for section in spec.sections if section.key == "results"
    )
    criteria = result_section.blocks[-1]
    assert "Unit" in criteria.columns
    units = {row[0]: row[criteria.columns.index("Unit")] for row in criteria.rows}
    assert units == {"stability": "m", "modal": "s", "motion": "1"}
    assert {row[-1] for row in criteria.rows} == {inputs["basis"]}
    assert report == render_html(build_chain_report(bundle, document=doc))
    # A complete standard report spec can be serialised without non-finite floats.
    json.loads(spec.model_dump_json())


@pytest.mark.parametrize("modal_band", [[5, 16], [3, 16], [1, 3]])
@pytest.mark.parametrize("gm_min", [0, 100])
def test_report_limits_match_margins_and_explicit_basis(
    inputs, tmp_path, modal_band, gm_min
):
    """Band margins use the nearer edge; scalar margins use the stated limit."""
    inputs["criteria"].update(
        modal_band_s=modal_band,
        gm_min_m=gm_min,
        mooring_stiffness_kN_per_m=10,
        max_offset_m=2,
    )
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    spec = build_chain_report(
        bundle,
        document=DocumentMeta(
            number="R0000-RPT-001-00",
            revision="00",
            title="Floating wind sizing analysis",
            project="Generic",
            client="Generic",
        ),
    )
    results = next(section for section in spec.sections if section.key == "results")
    rows = {row[0]: row for row in results.blocks[-1].rows}
    assert set(rows) == {"stability", "modal", "motion", "mooring"}
    for name, row in rows.items():
        _, value, limit, margin, _, basis = row
        value, margin = float(value), float(margin)
        if name == "modal":
            low, high = modal_band
            assert limit == f"{low}\u2013{high} (excluded)"
            distance = min(abs(value - low), abs(value - high))
            expected = -distance if low <= value <= high else distance
        else:
            expected = (
                value - float(limit) if name == "stability" else float(limit) - value
            )
        # At this fixture's largest margin, rounding contributes 0.05 + 0.005.
        assert margin == pytest.approx(expected, abs=0.055)
        assert basis == inputs["basis"]
    assert bundle.manifest["screening_criteria"] == inputs["criteria"]


def test_ballast_assumption_is_shared_with_screening(inputs, tmp_path, monkeypatch):
    """At ballast CG 0.2 m: total moment=100+56+120+160=436 t.m."""
    from digitalmodel.floating_wind import floaters

    monkeypatch.setattr(floaters, "BALLAST_VCG_DRAFT_FRACTION", 0.1, raising=False)
    inputs["ballast_vcg_m"] = 0.2
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert bundle.variant.properties.KG_m == pytest.approx(436 / 410)
    # Hull first moment=100+56=156 t.m; mass=380 t; SWL is 2 m above keel.
    assert bundle.diffraction["vessel"]["inertia"]["centre_of_gravity"][2] == (
        pytest.approx(156 / 380 - 2)
    )


@pytest.mark.skipif(sys.platform == "win32", reason="POSIX directory modes")
@pytest.mark.parametrize("existing", [False, True])
def test_published_output_preserves_directory_permissions(inputs, tmp_path, existing):
    """A new directory follows mkdir defaults; an empty one retains its mode."""
    reference = tmp_path / "reference"
    reference.mkdir()
    output = tmp_path / "output"
    expected_mode = stat.S_IMODE(reference.stat().st_mode)
    if existing:
        output.mkdir()
        output.chmod(0o750)
        expected_mode = 0o750
    prepare_chain(ChainInput.model_validate(inputs), output)
    assert stat.S_IMODE(output.stat().st_mode) == expected_mode
    assert (output / "manifest.json").is_file()


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
inputs = yaml.safe_load(Path(sys.argv[1]).read_text(encoding="utf-8"))
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
    marker.write_text("existing data", encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="empty"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert marker.read_text(encoding="utf-8") == "existing data"
    assert list(tmp_path.iterdir()) == [marker]


def test_repository_validator_errors_are_not_bypassed(inputs, tmp_path):
    inputs["template"]["generic"]["constraints"] = [
        dict(name="Other", in_frame_connection="UndefinedConnection")
    ]
    with pytest.raises(ValueError, match="validation failed"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())
    inputs["template"]["generic"]["constraints"] = []
    prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert (tmp_path / "manifest.json").exists()


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
    report = (tmp_path / "analysis.html").read_text(encoding="utf-8")
    assert "380.0" in report and "NOT_EVALUATED" in report
    # Rebuilds compare engineering values with tolerances, not float spelling.
    from bs4 import BeautifulSoup

    stored = BeautifulSoup(
        EXAMPLE.with_name("analysis.html").read_text(encoding="utf-8"), "html.parser"
    )
    rebuilt = BeautifulSoup(report, "html.parser")
    stored_rows = stored.select_one("#results table").select("tbody tr")
    rebuilt_rows = rebuilt.select_one("#results table").select("tbody tr")
    assert len(stored_rows) == len(rebuilt_rows) == 4
    for expected, actual in zip(stored_rows, rebuilt_rows, strict=True):
        expected_value = expected.find_all("td")[1]
        actual_value = actual.find_all("td")[1]
        assert float(actual_value.text) == pytest.approx(
            float(expected_value.text), abs=0.05
        )
        expected_value.string = actual_value.string = "numeric value checked above"
    # Preserve the report's structural, provenance and wording regression check.
    assert stored == rebuilt, (
        "Report drift; rebuild: PYTHONPATH=src python "
        'examples/workflows/floating-wind-chain/run.py --out "$OUTPUT_DIR"'
    )
    assert (tmp_path / "shared-input.json").exists()
    manifest = json.loads((tmp_path / "manifest.json").read_text(encoding="utf-8"))
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
    path.write_text(json.dumps([record_data]), encoding="utf-8", newline="\n")
    records = [
        ResultRecord.model_validate(r)
        for r in json.loads(path.read_text(encoding="utf-8"))
    ]
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
        train = yaml.safe_load(output.read_text(encoding="utf-8"))["Environment"][
            "WaveTrains"
        ][0]
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
        yaml.safe_load((tmp_path / "diffraction.yml").read_text(encoding="utf-8"))
        == bundle.diffraction
    )
    for name, spec in bundle.coupled_specs.items():
        assert (
            yaml.safe_load((tmp_path / f"{name}.yml").read_text(encoding="utf-8"))
            == spec
        )
    for name, digest in bundle.manifest["files"].items():
        assert hashlib.sha256((tmp_path / name).read_bytes()).hexdigest() == digest, (
            "Digest mismatch; rebuild: PYTHONPATH=src python "
            'examples/workflows/floating-wind-chain/run.py --out "$OUTPUT_DIR"'
        )


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


@pytest.mark.parametrize("water_surface_z", [5.0, -5.0])
def test_nonzero_water_datum_rejected_before_writing(inputs, tmp_path, water_surface_z):
    inputs["template"]["environment"]["raw_properties"] = {
        "WaterSurfaceZ": water_surface_z
    }
    with pytest.raises(ValueError, match="datum"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path / "output")
    assert not list(tmp_path.iterdir())


@pytest.mark.parametrize("raw_properties", [{}, {"WaterSurfaceZ": 0.0}])
def test_generated_water_and_hull_share_zero_datum(inputs, tmp_path, raw_properties):
    inputs["template"]["environment"]["raw_properties"] = raw_properties
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    includes = tmp_path / "operating" / "includes"
    environment = yaml.safe_load(
        (includes / "03_environment.yml").read_text(encoding="utf-8")
    )["Environment"]
    objects = yaml.safe_load(
        (includes / "20_generic_objects.yml").read_text(encoding="utf-8")
    )
    assert environment["WaterSurfaceZ"] == 0
    assert objects["Vessels"][0]["InitialPosition"] == [0, 0, 0]
    draught = objects["VesselTypes"][0]["Draughts"][0]
    assert draught["CentreOfMass"][2] == pytest.approx(128 / 380 - 2)
    assert (
        draught["CentreOfMass"]
        == bundle.diffraction["vessel"]["inertia"]["centre_of_gravity"]
    )


@pytest.mark.parametrize("wave_type", ["jonswap", "pierson_moskowitz"])
@pytest.mark.parametrize(
    "raw_train,expected_name",
    [(None, "CustomWave"), ({}, "CustomWave"), ({"Name": "RawWave"}, "RawWave")],
)
def test_wave_name_survives_generation(
    inputs, tmp_path, wave_type, raw_train, expected_name
):
    env = inputs["template"]["environment"]
    env["waves"].update(type=wave_type, name="CustomWave")
    if raw_train is not None:
        env["raw_properties"] = {"WaveTrains": [raw_train]}
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    output = tmp_path / "operating" / "includes" / "03_environment.yml"
    train = yaml.safe_load(output.read_text(encoding="utf-8"))["Environment"][
        "WaveTrains"
    ][0]
    assert train["Name"] == expected_name
    assert (
        bundle.coupled_specs["operating"]["environment"]["waves"]["trains"][0]["name"]
        == "CustomWave"
    )


def test_text_io_requires_utf8_and_lf():
    """Every text operation in the adapter, runner and tests declares its codec."""
    root = EXAMPLE.parents[3]
    paths = [
        root / "src/digitalmodel/floating_wind/solver_chain.py",
        EXAMPLE.with_name("run.py"),
        Path(__file__),
    ]
    for path in paths:
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call) or not isinstance(
                node.func, ast.Attribute
            ):
                continue
            if node.func.attr not in {"read_text", "write_text"}:
                continue
            keywords = {kw.arg: kw.value for kw in node.keywords}
            assert (
                ast.literal_eval(keywords.get("encoding", ast.Constant(None)))
                == "utf-8"
            ), f"{path.name}:{node.lineno}: explicit UTF-8 required"
            if node.func.attr == "write_text":
                assert (
                    ast.literal_eval(keywords.get("newline", ast.Constant(None)))
                    == "\n"
                )


def test_example_uses_utf8_with_non_utf8_locale(tmp_path):
    """Exercise file I/O with a cp1252 default even on UTF-8 systems."""
    script = """
import runpy
import sys
from pathlib import Path
original_open = Path.open
def locale_open(self, mode='r', buffering=-1, encoding=None, errors=None, newline=None):
    if 'b' not in mode and encoding in (None, 'locale'):
        encoding = 'cp1252'
    return original_open(self, mode, buffering, encoding, errors, newline)
Path.open = locale_open
runner, output = sys.argv[1:]
sys.argv = [runner, '--out', output]
runpy.run_path(runner, run_name='__main__')
"""
    result = subprocess.run(
        [
            sys.executable,
            "-X",
            "utf8=0",
            "-c",
            script,
            str(EXAMPLE.with_name("run.py")),
            str(tmp_path),
        ],
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    assert result.returncode == 0, result.stderr
    report = (tmp_path / "analysis.html").read_bytes().decode("utf-8")
    assert "×" in report
    assert b"\r\n" not in (tmp_path / "analysis.html").read_bytes()


@pytest.mark.parametrize(
    "wave_type",
    [
        "airy",
        "Airy",
        "dean_stream",
        "Dean stream",
        "stokes_5th",
        "Stokes' 5th",
        "cnoidal",
        "Cnoidal",
    ],
)
def test_regular_waves_rejected_without_design_wave_conversion(
    inputs, tmp_path, wave_type
):
    inputs["template"]["environment"]["waves"]["type"] = wave_type
    with pytest.raises(ValueError, match="spectral wave type"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())


@pytest.mark.parametrize("field", ["water_density_kg_m3", "ballast_vcg_m"])
def test_density_and_ballast_position_are_required(inputs, field):
    inputs.pop(field, None)
    with pytest.raises(ValueError, match=field):
        ChainInput.model_validate(inputs)


def test_template_density_mismatch_rejected(inputs, tmp_path):
    inputs["template"]["environment"]["water"]["density"] = 1.0
    with pytest.raises(ValueError, match="density mismatch"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())


def test_nonzero_steady_load_rejected_before_writing(inputs, tmp_path):
    inputs["load_cases"][0]["steady_load_kN"] = 10
    with pytest.raises(ValueError, match="steady load"):
        prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert not list(tmp_path.iterdir())


def test_hull_only_equilibrium_gap_in_manifest_and_report(inputs, tmp_path):
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    # 400 m3 * 1.025 t/m3 = 410 t displacement; 410 - 30 = 380 t hull.
    balance = bundle.manifest["hull_only_equilibrium"]
    assert balance["displacement_t"] == pytest.approx(410)
    assert balance["body_mass_t"] == pytest.approx(380)
    assert balance["missing_topside_mass_t"] == pytest.approx(30)
    assert balance["status"] == "NOT_EVALUATED"
    assert "hull_only_equilibrium" in bundle.manifest["not_evaluated"]
    document = DocumentMeta(
        number="R0000-RPT-001-00",
        revision="00",
        title="Sizing analysis",
        project="Generic",
        client="Generic",
    )
    report = render_html(build_chain_report(bundle, document=document))
    assert "hull_only_equilibrium" in report
    assert "410.000" in report and "380.000" in report


def test_explicit_basis_drives_mapped_mass_and_cg(inputs, tmp_path):
    inputs.update(water_density_kg_m3=1025.0, ballast_vcg_m=0.1)
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    # V=20*10*2=400; displacement=400*1.025=410; hull=410-30=380 t.
    assert bundle.diffraction["environment"]["water_density"] == pytest.approx(1025)
    # z=(100*1+280*0.1)/380-2 = -1.663 m above SWL.
    assert bundle.diffraction["vessel"]["inertia"]["centre_of_gravity"][
        2
    ] == pytest.approx(-1.6631578947368422)
    assert bundle.coupled_specs["operating"]["environment"]["water"][
        "density"
    ] == pytest.approx(1.025)
    assert any("CGPM" in source for source in bundle.manifest["sources"])
    assert any("NOAA" in source for source in bundle.manifest["sources"])


@pytest.mark.parametrize(
    "field,value", [("water_density_kg_m3", 1000), ("ballast_vcg_m", 0.5)]
)
def test_unsupported_screening_basis_fails_closed(inputs, field, value):
    inputs.update(water_density_kg_m3=1025.0, ballast_vcg_m=0.1)
    inputs[field] = value
    with pytest.raises(ValueError, match="unsupported screening"):
        ChainInput.model_validate(inputs)


@pytest.mark.parametrize("value", [None, 0, -1, float("nan"), float("inf")])
def test_invalid_water_density_rejected(inputs, value):
    inputs["water_density_kg_m3"] = value
    with pytest.raises(ValueError):
        ChainInput.model_validate(inputs)


@pytest.mark.parametrize("value", [None, -1, float("nan"), float("inf")])
def test_invalid_ballast_position_rejected(inputs, value):
    inputs["ballast_vcg_m"] = value
    with pytest.raises(ValueError):
        ChainInput.model_validate(inputs)


def test_output_symlink_rejected(inputs, tmp_path):
    output = tmp_path / "output"
    try:
        output.symlink_to(tmp_path, target_is_directory=True)
    except OSError as error:
        if getattr(error, "winerror", None) == 1314:
            pytest.skip("Directory symlink creation requires Windows privilege")
        raise
    with pytest.raises(ValueError, match="link"):
        prepare_chain(ChainInput.model_validate(inputs), output)


def test_output_junction_rejected(inputs, tmp_path, monkeypatch):
    # A junction is a link even though is_symlink() returns False on Windows.
    junction = tmp_path / "junction"
    monkeypatch.setattr(
        Path, "is_junction", lambda path: path == junction, raising=False
    )
    with pytest.raises(ValueError, match="link"):
        prepare_chain(ChainInput.model_validate(inputs), junction)


def test_manifest_paths_are_portable(inputs, tmp_path):
    bundle = prepare_chain(ChainInput.model_validate(inputs), tmp_path)
    assert bundle.manifest["files"]
    assert all("\\" not in path for path in bundle.manifest["files"])


def test_reparse_point_rejected_without_junction_api(inputs, tmp_path, monkeypatch):
    """Python 3.11 on Windows has reparse attributes without Path.is_junction."""
    output = tmp_path / "reparse"
    original = Path.lstat

    def attributes(path, *args, **kwargs):
        if path == output:
            return SimpleNamespace(
                st_mode=stat.S_IFDIR,
                st_file_attributes=stat.FILE_ATTRIBUTE_REPARSE_POINT,
            )
        return original(path, *args, **kwargs)

    monkeypatch.setattr(Path, "lstat", attributes)
    monkeypatch.setattr(Path, "is_junction", lambda path: False, raising=False)
    with pytest.raises(ValueError, match="link"):
        prepare_chain(ChainInput.model_validate(inputs), output)


def test_failure_in_later_case_does_not_publish_earlier_case(
    inputs, tmp_path, monkeypatch
):
    from digitalmodel.floating_wind.solver_chain import ModularModelGenerator

    inputs["load_cases"].append(dict(name="parked", hs_m=3, tp_s=9, steady_load_kN=0))
    original = ModularModelGenerator.generate
    calls = []

    def fail_second(generator, output):
        calls.append(output)
        if len(calls) == 2:
            raise ValueError("injected generation failure")
        return original(generator, output)

    monkeypatch.setattr(ModularModelGenerator, "generate", fail_second)
    parent = tmp_path / "generation"
    parent.mkdir()
    output = parent / "output"
    output.mkdir()
    with pytest.raises(ValueError, match="injected generation failure"):
        prepare_chain(ChainInput.model_validate(inputs), output)
    assert not list(output.iterdir())
    assert not list(parent.glob(".chain-*"))
