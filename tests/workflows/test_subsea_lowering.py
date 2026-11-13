"""Offline lowering screens use independent arithmetic, never solver claims."""

import copy
import hashlib
import json
import sys

import pytest

from digitalmodel.workflows.subsea_lowering import (
    assess_sweep,
    build_cases,
    build_report,
    main,
)


@pytest.fixture
def basis():
    """Invented arithmetic exercise, not engineering data or a benchmark.

    Hs=[1,2,3] m, Tp=[6,8,10,12] s, heading=[0,90,180] deg:
    3*4*3=36 cases. Project exercise limit=100 kN. Ratios are
    80/100=0.8, 120/100=1.2, 70/100=0.7 at the three Hs levels.
    A pass at 3 m cannot bridge the failure at 2 m; envelope=1 m.
    """

    def quantity(value, unit):
        return {
            "value": value,
            "unit": unit,
            "source": "Independent arithmetic exercise in test docstring",
        }

    return {
        "basis_id": "illustrative-v1",
        "purpose": "illustrative",
        "model_sha256": "a" * 64,
        "hs": [quantity(v, "m") for v in [1, 2, 3]],
        "tp": [quantity(v, "s") for v in [6, 8, 10, 12]],
        "wave_direction_deg": [
            {**quantity(v, "deg"), "convention": "orcaflex_global_propagation"}
            for v in [0, 90, 180]
        ],
        "criteria": [
            {
                "name": "hook_tension",
                "phase": phase,
                "bound": "maximum",
                "limit": quantity(100, "kN"),
            }
            for phase in ["splash", "submerged", "landing"]
        ],
    }


def records(basis):
    return [
        {
            "case_id": case["case_id"],
            "basis_id": basis["basis_id"],
            "model_sha256": basis["model_sha256"],
            "solver_version": "not-run",
            "kind": "illustrative",
            "sea_state": {
                key: case[key] for key in ("hs_m", "tp_s", "wave_direction_deg")
            },
            "metrics": [
                {
                    "name": "hook_tension",
                    "phase": phase,
                    "unit": "kN",
                    "value": {1: 80, 2: 120, 3: 70}[case["hs_m"]],
                }
                for phase in ["splash", "submerged", "landing"]
            ],
        }
        for case in build_cases(basis)
    ]


def test_source_table_and_cartesian_product(basis):
    cases = build_cases(basis)
    assert len(cases) == len({c["case_id"] for c in cases}) == 36
    assert cases[0]["hs_m"] == 1
    assert cases[-1]["tp_s"] == 12
    assert cases[-1]["wave_direction_deg"] == 180
    assert cases[0]["sources"]["hs"] == basis["hs"][0]["source"]


@pytest.mark.parametrize(
    "convention", [None, "", "coming_from", "vessel_relative", "clockwise_from_north"]
)
def test_wave_direction_requires_global_propagation_convention(basis, convention):
    for direction in basis["wave_direction_deg"]:
        direction["convention"] = "orcaflex_global_propagation"
    if convention is None:
        basis["wave_direction_deg"][-1].pop("convention")
    else:
        basis["wave_direction_deg"][-1]["convention"] = convention
    with pytest.raises(ValueError, match="convention"):
        build_cases(basis)


def test_contiguous_envelope_and_governing_comparison(basis):
    result = assess_sweep(basis, records(basis))
    assert result["screened_hs_limit_m"] == 1
    assert result["levels"] == [
        {"hs_m": 1, "status": "PASS"},
        {"hs_m": 2, "status": "FAIL"},
        {"hs_m": 3, "status": "PASS"},
    ]
    assert result["cases"][12]["checks"][0]["value"] == 120
    assert result["cases"][12]["checks"][0]["limit"] == 100
    assert result["engineering_status"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "mutation", ["missing", "phase", "units", "revision", "model", "version"]
)
def test_incomplete_or_mismatched_evidence_cannot_give_limit(basis, mutation):
    data = records(basis)
    if mutation == "missing":
        data.pop(0)
    if mutation == "phase":
        data[0]["metrics"].pop()
    if mutation == "units":
        data[0]["metrics"][0]["unit"] = "N"
    if mutation == "revision":
        data[0]["basis_id"] = "stale"
    if mutation == "model":
        data[0]["model_sha256"] = "b" * 64
    if mutation == "version":
        data[0]["solver_version"] = "other"
    result = assess_sweep(basis, data)
    assert result["screened_hs_limit_m"] is None
    assert result["screening_status"] == "NOT_EVALUATED"
    assert result["weather_window"]["status"] == "NOT_EVALUATED"


def test_no_criteria_no_acceptance(basis):
    basis["criteria"] = []
    result = assess_sweep(basis, records(basis))
    assert result["screened_hs_limit_m"] is None


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -1, True])
def test_invalid_physical_inputs(basis, bad):
    basis["hs"][0]["value"] = bad
    with pytest.raises(ValueError):
        build_cases(basis)


def test_duplicate_levels_and_records_rejected(basis):
    duplicate = copy.deepcopy(basis)
    duplicate["wave_direction_deg"].append(duplicate["wave_direction_deg"][0])
    with pytest.raises(ValueError):
        build_cases(duplicate)
    data = records(basis)
    with pytest.raises(ValueError):
        assess_sweep(basis, data + [data[0]])


def test_missing_source_rejected(basis):
    basis["tp"][0]["source"] = " "
    with pytest.raises(ValueError):
        build_cases(basis)


def test_minimum_bound_equality_and_failure(basis):
    """Exercise: 80 kN >= 80 kN passes; 80 kN < 81 kN fails."""
    for criterion in basis["criteria"]:
        criterion["bound"] = "minimum"
        criterion["limit"]["value"] = 80
    assert assess_sweep(basis, records(basis))["levels"][0]["status"] == "PASS"
    basis["criteria"][0]["limit"]["value"] = 81
    assert assess_sweep(basis, records(basis))["screened_hs_limit_m"] is None


def test_weather_window_and_standard_report(basis):
    """[0.5,1,2,0.5] at 1 h gives spells 2 h and 1 h, 75% workability."""
    weather = {
        "hs_timeseries_m": [0.5, 1, 2, 0.5],
        "time_step_hours": 1,
        "required_duration_hours": 2,
        "source": "Arithmetic exercise",
    }
    result = assess_sweep(basis, records(basis), weather)
    assert result["weather_window"]["workability_pct"] == 75
    assert result["weather_window"]["num_viable_windows"] == 1
    html = build_report(basis, result, date="2026-10-20")
    assert "Subsea structure lowering analysis" in html
    assert "NOT_EVALUATED" in html
    assert "Revision history" in html
    assert ">120</td>" in html
    assert "OrcFxAPI" not in sys.modules


def test_cli_readback_and_no_clobber(basis, tmp_path):
    source = tmp_path / "basis.json"
    source.write_text(json.dumps(basis))
    digest = hashlib.sha256(source.read_bytes()).hexdigest()
    out = tmp_path / "output"
    assert main([str(source), "--output", str(out), "--date", "2026-10-20"]) == 0
    saved = json.loads((out / "assessment.json").read_text())
    assert len(saved["cases"]) == 36
    assert saved["basis_sha256"] == digest
    assert saved["engineering_status"] == "NOT_EVALUATED"
    assert (out / "report.html").is_file()
    with pytest.raises(FileExistsError):
        main([str(source), "--output", str(out)])


def test_signed_clearance_failure(basis):
    """A clearance of -0.1 m is below an illustrative minimum of 0.0 m."""
    data = records(basis)
    for criterion in basis["criteria"]:
        criterion.update(name="clearance", bound="minimum")
        criterion["limit"].update(value=0, unit="m")
    for record in data:
        for metric in record["metrics"]:
            metric.update(name="clearance", value=-0.1, unit="m")
    assert assess_sweep(basis, data)["screening_status"] == "FAIL"


@pytest.mark.parametrize(
    "bound,value,status",
    [
        ("minimum", 0.0, "PASS"),
        ("minimum", -0.1, "PASS"),
        ("minimum", -0.2, "FAIL"),
        ("maximum", -0.2, "PASS"),
        ("maximum", -0.1, "PASS"),
        ("maximum", 0.0, "FAIL"),
    ],
)
def test_negative_criterion_bounds(basis, bound, value, status):
    """Compare signed metre values directly against an illustrative -0.1 m bound."""
    data = records(basis)
    for criterion in basis["criteria"]:
        criterion.update(name="clearance", bound=bound)
        criterion["limit"].update(value=-0.1, unit="m")
    for record in data:
        for metric in record["metrics"]:
            metric.update(name="clearance", value=value, unit="m")
    result = assess_sweep(basis, data)
    assert result["screening_status"] == status
    assert all(
        check["status"] == status and check["limit"] == -0.1
        for case in result["cases"]
        for check in case["checks"]
    )


@pytest.mark.parametrize(
    "bad", [float("nan"), float("inf"), -float("inf"), True, "-0.1"]
)
def test_criterion_bounds_require_finite_numbers(basis, bad):
    basis["criteria"][0]["limit"]["value"] = bad
    with pytest.raises(ValueError):
        assess_sweep(basis, records(basis))


def test_result_sea_state_must_match_case(basis):
    data = records(basis)
    for record, case in zip(data, build_cases(basis)):
        record["sea_state"] = {
            key: case[key] for key in ("hs_m", "tp_s", "wave_direction_deg")
        }
    data[0]["sea_state"]["hs_m"] = 99
    assert assess_sweep(basis, data)["screened_hs_limit_m"] is None


def test_unbounded_or_unknown_cases_rejected(basis):
    data = records(basis)
    data[0]["case_id"] = "unknown"
    with pytest.raises(ValueError):
        assess_sweep(basis, data)
    basis["wave_direction_deg"][0]["value"] = 360
    with pytest.raises(ValueError):
        build_cases(basis)


def test_nonfinite_response_rejected(basis):
    data = records(basis)
    data[0]["metrics"][0]["value"] = float("nan")
    with pytest.raises(ValueError):
        assess_sweep(basis, data)


def test_incomplete_grid_blocks_even_lower_passing_level(basis):
    data = records(basis)
    data.pop()
    assert assess_sweep(basis, data)["screened_hs_limit_m"] is None


def test_no_qualifying_weather_spell(basis):
    """Two separate one-hour spells cannot cover a two-hour operation."""
    weather = {
        "hs_timeseries_m": [0.5, 2, 0.5],
        "time_step_hours": 1,
        "required_duration_hours": 2,
        "source": "Arithmetic exercise",
    }
    result = assess_sweep(basis, records(basis), weather)
    assert result["weather_window"]["status"] == "FAIL"
    assert result["weather_window"]["num_viable_windows"] == 0


def test_report_revision_and_evidence(basis):
    result = assess_sweep(basis, records(basis))
    result["basis_sha256"] = "c" * 64
    html = build_report(basis, result, date="2026-10-20")
    assert 'id="revision-history"' in html
    assert (
        html.index('id="summary"')
        < html.index('id="revision-history"')
        < html.index('id="cases"')
    )
    assert "c" * 64 in html
    assert "not-run" in html


def test_report_preserves_small_limits_and_distinct_sea_states(basis):
    """Distinct 1.2/1.25 m waves and 0.03/0.04 m bounds stay legible."""
    data = records(basis)
    basis["hs"][0]["value"] = 1.2
    basis["hs"][1]["value"] = 1.25
    cases = build_cases(basis)
    for criterion in basis["criteria"]:
        criterion.update(name="clearance", bound="minimum")
        criterion["limit"].update(value=0.04, unit="m")
    for record, case in zip(data, cases):
        record["case_id"] = case["case_id"]
        record["sea_state"] = {
            key: case[key] for key in ("hs_m", "tp_s", "wave_direction_deg")
        }
        for metric in record["metrics"]:
            metric.update(name="clearance", value=0.03, unit="m")
    result = assess_sweep(basis, data)
    assert result["screening_status"] == "FAIL"
    html = build_report(basis, result, date="2026-10-22")
    cases_html = html.split('id="cases"', 1)[1].split("</section>", 1)[0]
    comparisons_html = html.split('id="comparisons"', 1)[1].split("</section>", 1)[0]
    for value in ("1.2", "1.25"):
        assert f">{value}</td>" in cases_html
    for value in ("0.03", "0.04"):
        assert f">{value}</td>" in comparisons_html


def test_report_input_echo_preserves_numeric_values(basis, monkeypatch):
    import digitalmodel.reporting as reporting

    basis["hs"][0]["value"] = 1.25
    basis["criteria"][0]["limit"]["value"] = 0.04
    original = copy.deepcopy(basis)
    captured = []
    render_html = reporting.render_html

    def capture(spec):
        captured.append(spec.input_echo)
        return render_html(spec)

    monkeypatch.setattr(reporting, "render_html", capture)
    build_report(basis, assess_sweep(basis, []), date="2026-10-22")
    assert captured == [original]
    assert basis == original


def test_report_weather_precision_matches_quantity(basis):
    weather = {
        "hs_timeseries_m": [0.5, 2, 2],
        "time_step_hours": 0.04,
        "required_duration_hours": 0.04,
        "source": "Arithmetic exercise",
    }
    result = assess_sweep(basis, records(basis), weather)
    html = build_report(basis, result, date="2026-10-22")
    weather_html = html.split('id="weather"', 1)[1].split("</section>", 1)[0]
    assert "0.04" in weather_html
    assert "33.3" in weather_html
    assert "33.333333" not in weather_html


@pytest.mark.parametrize("name", ["Sea", "Sea °"])
def test_prepare_case_models_and_preserve_input(basis, tmp_path, monkeypatch, name):
    """Independent overlay: 3*4*3=36 files; axes replace only wave fields.

    Minimal YAML is a structural exercise, not a simulation-ready model.
    Its depth=50 m and old wave values=9 are invented test inputs.
    """
    import yaml
    from pathlib import Path

    from digitalmodel.workflows.subsea_lowering import prepare_models

    read_text = Path.read_text

    def locale_read_text(path, encoding=None, errors=None):
        return read_text(path, encoding=encoding or "cp1252", errors=errors)

    monkeypatch.setattr(Path, "read_text", locale_read_text)
    model = {
        "General": {"UnitsSystem": "SI"},
        "Environment": {
            "WaterDepth": 50,
            "WaveTrains": [
                {
                    "Name": name,
                    "WaveType": "JONSWAP",
                    "WaveJONSWAPParameters": "Partially specified",
                    "WaveHs": 9,
                    "WaveTp": 9,
                    "WaveGamma": 1,
                    "WaveDirection": 9,
                }
            ],
        },
    }
    source = tmp_path / "source.yml"
    source.write_text(yaml.safe_dump(model, sort_keys=False), encoding="utf-8")
    original = source.read_bytes()
    basis["model_sha256"] = hashlib.sha256(original).hexdigest()
    output = tmp_path / "models"
    manifest = prepare_models(basis, source, output)
    assert len(manifest["models"]) == 36
    assert manifest["engineering_status"] == "NOT_EVALUATED"
    assert source.read_bytes() == original
    for case, item in zip(build_cases(basis), manifest["models"]):
        path = output / item["file"]
        assert hashlib.sha256(path.read_bytes()).hexdigest() == item["sha256"]
        serialized = path.read_text(encoding="utf-8")
        assert serialized.index("WaveGamma:") < serialized.index("WaveTp:")
        generated = yaml.safe_load(serialized)
        expected = copy.deepcopy(model)
        expected["Environment"]["WaveTrains"][0].update(
            WaveHs=case["hs_m"],
            WaveTp=case["tp_s"],
            WaveDirection=case["wave_direction_deg"],
        )
        assert generated == expected
        assert item["yaml_valid"] is True
    with pytest.raises(FileExistsError):
        prepare_models(basis, source, output)


def test_cli_prepares_model_grid_and_records_manifest(basis, tmp_path):
    """The CLI --model path writes the grid plus report and assessment."""
    import yaml

    model = tmp_path / "source.yml"
    model.write_text(
        yaml.safe_dump(
            {
                "General": {"UnitsSystem": "SI"},
                "Environment": {
                    "WaveTrains": [
                        {
                            "Name": "Sea °",
                            "WaveType": "JONSWAP",
                            "WaveJONSWAPParameters": "Partially specified",
                            "WaveTp": 9,
                            "WaveGamma": 1,
                        }
                    ]
                },
            },
            sort_keys=False,
            allow_unicode=True,
        ),
        encoding="utf-8",
    )
    original = model.read_bytes()
    basis["model_sha256"] = hashlib.sha256(original).hexdigest()
    source = tmp_path / "basis.json"
    source.write_text(json.dumps(basis), encoding="utf-8")
    out = tmp_path / "output"
    args = [str(source), "--model", str(model), "--output", str(out)]
    assert main([*args, "--date", "2026-10-22"]) == 0
    assessment = json.loads((out / "assessment.json").read_text(encoding="utf-8"))
    manifest_path = out / "models" / "manifest.json"
    manifest_bytes = manifest_path.read_bytes()
    manifest = json.loads(manifest_bytes)
    assert len(manifest["models"]) == len(assessment["cases"]) == 36
    assert manifest["source_sha256"] == basis["model_sha256"]
    assert assessment["basis_sha256"] == hashlib.sha256(source.read_bytes()).hexdigest()
    assert (
        assessment["engineering_status"]
        == manifest["engineering_status"]
        == "NOT_EVALUATED"
    )
    for case, item in zip(build_cases(basis), manifest["models"]):
        generated = out / "models" / item["file"]
        assert hashlib.sha256(generated.read_bytes()).hexdigest() == item["sha256"]
        text = generated.read_text(encoding="utf-8")
        assert text.index("WaveGamma:") < text.index("WaveTp:")
        wave = yaml.safe_load(text)["Environment"]["WaveTrains"][0]
        assert wave["Name"] == "Sea °"
        assert (
            wave["WaveDirection"]
            == item["wave_direction_deg"]
            == case["wave_direction_deg"]
        )
        assert item["convention"] == "orcaflex_global_propagation"
    assert "Subsea structure lowering analysis" in (out / "report.html").read_text(
        encoding="utf-8"
    )
    with pytest.raises(FileExistsError):
        main(args)
    assert model.read_bytes() == original
    assert manifest_path.read_bytes() == manifest_bytes


@pytest.mark.parametrize("units", ["US", "User", "si", None, "missing"])
def test_prepare_requires_explicit_si_units(basis, tmp_path, units):
    """Metre-valued Hs must not be written into a different native unit system."""
    import yaml
    from digitalmodel.workflows.subsea_lowering import prepare_models

    model = {
        "General": {"UnitsSystem": units},
        "Environment": {
            "WaterDepth": 50,
            "WaveTrains": [
                {
                    "WaveType": "JONSWAP",
                    "WaveJONSWAPParameters": "Partially specified",
                    "WaveGamma": 1,
                    "WaveHs": 9,
                    "WaveTp": 9,
                }
            ],
        },
    }
    if units is None:
        model["General"] = {}
    elif units == "missing":
        del model["General"]
    source = tmp_path / "source.yml"
    source.write_text(yaml.safe_dump(model))
    original = source.read_bytes()
    basis["model_sha256"] = hashlib.sha256(original).hexdigest()
    output = tmp_path / "models"
    with pytest.raises(ValueError, match="explicit SI units"):
        prepare_models(basis, source, output)
    assert not output.exists()
    assert source.read_bytes() == original


@pytest.mark.parametrize(
    "mutation", ["digest", "dependency", "waves", "spectrum", "invalid"]
)
def test_prepare_refuses_ambiguous_models(basis, tmp_path, mutation):
    import yaml
    from digitalmodel.workflows.subsea_lowering import prepare_models

    model = {
        "General": {"UnitsSystem": "SI"},
        "Environment": {"WaveTrains": [{"WaveType": "JONSWAP"}]},
    }
    if mutation == "dependency":
        model["BaseFile"] = "absent.yml"
    if mutation == "waves":
        model["Environment"]["WaveTrains"] *= 2
    if mutation == "spectrum":
        model["Environment"]["WaveTrains"][0]["WaveType"] = "Regular"
    if mutation == "invalid":
        model["Environment"]["WaveHs"] = 1
    source = tmp_path / "source.yml"
    source.write_text(yaml.safe_dump(model))
    basis["model_sha256"] = hashlib.sha256(source.read_bytes()).hexdigest()
    if mutation == "digest":
        basis["model_sha256"] = "b" * 64
    output = tmp_path / "models"
    with pytest.raises(ValueError):
        prepare_models(basis, source, output)
    assert not output.exists()


@pytest.mark.parametrize("mode", [None, "Automatic", "Fully specified"])
def test_jonswap_requires_hs_and_tp_to_be_independent_inputs(basis, tmp_path, mode):
    """Orcina web help, JONSWAP data: only partially specified admits Hs/Tp."""
    import yaml
    from digitalmodel.workflows.subsea_lowering import prepare_models

    wave = {"WaveType": "JONSWAP", "WaveGamma": 1}
    if mode is not None:
        wave["WaveJONSWAPParameters"] = mode
    source = tmp_path / "source.yml"
    source.write_text(
        yaml.safe_dump(
            {
                "General": {"UnitsSystem": "SI"},
                "Environment": {"WaveTrains": [wave]},
            }
        )
    )
    basis["model_sha256"] = hashlib.sha256(source.read_bytes()).hexdigest()
    with pytest.raises(ValueError, match="Partially specified"):
        prepare_models(basis, source, tmp_path / "models")


def test_example_is_executable_and_answers_are_independent():
    from pathlib import Path

    path = Path(__file__).parents[2] / "examples/workflows/subsea-lowering/README.md"
    code = path.read_text().split("```python\n", 1)[1].split("```", 1)[0]
    namespace = {}
    exec(compile(code, "worked-example", "exec"), namespace)
    result = namespace["result"]
    assert result["screened_hs_limit_m"] == 1
    assert result["weather_window"]["longest_window_hours"] == 2


@pytest.mark.parametrize("linked", ["WaveTz", "WaveFm"])
def test_linked_period_input_cannot_override_peak_period(basis, tmp_path, linked):
    """Orcina JONSWAP documentation: linked period/frequency inputs conflict."""
    import yaml
    from digitalmodel.workflows.subsea_lowering import prepare_models

    wave = {
        "WaveType": "JONSWAP",
        "WaveJONSWAPParameters": "Partially specified",
        "WaveGamma": 1,
        linked: 1,
    }  # Invented structural exercise values.
    source = tmp_path / "source.yml"
    source.write_text(
        yaml.safe_dump(
            {
                "General": {"UnitsSystem": "SI"},
                "Environment": {"WaveTrains": [wave]},
            }
        )
    )
    basis["model_sha256"] = hashlib.sha256(source.read_bytes()).hexdigest()
    with pytest.raises(ValueError, match="linked"):
        prepare_models(basis, source, tmp_path / "models")
