# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared synthetic exports and independent contract rejection checks."""

from copy import deepcopy
import hashlib
import json
from pathlib import Path
import subprocess
import sys

import pytest
from jsonschema import Draft202012Validator

from digitalmodel.riser_study.exchange import (
    VERSION, CASE_IDS, SEA_STATE_IDS, assert_token_ids, exchange_schema,
    load_campaign, load_case_matrix, load_json, load_result, load_token_registry,
    validate_campaign_run, validate_matrix, validate_result,
)

FIXTURES = Path(__file__).parent / "fixtures"
DATA = Path(__file__).parents[3] / "src/digitalmodel/riser_study/exchange/data"


@pytest.fixture
def exports():
    return load_json(FIXTURES / "exports-v1.json")["exports"]


@pytest.fixture
def campaign(tmp_path, exports):
    for name, record in exports.items():
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes((json.dumps(record, indent=2) + "\n").encode())
    return tmp_path / "results"


def strength(exports):
    return deepcopy(exports["results/R08-W1-SCR/strength.json"])


def fatigue(exports):
    return deepcopy(exports["results/R08-W1-SCR/fatigue-F03.json"])


def test_version():
    assert VERSION == "ris-exchange/1"


@pytest.mark.parametrize("kind", ["strength", "fatigue"])
def test_schema_round_trips(kind, exports, tmp_path):
    schema = exchange_schema(kind)
    Draft202012Validator.check_schema(schema)
    for name, record in exports.items():
        if Path(name).name.startswith(kind):
            Draft202012Validator(schema).validate(record)
            path = tmp_path / Path(name).name
            path.write_bytes(json.dumps(record, allow_nan=False).encode())
            assert load_result(path, kind) == record


def test_exact_campaign(campaign):
    records = load_campaign(campaign)
    assert set(records) == set(CASE_IDS)
    assert all(set(case["fatigue"]) == set(SEA_STATE_IDS) for case in records.values())


@pytest.mark.parametrize("missing", ["strength.json", "fatigue-F04.json", "case"])
def test_missing_campaign_member(campaign, missing):
    case = campaign / CASE_IDS[-1]
    if missing == "case":
        case.rename(case.with_name("unexpected-case"))
    else:
        (case / missing).rename(case / "missing.json")
    with pytest.raises(ValueError, match="campaign"):
        load_campaign(campaign)


def test_filename_identity(campaign):
    path = campaign / CASE_IDS[0] / "fatigue-F03.json"
    record = load_json(path)
    record["sea_state"]["id"] = "F04"
    path.write_bytes(json.dumps(record).encode())
    with pytest.raises(ValueError, match="identity"):
        load_campaign(campaign)


def test_matrix_and_pinned_bytes():
    rows = load_case_matrix()
    assert len(rows) == 12
    assert {r["case_id"] for r in rows} == set(CASE_IDS)
    assert {r["fatigue_seas"] for r in rows} == {"F03;F04;F05"}
    assert hashlib.sha256((DATA / "case-matrix-v1.tsv").read_bytes()).hexdigest() == (
        "f95a98735eb3882a354857fc56975a82d1b67095d6a57cb8cf565229e9ddf2c1"
    )
    assert rows[0]["outside_diameter_mm"] == "219.100"
    assert rows[-1]["checked_wall_mm"] == "28.7625"


@pytest.mark.parametrize("change", ["missing", "duplicate", "extra", "sea", "value"])
def test_matrix_rejection(change):
    rows = load_case_matrix()
    if change == "missing":
        rows.pop()
    elif change == "duplicate":
        rows[-1] = rows[0]
    elif change == "extra":
        rows.append({**rows[0], "case_id": "R14-W1-SCR"})
    elif change == "sea":
        rows[0]["fatigue_seas"] = "F03;F04"
    else:
        rows[0]["wall_thickness_mm"] = "1"
    with pytest.raises(ValueError, match="matrix"):
        validate_matrix(rows)


def test_matrix_and_registry_digest_rejection(tmp_path):
    for filename, loader in [("case-matrix-v1.tsv", load_case_matrix),
                             ("token-registry-v1.json", load_token_registry)]:
        path = tmp_path / filename
        path.write_bytes((DATA / filename).read_bytes() + b" ")
        with pytest.raises(ValueError, match="digest"):
            loader(path)


def test_registry_inventory():
    tokens = load_token_registry()
    assert len(tokens) == 58
    assert len({t["id"] for t in tokens}) == 58
    counts = {"riser_strength_runs.py": 30, "riser_configuration_plot.py": 1,
              "riser_fatigue_runs.py": 13, "decision_chart.py": 10,
              "wall_basis_table.py": 1, "rao_disposition.md": 1,
              "review_record.md": 1, "case_matrix.yml": 1}
    for module, count in counts.items():
        ids = {t["id"] for t in tokens if t["producing_module"] == module}
        assert len(ids) == count
        assert_token_ids(module, ids)
        with pytest.raises(ValueError, match="token"):
            assert_token_ids(module, [*ids, "obsolete"])
        with pytest.raises(ValueError, match="token"):
            assert_token_ids(module, list(ids)[1:])
        with pytest.raises(ValueError, match="token"):
            assert_token_ids(module, [*ids, next(iter(ids))])
    with pytest.raises(ValueError, match="module"):
        assert_token_ids("unknown.py", [])


def test_fixture_digests(exports):
    manifest = load_json(FIXTURES / "manifest-v1.json")
    for name, digest in manifest["files"].items():
        assert hashlib.sha256((FIXTURES / name).read_bytes()).hexdigest() == digest
    for name, digest in manifest["records"].items():
        raw = json.dumps(exports[name], sort_keys=True, separators=(",", ":"), allow_nan=False).encode()
        assert hashlib.sha256(raw).hexdigest() == digest
    assert len(exports) == 48


def test_hand_arithmetic(exports):
    data = fatigue(exports)
    damages = []
    for spot in data["hot_spots"]:
        cycles = spot["cycles_per_year"]
        damages.append(data["sea_state"]["occurrence"] * cycles
                       * (spot["stress_range_MPa"] * spot["scf"]) ** 3 / 10**12)
    assert damages == pytest.approx([0.2, 0.025])
    assert data["damage_per_year"] == max(damages)
    assert data["life_years"] == 5
    zero = exports["results/R12-W2-SLWR/fatigue-F05.json"]
    assert zero["damage_per_year"] == 0 and zero["life_years"] is None


# Field provenance and pinned PR heads are listed in the package README.
@pytest.mark.parametrize("variant", ["strength_alias", "basis_label", "offset", "date_time",
    "source_label", "thickness_null", "thickness_absent", "thickness_object",
    "reference_fields", "histogram", "arc_distribution", "life_absent", "run_fields"])
def test_peer_variants(variant, exports):
    kind = "strength" if variant in {"strength_alias", "basis_label", "offset"} else "fatigue"
    data = strength(exports) if kind == "strength" else fatigue(exports)
    if variant == "strength_alias":
        data["utilisation_method"] = data.pop("method")
        data["utilisation_allowable"] = data.pop("allowable")["value"]
    elif variant == "basis_label":
        data["allowable"]["basis_label"] = "ASSUMED"
        data["utilisation_basis"] = "U1"
    elif variant == "offset":
        data["load_case"]["offset"] = {}
    elif variant == "date_time":
        data["run"]["date"] = "2026-10-25T12:00:00+01:30"
    elif variant == "source_label":
        data["sn_curve"]["source_label"] = data["sn_curve"].pop("source")
    elif variant == "thickness_absent":
        del data["sn_curve"]["thickness_correction"]
    elif variant == "thickness_null":
        data["sn_curve"]["thickness_correction"] = None
    elif variant == "thickness_object":
        data["sn_curve"]["thickness_correction"] = {"thickness_mm": 40,
            "reference_thickness_mm": 20, "exponent": 1, "source_label": "ASSUMED"}
    elif variant == "reference_fields":
        data["sn_curve"].update(reference_cycles=1000000, reference_range_MPa=100,
                                endurance_limit_MPa=None)
    elif variant == "histogram":
        spot = data["hot_spots"][0]
        spot["range_histogram"] = [{"stress_range_MPa": spot.pop("stress_range_MPa"),
                                    "cycles_per_year": spot["cycles_per_year"]}]
    elif variant == "arc_distribution":
        data["arc_distribution"] = [{k: v for k, v in spot.items() if k != "name"}
                                    for spot in reversed(data["hot_spots"])]
    elif variant == "life_absent":
        del data["life_years"]
    elif variant == "run_fields":
        data["run"].update(id="synthetic", phase="A", status="completed", qualified=True,
                           data_basis="ASSUMED synthetic")
    assert validate_result(data, kind) == data


@pytest.mark.parametrize("date", ["2026-10-25T12:00:00+00:60", "2026-10-25T12:00:00+24:00",
    "2026-02-30", "2026-10-25T25:00:00Z", "2026-10-25T12:00:00", "not-a-date"])
def test_bad_dates(date, exports):
    data = fatigue(exports)
    data["run"]["date"] = date
    with pytest.raises(ValueError):
        validate_result(data, "fatigue")


@pytest.mark.parametrize("variant", ["both_sources", "both_methods", "touchdown_peak",
    "sample_peak", "array_length", "arc_order", "histogram_total", "both_ranges",
    "life", "zero_life", "duplicate_hotspot", "nonfinite", "boolean_damage", "unknown",
    "absolute_path", "traversal", "unsupported_version", "uncompleted_qualified"])
def test_rejections(variant, exports, tmp_path):
    kind = "strength" if variant in {"both_methods", "touchdown_peak", "sample_peak",
                                      "array_length", "arc_order"} else "fatigue"
    data = strength(exports) if kind == "strength" else fatigue(exports)
    if variant == "both_sources":
        data["sn_curve"]["source_label"] = "different"
    elif variant == "both_methods":
        data.update(utilisation_method="other", utilisation_allowable=1)
    elif variant == "touchdown_peak":
        data["touchdown"].update(arc_length_m=75, utilisation=1.2)
    elif variant == "sample_peak":
        data["max_utilisation"]["arc_length_m"] = 0
    elif variant == "array_length":
        data["effective_tension_kN"].pop()
    elif variant == "arc_order":
        data["arc_length_m"][1] = 0
    elif variant == "histogram_total":
        spot = data["hot_spots"][0]
        del spot["stress_range_MPa"]
        spot["range_histogram"] = [{"stress_range_MPa": 1, "cycles_per_year": 1}]
    elif variant == "both_ranges":
        data["hot_spots"][0]["range_histogram"] = [{"stress_range_MPa": 1, "cycles_per_year": 1}]
    elif variant == "life":
        data["life_years"] = 2
    elif variant == "zero_life":
        data.update(damage_per_year=0, life_years=1)
    elif variant == "duplicate_hotspot":
        data["hot_spots"][1] = data["hot_spots"][0]
    elif variant == "nonfinite":
        data["damage_per_year"] = float("inf")
    elif variant == "boolean_damage":
        data["damage_per_year"] = True
    elif variant == "unknown":
        data["typo"] = 1
    elif variant == "absolute_path":
        data["run"]["result_file"] = str(tmp_path / "result.json")
    elif variant == "traversal":
        data["run"]["result_file"] = "../result.json"
    elif variant == "unsupported_version":
        data["exchange_version"] = "ris-exchange/2"
    elif variant == "uncompleted_qualified":
        data["run"].update(id="synthetic", phase="A", status="failed", qualified=True)
    with pytest.raises(ValueError):
        validate_result(data, kind)


@pytest.mark.parametrize("raw", ['{"a":1,"a":2}', '{"a":NaN}', '{"a":1e999}', '{'])
def test_strict_json(raw, tmp_path):
    path = tmp_path / "bad.json"
    path.write_bytes(raw.encode())
    with pytest.raises(ValueError):
        load_json(path)


def test_malformed_fixture():
    malformed = load_json(FIXTURES / "malformed-v1.json")
    for item in malformed["records"]:
        with pytest.raises(ValueError, match=item["error"]):
            validate_result(item["data"], item["kind"])


def test_campaign_status():
    validate_campaign_run({"id": "synthetic", "phase": "A", "status": "completed", "qualified": True})
    with pytest.raises(ValueError):
        validate_campaign_run({"id": "synthetic", "phase": "A", "status": "failed", "qualified": True})


def test_no_licensed_imports():
    code = '''
import sys
class Guard:
    def find_spec(self, fullname, *args):
        if fullname.lower().startswith(("orcf", "orcaw", "ansys")):
            raise AssertionError("licensed interface import")
sys.meta_path.insert(0, Guard())
from digitalmodel.riser_study.exchange import load_case_matrix, load_token_registry
assert len(load_case_matrix()) == 12
assert len(load_token_registry()) == 58
'''
    subprocess.run([sys.executable, "-c", code], check=True, capture_output=True)


@pytest.mark.parametrize("dynamic,condition", [(True, "extreme"), (False, "functional"), (False, "extreme")])
def test_strength_screening_variants(dynamic, condition, exports):
    """#3517 test_riser_study.py:145-177 supplies these extension forms."""
    data = strength(exports)
    data["screening"] = screening(dynamic, condition)
    data["hang_off_tension_kN"]["dynamic_max"] = 140 if dynamic else 120
    assert validate_result(data, "strength") == data


def screening(dynamic=True, condition="extreme"):
    """ASSUMED synthetic values; no strength criterion is calculated here."""
    return {
        "condition": condition, "dynamic": dynamic, "qualified": False,
        "u1": {"method": "API RP 2RD", "smys_MPa": 450,
               "stress_MPa": {"static": {"inner": [120, 180, 150], "outer": [110, 210, 130]},
                              "dynamic_max": {"inner": [144, 216, 180], "outer": [180, 288, 200]}
                              if dynamic else None}},
        "tension_envelope_kN": {"dynamic_min": [110, 80, -2] if dynamic else None,
                                "dynamic_max": [140, 110, 90] if dynamic else None},
        "hang_off_dynamic_min_kN": 110 if dynamic else None,
        "excursion_reference_deg": 10, "excursion_limit_deg": 10,
        "static_check": {"analytical": {"top_tension_kN": 100, "top_angle_deg": 9,
                                        "touchdown_x_m": 99},
                         "software_touchdown_x_m": 100, "basis": "ASSUMED synthetic catenary"}
                        if condition == "functional" else None,
        "api_rp_1111": {"in_scope": True, "method": "ASSUMED synthetic check",
                        "reason": "ASSUMED synthetic history", "checks": [
                            {"arc_length_m": 50, "utilisation": 0.4, "check": "combined loading"}]},
    }


@pytest.mark.parametrize("defect", ["fibres", "tension", "hang_off", "reversed", "applicability",
                                     "check_arc", "condition", "missing_dynamic", "static_dynamic"])
def test_screening_rejection(defect, exports):
    data = strength(exports)
    screen = data["screening"] = screening()
    data["hang_off_tension_kN"]["dynamic_max"] = 140
    if defect == "fibres":
        screen["u1"]["stress_MPa"]["static"]["inner"].pop()
    elif defect == "tension":
        screen["tension_envelope_kN"]["dynamic_min"].pop()
    elif defect == "hang_off":
        screen["hang_off_dynamic_min_kN"] = 999
    elif defect == "reversed":
        screen["tension_envelope_kN"]["dynamic_min"][2] = 999
    elif defect == "applicability":
        screen["api_rp_1111"]["in_scope"] = False
    elif defect == "check_arc":
        screen["api_rp_1111"]["checks"][0]["arc_length_m"] = 1000
    elif defect == "condition":
        screen["condition"] = "functional"
    elif defect == "missing_dynamic":
        screen["u1"]["stress_MPa"]["dynamic_max"] = None
    else:
        screen["dynamic"] = False
    with pytest.raises(ValueError):
        validate_result(data, "strength")


def test_screening_out_of_scope_checks(exports):
    data = strength(exports)
    data["screening"] = screening(False, "functional")
    data["screening"]["api_rp_1111"].update(in_scope=False, checks=[])
    validate_result(data, "strength")


def test_g4_compatibility():
    """#3518 results_exchange.schema.json:515-563; structural evidence only."""
    record = {"comparison_cases": ["R08-W1-SCR", "R08-W2-SCR"],
              "mesh_step": {"wall_delta_pct": 1, "tolerance_pct": 2, "source_sha256": "a" * 64},
              "half_hour": {"wall_delta_pct": 1, "tolerance_pct": 2, "source_sha256": "b" * 64}}
    assert validate_result(record, "g4_checks") == record
    record["comparison_cases"].pop()
    with pytest.raises(ValueError):
        validate_result(record, "g4_checks")


@pytest.mark.parametrize("kind", ["strength", "fatigue"])
def test_case_id_trailing_newline(kind, exports):
    data = strength(exports) if kind == "strength" else fatigue(exports)
    data["case_id"] += "\n"
    with pytest.raises(ValueError):
        validate_result(data, kind)


def test_legacy_minimum_records(exports):
    """#3518 permits one arc sample; #3519 permits empty load metadata."""
    data = strength(exports)
    for field in ("arc_length_m", "effective_tension_kN", "bending_moment_kNm", "von_mises_utilisation"):
        data[field] = data[field][:1]
    data["max_utilisation"].update(value=0.4, arc_length_m=0)
    data["touchdown"].update(utilisation=0.4, arc_length_m=0)
    data["load_case"].update(wave={}, current={}, offset={})
    validate_result(data, "strength")


def test_format_checker_is_present(exports):
    import digitalmodel.riser_study.exchange.validation as validation
    assert "date" in validation._FORMATS.checkers
    assert "date-time" in validation._FORMATS.checkers


def test_pinned_checkout_attributes():
    names = ["src/digitalmodel/riser_study/exchange/data/case-matrix-v1.tsv",
             "src/digitalmodel/riser_study/exchange/data/token-registry-v1.json",
             "tests/subsea/riser_exchange/fixtures/exports-v1.json",
             "tests/subsea/riser_exchange/fixtures/malformed-v1.json"]
    result = subprocess.run(["git", "check-attr", "text", "eol", "--", *names],
                            text=True, capture_output=True, check=True)
    lines = result.stdout.splitlines()
    assert all(line.endswith("text: set") or line.endswith("eol: lf") for line in lines)
    assert len(lines) == 2 * len(names)


def test_schema_references_resolve():
    for kind in ("strength", "fatigue", "g4_checks", "campaign_run"):
        schema = exchange_schema(kind)
        def walk(node):
            if isinstance(node, dict):
                if "$ref" in node:
                    assert node["$ref"].startswith("#/$defs/")
                    assert node["$ref"].split("/")[-1] in schema["$defs"]
                for child in node.values():
                    walk(child)
            elif isinstance(node, list):
                for child in node:
                    walk(child)
        walk(schema)


def test_package_data_included():
    import tomllib
    root = Path(__file__).resolve().parents[3]
    config = tomllib.loads((root / "pyproject.toml").read_text())
    patterns = config["tool"]["setuptools"]["package-data"]["digitalmodel"]
    for pattern in ("riser_study/exchange/*.schema.json", "riser_study/exchange/data/*.json",
                    "riser_study/exchange/data/*.tsv"):
        assert pattern in patterns


@pytest.mark.parametrize("state", ["synthetic-1", "S1", "S2", "S3"])
def test_generic_peer_sea_ids(state, exports):
    """Legacy single-record examples remain readable; campaigns freeze F03-F05."""
    data = fatigue(exports)
    data["sea_state"]["id"] = state
    validate_result(data, "fatigue")


@pytest.mark.parametrize("payload", ['{"value":1}'.encode("utf-16"), b'\xff', b'\xef\xbb\xbf{}'])
def test_json_encoding_rejection(payload, tmp_path):
    path = tmp_path / "encoding.json"
    path.write_bytes(payload)
    with pytest.raises(ValueError):
        load_json(path)


@pytest.mark.parametrize("index", range(5))
def test_unmodified_peer_examples(index):
    """Read exact example records from the inspected PR test sources."""
    example = load_json(FIXTURES / "peer-examples-v1.json")["examples"][index]
    validate_result(example["data"], example["kind"])
