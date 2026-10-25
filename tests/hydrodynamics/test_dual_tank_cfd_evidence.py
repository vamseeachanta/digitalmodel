# ABOUTME: Public archive projection and evidence-to-page regression checks.
"""Exercise source isolation, numerical bindings and publication boundaries."""

import importlib.util
import json
import re
from pathlib import Path
from xml.etree import ElementTree

import pytest

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "docs/api/cfd/data/dual_tank_cfd_evidence.json"


def module(name):
    path = ROOT / f"scripts/capabilities/{name}.py"
    spec = importlib.util.spec_from_file_location(name, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def records(node):
    if isinstance(node, dict) and "value" in node:
        yield node
    elif isinstance(node, dict):
        for value in node.values():
            yield from records(value)
    elif isinstance(node, list):
        for value in node:
            yield from records(value)
    else:
        pytest.fail("Unprovenanced evidence leaf")


def test_every_fact_has_relative_provenance():
    data = json.loads(DATA.read_text())
    for fact in records(data):
        source = fact["provenance"]
        path = Path(source["path"])
        assert path.parts[0] == "archived-cfd-campaign-2026-07"
        assert not path.is_absolute() and ".." not in path.parts
        assert "\\" not in str(path)
        assert source["pointer"]
        assert re.fullmatch(r"[a-f0-9]{64}", source["sha256"])
        assert isinstance(fact["unit"], str)


def test_extractor_reads_selected_fields_and_rejects_disclosures(tmp_path):
    extractor = module("extract_dual_tank_cfd_evidence")
    source = tmp_path / "analytical/example.json"
    source.parent.mkdir()
    source.write_text(json.dumps({"amplitude": 1.25, "private": "not for export"}))
    archive = extractor.Archive(tmp_path)
    fact = archive.fact("analytical/example.json", "/amplitude", "m³")
    assert fact["value"] == 1.25
    assert "private" not in json.dumps(fact)
    assert fact["provenance"]["path"] == "archived-cfd-campaign-2026-07/analytical/example.json"
    for bad in ("../escape.json", str(tmp_path / "escape.json")):
        with pytest.raises(ValueError):
            archive.fact(bad, "/amplitude", "m³")
    for bad in ("/" + "home/secret", "/" + "mnt/secret", "B" + "15xxx",
                "test" + "@" + "example.invalid", "ws" + "-ab",
                "ace" + "-linux-1"):
        with pytest.raises(ValueError):
            extractor.validate_public({"value": bad})


def test_builder_evidence_reads_only_json(monkeypatch, tmp_path):
    builder = module("build_dual_tank_sloshing_study")
    data = json.loads(DATA.read_text())
    permitted = tmp_path / "evidence.json"
    permitted.write_text(json.dumps(data))
    original = Path.read_text

    def guarded(path, *args, **kwargs):
        assert path == permitted, "Evidence rendering accessed a source other than JSON"
        return original(path, *args, **kwargs)

    monkeypatch.setattr(builder, "EVIDENCE_PATH", permitted)
    monkeypatch.setattr(Path, "read_text", guarded)
    first = builder.render_cfd_evidence()
    data["area_sweep"][0]["cfd_amplitude"]["value"] = 8.25
    permitted.write_text(json.dumps(data))
    second = builder.render_cfd_evidence()
    assert first != second
    assert 'value="8.25"' in second
    assert re.findall(r'<g[^>]+data-y=[^>]+>', first) != re.findall(r'<g[^>]+data-y=[^>]+>', second)
    assert '<polyline' not in first and '<polyline' not in second


def test_new_sections_numbers_come_from_json():
    builder = module("build_dual_tank_sloshing_study")
    evidence = builder.render_cfd_evidence()
    data = json.loads(DATA.read_text())
    allowed = set()
    for fact in records(data):
        value = fact["value"]
        if isinstance(value, (float, int)):
            allowed.add(str(value))
            allowed.update(f"{value:.{precision}f}" for precision in range(7))
        elif isinstance(value, str):
            allowed.update(re.findall(r"(?<![\w])\d+(?:\.\d+)?", value))
            # Percentage prose may display evidence rounded to 0.1 %.
            for percentage in re.findall(r"(\d+(?:\.\d+)?(?:[-–]\d+(?:\.\d+)?)?)\s*%", value):
                allowed.update(f"{float(n):.1f}" for n in re.split("[-–]", percentage))
    # Round axis ticks are presentation values; evidence prose remains source-bound.
    prose = re.sub(r'<figure\b.*?</figure>', '', evidence, flags=re.S)
    prose = re.sub(r'<h3[^>]*>.*?</h3>', '', prose, flags=re.S)
    prose = prose.replace('Appendix A', '')
    visible = re.sub(r"<[^>]+>", " ", prose)
    for number in re.findall(r"(?<![\w])\d+(?:\.\d+)?", visible):
        assert number in allowed, number
    assert "exception" in evidence.lower()
    assert "not CFD solves" in evidence
    assert "phase and anti-roll benefit remain withheld" in evidence
    assert "not shown" in evidence


def test_comparison_percentages_keep_raw_values_and_one_decimal():
    builder = module("build_dual_tank_sloshing_study")
    data = json.loads(DATA.read_text())
    root = ElementTree.fromstring('<div>' + builder.render_cfd_evidence() + builder.evidence_classes() + '</div>')
    cells = {node.attrib["data-provenance"]: node for node in root.iter("data")}
    loss, mesh, frequency = data["loss"], data["mesh"], data["frequency"]
    comparisons = [loss[key] for key in ("rms", "linear_rms", "small_area_rms", "large_area_rms")]
    comparisons += [loss["third"][key] for key in ("independent_rms", "rms")]
    comparisons += [mesh[key] for key in ("exchange_error", "moment_error")]
    comparisons += [frequency[key] for key in ("minimum_error", "maximum_error", "gci")]
    for fact in comparisons:
        source = fact["provenance"]
        node = cells[source["path"] + "#" + source["pointer"]]
        assert node.attrib["value"] == str(fact["value"])
        assert node.text == f"{fact['value']:.1f}"
    for row in data["evidence_classes"]:
        for key in ("key_finding", "limitation"):
            fact = row[key]
            source = fact["provenance"]
            node = cells[source["path"] + "#" + source["pointer"]]
            assert node.attrib["value"] == fact["value"]
            assert not re.search(r"\d\.\d{2,}\s*%", node.text)
    frequency_text = " ".join(root.itertext())
    assert "0.755580-0.755807 Hz vs 0.758054 Hz analytical; 0.3-0.3 % error" in frequency_text
    assert "0.8 % exchange and 1.5 % roll moment" in frequency_text


@pytest.mark.parametrize(("source", "expected"), [
    ("error -0.2964–0.3264%; GCI 0.1239%", "error -0.3–0.3 %; GCI 0.1 %"),
    ("error +1.4598 %; 7/7 cases", "error 1.5 %; 7/7 cases"),
    ("0.755580-0.755807 Hz; 0.682764 s", "0.755580-0.755807 Hz; 0.682764 s"),
])
def test_status_percentage_rounding_preserves_other_evidence(source, expected):
    builder = module("build_dual_tank_sloshing_study")
    fact = {"value": source, "provenance": {"path": "evidence.json", "pointer": "/finding"}}
    node = ElementTree.fromstring(builder.evidence_value(fact))
    assert node.attrib["value"] == source
    assert node.attrib["data-provenance"] == "evidence.json#/finding"
    assert node.text == expected


def test_evidence_json_is_registered_and_public():
    extractor = module("extract_dual_tank_cfd_evidence")
    for path in (DATA, ROOT / "docs/api/cfd/dual-tank-sloshing-cfd-study.html",
                 ROOT / "scripts/capabilities/extract_dual_tank_cfd_evidence.py"):
        extractor.validate_public(path.read_text())
    from scripts import check_generated_html as checker
    entry = next(e for e in checker.GENERATORS if e.script.endswith("build_dual_tank_sloshing_study.py"))
    assert DATA.relative_to(ROOT).as_posix() in entry.inputs


def test_projection_is_allowlisted_reproducible_and_read_only(tmp_path):
    """Exercise the complete reducer against independent invented reductions."""
    extractor = module("extract_dual_tank_cfd_evidence")
    source = tmp_path / "archive"
    def write(name, content):
        path = source / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(content) if isinstance(content, dict) else content)
    row = {"A_c_m2": 4, "exchange_period_s": 8, "cfd_exchange_amp_m3": 2,
           "analytical_exchange_amp_m3": 1, "cfd_ratio_to_first": 1,
           "analytical_ratio_to_first": 1, "restricted": "do not export"}
    write("analytical/ac_validation.json", {"rows": [row]})
    write("analytical/ac_scan_results.json", {"forcing_period_s": 7,
          "cases": [{"roll_amplitude_deg": 3}]})
    case = {"period_s": 7, "roll_amplitude_deg": 3, "zeta_measured": 0.2,
            "zeta_predicted": 0.3}
    group = {"group": "fill_30pct", "conduit_area_m2": 4, "predicted_Tn_s": 8,
             "measured_Tn_s": 9, "Tn_error_pct": -11.1, "cases": [case]}
    for name in ("prediction_checks", "third_area_prediction_check"):
        write(f"review_output/damping/{name}.json", {"model": {"both_fitted_at": "reference, 40 % fill"}, "groups": [group]})
    write("review_output/damping/exchange_damping_v2.json", {"natural_period_s": 9})
    write("analytical/utube_damped.json", {"assumptions": {"C6_Tn_meas": {"value": 9}, "C7_Ac_ref": {"value": 4}}})
    write("analytical/design_sweep.json", {"chain": ["inertia +/-4 %"]})
    write("analytical/loss_scaling.json", {"fitted": {"exponent_n": 2, "rms_error_pct": 7},
          "cases_used": 10, "variants": {"v0.3_area_exponent_1": {"rms_error_pct": 30,
          "rms_by_group_pct": {"conduit_area_3.4": 40, "conduit_area_13.5": 80}}}})
    write("review_output/damping/loss_scaling_third_area.json", {
          "fitted": {"exponent_n": 2, "rms_error_pct": 7}, "cases_used": 11})
    write("review_output/damping/third_area_report.md", "for **7.21% RMS**")
    write("review_output/period_refinement.csv", "status,cycle_converged,mesh,period_s,exchange_amplitude_m3,roll_moment_amplitude_Nm\ncompleted,True,medium,7,2,3\n")
    write("review_output/mesh_confirmation.json", {"period_s": 7,
          "exchange_change_pct": 0.2, "roll_moment_change_pct": 0.3})
    write("review_output/fine_T24.json", {"max_courant": 0.4,
          "exchange_amplitude_m3": 2, "roll_moment_amplitude_Nm": 3})
    write("review_output/README.md", "Courant 0.4, slightly above the nominal 0.3 target; an error of 0.1-0.2%; Richardson order (0.5); reported 0.6% fine-grid")
    write("review_output/algorithm_status.csv", "evidence_class,result_family,status,key_finding,limitation\nAnalytical,screening_manifest,methodology_only,11/11 cases,Not physical validation\n")
    before = {p.relative_to(source): p.read_bytes() for p in source.rglob("*") if p.is_file()}
    first = extractor.extract(source)
    assert extractor.extract(source) == first
    assert first["area_sweep"][0]["cfd_amplitude"]["value"] == 2
    assert "restricted" not in json.dumps(first)
    assert {p.relative_to(source): p.read_bytes() for p in source.rglob("*") if p.is_file()} == before
    write("review_output/period_refinement.csv", "status,cycle_converged,mesh\nfailed,False,medium\n")
    with pytest.raises(ValueError, match="unqualified"):
        extractor.extract(source)


def test_calibration_fill_is_read_and_ambiguity_rejected(tmp_path):
    extractor = module("extract_dual_tank_cfd_evidence")
    source = tmp_path / "checks.json"
    content = {"model": {"both_fitted_at": "area reference, 40 % fill"}, "groups": [
        {"group": "conduit_area_4", "conduit_area_m2": 4, "predicted_Tn_s": 8,
         "measured_Tn_s": 9, "Tn_error_pct": -11.1, "cases": []}]}
    source.write_text(json.dumps(content))
    row = extractor.prediction_groups(extractor.Archive(tmp_path), source.name)[0]
    assert row["fill"]["value"] == 40
    for bad in ("no fill provided", "40 % fill and 60 % fill"):
        content["model"]["both_fitted_at"] = bad
        source.write_text(json.dumps(content))
        with pytest.raises(ValueError, match="calibration fill"):
            extractor.prediction_groups(extractor.Archive(tmp_path), source.name)


def test_period_join_and_fill_selection_follow_evidence(monkeypatch, tmp_path):
    builder = module("build_dual_tank_sloshing_study")
    data = json.loads(DATA.read_text())
    original = builder.area_period_pairs(data)
    data["area_sweep"].reverse()
    reordered = builder.area_period_pairs(data)
    assert [(r[0]["area"]["value"], r[1]["value"], r[2]["value"]) for r in reordered] == [
        (r[0]["area"]["value"], r[1]["value"], r[2]["value"]) for r in reversed(original)]
    old_fill = data["calibration"]["fill"]["value"]
    data["calibration"]["fill"]["value"] = 40
    for row in data["period_checks"]:
        if row["fill"]["value"] == old_fill:
            row["fill"]["value"] = 40
    path = tmp_path / "evidence.json"
    path.write_text(json.dumps(data))
    monkeypatch.setattr(builder, "EVIDENCE_PATH", path)
    text = builder.render_cfd_evidence()
    loss = text.split('id="cfd-loss"', 1)[1].split('id="cfd-forced"', 1)[0]
    assert loss.count('<data value="40"') == 3
    errors = text.split('id="cfd-inertia"', 1)[1].split('id="cfd-fill"', 1)[0]
    for row in data["period_checks"]:
        assert f'value="{row["error"]["value"]}"' in errors
