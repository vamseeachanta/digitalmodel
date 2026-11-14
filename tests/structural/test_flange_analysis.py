"""Published Appendix 2 checks; no licensed solver is used."""

from copy import deepcopy

import pytest


@pytest.fixture
def case():
    # PVEng, Loads on Flanges - The ASME Way (2008), pp. 12-14.
    return {
        "flange_type": "integral",
        "units": "US_customary",
        "geometry": {"A": 22.0, "B": 16.0, "C": 20.25, "t": 1.75,
                     "g0": 0.75, "g1": 0.75, "h": 0.375},
        "bolts": {"count": 16, "diameter": 1.0, "root_area": 0.566,
                  "Sa": 20000.0, "Sb": 20000.0,
                  "root_area_source": "PVEng, p. 13, 1-8 UNC 2A"},
        "gasket": {"m": 3.0, "y": 10000.0, "b0": 0.375,
                   "outer_diameter": 17.75, "inner_diameter": 16.25},
        "design": {"P": 100.0, "temperature_F": None,
                   "Sf_operating": 20000.0, "Sf_seating": 20000.0,
                   "Sn_operating": 20000.0, "Sn_seating": 20000.0,
                   "hub_stress_multiplier": 1.5,
                   "E_operating": 27900000.0, "E_seating": 27900000.0,
                   "apply_bolt_spacing_correction": False},
    }


def test_published_bolt_loads_and_areas(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    loads = analyze(case)["bolt_loads"]
    # Independent published values, including the source's rounded pi factors.
    assert loads["Wm1"] == pytest.approx(32941, rel=0.001)
    assert loads["Wm2"] == pytest.approx(164849, rel=0.001)
    assert loads["Am"] == pytest.approx(8.242, rel=0.001)
    assert loads["Ab"] == pytest.approx(9.056, rel=1e-12)


def test_published_stresses_and_rigidity(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    result = analyze(case)
    for name, value in {"T": 1.765, "U": 6.877, "Y": 6.258, "Z": 3.246}.items():
        assert result["factors"][name] == pytest.approx(value, abs=0.0006)
    assert result["moments"]["operating"] == pytest.approx(55998, rel=0.001)
    assert result["moments"]["seating"] == pytest.approx(269196, rel=0.001)
    expected = {
        "operating": {"S_H": 5945, "S_R": 1759, "S_T": 1444, "J": 0.094},
        "seating": {"S_H": 28577, "S_R": 8454, "S_T": 6943, "J": 0.452},
    }
    for condition, values in expected.items():
        for name, value in values.items():
            assert result["conditions"][condition][name] == pytest.approx(value, rel=0.001)
    assert result["screening_status"] == "pass"
    assert result["governing_check"] == "seating.S_H"
    assert result["governing_utilization"] == pytest.approx(28577 / 30000, rel=0.001)
    assert len(result["checks"]) == 13  # area + five stresses + rigidity at each condition
    assert all(row["clause"] and row["passes"] for row in result["checks"])
    assert {row["units"] for row in result["checks"]} == {"in^2", "psi", "dimensionless"}


@pytest.mark.parametrize("condition,neck_allowable,multiplier,limit", [
    ("seating", 17100.0, 1.5, 25650.0),
    ("seating", 10000.0, 2.5, 25000.0),
    ("operating", 3000.0, 1.5, 4500.0),
    ("operating", 2000.0, 2.5, 5000.0),
])
def test_neck_material_governs_hub_stress(case, condition, neck_allowable, multiplier, limit):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["design"][f"Sn_{condition}"] = neck_allowable
    case["design"]["hub_stress_multiplier"] = multiplier
    result = analyze(case)
    check = next(row for row in result["checks"] if row["name"] == f"{condition}.S_H")
    assert check["allowable"] == limit
    assert check["utilization"] > 1
    assert not check["passes"]
    assert result["screening_status"] == "fail"
    assert result["governing_check"] == f"{condition}.S_H"


@pytest.mark.parametrize("multiplier", [1.5, 2.5])
def test_flange_material_can_still_govern_hub_stress(case, multiplier):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["design"].update(Sn_operating=40000.0, Sn_seating=40000.0,
                          hub_stress_multiplier=multiplier)
    result = analyze(case)
    assert all(row["allowable"] == 30000.0 for row in result["checks"]
               if row["name"].endswith(".S_H"))
    assert result["screening_status"] == "pass"


@pytest.mark.parametrize("key", ["Sn_operating", "Sn_seating", "hub_stress_multiplier"])
def test_neck_material_basis_is_required(case, key):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    del case["design"][key]
    with pytest.raises(ValueError):
        analyze(case)


def test_published_diameter_and_tapered_hub_factors():
    # PVEng Heat Exchanger Sample PVE-4293 (2012), pp. 12-14; net geometry.
    from digitalmodel.structural.flange_analysis.factors import flange_factors, hub_factors

    factors = flange_factors(23.5 / 18.25)
    for name, value in {"T": 1.803, "U": 8.595, "Y": 7.822, "Z": 4.039}.items():
        assert factors[name] == pytest.approx(value, abs=0.0006)
    hub = hub_factors(g0=0.125, g1=0.5, h=1.125, h0=(18.25 * 0.125)**0.5)
    assert hub["F"] == pytest.approx(0.736, abs=0.0006)
    assert hub["V"] == pytest.approx(0.081, abs=0.0006)
    assert hub["f"] == pytest.approx(2.9303, abs=0.0001)


def test_insufficient_bolt_area_fails_even_with_small_stresses(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["bolts"]["root_area"] = 0.01
    result = analyze(case)
    checks = {c["name"]: c for c in result["checks"]}
    assert not checks["bolt_area"]["passes"]
    assert all(row["passes"] for name, row in checks.items() if name != "bolt_area")
    assert result["screening_status"] == "fail"
    assert result["governing_check"] == "bolt_area"


@pytest.mark.parametrize("group,key,value", [
    ("geometry", "A", 15), ("geometry", "B", 0),
    ("geometry", "C", 16), ("geometry", "t", -1),
    ("geometry", "h", float("nan")), ("geometry", "g1", 0.5),
    ("bolts", "count", 0), ("bolts", "count", 2.5),
    ("bolts", "count", True), ("bolts", "root_area", 0),
    ("bolts", "Sa", float("inf")), ("bolts", "Sb", -1),
    ("gasket", "outer_diameter", 21), ("gasket", "inner_diameter", 15),
    ("gasket", "b0", -0.5), ("gasket", "m", -1),
    ("design", "P", -1), ("design", "Sf_operating", 0),
    ("design", "E_seating", float("nan")),
    ("design", "apply_bolt_spacing_correction", "false"),
    ("design", "Sn_operating", 0), ("design", "Sn_seating", -1),
    ("design", "Sn_operating", True), ("design", "Sn_seating", float("nan")),
    ("design", "hub_stress_multiplier", 2.0),
    ("design", "hub_stress_multiplier", float("inf")),
    ("design", "hub_stress_multiplier", True),
])
def test_invalid_inputs_fail_closed(case, group, key, value):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case[group][key] = value
    with pytest.raises(ValueError):
        analyze(case)


@pytest.mark.parametrize("flange_type", ["loose", "lap_joint", "optional"])
def test_unsupported_types_are_rejected(case, flange_type):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["flange_type"] = flange_type
    with pytest.raises(ValueError, match="integral"):
        analyze(case)


def test_unit_system_is_explicit(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["units"] = "SI"
    with pytest.raises(ValueError, match="US_customary"):
        analyze(case)


def test_optional_spacing_correction_does_not_increase_rigidity_moment(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    base = analyze(case)
    case["design"]["apply_bolt_spacing_correction"] = True
    corrected = analyze(case)
    assert corrected["factors"]["Bsc"] > 1
    for condition in ("operating", "seating"):
        assert corrected["conditions"][condition]["S_H"] > base["conditions"][condition]["S_H"]
        assert corrected["conditions"][condition]["J"] == base["conditions"][condition]["J"]


def test_missing_or_mismatched_citation_stops_calculation(case, monkeypatch, tmp_path):
    from digitalmodel.citations import CitationResolutionError
    from digitalmodel.structural.flange_analysis.calculations import WIKI_PATH, analyze

    (tmp_path / "wikis").mkdir()
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    with pytest.raises(CitationResolutionError, match="page_missing"):
        analyze(case)
    page = tmp_path / WIKI_PATH
    page.parent.mkdir(parents=True)
    page.write_text("---\ncode_id: asme-viii-1-app2\npublisher: ASME\nrevision: 2023\n---\n")
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch:revision"):
        analyze(case)


@pytest.mark.parametrize("neck_allowable,status", [(20000.0, "pass"), (17100.0, "fail")])
def test_workflow_emits_consistent_csv_json_and_deterministic_html(
    case, tmp_path, monkeypatch, neck_allowable, status,
):
    import csv
    import json
    import sys
    from html.parser import HTMLParser
    from pathlib import Path
    from digitalmodel.structural.flange_analysis.workflow import router

    class ResourceParser(HTMLParser):
        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if tag in {"script", "link", "iframe"}:
                assert not attrs.get("src") and not attrs.get("href")

    case["design"]["Sn_seating"] = neck_allowable
    case["source"] = {"title": "Published vendor example", "pages": "12-14"}
    cfg = {"basename": "flange_analysis", "_config_dir_path": str(tmp_path),
           "_config_file_path": str(tmp_path / "input.yml"),
           "flange_analysis": deepcopy(case)}
    result = router(cfg)["flange_analysis"]
    out = tmp_path / "results/flange_analysis"
    original_read_text = Path.read_text

    def locale_read_text(path, *args, **kwargs):
        # Model a legacy Windows locale when reading the UTF-8 workflow outputs.
        if path.parent == out and not args and "encoding" not in kwargs:
            kwargs["encoding"] = "cp1252"
        return original_read_text(path, *args, **kwargs)

    monkeypatch.setattr(Path, "read_text", locale_read_text)
    summary = json.loads((out / "flange_analysis_summary.json").read_text(encoding="utf-8"))
    with (out / "flange_analysis_checks.csv").open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    report = (out / "flange_analysis_report.html").read_text(encoding="utf-8")
    assert len(rows) == len(summary["checks"]) == 13
    assert summary == result
    assert cfg["screening_status"] == status
    assert {row["name"] for row in rows} == {row["name"] for row in summary["checks"]}
    for row, check in zip(rows, summary["checks"]):
        assert float(row["utilization"]) == check["utilization"]
        assert row["status"] == ("pass" if check["passes"] else "fail")
        assert row["clause"] == check["clause"]
    for text in ("Input echo", "Bolt loads", "Stress checks", "Data provenance",
                 "Assumptions", "Citations", "Plotly.newPlot", "App 2-8", "root_area_source"):
        assert text in report
    assert str(tmp_path) not in report and str(tmp_path) not in json.dumps(summary)
    assert f"<strong>{status.upper()}</strong>" in report
    hub_check = next(row for row in summary["checks"] if row["name"] == "seating.S_H")
    assert hub_check["allowable"] == min(30000.0, 1.5 * neck_allowable)
    assert "Hub allowable is the smaller flange/neck bound" in report
    assert "neck allowables" in report
    assert "hub_stress_multiplier" in report
    ResourceParser().feed(report)
    assert "OrcFxAPI" not in sys.modules
    router({**cfg, "flange_analysis": deepcopy(case)})
    assert (out / "flange_analysis_report.html").read_text(encoding="utf-8") == report


def test_report_requires_provenance(case):
    from digitalmodel.reporting import ProvenanceError
    from digitalmodel.structural.flange_analysis.calculations import analyze
    from digitalmodel.structural.flange_analysis.render import render_report

    with pytest.raises(ProvenanceError):
        render_report(analyze(case), inputs=case, data_source=None)


def test_gasket_width_transition_and_zero_pressure(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["gasket"]["b0"] = 0.25
    case["design"]["P"] = 0.0
    result = analyze(case)
    assert result["gasket"] == {"b": 0.25, "G": 17.0}
    assert result["bolt_loads"]["Wm1"] == 0
    assert result["moments"]["operating"] == 0
    assert result["conditions"]["operating"]["S_H"] == 0
    assert result["conditions"]["seating"]["S_H"] > 0
    case["gasket"]["b0"] = 0.250001
    result = analyze(case)
    assert result["gasket"]["b"] == pytest.approx(0.2500005, abs=1e-10)
    assert result["gasket"]["G"] == pytest.approx(17.249999, abs=1e-9)


def test_input_is_preserved_and_stress_failure_propagates(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["design"]["Sf_seating"] = 1000.0
    before = deepcopy(case)
    result = analyze(case)
    assert case == before
    assert result["screening_status"] == "fail"
    assert any(not c["passes"] for c in result["checks"] if c["name"].startswith("seating."))


def test_each_check_has_a_validated_clause(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    result = analyze(case)
    clauses = {citation["section"] for citation in result["citations"]}
    assert all(check["clause"] in clauses for check in result["checks"])


def test_root_area_cannot_exceed_nominal_bolt_cross_section(case):
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["bolts"]["root_area"] = 1.0  # impossible for a 1-inch bolt
    with pytest.raises(ValueError, match="root_area"):
        analyze(case)


def test_tapered_hub_rating_against_published_stress_response(case):
    """Carry the published tapered factors through both loading conditions.

    The vendor case includes a partition gasket, which is outside this workflow.
    Published stresses (printed p. 14) are scaled by the moment ratio; the net
    geometry is identical. This checks stress response, not a full reproduction
    of the vendor's partition-gasket rating.
    """
    from digitalmodel.structural.flange_analysis.calculations import analyze

    case["geometry"].update(A=23.5, B=18.25, C=21.75, g0=0.125, g1=0.5, h=1.125)
    case["bolts"].update(diameter=0.625, root_area=0.202, Sa=25000.0, Sb=25000.0,
                          root_area_source="Synthetic area for the tapered stress-response test")
    case["gasket"].update(outer_diameter=19.125, inner_diameter=18.25,
                           b0=0.21875, m=2.5, y=2900.0)
    case["design"].update(P=150.0, Sf_operating=17800.0,
                          Sn_operating=17800.0, apply_bolt_spacing_correction=True)
    result = analyze(case)
    assert result["factors"]["f"] == pytest.approx(2.9303, abs=0.0001)
    expected = {
        "operating": (97547.0, {"S_H": 19821.01, "S_R": 1180.33, "S_T": 8883.92}),
        "seating": (125214.0, {"S_H": 25442.91, "S_R": 1515.11, "S_T": 11403.70}),
    }
    # Published beta = 2.1376 uses a less-rounded radial coefficient. Normalize
    # that response to the method's 1.33 using the independently printed e.
    radial_ratio = (1 + 1.33 * 1.75 * 0.4875) / 2.1376
    for condition, (published_moment, stresses) in expected.items():
        published_radial = stresses["S_R"]
        stresses = {**stresses, "S_R": published_radial * radial_ratio,
                    "S_T": stresses["S_T"] + 4.0391 * published_radial * (1 - radial_ratio)}
        ratio = result["moments"][condition] * result["factors"]["Bsc"] / published_moment
        for name, stress in stresses.items():
            assert result["conditions"][condition][name] == pytest.approx(stress * ratio, rel=0.001)
    assert result["screening_status"] == "pass"
    assert result["governing_check"] == "seating.average_HT"
