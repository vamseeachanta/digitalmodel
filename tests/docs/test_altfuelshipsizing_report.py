# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the sizing report and check displayed evidence against the public API."""

import ast
import importlib.util
import os
import json
import re
import subprocess
from dataclasses import asdict
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

from digitalmodel.alt_fuel_ship_sizing import (
    FuelProperties,
    PortLeg,
    SeaLeg,
    TankParameters,
    size_fuel_chain,
    size_fuel_chain_voyage,
)
from digitalmodel.alt_fuel_ship_sizing.workflow import router

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/altfuelshipsizing"
EXAMPLE = ROOT / "examples/workflows/alt-fuel-report-pack/sizing_input.yml"


def load_builder():
    """Load the report generator without changing the import path."""
    spec = importlib.util.spec_from_file_location("sizing_report", REPORT / "generate.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def flatten(value, prefix=""):
    """Map each numeric leaf to its unambiguous source key."""
    if isinstance(value, dict):
        return {k: v for name, item in value.items()
                for k, v in flatten(item, f"{prefix}.{name}".strip(".")).items()}
    if isinstance(value, list):
        return {k: v for index, item in enumerate(value)
                for k, v in flatten(item, f"{prefix}.{index}").items()}
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return {prefix: value}
    return {}


class EvidenceParser(HTMLParser):
    """Read the actual numeric text and the self-contained document resources."""

    def __init__(self):
        super().__init__()
        self.numbers = []
        self.tags = []
        self.current = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        self.tags.append((tag, attrs))
        if "data-number" in attrs:
            self.current = [attrs["data-number"], ""]

    def handle_data(self, data):
        if self.current is not None:
            self.current[1] += data

    def handle_endtag(self, tag):
        if tag == "span" and self.current is not None:
            self.numbers.append(tuple(self.current))
            self.current = None


@pytest.fixture
def module_results():
    """Call the same public API to check report/export consistency only."""
    settings = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))["alt_fuel_ship_sizing"]
    fuel = FuelProperties(lhv_mj_per_kg=settings["fuel"]["lhv_mj_per_kg"])
    kwargs = dict(
        shaft_power_kw=settings["powering"]["shaft_power_kw"],
        hotel_load_kw=settings["powering"]["hotel_load_kw"],
        eta_electric_drivetrain=settings["efficiency"]["electric_drivetrain"],
        eta_fuel_cell_lhv=settings["efficiency"]["fuel_cell_lhv"],
        tank=TankParameters(**settings["tank"]),
        fuel=fuel,
        fuel_margin_fraction=settings["fuel"]["margin_fraction"],
    )
    legs = [SeaLeg(**{k: v for k, v in leg.items() if k != "type"})
            if leg["type"] == "sea" else PortLeg(dwell_h=leg["dwell_h"])
            for leg in settings["route"]["legs"]]
    profile = size_fuel_chain_voyage(legs=legs, **kwargs)
    single = size_fuel_chain(
        distance_nm=sum(leg.distance_nm for leg in legs if isinstance(leg, SeaLeg)),
        speed_kn=settings["route"]["service_speed_kn"], **kwargs,
    )
    return settings, fuel, single, profile


REBUILD = (
    "PYTHONPATH=src python docs/reports/modules/altfuelshipsizing/generate.py "
    "--output-dir docs/reports/modules/altfuelshipsizing"
)


def test_rebuild_preserves_evidence_and_lf(tmp_path):
    load_builder().build(tmp_path)
    for name in ("index.html", "changes.json"):
        assert b"\r" not in (tmp_path / name).read_bytes(), REBUILD
    rebuilt = (tmp_path / "index.html").read_text(encoding="utf-8")
    retained = (REPORT / "index.html").read_text(encoding="utf-8")
    for html in (rebuilt, retained):
        assert html.endswith("\n"), REBUILD
    parsers = [EvidenceParser(), EvidenceParser()]
    for parser, html in zip(parsers, (rebuilt, retained)):
        parser.feed(html)
    assert [k for k, _ in parsers[0].numbers] == [k for k, _ in parsers[1].numbers], REBUILD
    for (_, actual), (_, expected) in zip(parsers[0].numbers, parsers[1].numbers):
        assert numeric(actual) == pytest.approx(numeric(expected)), REBUILD
    pattern = r'(<script id="chart-data" type="application/json">)(.*?)(</script>)'
    actual, expected = [json.loads(re.search(pattern, html, re.S).group(2))
                        for html in (rebuilt, retained)]
    assert actual["limit"] == pytest.approx(expected["limit"]), REBUILD
    assert actual["boundary"] == pytest.approx(expected["boundary"]), REBUILD
    assert len(actual["points"]) == len(expected["points"]), REBUILD
    for point, reference in zip(actual["points"], expected["points"]):
        assert point == pytest.approx(reference), REBUILD
    def structure(html):
        html = re.sub(pattern, r"\1CHART\3", html, flags=re.S)
        return re.sub(r'(<span data-number="[^"]+">)[^<]*(</span>)', r"\1NUMBER\2", html)
    assert structure(rebuilt) == structure(retained), REBUILD
    assert json.loads((tmp_path / "changes.json").read_text(encoding="utf-8")) == json.loads(
        (REPORT / "changes.json").read_text(encoding="utf-8")), REBUILD


def numeric(text):
    """Read engineering display notation without depending on the formatter."""
    return float(text.replace(",", "").replace("−", "-"))


def test_every_displayed_example_number_matches_module(module_results):
    settings, fuel, single, profile = module_results
    expected = flatten({"input": settings, "fuel": asdict(fuel),
                        "single": asdict(single), "profile": asdict(profile),
                        "reference": {
                            "density_kg_per_m3": 70.78,
                            "heat_of_vaporization_kj_per_kg": 445.5,
                            "storage_temperature_k": 20.268,
                            "density_change_percent": 0.0988980,
                            "latent_heat_change_percent": 0.1122334,
                            "gross_tank_volume_m3": 1905.847896,
                        },
                        "chart": {"limit": 7920.0, "boundary": {"x": 38.991374}},
                        "comparison": {
                            "inventory_change_percent": 100 * (profile.required_fuel_mass_kg - single.required_fuel_mass_kg) / single.required_fuel_mass_kg,
                            "volume_change_percent": 100 * (profile.gross_tank_volume_m3 - single.gross_tank_volume_m3) / single.gross_tank_volume_m3,
                        }})
    parser = EvidenceParser()
    parser.feed(load_builder().render())
    assert parser.numbers
    for key, text in parser.numbers:
        assert key in expected, key
        tolerance = 0.0 if key.startswith(("input.", "fuel.")) else 0.050001
        assert numeric(text) == pytest.approx(expected[key], rel=0, abs=tolerance), key
    # These are the complete numerical result records, not selected headline values.
    keys = {key for key, _ in parser.numbers}
    assert set(flatten({"single": asdict(single), "profile": asdict(profile)})) <= keys
    assert set(flatten({"fuel": asdict(fuel)})) <= keys
    assert set(flatten({"input": settings})) <= keys


def test_worked_example_contains_no_unbound_numbers():
    html = load_builder().render()
    worked = html.split('id="worked-example"', 1)[1].split("</section>", 1)[0]
    # Every digit in the worked-example prose/table belongs to a source-bound span.
    text = re.sub(r'<span data-number="[^"]+">[^<]*</span>', "", worked)
    text = re.sub(r"<[^>]+>", "", text)
    assert not re.search(r"\d", text)


def test_router_exports_equal_the_report(tmp_path, module_results):
    _, _, single, profile = module_results
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg["_config_dir_path"] = tmp_path.as_posix()
    cfg["_config_file_path"] = (tmp_path / "example.yml").as_posix()
    summary = router(cfg)["alt_fuel_ship_sizing"]["summary"]
    for key in ("required_fuel_mass_kg", "gross_tank_volume_m3", "bog_lost_kg"):
        assert summary[key] == getattr(profile, key)
    assert profile.required_fuel_mass_kg > single.required_fuel_mass_kg


def test_chart_values_and_warning_boundary(module_results):
    _, fuel, single, _ = module_results
    builder = load_builder()
    model = builder.calculate()
    html = builder.render()
    encoded = re.search(r'<script id="chart-data" type="application/json">(.*?)</script>', html, re.S)
    assert json.loads(encoded.group(1)) == model["chart"]
    expected_draw = single.fuel_mass_flow_kg_per_h * builder.HOURS_PER_DAY
    assert model["chart"]["limit"] == expected_draw
    settings = module_results[0]
    for point in model["chart"]["points"]:
        tank = TankParameters(**{**settings["tank"],
                                 "insulation_heat_flux_w_per_m2": point["x"]})
        from digitalmodel.alt_fuel_ship_sizing import boil_off
        result = boil_off(single.tank, tank, fuel)
        assert point["y"] == result.bog_kg_per_day
    assert model["chart"]["points"][0]["y"] < expected_draw
    assert model["chart"]["points"][-1]["y"] > expected_draw
    assert model["chart"]["boundary"]["y"] == pytest.approx(expected_draw)


def test_single_file_style_comments_and_changes():
    html = load_builder().render()
    parser = EvidenceParser()
    parser.feed(html)
    assert ("body", {"class": "hs-doc"}) in parser.tags
    assert not any(tag == "script" and "src" in attrs for tag, attrs in parser.tags)
    assert not any(tag == "link" and attrs.get("rel") == "stylesheet"
                   for tag, attrs in parser.tags)
    assert html.count("<h2") == 10
    assert "showSaveFilePicker" in html
    assert "comments.json" in html
    assert "textContent" in html
    assert "createObjectURL" in html
    assert "type=\"file\"" in html
    assert "<style>" in html
    assert "--brand:" in html
    assert "--brand:" in (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes == load_builder().CHANGES


def test_built_public_text_and_publication_hygiene():
    html = load_builder().render()
    text = re.sub(r"<(script|style)\b[^>]*>.*?</\1>", "", html, flags=re.S)
    text = re.sub(r"<[^>]+>", " ", text)
    assert not re.search(r"\b(could|should|would|validated|certified|compliant)\b", text, re.I)
    assert "NOT_EVALUATED" in text
    assert "ASSUMED - to be confirmed" in text
    assert not re.search(r"\b\d+\.\d{4,}\b", text)
    assert "<b>" not in html and "<strong>" not in html
    assert "http-equiv" not in html
    assert not re.search(r"[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}", html)


def test_reference_targets_and_house_style():
    html = load_builder().render()
    ids = set(re.findall(r'\bid="([^"]+)"', html))
    assert all(target in ids for target in re.findall(r'href="#([^"]+)"', html))
    source = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    exemplar = re.search(r"<style>.*?</style>", source, re.S).group()
    assert exemplar in html


def test_only_report_deliverables_are_reincluded():
    for name in ("index.html", "generate.py", "changes.json"):
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", "-q", (REPORT / name).relative_to(ROOT).as_posix()],
            cwd=ROOT, check=False, env=git_environment(),
        )
        assert result.returncode == 1
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "-q", (REPORT / "intermediate.csv").relative_to(ROOT).as_posix()],
        cwd=ROOT, check=False, env=git_environment(),
    )
    assert result.returncode == 0


def git_environment():
    """Keep caller repository bindings out of ignore checks and fixtures."""
    return {key: value for key, value in os.environ.items()
            if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}


def test_headlines_match_hand_worked_240_hour_case():
    model = load_builder().calculate()
    # Pe = 4750/0.95 + 500 = 5500 kW; Pf = 5500/0.50 = 11000 kW.
    # Flow = 11000*3.6/120 = 330 kg/h; Mv = 330*(4000/12)*1.10 = 121000 kg.
    # Vg = M/(70.85*0.92); D = (4*Vg/(5*pi))**(1/3); A = 5.5*pi*D**2.
    # Solve M = 121000 + 1.5*A(M)*240*3600/446000 by fixed-point substitution:
    # M = 124098.624655 kg; Vg = 1903.878750 m3; loss = 3098.624655 kg.
    # Increase = 100*(M/121000-1) = 2.560847%; sea q* = 7920*446000/(86400*A).
    assert model["single"]["fuel_mass_flow_kg_per_h"] == pytest.approx(330.0)
    assert model["single"]["required_fuel_mass_kg"] == pytest.approx(121000.0)
    assert model["single"]["gross_tank_volume_m3"] == pytest.approx(1856.340708, rel=0, abs=0.00001)
    assert model["profile"]["required_fuel_mass_kg"] == pytest.approx(124098.624655, rel=0, abs=0.00001)
    assert model["profile"]["gross_tank_volume_m3"] == pytest.approx(1903.878750, rel=0, abs=0.00001)
    assert model["profile"]["bog_lost_kg"] == pytest.approx(3098.624655, rel=0, abs=0.00001)
    assert model["comparison"]["volume_change_percent"] == pytest.approx(2.560847, rel=0, abs=0.00001)
    assert model["chart"]["boundary"]["x"] == pytest.approx(38.991374, rel=0, abs=0.00001)


def test_text_io_declares_utf8_and_lf():
    for path in (REPORT / "generate.py", Path(__file__)):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call) or not isinstance(node.func, ast.Attribute):
                continue
            if node.func.attr not in {"read_text", "write_text"}:
                continue
            keywords = {item.arg: ast.literal_eval(item.value) for item in node.keywords}
            assert keywords.get("encoding") == "utf-8", node.lineno
            if node.func.attr == "write_text":
                assert keywords.get("newline") == "\n", node.lineno


@pytest.mark.parametrize("data", [{"unrecognised": 1.0}, {"unexpected": [1.0]}])
def test_ledger_rejects_unknown_numeric_fields(data):
    builder = load_builder()
    with pytest.raises(ValueError, match="Unknown numeric field"):
        builder.ledger("input", data, builder.flatten(data, "input"), "Example")


def test_leg_labels_and_prose_follow_each_leg(monkeypatch):
    builder = load_builder()
    model = builder.calculate()
    # Deliberately differ from route service speed and add a fourth leg.
    model["input"]["route"]["legs"][0]["speed_kn"] = 10.0
    model["input"]["route"]["legs"][2]["speed_kn"] = 14.0
    model["input"]["route"]["legs"].append({"type": "port", "dwell_h": 48.0})
    monkeypatch.setattr(builder, "calculate", lambda: model)
    html = builder.render()
    worked = html.split('id="worked-example"', 1)[1].split("</section>", 1)[0]
    parser = EvidenceParser()
    parser.feed(worked)
    numbers = dict(parser.numbers)
    assert numeric(numbers["input.route.legs.0.speed_kn"]) == 10.0
    assert numeric(numbers["input.route.legs.2.speed_kn"]) == 14.0
    assert numeric(numbers["input.route.legs.3.dwell_h"]) == 48.0
    assert "Leg 4 (port)" in html


def test_input_and_property_precision_is_preserved():
    builder = load_builder()
    values = {"input.thickness_m": 0.0254, "fuel.density_kg_per_m3": 70.85,
              "fuel.storage_temperature_k": 20.28}
    for key, value in values.items():
        parser = EvidenceParser()
        parser.feed(builder.number(key, values))
        assert numeric(parser.numbers[0][1]) == value


def test_open_reference_property_comparison_matches_hand_values():
    reference = load_builder().calculate()["reference"]
    # NBS 168 (1981), 1.2.15 Fixed Points and 1.2.25 Latent Heat of Vaporization.
    # 100*(70.85/70.78-1) = 0.0988980%; 100*(446.0/445.5-1) = 0.1122334%.
    # The same mass/area substitution above, with rho=70.78 and hv=445500 J/kg,
    # gives M=124104.240944 kg and Vg=M/(70.78*0.92)=1905.847896 m3.
    assert reference["density_change_percent"] == pytest.approx(0.0988980, rel=0, abs=0.0000001)
    assert reference["latent_heat_change_percent"] == pytest.approx(0.1122334, rel=0, abs=0.0000001)
    assert reference["gross_tank_volume_m3"] == pytest.approx(1905.847896, rel=0, abs=0.00001)


def test_shared_ignore_rules_preserve_sibling_deliverables(tmp_path):
    env = git_environment()
    subprocess.run(["git", "init", "-q", tmp_path.as_posix()], check=True, env=env)
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    # Another report may add its exceptions before or after this report's rules.
    sibling = ("!docs/reports/modules/another_report/\n"
               "!docs/reports/modules/another_report/index.html\n")
    anchor = "!docs/reports/modules/altfuelshipsizing/"
    for combined in (rules.replace(anchor, sibling + anchor), rules + sibling):
        (tmp_path / ".gitignore").write_text(combined, encoding="utf-8", newline="\n")
        for module in ("altfuelshipsizing", "another_report"):
            for filename, ignored in (("index.html", False), ("intermediate.csv", True)):
                result = subprocess.run(
                    ["git", "check-ignore", "--no-index", "-q",
                     f"docs/reports/modules/{module}/{filename}"],
                    cwd=tmp_path, check=False, env=env,
                )
                assert result.returncode == int(not ignored), (module, filename)
