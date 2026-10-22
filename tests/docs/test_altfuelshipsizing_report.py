# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the sizing report and check displayed evidence against the public API."""

import importlib.util
import json
import re
import shlex
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
def api_results():
    """Recalculate through the same API as the builder for consistency checks."""
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


def test_rebuild_is_byte_identical(tmp_path):
    builder = load_builder()
    builder.build(tmp_path)
    for name in ("index.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (REPORT / name).read_bytes()


def test_every_displayed_example_number_matches_module(api_results):
    settings, fuel, single, profile = api_results
    reference_fuel = FuelProperties(**{**asdict(fuel), "density_kg_per_m3": 70.78,
                                     "heat_of_vaporization_kj_per_kg": 445.5,
                                     "storage_temperature_k": 20.268})
    reference = size_fuel_chain_voyage(
        legs=[SeaLeg(2000.0, 12.0), PortLeg(240.0), SeaLeg(2000.0, 12.0)],
        shaft_power_kw=4750.0, hotel_load_kw=500.0,
        eta_electric_drivetrain=0.95, eta_fuel_cell_lhv=0.5,
        tank=TankParameters(**settings["tank"]), fuel=reference_fuel,
        fuel_margin_fraction=0.1,
    )
    expected = flatten({"input": settings, "fuel": asdict(fuel),
                        "single": asdict(single), "profile": asdict(profile),
                        "chart": {"limit": single.fuel_mass_flow_kg_per_h * 24,
                                  "boundary": {"x": 1.5 * single.fuel_mass_flow_kg_per_h * 24 / single.boil_off.bog_kg_per_day}},
                        "comparison": {
                            "density_change_percent": 100 * (fuel.density_kg_per_m3 / 70.78 - 1),
                            "latent_heat_change_percent": 100 * (fuel.heat_of_vaporization_kj_per_kg / 445.5 - 1),
                            "reference_gross_volume_m3": reference.gross_tank_volume_m3,
                            "inventory_change_percent": 100 * (profile.required_fuel_mass_kg - single.required_fuel_mass_kg) / single.required_fuel_mass_kg,
                            "volume_change_percent": 100 * (profile.gross_tank_volume_m3 - single.gross_tank_volume_m3) / single.gross_tank_volume_m3,
                        }})
    parser = EvidenceParser()
    parser.feed(load_builder().render())
    assert parser.numbers
    for key, text in parser.numbers:
        assert key in expected, key
        if key in {"fuel.density_kg_per_m3", "fuel.storage_temperature_k"}:
            assert text == f"{expected[key]:.2f}", key
        elif key.startswith("comparison.") and key.endswith("_percent"):
            assert text == f"{expected[key]:.1f}", key
        else:
            assert text == load_builder().engineering_number(expected[key]), key
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


def test_router_exports_equal_the_report(tmp_path, api_results):
    _, _, single, profile = api_results
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg["_config_dir_path"] = str(tmp_path)
    cfg["_config_file_path"] = str(tmp_path / "example.yml")
    summary = router(cfg)["alt_fuel_ship_sizing"]["summary"]
    for key in ("required_fuel_mass_kg", "gross_tank_volume_m3", "bog_lost_kg"):
        assert summary[key] == getattr(profile, key)
    assert profile.required_fuel_mass_kg > single.required_fuel_mass_kg


def test_chart_values_and_warning_boundary(api_results):
    _, fuel, single, _ = api_results
    builder = load_builder()
    model = builder.calculate()
    html = builder.render()
    encoded = re.search(r'<script id="chart-data" type="application/json">(.*?)</script>', html, re.S)
    assert json.loads(encoded.group(1)) == model["chart"]
    expected_draw = single.fuel_mass_flow_kg_per_h * builder.HOURS_PER_DAY
    assert model["chart"]["limit"] == expected_draw
    settings = api_results[0]
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


def test_built_client_text_and_publication_hygiene():
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
            ["git", "check-ignore", "--no-index", "-q", str(REPORT / name)],
            cwd=ROOT, check=False,
        )
        assert result.returncode == 1
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "-q", str(REPORT / "intermediate.csv")],
        cwd=ROOT, check=False,
    )
    assert result.returncode == 0


def test_headlines_against_hand_worked_literals():
    """Pin arithmetic independent of the sizing API, including the fixed point.

    P = 4750/0.95 + 500 = 5500 kW; flow = 5500/0.5*3.6/120.
    Reserve inventory = 330*(4000/12)*1.1 = 121000 kg.
    For L/D=5, D=(4V/(5*pi))**(1/3), A=5.5*pi*D**2.
    V=M/(70.85*0.92); port loss = 1.5*A*240*3600/446000.
    Solving M=121000+port_loss gives the literals below. Tolerances
    reflect the last retained hand-worked digit, not the report rounding.
    """
    model = load_builder().calculate()
    single, profile = model["single"], model["profile"]
    assert single["fuel_mass_flow_kg_per_h"] == pytest.approx(330.0, abs=0.00001, rel=0)
    assert single["required_fuel_mass_kg"] == pytest.approx(121000.0, abs=0.001, rel=0)
    assert profile["gross_tank_volume_m3"] == pytest.approx(1903.87875, abs=0.00001, rel=0)
    assert profile["required_fuel_mass_kg"] == pytest.approx(124098.62, abs=0.005, rel=0)
    assert profile["bog_lost_kg"] == pytest.approx(3098.62, abs=0.005, rel=0)
    assert model["comparison"]["inventory_change_percent"] == pytest.approx(2.56085, abs=0.00001, rel=0)
    assert model["chart"]["boundary"]["x"] == pytest.approx(38.99137, abs=0.00001, rel=0)
    parser = EvidenceParser()
    parser.feed(load_builder().render())
    visible = dict(parser.numbers)
    assert visible["profile.gross_tank_volume_m3"] == "1,903.9"
    assert visible["profile.required_fuel_mass_kg"] == "124,098.6"
    assert visible["profile.bog_lost_kg"] == "3,098.6"
    assert visible["comparison.inventory_change_percent"] == "2.6"


def test_lf_writes_and_explicit_utf8_reads(tmp_path, monkeypatch):
    """Exercise Windows newline translation and reject locale-dependent reads."""
    builder = load_builder()
    original_read, original_write = Path.read_text, Path.write_text

    def read(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        return original_read(path, *args, **kwargs)

    def write(path, data, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        assert kwargs.get("newline") == "\n"
        # newline='\r\n' simulates Windows translation if LF is not explicit.
        return original_write(path, data, encoding="utf-8", newline=kwargs.get("newline", "\r\n"))

    monkeypatch.setattr(Path, "read_text", read)
    monkeypatch.setattr(Path, "write_text", write)
    builder.build(tmp_path)
    assert b"\r" not in (tmp_path / "index.html").read_bytes()
    assert b"\r" not in (tmp_path / "changes.json").read_bytes()


def test_summary_comparators_and_property_precision():
    builder = load_builder()
    html = builder.render()
    summary = html.split('id="executive"', 1)[1].split("</section>", 1)[0]
    for key in ("single.boil_off.bog_kg_per_day", "profile.boil_off.bog_kg_per_day",
                "chart.limit", "chart.boundary.x"):
        assert f'data-number="{key}"' in summary
    parser = EvidenceParser()
    parser.feed(html)
    numbers = dict(parser.numbers)
    assert numbers["fuel.density_kg_per_m3"] == "70.85"
    assert numbers["fuel.storage_temperature_k"] == "20.28"
    assert numbers["comparison.reference_gross_volume_m3"] == "1,905.8"
    assert numbers["comparison.density_change_percent"] == "0.1"
    assert numbers["comparison.latent_heat_change_percent"] == "0.1"
    assert "70.78" in html and "20.268" in html and "445.5" in html
    assert "Draft for maintainer review" in html
    assert "the independent test calls the public API" not in html


def test_unknown_numeric_keys_raise():
    builder = load_builder()
    with pytest.raises(ValueError, match="unrecognized"):
        builder.ledger("input", {"unknown": 17}, {"input.unknown": 17}, "assumed")


def test_route_uses_each_leg_speed_and_accepts_more_legs(monkeypatch):
    builder = load_builder()
    original = builder.yaml.safe_load

    def settings(text):
        cfg = original(text)
        legs = cfg["alt_fuel_ship_sizing"]["route"]["legs"]
        legs[2]["speed_kn"] = 11.0
        legs.append({"type": "sea", "distance_nm": 100.0, "speed_kn": 10.0})
        return cfg

    monkeypatch.setattr(builder.yaml, "safe_load", settings)
    html = builder.render()
    worked = html.split('id="worked-example"', 1)[1].split("</section>", 1)[0]
    assert 'data-number="input.route.legs.2.speed_kn">11.0' in worked
    assert 'data-number="input.route.legs.3.speed_kn">10.0' in worked
    assert 'data-number="input.route.service_speed_kn"' not in worked


def test_ci_runs_the_sizing_and_report_tests():
    workflow = yaml.load((ROOT / ".github/workflows/quality-gates.yml").read_text(encoding="utf-8"), Loader=yaml.BaseLoader)
    steps = workflow["jobs"]["quality-gates"]["steps"]
    step = next(step for step in steps if step.get("name") == "Run sizing report regression tests")
    args = shlex.split(step["run"].replace("\\\n", " "))
    assert {"tests/docs/test_altfuelshipsizing_report.py",
            "tests/naval_architecture/test_alt_fuel_ship_sizing.py",
            "tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py",
            "tests/report_pack/test_alt_fuel_report_pack_demo.py"} <= set(args)
    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8"))
    assert "tests/docs/" in shlex.split(gates["gates"]["tests-contracts"]["command"])
