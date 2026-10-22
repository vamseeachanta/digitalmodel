# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the sizing report and check displayed evidence against the public API."""

import importlib.util
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
def oracle():
    """Calculate the documented example directly, independently of the builder."""
    settings = yaml.safe_load(EXAMPLE.read_text())["alt_fuel_ship_sizing"]
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


def test_every_displayed_example_number_matches_module(oracle):
    settings, fuel, single, profile = oracle
    expected = flatten({"input": settings, "fuel": asdict(fuel),
                        "single": asdict(single), "profile": asdict(profile),
                        "comparison": {
                            "inventory_change_percent": 100 * (profile.required_fuel_mass_kg - single.required_fuel_mass_kg) / single.required_fuel_mass_kg,
                            "volume_change_percent": 100 * (profile.gross_tank_volume_m3 - single.gross_tank_volume_m3) / single.gross_tank_volume_m3,
                        }})
    parser = EvidenceParser()
    parser.feed(load_builder().render())
    assert parser.numbers
    for key, text in parser.numbers:
        assert key in expected, key
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


def test_router_exports_equal_the_report(tmp_path, oracle):
    _, _, single, profile = oracle
    cfg = yaml.safe_load(EXAMPLE.read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    cfg["_config_file_path"] = str(tmp_path / "example.yml")
    summary = router(cfg)["alt_fuel_ship_sizing"]["summary"]
    for key in ("required_fuel_mass_kg", "gross_tank_volume_m3", "bog_lost_kg"):
        assert summary[key] == getattr(profile, key)
    assert profile.required_fuel_mass_kg > single.required_fuel_mass_kg


def test_chart_values_and_warning_boundary(oracle):
    _, fuel, single, _ = oracle
    builder = load_builder()
    model = builder.calculate()
    html = builder.render()
    encoded = re.search(r'<script id="chart-data" type="application/json">(.*?)</script>', html, re.S)
    assert json.loads(encoded.group(1)) == model["chart"]
    expected_draw = single.fuel_mass_flow_kg_per_h * builder.HOURS_PER_DAY
    assert model["chart"]["limit"] == expected_draw
    settings = oracle[0]
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
    assert "--brand:" in (ROOT / "docs/api/_assets/brand.css").read_text()
    changes = json.loads((REPORT / "changes.json").read_text())
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
    source = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text()
    exemplar = re.search(r"<style>.*?</style>", source, re.S).group()
    assert exemplar in html


def test_only_report_deliverables_are_reincluded():
    for name in ("index.html", "generate.py", "changes.json"):
        result = subprocess.run(
            ["git", "check-ignore", "-q", str(REPORT / name)],
            cwd=ROOT, check=False,
        )
        assert result.returncode == 1
    result = subprocess.run(
        ["git", "check-ignore", "-q", str(REPORT / "intermediate.csv")],
        cwd=ROOT, check=False,
    )
    assert result.returncode == 0
