# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep the pilot reference page bound to its retained inputs and limitations."""

import json
from pathlib import Path
import re
from urllib.parse import unquote, urlsplit

import pytest


ROOT = Path(__file__).resolve().parents[2]
PAGE = ROOT / "docs/domains/hull_library/parametric-rao-pilot.md"
INPUT = ROOT / "examples/hydrodynamics/parametric_rao"
OUTPUT = ROOT / "docs/domains/hydrodynamics/parametric-rao"
BASIS_KEYS = (
    "dimensions.length_m", "dimensions.beam_m", "dimensions.draft_m",
    "omega_rad_s", "heading_deg", "water_depth_m", "rho_kg_m3", "gravity_m_s2",
    "box_gyradii_m", "box_resolution", "convergence_resolutions",
    "reference_dimensions_m", "cog_draft_fraction", "ship_target_panels",
    "irregular_frequency_lower_fraction", "irregular_frequency_upper_fraction",
)
CRITERIA_KEYS = (
    "heave_absolute_tolerance_m_per_m", "heave_relative_tolerance",
    "convergence_absolute_tolerance_m_per_m",
)


def read_page():
    assert PAGE.is_file(), "The pilot ranges and assumptions page is missing"
    return PAGE.read_text(encoding="utf-8")


def test_pilot_reference_page_exists():
    assert PAGE.is_file()


@pytest.mark.parametrize("source,key", [
    *[("basis.json", key) for key in BASIS_KEYS],
    *[("criteria.json", key) for key in CRITERIA_KEYS],
])
def test_documented_values_equal_pinned_inputs(source, key):
    page = read_page()
    rows = re.findall(r"^\| `([^`]+)` \| `([^`]+)` \|[^\n]*\|$", page, re.M)
    matches = [value for row_key, value in rows if row_key == key]
    assert len(matches) == 1, f"Expected exactly one table value for {key}"
    expected = json.loads((INPUT / source).read_text(encoding="utf-8"))
    for part in key.split("."):
        expected = expected[part]
    assert json.loads(matches[0]) == expected, key


def test_qualification_matches_manifest_verbatim():
    page = read_page()
    manifest = json.loads((OUTPUT / "manifest.json").read_text(encoding="utf-8"))
    assert manifest["qualification"] in page.splitlines()


def test_every_relative_link_resolves_to_a_file():
    page = read_page()
    targets = re.findall(r"\[[^\]]+\]\(([^)]+)\)", page)
    assert targets, "The page must link to its inputs and retained evidence"
    resolved = set()
    for target in targets:
        url = urlsplit(target)
        if url.scheme or url.netloc:
            continue
        path = (PAGE.parent / unquote(url.path)).resolve()
        assert path.is_file(), target
        resolved.add(path)
    assert {
        INPUT / "basis.json", INPUT / "criteria.json", INPUT / "run.py",
        OUTPUT / "manifest.json", OUTPUT / "lookup.html",
        PAGE.with_name("parametric-form.md"),
    } <= resolved


def test_form_reference_links_back_to_pilot_page():
    form_page = PAGE.with_name("parametric-form.md").read_text(encoding="utf-8")
    assert re.search(r"\[[^\]]+\]\(parametric-rao-pilot\.md\)", form_page)


def test_lookup_limitations_remain_visible():
    text = " ".join(read_page().split())
    for limitation in (
        "Box sway-roll reciprocity remains unresolved",
        "box beam-sea roll is not mesh-qualified",
        "1.3 %", "0.46 %", "cause is not established",
        "does not discriminate at 1.32 rad/s",
        "not usable for operations", "No measured loading",
    ):
        assert limitation in text
