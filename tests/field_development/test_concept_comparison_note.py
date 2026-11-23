# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# ABOUTME: Tests for W6 concept_comparison_note — markdown render, sensitivity, determinism.
# ABOUTME: Asserts scope banner + no client names + byte-identical repeat render.
"""Tests for digitalmodel.field_development.concept_comparison_note (W6)."""

from __future__ import annotations

import pytest

from digitalmodel.field_development.concept_comparison_note import (
    build_comparison_note,
    render_comparison_note,
    weight_sensitivity_sweep,
)
from digitalmodel.field_development.concept_screening import ProspectSpec, screen_concepts


@pytest.fixture()
def spec() -> ProspectSpec:
    return ProspectSpec(
        name="Test Analogue",
        water_depth_m=1500.0,
        well_count=8,
        reservoir_size_mmbbl=300.0,
        production_capacity_bopd=120_000.0,
        fluid_type="oil",
        distance_to_infra_km=80.0,
    )


class TestSensitivitySweep:
    def test_includes_base_plus_each_axis(self, spec):
        rows = weight_sensitivity_sweep(spec)
        scenarios = {r.scenario for r in rows}
        assert "base weights" in scenarios
        for axis in ("capex", "schedule", "rig_days", "intervention"):
            assert f"{axis}-dominant" in scenarios

    def test_dominant_weight_assigned(self, spec):
        rows = weight_sensitivity_sweep(spec, dominant_weight=0.55)
        cap_row = next(r for r in rows if r.scenario == "capex-dominant")
        assert cap_row.weights["capex"] == pytest.approx(0.55)


class TestRender:
    def test_scope_banner_present(self, spec):
        result = screen_concepts(spec)
        md = render_comparison_note(result)
        assert "Scope (screening-grade)" in md
        assert "paid feed" in md  # rig $/day honesty flag

    def test_no_client_names(self, spec):
        result, md = build_comparison_note(spec)
        lowered = md.lower()
        # public-repo hygiene: no FDAS / client identifiers
        for forbidden in ("fdas", "frontier", "frps "):
            assert forbidden not in lowered

    def test_governing_trade_off_section(self, spec):
        result, md = build_comparison_note(spec)
        assert "Governing trade-off" in md

    def test_capex_breakdown_section(self, spec):
        result, md = build_comparison_note(spec)
        assert "CAPEX driver breakdown" in md
        assert "total" in md


class TestDeterminism:
    def test_note_byte_identical(self, spec):
        _, md1 = build_comparison_note(spec)
        _, md2 = build_comparison_note(spec)
        assert md1 == md2


class TestOverrideNote:
    def test_override_provenance_marker_and_determinism(self, spec):
        override = {"Spar": 0.5}
        result, note = build_comparison_note(spec, capex_override=override)
        assert "CAPEX basis: absolute calibrated totals (override) for Spar" in note
        assert "0.50*" in note
        assert "* Absolute CAPEX override" in note
        assert note == build_comparison_note(spec, capex_override=override)[1]

    def test_top_override_and_raw_breakdown_render(self, spec):
        result = screen_concepts(spec)
        top = result.ranked[0]
        top.axes.capex_line_items_usd_bn = {"total": 0.5, "source": "absolute_override"}
        note = render_comparison_note(result)
        assert "| total | 0.500 |" in note
        assert "| source |" not in note
        override = {top.host_type.value: 0.5}
        result, note = build_comparison_note(spec, capex_override=override)
        assert result.ranked[0].host_type == top.host_type
        assert "0.50*" in note

    def test_sweep_override_flips_capex_winner(self, spec):
        default = weight_sensitivity_sweep(spec)
        winner = next(row.winner for row in default if row.scenario == "capex-dominant")
        assert winner != "Spar"
        rows = weight_sensitivity_sweep(spec, capex_override={"Spar": 0.5})
        assert next(row.winner for row in rows if row.scenario == "capex-dominant") == "Spar"

    def test_default_has_no_override_text(self, spec):
        _, note = build_comparison_note(spec)
        assert "CAPEX basis:" not in note
        assert "Absolute CAPEX override" not in note
        assert "calibrated totals" not in note
