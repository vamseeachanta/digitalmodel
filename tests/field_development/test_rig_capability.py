# ABOUTME: Tests for rig_capability — onshore rig capability assessment (stages 1-4).
# ABOUTME: Issue #821 — hard-gate screen, weighted scoring, defaults-vs-override resolution.
"""Tests for digitalmodel.field_development.rig_capability."""

from __future__ import annotations

import pytest

from digitalmodel.field_development.rig_capability import (
    RIG_CLASS_DEFAULTS,
    SUPERSPEC_MIN_HOOKLOAD_LBF,
    assess,
    derive_required_capability,
    resolve_rig,
    score_rig,
    screen_rig,
)


# --- stage 1: required capability -------------------------------------------
class TestDeriveRequiredCapability:
    def test_floors_at_superspec_when_well_light(self):
        cfg = {"program": {"well_design": {"heaviest_string_lbf": 100_000}}}
        req = derive_required_capability(cfg)
        assert req.min_hookload_lbf == SUPERSPEC_MIN_HOOKLOAD_LBF

    def test_heavy_string_drives_hookload_above_floor(self):
        cfg = {"program": {"well_design": {"heaviest_string_lbf": 800_000}}}
        req = derive_required_capability(cfg)
        assert req.min_hookload_lbf == pytest.approx(800_000 * 1.25)

    def test_explicit_required_capability_wins(self):
        cfg = {
            "program": {"well_design": {"heaviest_string_lbf": 800_000}},
            "required_capability": {"min_hookload_lbf": 600_000, "pad_capability": "any"},
        }
        req = derive_required_capability(cfg)
        assert req.min_hookload_lbf == 600_000
        assert req.pad_capability == "any"


# --- stage 2: defaults + overrides ------------------------------------------
class TestResolveRig:
    def test_pulls_class_defaults(self):
        rig = resolve_rig({"id": "r1", "source": "hp_flexrig"})
        assert rig["contractor"] == "Helmerich & Payne"
        assert rig["pad"] == "walking"

    def test_overrides_merge_over_defaults(self):
        rig = resolve_rig(
            {"id": "r1", "source": "hp_flexrig", "overrides": {"ht_track_record": 0.9}}
        )
        assert rig["ht_track_record"] == 0.9
        assert rig["drive_type"] == "AC"  # untouched default

    def test_unknown_source_yields_minimal_rig(self):
        rig = resolve_rig({"id": "mystery"})
        assert rig["rig_id"] == "mystery"


# --- stage 3: hard-gate screen ----------------------------------------------
class TestScreenRig:
    def test_superspec_rig_passes(self):
        req = derive_required_capability({"program": {"well_design": {}}})
        assert screen_rig(RIG_CLASS_DEFAULTS["hp_flexrig"], req) == []

    def test_skid_rig_fails_walking_gate(self):
        req = derive_required_capability({"program": {"well_design": {}}})
        fails = screen_rig(RIG_CLASS_DEFAULTS["nabors_pace_x800"], req)
        assert any("pad capability" in f for f in fails)

    def test_low_hookload_fails(self):
        req = derive_required_capability({"program": {"well_design": {}}})
        rig = {**RIG_CLASS_DEFAULTS["hp_flexrig"], "hookload_lbf": 500_000}
        fails = screen_rig(rig, req)
        assert any("hookload" in f for f in fails)

    def test_dc_drive_fails_ac_gate(self):
        req = derive_required_capability({"program": {"well_design": {}}})
        rig = {**RIG_CLASS_DEFAULTS["hp_flexrig"], "drive_type": "DC"}
        assert any("drive type" in f for f in screen_rig(rig, req))


# --- stage 4: scoring -------------------------------------------------------
class TestScoreRig:
    def test_higher_hookload_scores_higher_margin(self):
        req = derive_required_capability({"program": {"well_design": {}}})
        weights = {"capability_margin": 1.0}
        low, _ = score_rig({**RIG_CLASS_DEFAULTS["hp_flexrig"]}, req, weights)
        high, _ = score_rig(
            {**RIG_CLASS_DEFAULTS["hp_flexrig"], "hookload_lbf": 1_125_000}, req, weights
        )
        assert high > low

    def test_score_bounded_unit_interval(self):
        req = derive_required_capability({"program": {"well_design": {}}})
        fit, dims = score_rig(RIG_CLASS_DEFAULTS["precision_st1500"], req, None or {"capability_margin": 1.0})
        assert 0.0 <= fit <= 1.0
        assert all(0.0 <= v <= 1.0 for v in dims.values())


# --- end-to-end assess() ----------------------------------------------------
class TestAssess:
    def _cfg(self):
        return {
            "program": {"well_design": {"heaviest_string_lbf": 600_000, "mud_max_circulating_psi": 7500}},
            "candidate_rigs": [
                {"id": "HP", "source": "hp_flexrig"},
                {"id": "PD", "source": "precision_st1500"},
                {"id": "NBR", "source": "nabors_pace_x800"},
            ],
        }

    def test_counts_and_disqualification(self):
        out = assess(self._cfg())
        assert out["n_candidates"] == 3
        assert out["n_qualified"] == 2  # Nabors skid disqualified
        nbr = next(r for r in out["ranked_rigs"] if r["rig_id"] == "NBR")
        assert not nbr["qualified"]
        assert nbr["fail_reasons"]

    def test_qualified_rigs_ranked_first_and_numbered(self):
        out = assess(self._cfg())
        qualified = [r for r in out["ranked_rigs"] if r["qualified"]]
        # qualified come before disqualified
        assert out["ranked_rigs"][0]["qualified"]
        assert out["ranked_rigs"][-1]["rig_id"] == "NBR"
        # ranks assigned 1..n
        ranks = [r["dimension_scores"]["_rank"] for r in qualified]
        assert ranks == list(range(1, len(qualified) + 1))

    def test_precision_outranks_flexrig_on_capability_margin_only(self):
        # ST-1500 has higher hookload (825k vs 750k) -> higher capability margin,
        # so it wins when scoring on capability margin alone.
        cfg = {
            **self._cfg(),
            "scoring_weights": {
                "capability_margin": 1.0,
                "pad_mobility": 0.0,
                "automation_digital": 0.0,
                "power_emissions": 0.0,
                "ht_readiness": 0.0,
            },
        }
        out = assess(cfg)
        order = [r["rig_id"] for r in out["ranked_rigs"] if r["qualified"]]
        assert order.index("PD") < order.index("HP")
