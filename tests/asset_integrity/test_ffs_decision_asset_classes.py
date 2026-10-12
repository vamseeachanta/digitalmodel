# ABOUTME: Tests for the asset-class-generalised FFS decision engine (#2205) —
# ABOUTME: shared verdict vocabulary, per-class action map, unit-tagged inputs.
"""Asset-class FFS decision tests (issue #2205, owner decision D1 = A).

One shared verdict vocabulary (ACCEPT / MONITOR / DERATE / REPAIR / REPLACE /
ESCALATE) with a per-asset-class action map that gives each class its native
report wording.  ``RE_RATE`` stays an alias of ``DERATE`` so no legacy caller
breaks.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.asset_integrity.assessment.ffs_decision import (
    ASSET_CLASSES,
    DecisionBands,
    FFSDecision,
    UnitTagError,
    Verdict,
    action_for,
    decide,
    from_traffic_light,
    reduced_mawp,
    remaining_life,
    scaled_limit,
)
from digitalmodel.units import Q_

CLASSES = ("pressure", "pipeline", "mooring_chain", "hull_plating", "jacket_member", "tank")

# Default bands are today's pressure values: RSFa=0.90, band 0.05, floor 0.50.
RSFA = 0.90
CASES = [
    # (margin, remaining_life_yr, expected verdict)
    (0.97, 10.0, Verdict.ACCEPT),           # above monitor band
    (0.9501, 10.0, Verdict.ACCEPT),         # just above the band edge
    (0.9499, 10.0, Verdict.MONITOR),        # just inside band
    (0.90, 10.0, Verdict.MONITOR),          # exactly at allowable
    (0.8999, 10.0, Verdict.DERATE),         # just below allowable
    (0.50, 10.0, Verdict.DERATE),           # exactly at floor -> still de-rate
    (0.4999, 10.0, Verdict.REPLACE),        # below floor
    (0.95, 0.0, Verdict.REPAIR),            # screening fails (life exhausted), margin OK
    (float("nan"), 10.0, Verdict.ESCALATE), # margin not evaluable
]


# ---------------------------------------------------------------------------
# Table-driven: every asset class hits every verdict boundary identically
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("asset_class", CLASSES)
@pytest.mark.parametrize("margin,life,expected", CASES)
def test_verdict_boundaries_per_class(asset_class, margin, life, expected):
    d = decide(margin, RSFA, life, asset_class)
    assert d.verdict is expected
    assert d.asset_class == asset_class
    assert d.action == action_for(expected, asset_class)
    assert isinstance(d.governing_criterion, str) and d.governing_criterion


@pytest.mark.parametrize("asset_class", CLASSES)
def test_every_class_maps_every_verdict(asset_class):
    for v in Verdict:
        assert isinstance(action_for(v, asset_class), str)


def test_class_native_wording():
    assert action_for(Verdict.ACCEPT, "mooring_chain") == "CONTINUE"
    assert action_for(Verdict.MONITOR, "mooring_chain") == "SHORTEN INTERVAL"
    assert action_for(Verdict.REPAIR, "mooring_chain") == "REPLACE SEGMENT"
    assert action_for(Verdict.ACCEPT, "hull_plating") == "ACCEPT"
    assert action_for(Verdict.MONITOR, "hull_plating") == "SUBSTANTIAL CORROSION"
    assert action_for(Verdict.REPAIR, "hull_plating") == "RENEW"
    assert action_for(Verdict.ACCEPT, "jacket_member") == "ACCEPT"
    assert action_for(Verdict.DERATE, "jacket_member") == "MITIGATE"
    assert action_for(Verdict.REPAIR, "jacket_member") == "REPAIR"
    for cls in ("pressure", "pipeline", "tank"):
        assert action_for(Verdict.DERATE, cls) == "RE_RATE"  # today's word
        assert action_for(Verdict.ACCEPT, cls) == "ACCEPT"
        assert action_for(Verdict.REPLACE, cls) == "REPLACE"


def test_unknown_asset_class_rejected():
    with pytest.raises(ValueError, match="asset_class"):
        decide(0.95, RSFA, 10.0, "spaceship")


def test_bands_override_moves_boundaries():
    wide = DecisionBands(monitor_band=0.20, derate_floor=0.70, repair_life_yr=2.0)
    assert decide(1.05, RSFA, 10.0, "jacket_member", bands=wide).verdict is Verdict.MONITOR
    assert decide(0.65, RSFA, 10.0, "jacket_member", bands=wide).verdict is Verdict.REPLACE
    # default per-class bands equal today's pressure values
    for cls in CLASSES:
        assert ASSET_CLASSES[cls].bands == DecisionBands()


# ---------------------------------------------------------------------------
# RE_RATE alias
# ---------------------------------------------------------------------------
def test_rerate_is_alias_of_derate():
    assert Verdict.RE_RATE is Verdict.DERATE
    assert Verdict("RE_RATE") is Verdict.DERATE
    assert Verdict("DERATE") is Verdict.DERATE
    assert Verdict.DERATE.value == "DERATE"
    # legacy signature keeps emitting the legacy word
    legacy = FFSDecision.decide(
        level1_verdict="FAIL_LEVEL_1", level2_verdict="FAIL_LEVEL_2",
        rsf=0.72, rsf_a=RSFA, t_mm_in=0.60, t_min_in=0.40,
        corrosion_rate_in_per_yr=0.01, design_pressure_psi=1000.0,
    )
    assert legacy["verdict"] == "RE_RATE"
    assert legacy["action"] == "RE_RATE"
    assert legacy["asset_class"] == "pressure"


def test_decision_to_dict_is_legacy_shaped():
    d = decide(0.72, RSFA, 5.0, "pressure", derate=reduced_mawp(Q_(1000.0, "psi")))
    payload = d.to_dict()
    for key in ("verdict", "action", "asset_class", "remaining_life_yr",
                "governing_criterion", "rsf", "rsf_a", "rerated_mawp_psi"):
        assert key in payload
    assert payload["verdict"] == "DERATE"
    assert payload["action"] == "RE_RATE"
    assert payload["rerated_mawp_psi"] == pytest.approx(800.0)
    import json
    json.dumps(payload)


# ---------------------------------------------------------------------------
# Injected derate callables and unit tagging
# ---------------------------------------------------------------------------
def test_pressure_derate_is_reduced_mawp():
    d = decide(0.72, RSFA, 5.0, "pressure", derate=reduced_mawp(Q_(1000.0, "psi")))
    assert d.verdict is Verdict.DERATE
    assert d.derated.to("psi").magnitude == pytest.approx(800.0)
    assert "MAWP_r=800 psi" in d.governing_criterion
    assert "2.4.2.2" in d.governing_criterion
    # accepts any pressure unit, reports in its own unit
    d_si = decide(0.72, RSFA, 5.0, "pressure", derate=reduced_mawp(Q_(10.0, "MPa")))
    assert d_si.derated.to("MPa").magnitude == pytest.approx(8.0)


def test_derate_capped_at_limit_and_none_without_callable():
    d = decide(0.97, RSFA, 5.0, "pressure", derate=reduced_mawp(Q_(1000.0, "psi")))
    assert d.derated.to("psi").magnitude == pytest.approx(1000.0)
    d0 = decide(0.72, RSFA, 5.0, "pressure")
    assert d0.derated is None
    assert "Reduce MAWP or operating pressure" in d0.governing_criterion


def test_mooring_derate_is_tension_limit():
    d = decide(0.72, RSFA, 5.0, "mooring_chain",
               derate=scaled_limit(Q_(8000.0, "kN"), "[force]", label="T_limit"))
    assert d.action == "REDUCE TENSION LIMIT"
    assert d.derated.to("kN").magnitude == pytest.approx(8000.0 * 0.72 / 0.90)
    assert "T_limit" in d.governing_criterion


@pytest.mark.parametrize("bad", [1000.0, Q_(1000.0, "inch"), Q_(1000.0, "kN")])
def test_reduced_mawp_rejects_untagged_or_mismatched(bad):
    with pytest.raises(UnitTagError):
        reduced_mawp(bad)


def test_scaled_limit_rejects_mismatched_dimension():
    with pytest.raises(UnitTagError):
        scaled_limit(Q_(1000.0, "psi"), "[force]")


def test_margin_with_units_rejected():
    with pytest.raises(UnitTagError):
        decide(Q_(0.95, "psi"), RSFA, 10.0, "pressure")
    with pytest.raises(UnitTagError):
        decide(0.95, Q_(0.90, "inch"), 10.0, "pressure")
    # dimensionless quantities are fine
    assert decide(Q_(0.97, ""), Q_(0.90, ""), 10.0, "pressure").verdict is Verdict.ACCEPT


def test_remaining_life_accepts_time_quantity_and_rejects_others():
    d = decide(0.95, RSFA, Q_(60, "month"), "tank")
    assert d.remaining_life_yr == pytest.approx(5.0)
    with pytest.raises(UnitTagError):
        decide(0.95, RSFA, Q_(5.0, "m"), "tank")


def test_remaining_life_helper_is_unit_aware():
    yrs = remaining_life(Q_(10.0, "mm"), Q_(8.0, "mm"), Q_(0.1, "mm/yr"))
    assert yrs == pytest.approx(20.0)
    yrs_in = remaining_life(Q_(0.400, "inch"), Q_(0.300, "inch"), Q_(0.010, "inch/yr"))
    assert yrs_in == pytest.approx(10.0)
    assert remaining_life(Q_(0.4, "inch"), Q_(0.3, "inch"), Q_(0.0, "inch/yr")) == math.inf
    assert remaining_life(Q_(0.2, "inch"), Q_(0.3, "inch"), Q_(0.01, "inch/yr")) == 0.0
    with pytest.raises(UnitTagError):
        remaining_life(0.4, 0.3, 0.01)
    with pytest.raises(UnitTagError):
        remaining_life(Q_(0.4, "inch"), Q_(0.3, "psi"), Q_(0.01, "inch/yr"))


# ---------------------------------------------------------------------------
# Mooring GREEN / AMBER / RED / ESCALATE adapter
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("light,verdict,action", [
    ("GREEN", Verdict.ACCEPT, "CONTINUE"),
    ("AMBER", Verdict.MONITOR, "SHORTEN INTERVAL"),
    ("RED", Verdict.REPAIR, "REPLACE SEGMENT"),
    ("ESCALATE", Verdict.ESCALATE, "ESCALATE"),
])
def test_mooring_traffic_light_maps_onto_shared_set(light, verdict, action):
    v = from_traffic_light(light)
    assert v is verdict
    assert action_for(v, "mooring_chain") == action


def test_mooring_traffic_light_rejects_unknown():
    with pytest.raises(ValueError):
        from_traffic_light("PURPLE")


# ---------------------------------------------------------------------------
# Legacy signature is a thin wrapper over decide()
# ---------------------------------------------------------------------------
def test_legacy_wrapper_agrees_with_decide():
    legacy = FFSDecision.decide(
        level1_verdict="ACCEPT", level2_verdict="FAIL_LEVEL_2",
        rsf=0.80, rsf_a=RSFA, t_mm_in=0.350, t_min_in=0.300,
        corrosion_rate_in_per_yr=0.008, design_pressure_psi=900.0,
    )
    new = decide(0.80, RSFA, 50.0 / 8.0, "pressure",
                 derate=reduced_mawp(Q_(900.0, "psi")), screening_pass=False)
    assert legacy["verdict"] == new.action == "RE_RATE"
    assert legacy["rerated_mawp_psi"] == pytest.approx(new.derated.to("psi").magnitude)
    assert legacy["remaining_life_yr"] == pytest.approx(new.remaining_life_yr)


def test_report_shows_class_wording():
    import pandas as pd
    from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport

    d = decide(0.72, RSFA, 5.0, "mooring_chain",
               derate=scaled_limit(Q_(8000.0, "kN"), "[force]", label="T_limit"))
    html_doc = FFSReport.generate_html(
        grid_df=pd.DataFrame([[0.6] * 4] * 4), decision=d.to_dict(),
        component_id="ML-07", nominal_od_in=3.0, nominal_wt_in=0.5,
        t_min_in=0.4, design_code="API RP 2SK", design_pressure_psi=0.0,
    )
    assert "REDUCE TENSION LIMIT" in html_doc
