"""#1280/#1453 in-context goldens: the API RP 16Q minimum-top-tension chain vs
the calc contracts on the PRIVATE llm-wiki riser registry + source pages —
dm carries only key names; values resolve wiki-side at test time via
``digitalmodel.drilling_riser.calc_contracts``.

Loud skip standalone. Tolerance: abs <= 0.5 (contract unit: t or kips) AND
rel <= 0.15%, derived from the contract inputs' quantization. This CANNOT
mask a wrong formula: omitting the fleet-angle factor alone produces ~1.48%
error on RSU-0007 (10x the tolerance); a wrong N/(N-n) ratio on the wave-2
contracts is >6%.

Wave-2 contracts (llm-wiki#826): RSU-0010/RSU-0012 (959 m, 16x3600k DAT),
RSU-0014 (1783 m, semisub 6x833k DAT), RSU-0023 (1981 m, 2H standard calc
incl. 21-row mud sweep). Their workbooks apply the bare N/(N-n) allowance
(Rf = 1.0, no separate fleet-angle factor — unlike RSU-0007), so the engine
kwargs are passed neutral (efficiency=1.0, fleet_angle_factor=1.0).
"""

from __future__ import annotations

import pytest

from digitalmodel.drilling_riser.assembly import (
    minimum_top_tension_16q,
    tensioner_system_factor,
)
from digitalmodel.drilling_riser.calc_contracts import find_contract, resolve_wiki_root
from digitalmodel.drilling_riser.stackup import minimum_slip_ring_tension

_CALC = "16q-min-top-tension"
_SHALLOW_WATER_CALC = "shallow-water-tension-to-rotate"

_RSU_0007_KEYS = {
    "rsu_id",
    "calc",
    "factored_submerged_steel_t",
    "factored_buoyancy_t",
    "n_wires",
    "n_fail",
    "efficiency",
    "fleet_angle_factor",
    "min_top_tension_t",
    "connector_pull_t",
    "tension_setting_t",
}
_RSU_31245_KEYS = {
    "rsu_id",
    "calc",
    "mud_weight_ppg",
    "n_wires",
    "n_fail",
    "min_slip_ring_tension_kips",
    "min_vertical_top_tension_kips",
    "min_bop_overpull_kips",
    "recoil_margin_kips",
    "bop_overpull_at_tmin_kips",
    "overall_min_top_tension_kips",
}
_RSU_0014_KEYS = {
    "rsu_id",
    "calc",
    "mud_weight_ppg",
    "n_tensioners",
    "n_fail",
    "dynamic_tension_limit_kips",
    "dtl_setting_fraction",
    "max_tension_setting_kips",
    "tsr_min_kips",
    "api_min_vertical_top_tension_kips",
    "recoil_margin_kips",
    "overall_min_top_tension_kips",
}
_RSU_0023_KEYS = {
    "rsu_id",
    "calc",
    "mud_weight_ppg",
    "n_tensioners",
    "n_fail",
    "ws_fwt_kips",
    "bn_fbt_kips",
    "internal_fluid_term_kips",
    "tsr_min_kips",
    "tmin_vert_kips",
    "bop_overpull_at_tmin_kips",
    "min_bop_overpull_kips",
    "additional_pull_for_min_overpull_kips",
    "recoil_overpull_kips",
    "overall_min_top_tension_kips",
    "mud_weight_sweep",
}
_RSU_0077_SHALLOW_KEYS = {
    "rsu_id",
    "calc",
    "units",
    "mud_weight_ppg",
    "n_tensioners",
    "governing_case",
    "governing_tension_to_apply_kips",
    "cases",
    "reproduction",
}


def _calc_contract_for(rsu_id: str, calc: str, required_keys: set[str]) -> dict:
    root = resolve_wiki_root()
    if root is None:
        pytest.skip(
            "llm-wiki registry not available (standalone mode) — the 16Q "
            "goldens are part of the in-context merge gate"
        )
    contract = find_contract(rsu_id, calc, root)
    if contract is None:
        pytest.fail(
            f"llm-wiki found but no {calc} contract for {rsu_id} — the "
            "paired llm-wiki calc-contract change is not on this clone"
        )
    missing = required_keys - set(contract)
    assert not missing, f"{rsu_id} contract schema drift — missing: {missing}"
    return contract


def _calc_contract(rsu_id: str, required_keys: set[str]) -> dict:
    return _calc_contract_for(rsu_id, _CALC, required_keys)


def _assert_golden(result: float, documented: float) -> None:
    assert abs(result - documented) <= 0.5  # contract unit (t or kips)
    assert abs(result - documented) / documented <= 0.0015


# -- RSU-0007 (registry page, #1280) ---------------------------------------------------


def test_16q_min_top_tension_golden():
    c = _calc_contract("RSU-0007", _RSU_0007_KEYS)
    factored_net_t = c["factored_submerged_steel_t"] + c["factored_buoyancy_t"]
    system = tensioner_system_factor(c["n_wires"], c["n_fail"], c["efficiency"])
    result_t = minimum_top_tension_16q(
        factored_net_t,
        n_units=c["n_wires"],
        n_fail=c["n_fail"],
        efficiency=c["efficiency"],
        fleet_angle_factor=c["fleet_angle_factor"],
    )
    _assert_golden(result_t, c["min_top_tension_t"])
    # The chain's own consistency: system factor from the contract's inputs
    # must equal the composition the engine applied.
    assert result_t == pytest.approx(factored_net_t * system * c["fleet_angle_factor"])


def test_setting_side_values_present_but_not_asserted():
    """The contract carries setting-side values as DATA (composition not yet
    reconciled against the underlying workbook — explicitly out of #1280
    scope). This test only pins their presence so the contract can't silently
    shed them before the follow-up slice."""
    c = _calc_contract("RSU-0007", _RSU_0007_KEYS)
    assert c["connector_pull_t"] > 0
    assert c["tension_setting_t"] > c["min_top_tension_t"]


# -- wave 2 (#1453): min vertical top tension, TSRmin * N/(N-n) ------------------------


@pytest.mark.parametrize(
    ("rsu_id", "keys", "tsr_key", "n_key", "tmin_key"),
    [
        (
            "RSU-0010",
            _RSU_31245_KEYS,
            "min_slip_ring_tension_kips",
            "n_wires",
            "min_vertical_top_tension_kips",
        ),
        (
            "RSU-0012",
            _RSU_31245_KEYS,
            "min_slip_ring_tension_kips",
            "n_wires",
            "min_vertical_top_tension_kips",
        ),
        (
            "RSU-0014",
            _RSU_0014_KEYS,
            "tsr_min_kips",
            "n_tensioners",
            "api_min_vertical_top_tension_kips",
        ),
        (
            "RSU-0023",
            _RSU_0023_KEYS,
            "tsr_min_kips",
            "n_tensioners",
            "tmin_vert_kips",
        ),
    ],
)
def test_16q_wave2_min_vertical_top_tension_golden(
    rsu_id, keys, tsr_key, n_key, tmin_key
):
    """Tmin_vert = TSRmin * N/(N-n) with neutral Rf/fleet-angle (the
    documented workbook convention for these four contracts)."""
    c = _calc_contract(rsu_id, keys)
    result_kips = minimum_top_tension_16q(
        c[tsr_key],
        n_units=c[n_key],
        n_fail=c["n_fail"],
        efficiency=1.0,
        fleet_angle_factor=1.0,
    )
    _assert_golden(result_kips, c[tmin_key])


def test_16q_wave2_overall_min_top_tension_golden():
    """Overall minimum = Tmin_vert + shortfall-to-minimum-BOP-overpull +
    recoil margin, against each contract's documented overall value.

    ENGINE GAP (documented, not tolerance-masked): the overpull/recoil
    composition is contract arithmetic here — ``assembly`` has no
    overpull-margin helper yet (candidate follow-up under #1279). The
    engine-side step (TSRmin -> Tmin_vert) is asserted in
    ``test_16q_wave2_min_vertical_top_tension_golden``.
    """
    cases = [
        # (rsu_id, keys, tmin_key, overpull-at-Tmin key, min-overpull key,
        #  recoil key) — None keys: term absent from that workbook's chain.
        (
            "RSU-0010",
            _RSU_31245_KEYS,
            "min_vertical_top_tension_kips",
            "bop_overpull_at_tmin_kips",
            "min_bop_overpull_kips",
            "recoil_margin_kips",
        ),
        (
            "RSU-0012",
            _RSU_31245_KEYS,
            "min_vertical_top_tension_kips",
            "bop_overpull_at_tmin_kips",
            "min_bop_overpull_kips",
            "recoil_margin_kips",
        ),
        (
            "RSU-0014",
            _RSU_0014_KEYS,
            "api_min_vertical_top_tension_kips",
            None,
            None,
            "recoil_margin_kips",
        ),
        (
            "RSU-0023",
            _RSU_0023_KEYS,
            "tmin_vert_kips",
            "bop_overpull_at_tmin_kips",
            "min_bop_overpull_kips",
            "recoil_overpull_kips",
        ),
    ]
    for rsu_id, keys, tmin_key, at_tmin_key, min_key, recoil_key in cases:
        c = _calc_contract(rsu_id, keys)
        shortfall = max(0.0, c[min_key] - c[at_tmin_key]) if at_tmin_key else 0.0
        overall = c[tmin_key] + shortfall + c[recoil_key]
        _assert_golden(overall, c["overall_min_top_tension_kips"])


def test_16q_rsu0023_tsrmin_composition_golden():
    """TSRmin = Ws*fwt - Bn*fbt + Ai*(dm*Hm - dw*Hw), exercised through
    ``minimum_slip_ring_tension`` with the contract's pre-factored line
    values (f_wt = f_bt = 1.0; the fluid term enters as a unit-column
    density). Tolerance 0.05 kips: three 2-dp-rounded addends."""
    c = _calc_contract("RSU-0023", _RSU_0023_KEYS)
    result_kips = minimum_slip_ring_tension(
        submerged_weight_kn=c["ws_fwt_kips"],
        buoyancy_uplift_kn=c["bn_fbt_kips"],
        internal_area_m2=1.0,
        mud_density_kn_m3=c["internal_fluid_term_kips"],
        mud_column_m=1.0,
        seawater_density_kn_m3=0.0,
        seawater_column_m=0.0,
        f_wt=1.0,
        f_bt=1.0,
    )
    assert abs(result_kips - c["tsr_min_kips"]) <= 0.05


def test_16q_rsu0023_mud_weight_sweep_golden():
    """All 21 sweep rows: api_min = tsr_min * 16/14 (Rf = 1.0) through the
    engine; applied tension >= api_min + recoil (the applied column adds the
    mud-weight-dependent BOP-overpull shortfall, sheet-side arithmetic —
    only its floor is asserted)."""
    c = _calc_contract("RSU-0023", _RSU_0023_KEYS)
    rows = c["mud_weight_sweep"]["rows"]
    assert len(rows) == 21
    recoil = c["recoil_overpull_kips"]
    previous_ppg = 0.0
    for ppg, tsr_min, api_min, applied in rows:
        assert ppg > previous_ppg
        previous_ppg = ppg
        result = minimum_top_tension_16q(
            tsr_min,
            n_units=c["n_tensioners"],
            n_fail=c["n_fail"],
            efficiency=1.0,
            fleet_angle_factor=1.0,
        )
        _assert_golden(result, api_min)
        assert applied >= api_min + recoil - 0.01


def test_16q_rsu0014_setting_side_consistency():
    """RSU-0014 setting-side DATA: max tension setting = DTL x setting
    fraction. The overpull ladder is pinned as data only (its top-tension
    column is dynamic-analysis output, not the closed-form 16Q chain)."""
    c = _calc_contract("RSU-0014", _RSU_0014_KEYS)
    assert c["max_tension_setting_kips"] == pytest.approx(
        c["dynamic_tension_limit_kips"] * c["dtl_setting_fraction"]
    )
    ladder = c.get("overpull_ladder", [])
    assert len(ladder) >= 2
    for (op_a, top_a), (op_b, top_b) in zip(ladder, ladder[1:]):
        assert top_b - top_a == pytest.approx(op_b - op_a, abs=0.11)


# -- wave 6 (#1470): shallow-water tension-to-rotate workbook -------------------------


def test_rsu0077_shallow_water_tension_to_rotate_contract_schema():
    """RSU-0077 is not a fabricated 16Q chain: the private source workbook
    carries a shallow-water tension-to-rotate case table. The contract pins
    only machine-readable schema and internal consistency here; the workbook
    values stay wiki-side."""
    c = _calc_contract_for("RSU-0077", _SHALLOW_WATER_CALC, _RSU_0077_SHALLOW_KEYS)
    assert c["units"] == "kips"
    assert c["n_tensioners"] > 0
    cases = c["cases"]
    assert cases
    case_ids = {case["case"] for case in cases}
    assert len(case_ids) == len(cases)
    assert c["governing_case"] in case_ids
    governing = next(case for case in cases if case["case"] == c["governing_case"])
    assert c["governing_tension_to_apply_kips"] == pytest.approx(
        governing["op_tension_to_apply_kips"]
    )
    assert all(case["op_tension_to_apply_kips"] > 0 for case in cases)
