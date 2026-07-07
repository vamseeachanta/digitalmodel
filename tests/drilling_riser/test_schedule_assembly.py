"""#1458 schedule-assembly goldens: string weight assembled FROM the wiki
joint schedules + joint libraries, validated against the wave-3
``16q-min-tension-endpoints`` contracts (llm-wiki#828).

dm carries only page/key names, parse shapes and the engine; the schedule
counts, joint weights and documented tensions live wiki-side and resolve at
test time via ``digitalmodel.drilling_riser.schedule_assembly``. Loud skip
standalone (same discipline as ``test_assembly_golden``).

Validation stance (issue #1458 scope 2 — tolerances stated per RSU, misses
are FINDINGS, never masked by loosening):

* RSU-0019 — PASS at 3 %: the assembled chain reproduces all three Sec 5.2
  endpoints (observed max |delta| ~1.7 %). Tolerance basis: the documented
  assumption stack (pup/termination joints at the slick per-ft rate,
  flexjoint/outer-barrel submerged weight from steel displacement, seawater
  at 1025 kg/m3, mud in main bore + C&K + boost) each contribute <=~1 %.
* RSU-0021 — FINDING: the sister-well formulation that passes RSU-0019
  under-predicts all three Table 2.2 endpoints by 20-40 % (observed
  -35/-27/-23 %). The contract's own figure caveats (tension-ring datum,
  fleet angle excluded) cannot explain that magnitude. Adding the documented
  LMRP submerged weight brings all three within ~6 % — recorded here as a
  quantified hypothesis, NOT adopted (undocumented in the source).
* RSU-0040 — FINDING: the SES "API 16Q stability" 15-bare sweep is
  under-predicted by 3-15 % (largest at light mud). BUT the documented
  15-bare minus 5-bare offset equals the UNfactored 10-joint
  slick-for-5000ft-class swap from the RSU-0039 library to <1 % — the
  schedule + library assembly itself is validated; the SES line carries
  additional (undocumented) dead load / a different factor treatment.
* RSU-0038 — no schedule-assembly: the joint counts are raster-only in the
  source (wiki page caveat), so the loader must fail closed.
"""

from __future__ import annotations

import pytest

from digitalmodel.drilling_riser.calc_contracts import resolve_wiki_root
from digitalmodel.drilling_riser.schedule_assembly import (
    SCHEDULE_RSU_IDS,
    ScheduleAssemblyError,
    documented_endpoints,
    load_schedule_assembly,
)


def _wiki_root():
    root = resolve_wiki_root()
    if root is None:
        pytest.skip(
            "llm-wiki registry not available (standalone mode) — the "
            "schedule-assembly goldens are part of the in-context merge gate"
        )
    return root


def _deltas(rsu_id: str, *, variant: str = "primary") -> list[float]:
    """Relative (computed - documented) / documented per endpoint row."""
    root = _wiki_root()
    assembly = load_schedule_assembly(rsu_id, root)
    rows = documented_endpoints(rsu_id, root, variant=variant)
    assert len(rows) >= 3
    return [
        (assembly.minimum_top_tension_16q_kn(mud_kg_m3) - documented_kn) / documented_kn
        for mud_kg_m3, documented_kn in rows
    ]


# -- loader structure / fail-closed ----------------------------------------------------


@pytest.mark.parametrize("rsu_id", sorted(SCHEDULE_RSU_IDS))
def test_schedule_assembly_structure(rsu_id):
    """Every schedule-assembly RSU yields a physically coherent string:
    positive dry weight, net submerged below dry, positive net buoyancy lift
    (all three strings carry buoyed joints), and a mud column longer than
    the seawater column (drill floor above waterline)."""
    assembly = load_schedule_assembly(rsu_id, _wiki_root())
    assert len(assembly.model.items) >= 5
    dry = assembly.model.total_dry_weight_kn()
    net = assembly.model.total_submerged_weight_kn()
    assert 0.0 < net < dry
    assert assembly.buoyancy_net_lift_kn > 0.0
    geo = assembly.geometry
    assert geo.mud_column_m > geo.seawater_column_m > 0.0
    assert geo.internal_area_m2 > 0.0


def test_rsu0038_schedule_is_raster_only():
    """RSU-0038's joint counts are raster-only in the source — the loader
    must fail closed, never guess a schedule."""
    with pytest.raises(ScheduleAssemblyError, match="RSU-0038"):
        load_schedule_assembly("RSU-0038", _wiki_root())


def test_unknown_rsu_fails_closed():
    with pytest.raises(ScheduleAssemblyError, match="RSU-9999"):
        load_schedule_assembly("RSU-9999", _wiki_root())


def test_missing_wiki_page_fails_closed(tmp_path):
    with pytest.raises(ScheduleAssemblyError, match="missing"):
        load_schedule_assembly("RSU-0019", tmp_path)


def test_tension_is_mud_linear_increasing():
    """The 16Q chain is linear and increasing in mud density (fixed
    geometry) — a slope/sign regression fails loudly here."""
    assembly = load_schedule_assembly("RSU-0019", _wiki_root())
    t1 = assembly.minimum_top_tension_16q_kn(1000.0)
    t2 = assembly.minimum_top_tension_16q_kn(1500.0)
    t3 = assembly.minimum_top_tension_16q_kn(2000.0)
    assert t1 < t2 < t3
    assert (t3 - t2) == pytest.approx(t2 - t1, rel=1e-9)


# -- RSU-0019: endpoint validation PASS at 3 % ------------------------------------------


def test_rsu0019_endpoints_within_3_percent():
    deltas = _deltas("RSU-0019")
    assert max(abs(d) for d in deltas) <= 0.03, deltas


# -- RSU-0021: documented FINDING (delta band pinned, not masked) -----------------------


def test_rsu0021_endpoints_finding_band():
    """Characterization: all endpoints under-predicted by 20-40 %. If this
    band ever shifts (physics or wiki-side contract change), re-investigate
    rather than silently re-baseline."""
    deltas = _deltas("RSU-0021")
    assert all(-0.40 < d < -0.20 for d in deltas), deltas


def test_rsu0021_lmrp_inclusion_hypothesis():
    """Quantified hypothesis (NOT adopted): adding the documented LMRP
    submerged weight to the tensioned string brings all three endpoints
    within ~6 %."""
    root = _wiki_root()
    assembly = load_schedule_assembly("RSU-0021", root)
    lmrp_kn = assembly.lmrp_submerged_kn
    assert lmrp_kn is not None and lmrp_kn > 0.0
    factor = assembly.top_tension_factor
    for mud_kg_m3, documented_kn in documented_endpoints("RSU-0021", root):
        adjusted = assembly.minimum_top_tension_16q_kn(mud_kg_m3) + factor * lmrp_kn
        assert abs(adjusted - documented_kn) / documented_kn < 0.06


# -- RSU-0040: documented FINDING + positive swap-consistency validation ----------------


def test_rsu0040_endpoints_finding_band():
    """Characterization of the 15-bare SES stability sweep: under-predicted
    by 3-15 %, monotonically shrinking with mud weight (constant absolute
    weight-side shortfall + slightly steeper model mud slope)."""
    deltas = _deltas("RSU-0040")
    assert all(-0.15 < d < -0.03 for d in deltas), deltas
    # shrinking magnitude with increasing mud weight
    magnitudes = [abs(d) for d in deltas]
    assert magnitudes == sorted(magnitudes, reverse=True)


def test_rsu0040_five_bare_swap_consistency():
    """Positive validation of the schedule + RSU-0039 library: the
    documented 15-bare minus 5-bare tension offset equals the UNfactored
    10-joint slick-for-5000ft-class swap to <1 % on every sweep row."""
    root = _wiki_root()
    assembly = load_schedule_assembly("RSU-0040", root)
    items = {item.component_id: item.component for item in assembly.model.items}
    slick = items["RSU-0040/slick-joint"]
    b5000 = items["RSU-0040/buoyancy-joint-5000ft"]
    swap_kn = 10.0 * (
        float(slick["submerged_weight_kn"]) - float(b5000["submerged_weight_kn"])
    )
    rows_15 = documented_endpoints("RSU-0040", root)
    rows_5 = documented_endpoints("RSU-0040", root, variant="5-bare")
    assert len(rows_15) == len(rows_5) == 9
    for (mud_15, t15), (mud_5, t5) in zip(rows_15, rows_5):
        assert mud_15 == mud_5
        assert abs((t15 - t5) - swap_kn) / swap_kn < 0.01
