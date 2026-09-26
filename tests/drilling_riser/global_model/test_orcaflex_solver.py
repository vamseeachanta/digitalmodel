"""Solver round trip on the synthetic riser (needs OrcFxAPI and a licence)."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex import orcaflex_api

pytestmark = [
    pytest.mark.solver,
    pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available"),
]


def test_statics_tension_and_modes_agree_with_hand_checks(tmp_path: Path, spec):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.hand_checks import (
        reference_periods,
        tension_references,
    )
    from digitalmodel.drilling_riser.global_model.orcaflex_run import (
        end_effective_tensions,
        load_and_solve_statics,
        riser_modal_periods,
        ring_vertical_residual_n,
        tensioner_vertical_sum_n,
    )

    out = write_model(spec, tmp_path)
    model = load_and_solve_statics(out / "master.yml")
    te = end_effective_tensions(model)
    ref = tension_references(spec)
    assert tensioner_vertical_sum_n(model) == pytest.approx(spec.tensioners.total_vertical_tension_n, rel=2e-3)
    assert abs(ring_vertical_residual_n(model, ref["ring_weight_n"])) < 1e-3 * spec.tensioners.total_vertical_tension_n
    assert te["riser_top_n"] == pytest.approx(ref["riser_top_n"], rel=5e-3)
    assert te["riser_bottom_n"] == pytest.approx(ref["riser_bottom_n"], rel=5e-3)
    assert te["riser_top_n"] - te["stack_bottom_n"] == pytest.approx(ref["submerged_weight_n"], rel=5e-3)

    modes = riser_modal_periods(model, n_modes=3)
    hand = reference_periods(spec, n_modes=3)
    for m, h in zip(modes, hand):
        assert m["period_s"] == pytest.approx(h, rel=0.05)


def test_as_analysed_sections_read_back_the_spec(tmp_path: Path, spec):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import (
        as_analysed_sections,
        load_and_solve_statics,
        model_files_sha256,
    )

    out = write_model(spec, tmp_path)
    model = load_and_solve_statics(out / "master.yml")
    rows = as_analysed_sections(model)
    by_name = {r["section"]: r for r in rows}
    assert len(rows) == len(spec.inner_barrel) + len(spec.riser) + len(spec.stack)
    for s in (*spec.inner_barrel, *spec.riser, *spec.stack):
        r = by_name[s.name]
        assert r["mass_dry_kg_m"] == pytest.approx(s.mass_per_m_kg, rel=1e-9)
        assert r["ei_nm2"] == pytest.approx(s.ei_nm2, rel=1e-9)
        assert r["length_m"] == pytest.approx(s.length_m)
    riser_rows = [r for r in rows if r["line"] == "Riser"]
    assert riser_rows[0]["z_top_m"] > riser_rows[-1]["z_bottom_m"]
    digest = model_files_sha256(out / "master.yml")
    assert "master.yml" in digest["files"] and len(digest["combined"]) == 64
