"""System-wide invariants over EVERY committed parametric atlas.

Generalises the hand-picked checks in test_refresh.py to all parametric
workflows, so a newly added atlas that lacks a query handler, drifts, or fails to
interpolate a mid-grid point fails CI here — rather than NotImplementedError-ing
at query time. See docs/plans/parametric-refresh/adding-an-atlas.md.
"""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.parametric import refresh
from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.generate import RESPONSE_FUNCS
from digitalmodel.parametric.query import _HANDLERS


def _rows_by_id() -> dict[str, dict]:
    return {row["id"]: row for row in refresh._registry_rows()}


def _basename(wid: str) -> str:
    return _rows_by_id()[wid]["basename"]


@pytest.mark.parametrize("basename", sorted(RESPONSE_FUNCS))
def test_every_response_evaluates_a_registered_point(basename, tmp_path, monkeypatch):
    # Supply citation metadata as in the watch-circle workflow tests; no
    # standard values or numerical responses are replaced by this fixture.
    page = tmp_path / "wikis/engineering-standards/wiki/standards/dnv-rp-0360.md"
    page.parent.mkdir(parents=True)
    page.write_text("---\ncode_id: DNV-RP-0360\npublisher: DNV\nrevision: 2016\n---\n",
                    encoding="utf-8", newline="\n")
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    row = next(row for row in refresh._registry_rows() if row["basename"] == basename)
    block = row["parametric"]
    point = {axis["name"]: axis.get("values", axis.get("grid"))[0]
             for axis in block["axes"]}
    value = RESPONSE_FUNCS[basename](point, **block.get("response_kwargs", {}))
    assert math.isfinite(value)
    assert value >= 0


@pytest.mark.parametrize("scf", [1.0, 1.15])
def test_riser_response_preserves_reference_damage(scf):
    # DNV-RP-C203 (October 2011), section 2.2 (Miner sum), section 2.4.1
    # and Table 2-2 (F1, seawater with CP). Derive the knee from the first
    # branch, then anchor the second branch there as specified by pyLife.
    # The synthetic reference uses 365.25 days/year, 24 h/day and 3600 s/h.
    # The 22.2 mm wall is below the 25 mm reference: no thickness penalty.
    # All these wave/VIV stresses are below the knee, so N = ND*(SD/S)^5.
    knee_stress = 10 ** ((11.299 - 6.0) / 3.0)
    wave_damage = sum(
        cycles * (stress * scf / knee_stress) ** 5 / 1e6
        for stress, cycles in zip([6, 10, 16, 24, 36], [4e6, 6e5, 45000, 2500, 90])
    )
    fraction_cycles = .42 * .02 * 365.25 * 24 * 3600
    hours_cycles = .55 * 40 * 3600
    viv_damage = (fraction_cycles * (9 / knee_stress) ** 5
                  + hours_cycles * (12 / knee_stress) ** 5) / 1e6
    # Hand sums: sum(n*S^5)/(1e6*SD^5) = 2.41111904190e-4 at SCF=1;
    # SCF=1.15 multiplies the wave term by 1.15^5. VIV is independent of SCF.
    assert wave_damage == pytest.approx({1.0: 2.41111904190e-4, 1.15: 4.84962161484e-4}[scf], rel=1e-11)
    # Hand exposure conversions: .42*.02*31557600 = 265083.84 cycles;
    # .55*40*3600 = 79200 cycles; their Miner sum is 5.21015773766e-5.
    assert fraction_cycles == pytest.approx(265083.84, rel=1e-12)
    assert hours_cycles == pytest.approx(79200.0, rel=1e-12)
    assert viv_damage == pytest.approx(5.21015773766e-5, rel=1e-11)
    value = RESPONSE_FUNCS["riser_fatigue"]({"scf": scf, "viv_stress_scale": 1.0})
    # Relative tolerance covers floating-point evaluation, not rounded outputs.
    assert value == pytest.approx(wave_damage + viv_damage, rel=1e-12)


def test_riser_validation_helper_changes_fingerprint(monkeypatch):
    helper = refresh.REPO_ROOT / "src/digitalmodel/riser_fatigue/validation.py"
    original_read = Path.read_bytes
    before = refresh.content_fingerprint("riser-fatigue")
    mooring_before = refresh.content_fingerprint("mooring-fatigue")
    reads = []

    def changed_bytes(path):
        raw = original_read(path)
        if path == helper:
            reads.append(path)
            return raw + b"\n# synthetic helper change\n"
        return raw

    with monkeypatch.context() as patch:
        patch.setattr(Path, "read_bytes", changed_bytes)
        changed = refresh.content_fingerprint("riser-fatigue")
        assert reads, "validation helper was absent from the source fingerprint"
        assert changed != before
        assert refresh.content_fingerprint("mooring-fatigue") == mooring_before
    assert refresh.content_fingerprint("riser-fatigue") == before


def test_riser_source_evidence_includes_validation_helper():
    from digitalmodel.parametric import historical_provenance as history

    helper = "src/digitalmodel/riser_fatigue/validation.py"
    record = history.source_record("riser-fatigue", history.archive_reader(refresh.REPO_ROOT))
    assert helper in record["basis"]["response_sources"]
    assert record["sources"][helper] == history.sha256((refresh.REPO_ROOT / helper).read_bytes())


def test_some_atlases_are_registered():
    # guards against the discovery itself silently returning nothing
    assert len(refresh.parametric_workflow_ids()) >= 15


def test_every_parametric_atlas_has_a_query_handler():
    """Every workflow that owns an atlas must be serveable by parametric_query."""
    missing = [
        (wid, _basename(wid))
        for wid in refresh.parametric_workflow_ids()
        if _basename(wid) not in _HANDLERS
    ]
    assert not missing, f"parametric atlases without a _HANDLERS entry: {missing}"


def test_every_committed_atlas_is_current():
    """No committed atlas may be stale relative to its build basis."""
    stale = []
    for wid in refresh.parametric_workflow_ids():
        status = refresh.refresh_status(wid)
        if status.get("stale"):
            stale.append((wid, status.get("reason")))
    assert not stale, f"stale committed atlases: {stale}"


def test_every_atlas_interpolates_a_mid_grid_point_in_range():
    """Each committed atlas loads and predicts an interior point in-range — the
    grid is complete and the interpolator is well-formed for every atlas."""
    failures = []
    for wid in refresh.parametric_workflow_ids():
        atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, _basename(wid))
        point = {}
        for axis in atlas.axes:
            if axis.is_categorical:
                point[axis.name] = axis.values[len(axis.values) // 2]
            else:
                point[axis.name] = axis.grid[len(axis.grid) // 2]
        prediction = atlas.predict(point)
        if not prediction.in_range:
            failures.append((wid, prediction.reason))
    assert not failures, f"atlases that failed a mid-grid prediction: {failures}"
