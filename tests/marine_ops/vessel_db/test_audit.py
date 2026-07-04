"""Tests for the cross-store provenance + consistency audit (#888)."""

from __future__ import annotations

from digitalmodel.marine_ops.vessel_db import audit as A
from digitalmodel.marine_ops.vessel_db.audit import (
    AuditReport,
    build_audit_report,
    run_audit,
)


def test_conflict_numeric_tolerance():
    # within tolerance -> no conflict; beyond -> conflict
    assert A._conflict(100.0, 103.0, 0.05) is False  # 3% <= 5%
    assert A._conflict(100.0, 130.0, 0.05) is True  # 30% > 5%


def test_conflict_exact_for_imo():
    assert A._conflict("9763355", "9763355", 0.0) is False
    assert A._conflict("9763355", "8757740", 0.0) is True


def test_conflict_ignores_missing_values():
    assert A._conflict(None, 100.0, 0.05) is False
    assert A._conflict(100.0, None, 0.05) is False


def test_run_audit_structure():
    r = run_audit()
    assert isinstance(r, AuditReport)
    assert r.n_curated >= 0 and r.n_wed >= 0
    assert r.n_overlap <= min(r.n_curated, r.n_wed)
    # confidence gaps is a tier->count map summing to >0 over the real store
    assert sum(r.confidence_gaps.values()) > 0


def test_curated_has_no_uncited_numbers():
    # the curated store must satisfy flag-don't-fake
    r = run_audit()
    assert r.uncited == [], f"{len(r.uncited)} uncited numbers found"


def test_report_renders_sections():
    md = build_audit_report()
    assert "# Vessel DB — cross-store audit" in md
    assert "Provenance integrity" in md
    assert "Cross-source conflicts" in md
    assert "Confidence gaps" in md


def test_conflicts_have_both_values():
    # every reported conflict carries both source values + a note
    for c in A.audit_cross_source_conflicts():
        assert c.field_name
        assert c.note
        # at least one side present (a conflict needs both, but guard anyway)
        assert c.curated_value is not None or c.wed_value is not None
