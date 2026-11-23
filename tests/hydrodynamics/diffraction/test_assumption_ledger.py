"""Tests for diffraction assumption ledger provenance."""

from __future__ import annotations

from digitalmodel.hydrodynamics.diffraction.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)


def test_record_and_serialize_round_trip() -> None:
    ledger = AssumptionLedger()
    ledger.record(
        "vessel.inertia.mass",
        123.0,
        AssumptionSource.ESTIMATED_FROM_DATA,
        "test basis",
        Confidence.MEDIUM,
        reference="formula",
        impact=5,
    )

    from_dict = AssumptionLedger.from_dict(ledger.to_dict())
    from_json = AssumptionLedger.from_json(ledger.to_json())

    assert len(from_dict) == 1
    assert len(from_json) == 1
    assert from_json.records[0].field == "vessel.inertia.mass"


def test_user_supplied_records_are_refused() -> None:
    ledger = AssumptionLedger()
    ledger.record(
        "environment.water_depth",
        100.0,
        AssumptionSource.USER_SUPPLIED,
        "from user",
        Confidence.HIGH,
    )

    assert len(ledger) == 0
    assert bool(ledger) is False


def test_empty_ledger_contract_for_detailed_run() -> None:
    ledger = AssumptionLedger()

    assert len(ledger) == 0
    assert bool(ledger) is False
    assert ledger.rows() == []
