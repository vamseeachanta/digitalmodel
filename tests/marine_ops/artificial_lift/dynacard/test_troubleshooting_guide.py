# ABOUTME: Tests for the dynacard troubleshooting guide catalog.
# ABOUTME: Checks classifier-mode coverage, entry quality, and severity mapping.

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.troubleshooting import (
    TROUBLESHOOTING_GUIDE,
    TroubleshootingEntry,
    guide_for,
)
from digitalmodel.marine_ops.artificial_lift.field_health import _health_status

VALID_SEVERITIES = {"normal", "warning", "critical", "failure"}
LEGACY_ALIASES = {"VALVE_LEAK"}

ARCHIVE_LABELS = {
    "FULL_CARD",
    "INCOMPLETE_FILLAGE",
    "FILLAGE_COLLAPSE",
    "BUTTERFLY",
    "PUMPED_OFF",
}
DECK_PHENOMENA = {
    "FLUMPING",
    "DEVIATED_WELL_FRICTION",
    "HIGH_FLUID_LEVEL",
    "HEAVY_OIL",
    "BARREL_HOLE_SPLIT",
}


def _classifier_modes() -> set[str]:
    return set(PumpDiagnostics.FAILURE_MODES) - LEGACY_ALIASES


def test_all_classifier_modes_covered():
    missing = _classifier_modes() - set(TROUBLESHOOTING_GUIDE)
    assert not missing, f"Classifier modes missing from guide: {sorted(missing)}"
    assert len(_classifier_modes()) == 18


def test_archive_and_deck_phenomena_covered():
    missing = (ARCHIVE_LABELS | DECK_PHENOMENA) - set(TROUBLESHOOTING_GUIDE)
    assert not missing, f"Extra phenomena missing from guide: {sorted(missing)}"


def test_no_legacy_alias_in_guide():
    assert LEGACY_ALIASES.isdisjoint(TROUBLESHOOTING_GUIDE)


@pytest.mark.parametrize("key", sorted(TROUBLESHOOTING_GUIDE))
def test_entry_content_quality(key):
    entry = TROUBLESHOOTING_GUIDE[key]
    assert isinstance(entry, TroubleshootingEntry)
    assert entry.phenomenon == key
    assert entry.title.strip()
    assert entry.symptom.strip()
    assert entry.mechanism.strip()
    assert 3 <= len(entry.actions) <= 4
    assert all(action.strip() for action in entry.actions)
    assert entry.severity in VALID_SEVERITIES


def test_severity_matches_field_health_for_classifier_modes():
    for mode in _classifier_modes():
        expected = _health_status(mode)
        assert TROUBLESHOOTING_GUIDE[mode].severity == expected, (
            f"{mode}: guide severity {TROUBLESHOOTING_GUIDE[mode].severity!r} "
            f"!= field_health {expected!r}"
        )


def test_guide_for_returns_entry():
    entry = guide_for("GAS_INTERFERENCE")
    assert isinstance(entry, TroubleshootingEntry)
    assert entry.phenomenon == "GAS_INTERFERENCE"
    assert entry.severity == "warning"


def test_guide_for_raises_helpful_keyerror():
    with pytest.raises(KeyError, match="Valid keys:"):
        guide_for("NOT_A_PHENOMENON")
    with pytest.raises(KeyError, match="GAS_INTERFERENCE"):
        guide_for("NOT_A_PHENOMENON")
