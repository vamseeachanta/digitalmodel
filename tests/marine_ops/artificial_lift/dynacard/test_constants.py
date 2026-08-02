# ABOUTME: Tests for dynacard constants that must stay consistent with code.
# ABOUTME: Guards N_FAILURE_MODES against drifting from the classifier's modes.

import json
from pathlib import Path

from digitalmodel.marine_ops.artificial_lift.dynacard import constants
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)

_MODEL_PATH = (
    Path(constants.__file__).parent / "data" / "dynacard_classifier.json"
)


class TestFailureModeCount:
    """Tests for N_FAILURE_MODES (issue #1952 D2).

    The constant was hardcoded to 18 while ``PumpDiagnostics.FAILURE_MODES``
    held 22 entries -- 20 predictable modes plus 2 retired aliases kept only
    so archived results carrying an old label still resolve to a description.
    It must be derived, not restated, so it cannot drift again.
    """

    def test_retired_aliases_are_present_in_failure_modes(self):
        """The exclusion list must name keys that actually exist.

        If a retired alias is finally deleted from FAILURE_MODES, this fails
        rather than letting the exclusion silently under-count.
        """
        assert constants.RETIRED_FAILURE_MODE_ALIASES
        assert constants.RETIRED_FAILURE_MODE_ALIASES <= set(
            PumpDiagnostics.FAILURE_MODES
        )

    def test_count_excludes_retired_aliases(self):
        """N_FAILURE_MODES must be derived from FAILURE_MODES, minus aliases."""
        expected = len(
            set(PumpDiagnostics.FAILURE_MODES)
            - constants.RETIRED_FAILURE_MODE_ALIASES
        )

        assert constants.N_FAILURE_MODES == expected

    def test_count_matches_shipped_classifier(self):
        """The count must match what the trained model can actually predict.

        This is the semantic anchor: N_FAILURE_MODES documents the ML
        classifier, and the shipped model is a 20-class model whose labels
        are exactly FAILURE_MODES minus the retired aliases.
        """
        model = json.loads(_MODEL_PATH.read_text())

        assert constants.N_FAILURE_MODES == model["n_classes"]
        assert set(model["class_labels"]) == (
            set(PumpDiagnostics.FAILURE_MODES)
            - constants.RETIRED_FAILURE_MODE_ALIASES
        )

    def test_count_is_twenty(self):
        """Canary: adding or removing a mode must be a deliberate change."""
        assert constants.N_FAILURE_MODES == 20

    def test_count_importable_directly(self):
        """`from ... import N_FAILURE_MODES` must work despite lazy derivation."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.constants import (
            N_FAILURE_MODES,
        )

        assert N_FAILURE_MODES == constants.N_FAILURE_MODES

    def test_count_is_discoverable_via_dir(self):
        """The lazily-derived name must still show up in dir(constants)."""
        assert "N_FAILURE_MODES" in dir(constants)
