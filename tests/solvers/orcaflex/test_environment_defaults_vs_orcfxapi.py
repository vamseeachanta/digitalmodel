"""OQ-3 verification: ``environment_builder._DEFAULTS`` vs OrcFxAPI blank-model
defaults (#515 iter 6/7).

OQ-3 from ``SEMANTIC_DIFF_TAXONOMY.md`` §7 asks whether the 21 hardcoded
defaults in ``environment_builder._DEFAULTS`` match OrcaFlex's own defaults on
a blank model. If they don't, every monolithic model that uses OrcaFlex's
real default for one of these properties will see the generator silently
substitute the hardcoded value — a *silent physics change* and the largest
remaining risk on the §5.3 silent-substitution register in
``SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md``.

This test scaffolds the verification. It requires the OrcFxAPI Python
extension (Windows + licensed OrcaFlex installation) and is **skipped on
unlicensed machines** via ``pytest.importorskip("OrcFxAPI")``. On licensed
machines it must be opted in via the ``solver`` marker (deselect with
``-m "not solver"``).

Failure modes (when run on licensed-win-1):
  * If a default differs and is **legitimate** (e.g. the generator
    intentionally picks SI-pure for a property OrcFxAPI defaults to imperial),
    add the property to ``_EXPECTED_OVERRIDES`` below with a comment citing
    the rationale.
  * If a default differs and is **not** legitimate, fix ``_DEFAULTS`` in
    ``environment_builder.py``.

Disposition: SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md §5.1 OQ-3 row.
"""

from __future__ import annotations

from typing import Any

import pytest

# Skip the entire module on machines without OrcFxAPI. On the licensed
# machine, the import succeeds and the suite runs.
OrcFxAPI = pytest.importorskip(
    "OrcFxAPI",
    reason="OrcFxAPI not installed (Windows + licensed OrcaFlex required); "
    "OQ-3 verification skipped — see #515 iter 6/7 and machine:licensed-win-1.",
)

from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
    EnvironmentBuilder,
)


# Properties whose ``_DEFAULTS`` value is intentionally different from
# OrcFxAPI's own blank-model default. Populate this dict on the first
# licensed-win-1 run if any legitimate-override cases surface; each entry
# MUST carry a rationale comment.
_EXPECTED_OVERRIDES: dict[str, str] = {
    # Example shape (populate empirically on first licensed run):
    # "PropertyName": "Reason for intentional deviation from OrcFxAPI default",
}


pytestmark = pytest.mark.solver


@pytest.fixture(scope="module")
def blank_environment() -> Any:
    """OrcFxAPI Environment object on an empty model — the source of truth
    for 'OrcaFlex's own defaults'."""
    model = OrcFxAPI.Model()
    return model.environment


def _bool_compatible(default_val: Any, orcfx_val: Any) -> bool:
    """Apply the OQ-4 bool ↔ Yes/No normalization at the comparison layer."""
    yes_no = {"Yes": True, "No": False}
    if isinstance(default_val, str) and default_val in yes_no:
        default_val = yes_no[default_val]
    if isinstance(orcfx_val, str) and orcfx_val in yes_no:
        orcfx_val = yes_no[orcfx_val]
    return default_val == orcfx_val


class TestEnvironmentDefaultsAgainstOrcFxAPI:
    """Each ``_DEFAULTS`` entry must equal the OrcFxAPI blank-model default
    OR appear in ``_EXPECTED_OVERRIDES`` with a rationale."""

    @pytest.mark.parametrize("prop_name", sorted(EnvironmentBuilder._DEFAULTS.keys()))
    def test_default_matches_or_is_documented_override(
        self, prop_name: str, blank_environment: Any
    ) -> None:
        default_val = EnvironmentBuilder._DEFAULTS[prop_name]
        if default_val is None:
            # Sentinel for "use OrcFxAPI default" — by construction can't mismatch.
            return

        # Some properties may not exist on the blank Environment (e.g. only
        # surface when a parent mode toggles). Treat missing-as-attribute as
        # an automatic documented-override case rather than a failure.
        try:
            orcfx_val = getattr(blank_environment, prop_name)
        except (AttributeError, OrcFxAPI.OrcaFlexError) as exc:
            pytest.skip(
                f"{prop_name!r} not accessible on blank Environment "
                f"(likely mode-gated): {exc}"
            )
            return

        if _bool_compatible(default_val, orcfx_val):
            return

        if prop_name in _EXPECTED_OVERRIDES:
            # Legitimate override — registered with rationale. Test passes
            # but the rationale carries the audit trail.
            return

        pytest.fail(
            f"{prop_name!r} _DEFAULTS={default_val!r} but OrcFxAPI blank "
            f"model has {orcfx_val!r}. Either fix environment_builder."
            f"_DEFAULTS to match OrcFxAPI, or add {prop_name!r} to "
            f"_EXPECTED_OVERRIDES in this test with a rationale comment."
        )


class TestNoUndocumentedDefaultDrift:
    """Whitelist length sanity: every override entry MUST have a non-empty
    rationale string."""

    def test_expected_overrides_have_rationales(self) -> None:
        for name, rationale in _EXPECTED_OVERRIDES.items():
            assert rationale.strip(), (
                f"_EXPECTED_OVERRIDES[{name!r}] has empty rationale — every "
                f"intentional override must document why it differs from "
                f"OrcFxAPI's own default"
            )

    def test_expected_overrides_keys_are_in_defaults(self) -> None:
        """If an override entry references a property not in _DEFAULTS, the
        override is stale (the underlying default was likely renamed or
        removed) and must be cleaned up."""
        for name in _EXPECTED_OVERRIDES:
            assert name in EnvironmentBuilder._DEFAULTS, (
                f"_EXPECTED_OVERRIDES[{name!r}] is not in "
                f"EnvironmentBuilder._DEFAULTS — remove the stale override "
                f"entry or restore the default"
            )
