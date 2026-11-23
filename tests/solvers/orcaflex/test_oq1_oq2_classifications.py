"""OQ-1 + OQ-2 classification tests (#515 iter 5/7).

Closes Open Questions OQ-1 and OQ-2 from ``SEMANTIC_DIFF_TAXONOMY.md`` §7 per
the ``SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md`` §5.1 dispositions.

OQ-1 — ``VerticalWindVariationFactor`` is classified **conditional C3**:
        the property is only emitted under spectrum-class WindTypes
        (``API spectrum`` / ``NPD spectrum`` / ``ESDU spectrum``). Absent
        under ``Constant`` and ``Full field`` is C3, not C6.

OQ-2 — ``Groups`` emission is classified **builder-track-conditional C3**:
        generic-track models suppress the section entirely. Pipeline /
        riser tracks emit a derived Groups section from spec-context
        entity names; residual differences vs user-authored monolithic
        Groups land in ``MODEL_CLAIM_REGISTRY.yaml::known_diffs`` per
        family, not as taxonomy-level defects.
"""

from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
    EnvironmentBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.builders.groups_builder import (
    GroupsBuilder,
)


class TestOQ1WindFactorClassification:
    """``VerticalWindVariationFactor`` emission must follow the taxonomy
    §C3 sub-policy table."""

    SPECTRUM_WIND_TYPES = ("API spectrum", "NPD spectrum", "ESDU spectrum")
    NON_SPECTRUM_WIND_TYPES = ("Constant", "Full field")

    @pytest.mark.parametrize("wind_type", SPECTRUM_WIND_TYPES)
    def test_factor_emitted_under_spectrum_wind_types(self, wind_type: str) -> None:
        """Spectrum WindTypes MUST list VerticalWindVariationFactor in
        ``_WIND_TYPE_PROPS`` so it gets emitted. If a monolithic with this
        WindType lacks the property in generated output, the diff is C6,
        not C3."""
        assert wind_type in EnvironmentBuilder._WIND_TYPE_PROPS, (
            f"WindType {wind_type!r} missing from _WIND_TYPE_PROPS"
        )
        props = EnvironmentBuilder._WIND_TYPE_PROPS[wind_type]
        assert "VerticalWindVariationFactor" in props, (
            f"_WIND_TYPE_PROPS[{wind_type!r}] does not include "
            f"VerticalWindVariationFactor — generator will not emit it under "
            f"this WindType, contradicting the OQ-1 taxonomy classification"
        )

    @pytest.mark.parametrize("wind_type", NON_SPECTRUM_WIND_TYPES)
    def test_factor_absent_under_non_spectrum_wind_types(self, wind_type: str) -> None:
        """Non-spectrum WindTypes (Constant, Full field) MUST NOT carry
        VerticalWindVariationFactor in ``_WIND_TYPE_PROPS``. Its absence in
        generated output under these WindTypes is C3 intentional, not C6."""
        props = EnvironmentBuilder._WIND_TYPE_PROPS.get(wind_type, set())
        assert "VerticalWindVariationFactor" not in props, (
            f"_WIND_TYPE_PROPS[{wind_type!r}] unexpectedly carries "
            f"VerticalWindVariationFactor — non-spectrum WindTypes should "
            f"suppress this property per OQ-1 classification"
        )

    def test_full_field_in_wind_speed_dormant(self) -> None:
        """``Full field`` must be in ``_WIND_SPEED_DORMANT`` because WindSpeed
        itself is dormant under full-field wind input. This is a related but
        distinct C3 to the VerticalWindVariationFactor classification."""
        assert "Full field" in EnvironmentBuilder._WIND_SPEED_DORMANT


class TestOQ2GroupsPolicy:
    """``Groups`` emission must follow the taxonomy §C3 sub-policy table."""

    def test_generic_track_suppresses_groups(self) -> None:
        """Generic-track models suppress Groups entirely — C3 intentional."""
        spec = _FakeSpec(is_generic=True, is_pipeline=False, is_riser=False)
        builder = GroupsBuilder.__new__(GroupsBuilder)
        builder.spec = spec  # type: ignore[attr-defined]
        assert builder.should_generate() is False, (
            "Generic-track spec should not generate Groups; contradicts OQ-2 "
            "taxonomy classification"
        )

    def test_pipeline_track_emits_groups(self) -> None:
        """Pipeline-track models emit derived Groups."""
        spec = _FakeSpec(is_generic=False, is_pipeline=True, is_riser=False)
        builder = GroupsBuilder.__new__(GroupsBuilder)
        builder.spec = spec  # type: ignore[attr-defined]
        assert builder.should_generate() is True

    def test_riser_track_emits_groups(self) -> None:
        """Riser-track models emit derived Groups."""
        spec = _FakeSpec(is_generic=False, is_pipeline=False, is_riser=True)
        builder = GroupsBuilder.__new__(GroupsBuilder)
        builder.spec = spec  # type: ignore[attr-defined]
        assert builder.should_generate() is True


class _FakeSpec:
    """Minimal spec stub satisfying GroupsBuilder.should_generate()."""

    def __init__(self, *, is_generic: bool, is_pipeline: bool, is_riser: bool):
        self._is_generic = is_generic
        self._is_pipeline = is_pipeline
        self._is_riser = is_riser

    def is_generic(self) -> bool:
        return self._is_generic

    def is_pipeline(self) -> bool:
        return self._is_pipeline

    def is_riser(self) -> bool:
        return self._is_riser
