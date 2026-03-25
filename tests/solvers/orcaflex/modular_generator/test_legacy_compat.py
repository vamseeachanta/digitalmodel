"""Tests for legacy OrcInstallation config conversion to CampaignSpec."""
from __future__ import annotations

import warnings

import pytest


def _make_legacy_cfg(
    delta_elevations: list[float] | None = None,
    reference_depth: float | None = None,
    base_file: str = "base_model",
    analysis_folder: str = "output",
) -> dict:
    """Build a minimal legacy config dict for testing."""
    structure: dict = {}
    if delta_elevations is not None:
        structure["delta_elevations"] = delta_elevations
    if reference_depth is not None:
        structure["reference_depth"] = reference_depth
    structure["BaseFile"] = base_file
    return {
        "structure": structure,
        "Analysis": {"analysis_root_folder": analysis_folder},
    }


class TestFromLegacyConfigConvertsDeltaElevations:
    def test_from_legacy_config_converts_delta_elevations(self):
        """delta_elevations [0, -5, -10] with reference_depth=20 -> water_depths [20, 25, 30]."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec,
        )

        cfg = _make_legacy_cfg(
            delta_elevations=[0, -5, -10],
            reference_depth=20,
        )
        result = CampaignSpec.from_legacy_config(cfg)
        assert result.campaign.water_depths == [20, 25, 30]


class TestFromLegacyConfigReturnsCampaignSpec:
    def test_from_legacy_config_returns_campaign_spec(self):
        """Result is a CampaignSpec instance."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec,
        )

        cfg = _make_legacy_cfg(delta_elevations=[0, -5], reference_depth=10)
        result = CampaignSpec.from_legacy_config(cfg)
        assert isinstance(result, CampaignSpec)


class TestFromLegacyConfigMissingStructureRaises:
    def test_from_legacy_config_missing_structure_raises(self):
        """cfg without 'structure' key raises ValueError."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec,
        )

        cfg = {"Analysis": {"analysis_root_folder": "output"}}
        with pytest.raises(ValueError, match="structure"):
            CampaignSpec.from_legacy_config(cfg)


class TestFromLegacyConfigMissingDeltaElevationsRaises:
    def test_from_legacy_config_missing_delta_elevations_raises(self):
        """cfg.structure without 'delta_elevations' raises ValueError."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec,
        )

        cfg = {"structure": {"BaseFile": "base"}, "Analysis": {}}
        with pytest.raises(ValueError, match="delta_elevations"):
            CampaignSpec.from_legacy_config(cfg)


class TestFromLegacyConfigOutputNamingHasWaterDepth:
    def test_from_legacy_config_output_naming_has_water_depth(self):
        """output_naming template contains {water_depth}."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            CampaignSpec,
        )

        cfg = _make_legacy_cfg(delta_elevations=[0, -10], reference_depth=15)
        result = CampaignSpec.from_legacy_config(cfg)
        assert "{water_depth}" in result.output_naming


class TestOrcInstallationDeprecationWarning:
    def test_orc_installation_deprecation_warning(self):
        """Instantiating OrcInstallation raises DeprecationWarning."""
        from digitalmodel.solvers.orcaflex.orcaflex_installation import (
            OrcInstallation,
        )

        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            OrcInstallation()
            assert len(caught) == 1
            assert issubclass(caught[0].category, DeprecationWarning)
            assert "CampaignSpec" in str(caught[0].message)
