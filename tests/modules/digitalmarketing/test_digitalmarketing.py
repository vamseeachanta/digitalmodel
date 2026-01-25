"""Tests for DigitalMarketing router class."""

import pytest
from unittest.mock import patch, MagicMock

from digitalmodel.modules.digitalmarketing.digitalmarketing import DigitalMarketing


class TestDigitalMarketing:
    """Test suite for DigitalMarketing router."""

    def test_init(self):
        """Test DigitalMarketing initialization."""
        dm = DigitalMarketing()
        assert dm is not None
        assert dm.logger is not None

    def test_router_seo_analysis(self, seo_config):
        """Test routing to SEO analysis."""
        dm = DigitalMarketing()

        with patch('digitalmodel.modules.digitalmarketing.seo.seo_analysis.SEOAnalysis') as mock_seo:
            mock_instance = MagicMock()
            mock_instance.run.return_value = seo_config
            mock_seo.return_value = mock_instance

            result = dm.router(seo_config)

            mock_seo.assert_called_once()
            mock_instance.run.assert_called_once_with(seo_config)
            assert result is not None

    def test_router_invalid_calculation(self):
        """Test router with invalid calculation name."""
        dm = DigitalMarketing()
        cfg = {"calculation": {"name": "unknown_calculation"}}

        with pytest.raises(ValueError, match="not implemented"):
            dm.router(cfg)

    def test_router_empty_calculation(self):
        """Test router with missing calculation name."""
        dm = DigitalMarketing()
        cfg = {"calculation": {}}

        with pytest.raises(ValueError, match="not implemented"):
            dm.router(cfg)

    def test_router_no_calculation_key(self):
        """Test router with no calculation key."""
        dm = DigitalMarketing()
        cfg = {}

        with pytest.raises(ValueError, match="not implemented"):
            dm.router(cfg)
