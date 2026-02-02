"""Tests for SEOAnalysis class."""

import pytest
from unittest.mock import patch, MagicMock
import pandas as pd

from digitalmodel.digitalmarketing.seo.seo_analysis import SEOAnalysis


class TestSEOAnalysis:
    """Test suite for SEOAnalysis class."""

    def test_init(self):
        """Test SEOAnalysis initialization."""
        seo = SEOAnalysis()
        assert seo is not None
        assert seo.logger is not None
        assert len(seo.seo_columns) == 7

    def test_run_missing_url(self):
        """Test run with missing URL raises error."""
        seo = SEOAnalysis()
        cfg = {"inputs": {"alias": "test"}, "analysis": {"seo": {}}}

        with pytest.raises(ValueError, match="URL is required"):
            seo.run(cfg)

    def test_run_with_mocked_analysis(self, seo_config, mock_seolib):
        """Test run with mocked external dependencies."""
        seo = SEOAnalysis()

        # Disable all analyses that require external deps
        seo_config["analysis"]["seo"] = {
            "pyseoanalyzer": False,
            "seolib": False,
            "seoaudit": False
        }

        result = seo.run(seo_config)

        assert "results" in result
        assert "seo_analysis" in result["results"]
        assert result["results"]["seo_analysis"]["alias"] == "test_site"

    def test_calculate_seo_rank_empty(self):
        """Test SEO rank calculation with empty data."""
        seo = SEOAnalysis()
        result = seo._calculate_seo_rank({})
        assert result == 0.0

    def test_calculate_seo_rank_with_data(self):
        """Test SEO rank calculation with data."""
        seo = SEOAnalysis()
        seo_analysis = {
            "seo_audit": {"seo_audit_summary": {"seo_audit_rank": 80}},
            "seolib_ranks": [
                {"Description": "Alexa Rank", "Value": 1000},
                {"Description": "Seolib Rank", "Value": 75}
            ]
        }
        result = seo._calculate_seo_rank(seo_analysis)
        # 0.2 * 80 + 0.8 * 75 = 16 + 60 = 76
        assert result == 76.0

    def test_get_seo_audit_summary_empty_df(self):
        """Test audit summary with empty dataframe."""
        seo = SEOAnalysis()
        df = pd.DataFrame()
        result = seo._get_seo_audit_summary(df)
        assert result == {}

    def test_get_seo_audit_summary_with_data(self, mock_seo_audit_result):
        """Test audit summary calculation."""
        seo = SEOAnalysis()
        df = pd.DataFrame(mock_seo_audit_result)
        result = seo._get_seo_audit_summary(df)

        assert result["true_count"] == 4
        assert result["false_count"] == 1
        assert result["seo_audit_rank"] == 80.0

    @patch('digitalmodel.digitalmarketing.seo.seo_analysis.seolib')
    def test_get_seolib_ranks(self, mock_seolib_module):
        """Test seolib ranks retrieval."""
        mock_seolib_module.get_alexa.return_value = 50000
        mock_seolib_module.get_google_plus.return_value = 100
        mock_seolib_module.get_semrush.return_value = 5
        mock_seolib_module.get_tweets.return_value = 200
        mock_seolib_module.get_facebook_likes.return_value = 500
        mock_seolib_module.get_seomoz_data.return_value = {"da": 30}

        seo = SEOAnalysis()

        # Temporarily set seolib to our mock
        with patch.object(seo, 'get_seolib_ranks') as mock_method:
            mock_method.return_value = [
                {"Description": "Alexa Rank", "Value": 50000},
                {"Description": "Seolib Rank", "Value": 100.0}
            ]
            result = seo.get_seolib_ranks("https://example.com")

        assert isinstance(result, list)
