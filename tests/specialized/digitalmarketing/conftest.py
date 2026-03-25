"""Test fixtures for digitalmarketing module."""

import pytest
from unittest.mock import MagicMock, patch


@pytest.fixture
def seo_config():
    """Basic SEO analysis configuration."""
    return {
        "basename": "digitalmarketing",
        "inputs": {
            "url": "https://example.com",
            "alias": "test_site"
        },
        "analysis": {
            "seo": {
                "pyseoanalyzer": False,
                "seolib": True,
                "seoaudit": True
            }
        },
        "calculation": {
            "name": "seo_analysis"
        }
    }


@pytest.fixture
def mock_seolib():
    """Mock seolib module."""
    mock = MagicMock()
    mock.get_alexa.return_value = 50000
    mock.get_google_plus.return_value = 100
    mock.get_semrush.return_value = 5
    mock.get_tweets.return_value = 200
    mock.get_facebook_likes.return_value = 500
    mock.get_seomoz_data.return_value = {"domain_authority": 30}
    return mock


@pytest.fixture
def mock_seo_audit_result():
    """Mock SEO audit results."""
    return [
        {"test": "title_present", "result": True, "message": "Title tag present"},
        {"test": "meta_description", "result": True, "message": "Meta description present"},
        {"test": "h1_present", "result": True, "message": "H1 tag present"},
        {"test": "canonical_url", "result": False, "message": "Missing canonical URL"},
        {"test": "robots_txt", "result": True, "message": "Robots.txt present"},
    ]


@pytest.fixture
def sample_robots_txt():
    """Sample robots.txt content."""
    return """
User-agent: *
Disallow: /admin/
Disallow: /private/
Allow: /public/

User-agent: Googlebot
Allow: /
Disallow: /temp/
"""
