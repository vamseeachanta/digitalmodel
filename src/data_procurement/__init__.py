"""
Data Procurement Module for Digital Model

This module provides automated web scraping and data collection tools
for marine engineering data procurement.

Modules:
    scrapers: Web scraping classes for different data sources
    utils: Utility functions for data processing
    validators: Data validation and quality checks
"""

__version__ = "1.0.0"
__author__ = "Digital Model Team"

from .scrapers.base_scraper import BaseScraper
from .validators.data_validator import DataValidator
from .utils.metadata_generator import MetadataGenerator

__all__ = [
    'BaseScraper',
    'DataValidator',
    'MetadataGenerator'
]
