"""
Data Procurement Module for Digital Model.

ABOUTME: Automated web scraping and data collection tools for marine engineering
data procurement, including vessel databases, equipment catalogs, and PDF extraction.

Submodules:
    scrapers: Web scraping classes for different data sources (vessels, FPSOs, rigs)
    validators: Data validation and quality checks
"""

__version__ = "1.0.0"
__author__ = "Digital Model Team"

from .scrapers import (
    BaseScraper,
    VesselScraper,
    FPSOScraper,
    DrillingRigScraper,
    PipelayVesselScraper,
)
from .validators import DataValidator

__all__ = [
    "BaseScraper",
    "VesselScraper",
    "FPSOScraper",
    "DrillingRigScraper",
    "PipelayVesselScraper",
    "DataValidator",
]
