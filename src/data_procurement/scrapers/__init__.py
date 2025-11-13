"""
Web Scrapers for Marine Engineering Data

Available scrapers:
    - VesselScraper: Base class for vessel data collection
    - FPSOScraper: FPSO database scraper
    - DrillingRigScraper: Drilling rig data scraper
    - PipelayVesselScraper: Pipelay vessel scraper
"""

from .base_scraper import BaseScraper
from .vessel_scraper import VesselScraper
from .fpso_scraper import FPSOScraper
from .drilling_rig_scraper import DrillingRigScraper
from .pipelay_scraper import PipelayVesselScraper

__all__ = [
    'BaseScraper',
    'VesselScraper',
    'FPSOScraper',
    'DrillingRigScraper',
    'PipelayVesselScraper'
]
