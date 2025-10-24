"""
Pipelay Vessel Scraper

Scrapes pipelay vessel data from online sources.
"""

import pandas as pd
from typing import Optional
from .vessel_scraper import VesselScraper


class PipelayVesselScraper(VesselScraper):
    """Scraper for pipelay vessel databases."""

    def __init__(self, source_url: Optional[str] = None, **kwargs):
        """
        Initialize pipelay vessel scraper.

        Args:
            source_url: URL of pipelay vessel database
            **kwargs: Additional arguments for BaseScraper
        """
        base_url = source_url or "https://www.offshore-mag.com"
        super().__init__(base_url=base_url, **kwargs)

        self.vessel_type = "Pipelay Vessel"
        self.source_url = source_url

    def scrape_custom_url(self, url: str) -> Optional[pd.DataFrame]:
        """
        Scrape pipelay vessel data from a custom URL.

        Args:
            url: URL containing pipelay vessel data tables

        Returns:
            DataFrame with vessel data or None
        """
        self.logger.info(f"Scraping custom URL: {url}")

        response = self.fetch_page(url)
        if not response:
            return None

        tables = self.extract_tables(response.text)

        if not tables:
            self.logger.warning("No tables found in custom URL")
            return None

        df = max(tables, key=len)
        return df

    def scrape(self, custom_url: Optional[str] = None) -> Optional[pd.DataFrame]:
        """
        Main scraping method for pipelay vessel data.

        Args:
            custom_url: Optional custom URL to scrape

        Returns:
            DataFrame with vessel data or None
        """
        if custom_url:
            df = self.scrape_custom_url(custom_url)
            source = custom_url
        elif self.source_url:
            df = self.scrape_custom_url(self.source_url)
            source = self.source_url
        else:
            self.logger.warning("No URL provided for pipelay vessel scraping")
            return None

        if df is not None and not df.empty:
            self.logger.info(f"Successfully scraped {len(df)} pipelay vessel records")

            from datetime import datetime
            filename = f"pipelay_vessels_{datetime.now().year}.csv"

            self.save_vessel_data(
                df=df,
                filename=filename,
                source_url=source,
                description=f"Pipelay vessel database from {source}"
            )

            return df
        else:
            self.logger.error("Failed to scrape pipelay vessel data")
            return None
