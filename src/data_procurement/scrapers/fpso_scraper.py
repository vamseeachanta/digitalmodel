"""
FPSO Database Scraper

Scrapes FPSO (Floating Production Storage and Offloading) vessel data
from various online sources.

Supported sources:
    - Offshore Magazine FPSO database
    - Industry fleet databases
    - Custom URLs with FPSO data tables
"""

import pandas as pd
from typing import Optional, List
from bs4 import BeautifulSoup
from .vessel_scraper import VesselScraper


class FPSOScraper(VesselScraper):
    """Scraper for FPSO vessel databases."""

    def __init__(self, source_url: Optional[str] = None, **kwargs):
        """
        Initialize FPSO scraper.

        Args:
            source_url: URL of FPSO database (or use default sources)
            **kwargs: Additional arguments for BaseScraper
        """
        # Default to Offshore Magazine if no URL provided
        base_url = source_url or "https://www.offshore-mag.com"
        super().__init__(base_url=base_url, **kwargs)

        self.vessel_type = "FPSO"
        self.source_url = source_url

    def scrape_offshore_magazine(self) -> Optional[pd.DataFrame]:
        """
        Scrape FPSO data from Offshore Magazine.

        Note: This is a template method. The actual implementation
        will depend on the current website structure.

        Returns:
            DataFrame with FPSO data or None
        """
        # Example implementation (actual URL and structure may vary)
        search_url = "/fpso-database"  # Placeholder - actual path may differ

        self.logger.info("Attempting to scrape Offshore Magazine FPSO database")
        self.logger.warning("Note: Website structure may have changed - manual verification needed")

        response = self.fetch_page(search_url)
        if not response:
            self.logger.error("Failed to fetch FPSO database page")
            return None

        # Try to extract tables
        tables = self.extract_tables(response.text)

        if tables:
            self.logger.info(f"Found {len(tables)} tables on page")
            # Usually the main data table is the largest
            df = max(tables, key=len)
            return df
        else:
            self.logger.warning("No tables found - trying BeautifulSoup parsing")
            return self._parse_with_bs4(response.text)

    def _parse_with_bs4(self, html: str) -> Optional[pd.DataFrame]:
        """
        Parse FPSO data using BeautifulSoup (fallback method).

        Args:
            html: HTML content

        Returns:
            DataFrame or None
        """
        soup = BeautifulSoup(html, 'html.parser')

        # Look for common FPSO data patterns
        # This is a template - actual implementation depends on website structure
        data_rows = []

        # Example: Find all divs/sections with FPSO data
        fpso_elements = soup.find_all(['div', 'tr'], class_=lambda x: x and 'fpso' in x.lower())

        for element in fpso_elements:
            row_data = {}
            # Extract data fields (customize based on actual HTML structure)
            # row_data['vessel_name'] = ...
            # row_data['year_built'] = ...
            # etc.

            if row_data:
                data_rows.append(row_data)

        if data_rows:
            return pd.DataFrame(data_rows)

        return None

    def scrape_custom_url(self, url: str) -> Optional[pd.DataFrame]:
        """
        Scrape FPSO data from a custom URL.

        Args:
            url: URL containing FPSO data tables

        Returns:
            DataFrame with FPSO data or None
        """
        self.logger.info(f"Scraping custom URL: {url}")

        response = self.fetch_page(url)
        if not response:
            return None

        # Try table extraction first
        tables = self.extract_tables(response.text)

        if not tables:
            self.logger.warning("No tables found in custom URL")
            return None

        # Return the largest table (likely the main data table)
        df = max(tables, key=len)
        self.logger.info(f"Extracted table with {len(df)} rows and {len(df.columns)} columns")

        return df

    def scrape(self, custom_url: Optional[str] = None) -> Optional[pd.DataFrame]:
        """
        Main scraping method for FPSO data.

        Args:
            custom_url: Optional custom URL to scrape

        Returns:
            DataFrame with FPSO data or None
        """
        if custom_url:
            df = self.scrape_custom_url(custom_url)
            source = custom_url
        elif self.source_url:
            df = self.scrape_custom_url(self.source_url)
            source = self.source_url
        else:
            df = self.scrape_offshore_magazine()
            source = self.base_url

        if df is not None and not df.empty:
            self.logger.info(f"Successfully scraped {len(df)} FPSO records")

            # Save data
            from datetime import datetime
            filename = f"fpso_database_{datetime.now().year}.csv"
            self.save_vessel_data(
                df=df,
                filename=filename,
                source_url=source,
                description=f"FPSO vessel database scraped from {source}"
            )

            return df
        else:
            self.logger.error("Failed to scrape FPSO data")
            return None


def scrape_fpso_database(
    url: Optional[str] = None,
    output_dir: str = "data"
) -> Optional[pd.DataFrame]:
    """
    Convenience function to scrape FPSO database.

    Args:
        url: Optional URL to scrape
        output_dir: Output directory for data

    Returns:
        DataFrame with FPSO data or None

    Example:
        >>> df = scrape_fpso_database()
        >>> print(f"Scraped {len(df)} FPSO vessels")
    """
    from pathlib import Path

    with FPSOScraper(source_url=url, output_dir=Path(output_dir)) as scraper:
        return scraper.scrape()
