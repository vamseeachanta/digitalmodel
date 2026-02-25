"""
Drilling Rig Scraper

Scrapes drilling rig data (both deepwater and jackup) from online sources.

Supported sources:
    - Rigzone database
    - Offshore Magazine rig databases
    - Custom URLs with rig data tables
"""

import pandas as pd
from typing import Optional, Literal
from .vessel_scraper import VesselScraper


class DrillingRigScraper(VesselScraper):
    """Scraper for drilling rig databases."""

    def __init__(
        self,
        rig_type: Literal['deepwater', 'jackup', 'both'] = 'both',
        source_url: Optional[str] = None,
        **kwargs
    ):
        """
        Initialize drilling rig scraper.

        Args:
            rig_type: Type of rigs to scrape ('deepwater', 'jackup', or 'both')
            source_url: URL of rig database
            **kwargs: Additional arguments for BaseScraper
        """
        base_url = source_url or "https://www.rigzone.com"
        super().__init__(base_url=base_url, **kwargs)

        self.rig_type = rig_type
        self.vessel_type = f"Drilling Rig - {rig_type.title()}"
        self.source_url = source_url

    def scrape_rigzone(self) -> Optional[pd.DataFrame]:
        """
        Scrape drilling rig data from Rigzone.

        Note: Rigzone may require registration or have API access.
        This is a template implementation.

        Returns:
            DataFrame with rig data or None
        """
        self.logger.info(f"Attempting to scrape Rigzone for {self.rig_type} rigs")
        self.logger.warning("Note: Rigzone may require registration - check website")

        # Template URL - actual endpoint may differ
        if self.rig_type == 'deepwater':
            search_url = "/rigs/deepwater"
        elif self.rig_type == 'jackup':
            search_url = "/rigs/jackup"
        else:
            search_url = "/rigs"

        response = self.fetch_page(search_url)
        if not response:
            return None

        # Extract tables
        tables = self.extract_tables(response.text)

        if tables:
            # Combine all tables if multiple
            if len(tables) > 1:
                df = pd.concat(tables, ignore_index=True)
            else:
                df = tables[0]

            return df

        return None

    def scrape_custom_url(self, url: str) -> Optional[pd.DataFrame]:
        """
        Scrape drilling rig data from a custom URL.

        Args:
            url: URL containing rig data tables

        Returns:
            DataFrame with rig data or None
        """
        self.logger.info(f"Scraping custom URL: {url}")

        response = self.fetch_page(url)
        if not response:
            return None

        tables = self.extract_tables(response.text)

        if not tables:
            self.logger.warning("No tables found in custom URL")
            return None

        # Combine all tables
        if len(tables) > 1:
            df = pd.concat(tables, ignore_index=True)
        else:
            df = tables[0]

        return df

    def scrape(self, custom_url: Optional[str] = None) -> Optional[pd.DataFrame]:
        """
        Main scraping method for drilling rig data.

        Args:
            custom_url: Optional custom URL to scrape

        Returns:
            DataFrame with rig data or None
        """
        if custom_url:
            df = self.scrape_custom_url(custom_url)
            source = custom_url
        elif self.source_url:
            df = self.scrape_custom_url(self.source_url)
            source = self.source_url
        else:
            df = self.scrape_rigzone()
            source = self.base_url

        if df is not None and not df.empty:
            self.logger.info(f"Successfully scraped {len(df)} drilling rig records")

            # Save data
            from datetime import datetime
            year = datetime.now().year

            if self.rig_type == 'both':
                # Save as combined database
                filename = f"drilling_rigs_combined_{year}.csv"
            else:
                filename = f"drilling_rigs_{self.rig_type}_{year}.csv"

            self.save_vessel_data(
                df=df,
                filename=filename,
                source_url=source,
                description=f"{self.rig_type.title()} drilling rig database from {source}"
            )

            return df
        else:
            self.logger.error("Failed to scrape drilling rig data")
            return None


def scrape_drilling_rigs(
    rig_type: Literal['deepwater', 'jackup', 'both'] = 'both',
    url: Optional[str] = None,
    output_dir: str = "data"
) -> Optional[pd.DataFrame]:
    """
    Convenience function to scrape drilling rig database.

    Args:
        rig_type: Type of rigs ('deepwater', 'jackup', or 'both')
        url: Optional URL to scrape
        output_dir: Output directory for data

    Returns:
        DataFrame with rig data or None

    Example:
        >>> # Scrape deepwater rigs
        >>> df_deep = scrape_drilling_rigs(rig_type='deepwater')
        >>>
        >>> # Scrape jackup rigs
        >>> df_jackup = scrape_drilling_rigs(rig_type='jackup')
    """
    from pathlib import Path

    with DrillingRigScraper(
        rig_type=rig_type,
        source_url=url,
        output_dir=Path(output_dir)
    ) as scraper:
        return scraper.scrape()
