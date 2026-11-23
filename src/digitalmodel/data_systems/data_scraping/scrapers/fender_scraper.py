"""
Fender Scraper

Scrapes marine fender specifications from manufacturer websites and catalogs.
Handles various fender types: pneumatic, foam, cone, arch, roller, etc.
"""

import pandas as pd
import re
from typing import Optional, List, Dict, Any
from pathlib import Path
from .equipment_scraper import EquipmentScraper


class FenderScraper(EquipmentScraper):
    """Scraper specifically for marine fender data."""

    # Fender-specific fields
    FENDER_FIELDS = [
        'fender_type',  # pneumatic, foam, cone, arch, roller, etc.
        'diameter_m',
        'length_m',
        'width_m',
        'height_m',
        'deflection_percent',
        'reaction_force_kn',
        'energy_absorption_kj',
        'pressure_bar',
        'foam_density',
        'color',
        'applications'
    ]

    # Known fender types for classification
    FENDER_TYPES = {
        'pneumatic': ['pneumatic', 'yokohama', 'inflatable'],
        'foam': ['foam', 'foam-filled', 'polyurethane'],
        'cone': ['cone', 'conical'],
        'arch': ['arch', 'arched'],
        'roller': ['roller', 'cylindrical roller'],
        'cell': ['cell', 'cellular'],
        'cylindrical': ['cylindrical', 'cylinder'],
        'square': ['square', 'd-bore', 'rectangular'],
        'v-type': ['v-type', 'v-fender'],
        'w-type': ['w-type', 'w-fender'],
        'floating': ['floating', 'float'],
    }

    def __init__(self, *args, **kwargs):
        """Initialize fender scraper."""
        super().__init__(*args, equipment_category="Fender", **kwargs)

    def classify_fender_type(self, product_name: str, description: str = "") -> str:
        """
        Classify fender type based on product name and description.

        Args:
            product_name: Fender product name
            description: Product description (optional)

        Returns:
            Classified fender type
        """
        text = f"{product_name} {description}".lower()

        for fender_type, keywords in self.FENDER_TYPES.items():
            for keyword in keywords:
                if keyword in text:
                    return fender_type

        return "unknown"

    def parse_fender_specifications(self, spec_dict: Dict[str, str]) -> Dict[str, Any]:
        """
        Parse fender-specific specifications.

        Args:
            spec_dict: Dictionary of raw specifications

        Returns:
            Dictionary of parsed fender specifications
        """
        fender_specs = {}

        # Extract key specifications
        for key, value in spec_dict.items():
            key_lower = key.lower()

            # Diameter
            if 'diameter' in key_lower or 'dia' in key_lower:
                diameter = self._extract_numeric_value(value)
                if diameter:
                    fender_specs['diameter_m'] = diameter

            # Length
            elif 'length' in key_lower or 'len' in key_lower:
                length = self._extract_numeric_value(value)
                if length:
                    fender_specs['length_m'] = length

            # Deflection
            elif 'deflection' in key_lower:
                deflection = self._extract_numeric_value(value)
                if deflection:
                    fender_specs['deflection_percent'] = deflection

            # Reaction force
            elif 'reaction' in key_lower or 'force' in key_lower:
                force = self._extract_numeric_value(value)
                if force:
                    fender_specs['reaction_force_kn'] = force

            # Energy absorption
            elif 'energy' in key_lower or 'absorption' in key_lower:
                energy = self._extract_numeric_value(value)
                if energy:
                    fender_specs['energy_absorption_kj'] = energy

            # Pressure (for pneumatic fenders)
            elif 'pressure' in key_lower:
                pressure = self._extract_numeric_value(value)
                if pressure:
                    fender_specs['pressure_bar'] = pressure

            # Foam density
            elif 'density' in key_lower or 'foam' in key_lower:
                density = self._extract_numeric_value(value)
                if density:
                    fender_specs['foam_density'] = density

        return fender_specs

    def scrape_nauticexpo(self, url: Optional[str] = None) -> pd.DataFrame:
        """
        Scrape fender data from NauticExpo.

        Args:
            url: URL to scrape (default: NauticExpo fender page)

        Returns:
            DataFrame with fender specifications
        """
        if url is None:
            url = "https://www.nauticexpo.com/boat-manufacturer/fender-996.html"

        self.logger.info(f"Scraping NauticExpo fender catalog: {url}")

        # Fetch page (try static first)
        response = self.fetch_page(url)
        if not response:
            self.logger.error("Failed to fetch NauticExpo page")
            return pd.DataFrame()

        # Try product cards first (NauticExpo uses card layouts)
        self.logger.info("Attempting to extract product cards...")
        products = self.extract_product_cards(response.text)

        # If no products found with static, try Selenium
        if not products or len(products) < 5:
            self.logger.warning(f"Only {len(products)} products found statically, trying Selenium...")

            from .dynamic_scraper import DynamicScraper

            with DynamicScraper(
                base_url=url,
                headless=True,
                output_dir=self.output_dir,
                wait_for_tables=15  # Wait longer for dynamic content
            ) as dynamic_scraper:
                html = dynamic_scraper.fetch_dynamic_page(url)
                if html:
                    products = self.extract_product_cards(html)

        # Try tables as fallback
        if not products:
            self.logger.info("No product cards found, trying table extraction...")
            tables = self.extract_tables(response.text)

            if not tables or all(len(t) < 5 for t in tables):
                # Try Selenium for tables
                from .dynamic_scraper import DynamicScraper
                with DynamicScraper(
                    base_url=url,
                    headless=True,
                    output_dir=self.output_dir
                ) as dynamic_scraper:
                    tables = dynamic_scraper.extract_dynamic_tables(url)

            if tables:
                df = max(tables, key=len)
                self.logger.info(f"Extracted table with {len(df)} rows")
                df = self.standardize_columns(df)

                # Add fender-specific processing
                if 'product_name' in df.columns:
                    df['fender_type'] = df['product_name'].apply(
                        lambda x: self.classify_fender_type(str(x))
                    )
                return df

        # Convert products list to DataFrame
        if products:
            self.logger.info(f"Successfully extracted {len(products)} fender products")
            df = pd.DataFrame(products)

            # Standardize columns
            df = self.standardize_columns(df)

            # Add fender-specific processing
            if 'product_name' in df.columns:
                df['fender_type'] = df['product_name'].apply(
                    lambda x: self.classify_fender_type(str(x))
                )

            # Add fender type from description if available
            if 'description' in df.columns:
                df['fender_type'] = df.apply(
                    lambda row: self.classify_fender_type(
                        str(row.get('product_name', '')),
                        str(row.get('description', ''))
                    ),
                    axis=1
                )

            return df

        self.logger.error("No fender data found on NauticExpo page")
        return pd.DataFrame()

    def scrape_marine_fenders_intl(self, url: Optional[str] = None) -> pd.DataFrame:
        """
        Scrape fender data from Marine Fenders International.

        Args:
            url: URL to scrape (default: marinefendersintl.com)

        Returns:
            DataFrame with fender specifications
        """
        if url is None:
            url = "https://www.marinefendersintl.com/"

        self.logger.info(f"Scraping Marine Fenders International: {url}")

        response = self.fetch_page(url)
        if not response:
            return pd.DataFrame()

        # Extract product links
        from bs4 import BeautifulSoup
        soup = BeautifulSoup(response.text, 'lxml')

        # Look for product pages or catalog links
        product_links = []
        for link in soup.find_all('a', href=True):
            href = link['href']
            text = link.get_text().lower()
            if any(keyword in text for keyword in ['product', 'fender', 'catalog', 'specification']):
                if not href.startswith('http'):
                    href = self._make_absolute_url(href)
                product_links.append(href)

        self.logger.info(f"Found {len(product_links)} product links")

        # Extract tables from main page
        tables = self.extract_tables(response.text)

        if tables:
            df = max(tables, key=len)
            df = self.standardize_columns(df)
            return df

        return pd.DataFrame()

    def scrape_pacific_marine(self, url: Optional[str] = None) -> pd.DataFrame:
        """
        Scrape fender data from Pacific Marine & Industrial.

        Args:
            url: URL to scrape (default: pacificmarine.net fenders)

        Returns:
            DataFrame with fender specifications
        """
        if url is None:
            url = "https://www.pacificmarine.net/marine-deck/marine-fenders.htm"

        self.logger.info(f"Scraping Pacific Marine: {url}")

        response = self.fetch_page(url)
        if not response:
            return pd.DataFrame()

        tables = self.extract_tables(response.text)

        if not tables:
            self.logger.warning("No tables found on Pacific Marine page")
            return pd.DataFrame()

        df = max(tables, key=len)
        df = self.standardize_columns(df)

        return df

    def scrape(
        self,
        sources: Optional[List[str]] = None,
        custom_url: Optional[str] = None,
        use_selenium: bool = False
    ) -> pd.DataFrame:
        """
        Main scraping method - scrapes multiple fender sources.

        Args:
            sources: List of sources to scrape ['nauticexpo', 'marine_fenders_intl', 'pacific_marine']
            custom_url: Custom URL to scrape
            use_selenium: Force Selenium usage

        Returns:
            Combined DataFrame with all fender data
        """
        all_data = []

        # Custom URL
        if custom_url:
            self.logger.info(f"Scraping custom URL: {custom_url}")

            if use_selenium:
                from .dynamic_scraper import DynamicScraper
                with DynamicScraper(
                    base_url=custom_url,
                    headless=True,
                    output_dir=self.output_dir
                ) as dynamic_scraper:
                    df = dynamic_scraper.scrape_dynamic(custom_url, min_rows=5)
                    if df is not None and not df.empty:
                        df = self.standardize_columns(df)
                        all_data.append(df)
            else:
                response = self.fetch_page(custom_url)
                if response:
                    tables = self.extract_tables(response.text)
                    if tables:
                        df = max(tables, key=len)
                        df = self.standardize_columns(df)
                        all_data.append(df)

        # Default sources
        if not sources:
            sources = ['nauticexpo']  # Start with NauticExpo as primary source

        for source in sources:
            self.logger.info(f"Scraping source: {source}")

            try:
                if source == 'nauticexpo':
                    df = self.scrape_nauticexpo()
                elif source == 'marine_fenders_intl':
                    df = self.scrape_marine_fenders_intl()
                elif source == 'pacific_marine':
                    df = self.scrape_pacific_marine()
                else:
                    self.logger.warning(f"Unknown source: {source}")
                    continue

                if df is not None and not df.empty:
                    all_data.append(df)

            except Exception as e:
                self.logger.error(f"Error scraping {source}: {e}")
                continue

        # Combine all data
        if not all_data:
            self.logger.warning("No data scraped from any source")
            return pd.DataFrame()

        combined_df = pd.concat(all_data, ignore_index=True)
        self.logger.info(f"Combined data: {len(combined_df)} rows from {len(all_data)} sources")

        # Drop duplicates based on product name and manufacturer
        if 'product_name' in combined_df.columns and 'manufacturer' in combined_df.columns:
            original_count = len(combined_df)
            combined_df = combined_df.drop_duplicates(subset=['product_name', 'manufacturer'], keep='first')
            duplicates_removed = original_count - len(combined_df)
            if duplicates_removed > 0:
                self.logger.info(f"Removed {duplicates_removed} duplicate entries")

        # Save to file
        if self.output_dir:
            output_file = self.output_dir / f"fenders_{pd.Timestamp.now().strftime('%Y%m%d')}.csv"
            combined_df.to_csv(output_file, index=False)
            self.logger.info(f"Saved fender data to: {output_file}")

        return combined_df

    def _extract_numeric_value(self, value_str: str) -> Optional[float]:
        """
        Extract numeric value from string.

        Args:
            value_str: String containing numeric value

        Returns:
            Extracted numeric value
        """
        if pd.isna(value_str) or not value_str:
            return None

        # Find first number (int or float)
        match = re.search(r'([0-9]+\.?[0-9]*)', str(value_str))
        if match:
            return float(match.group(1))

        return None
