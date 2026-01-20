"""
Equipment Scraper Base Class

Base class for all equipment-related scrapers (fenders, anchors, buoys, SPM).
Provides common equipment data extraction and cleaning functionality.
"""

import pandas as pd
import re
from typing import Optional, Dict, Any, List
from pathlib import Path
from bs4 import BeautifulSoup
from .base_scraper import BaseScraper


class EquipmentScraper(BaseScraper):
    """Base class for equipment-specific scrapers."""

    # Standard equipment data fields
    STANDARD_FIELDS = [
        'product_name',
        'manufacturer',
        'equipment_type',
        'model_number',
        'dimensions',
        'weight_kg',
        'capacity',
        'material',
        'standards',
        'working_load',
        'breaking_load',
        'catalog_url',
        'pdf_catalog',
        'specifications',
        'notes'
    ]

    def __init__(self, *args, equipment_category: str = "Generic Equipment", **kwargs):
        """
        Initialize equipment scraper.

        Args:
            equipment_category: Type of equipment (fender, anchor, buoy, etc.)
            *args, **kwargs: Arguments for BaseScraper
        """
        super().__init__(*args, **kwargs)
        self.equipment_category = equipment_category

    def clean_product_name(self, name: str) -> str:
        """
        Clean and standardize product name.

        Args:
            name: Raw product name

        Returns:
            Cleaned product name
        """
        if pd.isna(name) or not name:
            return ""

        # Convert to string and strip whitespace
        name = str(name).strip()

        # Remove extra spaces
        name = re.sub(r'\s+', ' ', name)

        # Remove special characters but keep hyphens and parentheses
        name = re.sub(r'[^\w\s\-\(\)\/]', '', name)

        return name

    def clean_manufacturer_name(self, manufacturer: str) -> str:
        """
        Clean and standardize manufacturer name.

        Args:
            manufacturer: Raw manufacturer name

        Returns:
            Cleaned manufacturer name
        """
        if pd.isna(manufacturer) or not manufacturer:
            return ""

        manufacturer = str(manufacturer).strip()

        # Common suffixes to remove
        suffixes = [
            'Inc.', 'Inc', 'LLC', 'Ltd.', 'Ltd', 'Limited',
            'Corp.', 'Corp', 'Corporation', 'Co.', 'Co'
        ]

        for suffix in suffixes:
            manufacturer = re.sub(rf'\s*{re.escape(suffix)}\s*$', '', manufacturer, flags=re.IGNORECASE)

        return manufacturer.strip()

    def parse_dimensions(self, dimension_str: str) -> Dict[str, float]:
        """
        Parse dimension string into structured format.

        Args:
            dimension_str: Dimension string (e.g., "2.5m x 3.0m x 4.5m")

        Returns:
            Dictionary with parsed dimensions
        """
        if pd.isna(dimension_str) or not dimension_str:
            return {}

        dimensions = {}
        dimension_str = str(dimension_str).lower()

        # Common patterns: LxWxH, LxW, Diameter x Length
        patterns = {
            'length': r'(?:length|l|len)[:\s]*([0-9.]+)\s*([a-z]+)',
            'width': r'(?:width|w|beam)[:\s]*([0-9.]+)\s*([a-z]+)',
            'height': r'(?:height|h)[:\s]*([0-9.]+)\s*([a-z]+)',
            'diameter': r'(?:diameter|dia|d)[:\s]*([0-9.]+)\s*([a-z]+)',
        }

        for key, pattern in patterns.items():
            match = re.search(pattern, dimension_str, re.IGNORECASE)
            if match:
                value, unit = match.groups()
                dimensions[f'{key}_{unit}'] = float(value)

        # Simple LxWxH pattern
        simple_match = re.search(r'([0-9.]+)\s*[x×]\s*([0-9.]+)(?:\s*[x×]\s*([0-9.]+))?', dimension_str)
        if simple_match and not dimensions:
            values = [float(v) for v in simple_match.groups() if v]
            if len(values) == 2:
                dimensions['length_m'] = values[0]
                dimensions['width_m'] = values[1]
            elif len(values) == 3:
                dimensions['length_m'] = values[0]
                dimensions['width_m'] = values[1]
                dimensions['height_m'] = values[2]

        return dimensions

    def parse_weight(self, weight_str: str) -> Optional[float]:
        """
        Parse weight string to kilograms.

        Args:
            weight_str: Weight string (e.g., "1500 kg", "1.5 tonnes")

        Returns:
            Weight in kilograms
        """
        if pd.isna(weight_str) or not weight_str:
            return None

        weight_str = str(weight_str).lower()

        # Extract number and unit
        match = re.search(r'([0-9,.]+)\s*(kg|t|ton|tonne|lb|lbs|pounds?)', weight_str, re.IGNORECASE)
        if not match:
            return None

        value_str, unit = match.groups()
        value = float(value_str.replace(',', ''))

        # Convert to kg
        conversions = {
            'kg': 1.0,
            't': 1000.0,
            'ton': 1000.0,
            'tonne': 1000.0,
            'lb': 0.453592,
            'lbs': 0.453592,
            'pound': 0.453592,
            'pounds': 0.453592,
        }

        return value * conversions.get(unit.lower(), 1.0)

    def parse_capacity(self, capacity_str: str) -> Dict[str, Any]:
        """
        Parse capacity/load rating string.

        Args:
            capacity_str: Capacity string (e.g., "500 kN", "50 tonnes")

        Returns:
            Dictionary with capacity information
        """
        if pd.isna(capacity_str) or not capacity_str:
            return {}

        capacity = {}
        capacity_str = str(capacity_str).lower()

        # Common patterns for load ratings
        patterns = {
            'working_load': r'(?:working|safe working|swl)[:\s]*([0-9,.]+)\s*([a-z]+)',
            'breaking_load': r'(?:breaking|break|mbl|minimum breaking)[:\s]*([0-9,.]+)\s*([a-z]+)',
            'capacity': r'(?:capacity|cap)[:\s]*([0-9,.]+)\s*([a-z]+)',
        }

        for key, pattern in patterns.items():
            match = re.search(pattern, capacity_str, re.IGNORECASE)
            if match:
                value_str, unit = match.groups()
                value = float(value_str.replace(',', ''))
                capacity[f'{key}_{unit}'] = value

        # Generic pattern for any load value
        if not capacity:
            match = re.search(r'([0-9,.]+)\s*(kn|t|ton|tonne|kg)', capacity_str, re.IGNORECASE)
            if match:
                value_str, unit = match.groups()
                value = float(value_str.replace(',', ''))
                capacity[f'capacity_{unit.lower()}'] = value

        return capacity

    def extract_pdf_catalog_links(self, html: str) -> List[str]:
        """
        Extract PDF catalog download links from HTML.

        Args:
            html: HTML content

        Returns:
            List of PDF URLs
        """
        soup = BeautifulSoup(html, 'lxml')
        pdf_links = []

        # Find all links ending in .pdf
        for link in soup.find_all('a', href=True):
            href = link['href']
            if href.lower().endswith('.pdf'):
                # Make absolute URL if needed
                if not href.startswith('http'):
                    href = self._make_absolute_url(href)
                pdf_links.append(href)

        return pdf_links

    def extract_specifications(self, html: str, container_selector: Optional[str] = None) -> Dict[str, str]:
        """
        Extract specifications from HTML tables or structured data.

        Args:
            html: HTML content
            container_selector: Optional CSS selector for specification container

        Returns:
            Dictionary of specifications
        """
        soup = BeautifulSoup(html, 'lxml')
        specs = {}

        # Find specification container if selector provided
        if container_selector:
            container = soup.select_one(container_selector)
            if container:
                soup = container

        # Look for key-value pairs in tables
        for table in soup.find_all('table'):
            for row in table.find_all('tr'):
                cells = row.find_all(['td', 'th'])
                if len(cells) == 2:
                    key = cells[0].get_text(strip=True)
                    value = cells[1].get_text(strip=True)
                    if key and value:
                        specs[key.lower().replace(' ', '_')] = value

        # Look for definition lists
        for dl in soup.find_all('dl'):
            dt_tags = dl.find_all('dt')
            dd_tags = dl.find_all('dd')
            for dt, dd in zip(dt_tags, dd_tags):
                key = dt.get_text(strip=True)
                value = dd.get_text(strip=True)
                if key and value:
                    specs[key.lower().replace(' ', '_')] = value

        return specs

    def standardize_columns(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Standardize equipment dataframe columns.

        Args:
            df: Raw equipment dataframe

        Returns:
            Standardized dataframe
        """
        if df is None or df.empty:
            return df

        # Make a copy
        df_copy = df.copy()

        # Ensure columns are strings and lowercase
        df_copy.columns = df_copy.columns.astype(str).str.lower().str.strip()

        # Map common column variations to standard names
        column_mappings = {
            'product_name': ['product', 'name', 'product_name', 'model', 'description'],
            'manufacturer': ['manufacturer', 'brand', 'supplier', 'maker'],
            'equipment_type': ['type', 'category', 'equipment_type'],
            'model_number': ['model', 'model_number', 'model_no', 'part_number'],
            'weight_kg': ['weight', 'mass', 'weight_kg'],
            'material': ['material', 'materials', 'construction'],
            'standards': ['standards', 'standard', 'certification', 'compliance'],
        }

        # Rename columns based on mappings
        for standard_col, variations in column_mappings.items():
            for col in df_copy.columns:
                if col in variations:
                    df_copy.rename(columns={col: standard_col}, inplace=True)
                    break

        # Add equipment category
        df_copy['equipment_category'] = self.equipment_category

        # Add scrape metadata
        df_copy['scraped_date'] = pd.Timestamp.now().strftime('%Y-%m-%d')
        df_copy['source_url'] = self.base_url

        return df_copy

    def validate_equipment_specs(self, df: pd.DataFrame) -> Dict[str, Any]:
        """
        Validate equipment specifications completeness.

        Args:
            df: Equipment dataframe

        Returns:
            Validation report dictionary
        """
        if df is None or df.empty:
            return {
                'valid': False,
                'error': 'Empty dataframe',
                'rows': 0
            }

        report = {
            'valid': True,
            'rows': len(df),
            'columns': len(df.columns),
            'missing_critical_fields': [],
            'completeness': {}
        }

        # Check for critical fields
        critical_fields = ['product_name', 'manufacturer', 'equipment_type']
        for field in critical_fields:
            if field not in df.columns:
                report['missing_critical_fields'].append(field)
            else:
                # Check how many rows have this field populated
                non_null = df[field].notna().sum()
                completeness = (non_null / len(df)) * 100
                report['completeness'][field] = f"{completeness:.1f}%"

        if report['missing_critical_fields']:
            report['valid'] = False

        # Calculate overall completeness
        total_cells = df.shape[0] * df.shape[1]
        filled_cells = df.notna().sum().sum()
        report['overall_completeness'] = f"{(filled_cells / total_cells * 100):.1f}%"

        return report

    def _make_absolute_url(self, url: str) -> str:
        """
        Convert relative URL to absolute URL.

        Args:
            url: Relative or absolute URL

        Returns:
            Absolute URL
        """
        if url.startswith('http'):
            return url

        from urllib.parse import urljoin
        return urljoin(self.base_url, url)

    def extract_product_cards(self, html: str, card_selector: str = ".product-item") -> List[Dict[str, str]]:
        """
        Extract product information from card/grid layouts.

        Args:
            html: HTML content
            card_selector: CSS selector for product cards

        Returns:
            List of product dictionaries
        """
        soup = BeautifulSoup(html, 'lxml')
        products = []

        # Find all product cards
        cards = soup.select(card_selector)

        if not cards:
            # Try common alternative selectors
            alternative_selectors = [
                '.product-card', '.product', '.item', '.listing-item',
                'div[class*="product"]', 'article[class*="product"]',
                'div[data-product-id]', 'div[class*="card"]'
            ]

            for selector in alternative_selectors:
                cards = soup.select(selector)
                if cards:
                    self.logger.info(f"Found {len(cards)} products with selector: {selector}")
                    break

        for card in cards:
            product = {}

            # Extract product name (try multiple selectors)
            name_elem = (
                card.select_one('h2, h3, h4, .product-name, .title, [class*="name"]') or
                card.select_one('a[class*="title"]')
            )
            if name_elem:
                product['product_name'] = name_elem.get_text(strip=True)

            # Extract manufacturer/brand
            brand_elem = (
                card.select_one('.brand, .manufacturer, [class*="brand"]') or
                card.select_one('[class*="manufacturer"]')
            )
            if brand_elem:
                product['manufacturer'] = brand_elem.get_text(strip=True)

            # Extract description
            desc_elem = card.select_one('.description, p, [class*="desc"]')
            if desc_elem:
                product['description'] = desc_elem.get_text(strip=True)

            # Extract link
            link_elem = card.select_one('a[href]')
            if link_elem:
                product['product_url'] = self._make_absolute_url(link_elem['href'])

            # Extract image
            img_elem = card.select_one('img[src]')
            if img_elem:
                product['image_url'] = self._make_absolute_url(img_elem['src'])

            # Extract any data attributes
            for attr, value in card.attrs.items():
                if attr.startswith('data-'):
                    product[attr.replace('data-', '')] = value

            if product:  # Only add if we extracted something
                products.append(product)

        return products

    def close(self):
        """Close scraper session and clean up."""
        if hasattr(super(), 'close'):
            super().close()
