"""
Vessel Scraper Base Class

Base class for all vessel-related scrapers (FPSOs, drilling rigs, pipelay vessels).
Provides common vessel data extraction and cleaning functionality.
"""

import pandas as pd
from typing import Optional, Dict, Any
from pathlib import Path
from .base_scraper import BaseScraper


class VesselScraper(BaseScraper):
    """Base class for vessel-specific scrapers."""

    # Standard vessel data fields
    STANDARD_FIELDS = [
        'vessel_name',
        'vessel_type',
        'year_built',
        'owner',
        'operator',
        'flag',
        'classification_society',
        'length_m',
        'beam_m',
        'depth_m',
        'draft_m',
        'displacement_tonnes',
        'water_depth_m',
        'deck_space_m2',
        'accommodation_capacity',
        'positioning_system',
        'propulsion',
        'notes'
    ]

    def __init__(self, *args, **kwargs):
        """Initialize vessel scraper."""
        super().__init__(*args, **kwargs)
        self.vessel_type = "Generic Vessel"

    def clean_vessel_name(self, name: str) -> str:
        """
        Clean and standardize vessel name.

        Args:
            name: Raw vessel name

        Returns:
            Cleaned vessel name
        """
        if pd.isna(name):
            return ""

        # Remove extra whitespace
        name = " ".join(str(name).split())

        # Remove special characters except hyphens and apostrophes
        # name = re.sub(r'[^\w\s\-\']', '', name)

        return name.strip()

    def parse_year(self, year_str: str) -> Optional[int]:
        """
        Parse year from various formats.

        Args:
            year_str: Year as string (e.g., '2018', 'Built 2018', '2018-01-15')

        Returns:
            Year as integer or None
        """
        if pd.isna(year_str):
            return None

        try:
            # Extract first 4-digit number
            import re
            match = re.search(r'\b(19|20)\d{2}\b', str(year_str))
            if match:
                return int(match.group())
        except:
            pass

        return None

    def parse_dimension(self, dim_str: str) -> Optional[float]:
        """
        Parse dimension from string (handles units).

        Args:
            dim_str: Dimension string (e.g., '300 m', '300m', '984 ft')

        Returns:
            Dimension in meters or None
        """
        if pd.isna(dim_str):
            return None

        try:
            import re
            # Extract number
            match = re.search(r'[\d,]+\.?\d*', str(dim_str))
            if match:
                value = float(match.group().replace(',', ''))

                # Check for feet and convert to meters
                if 'ft' in str(dim_str).lower() or "'" in str(dim_str):
                    value = value * 0.3048

                return round(value, 2)
        except:
            pass

        return None

    def standardize_columns(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Standardize DataFrame columns to match template.

        Args:
            df: Raw DataFrame

        Returns:
            DataFrame with standardized columns
        """
        # Create mapping for common column name variations
        column_mapping = {
            'name': 'vessel_name',
            'vessel': 'vessel_name',
            'ship_name': 'vessel_name',
            'unit_name': 'vessel_name',
            'type': 'vessel_type',
            'built': 'year_built',
            'year': 'year_built',
            'delivered': 'year_built',
            'loa': 'length_m',
            'length': 'length_m',
            'length_oa': 'length_m',
            'breadth': 'beam_m',
            'width': 'beam_m',
            'draught': 'draft_m',
            'draft': 'draft_m',
            'dwt': 'displacement_tonnes',
            'displacement': 'displacement_tonnes',
            'water_depth': 'water_depth_m',
            'max_water_depth': 'water_depth_m',
            'deck_area': 'deck_space_m2',
            'accommodation': 'accommodation_capacity',
            'pob': 'accommodation_capacity',
            'dp_system': 'positioning_system',
            'positioning': 'positioning_system',
        }

        # Rename columns (case-insensitive)
        df_copy = df.copy()
        df_copy.columns = df_copy.columns.str.lower().str.strip()

        for old_col, new_col in column_mapping.items():
            if old_col in df_copy.columns and new_col not in df_copy.columns:
                df_copy.rename(columns={old_col: new_col}, inplace=True)

        return df_copy

    def validate_vessel_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Validate and clean vessel data.

        Args:
            df: Raw vessel DataFrame

        Returns:
            Validated and cleaned DataFrame
        """
        df_clean = df.copy()

        # Clean vessel names
        if 'vessel_name' in df_clean.columns:
            df_clean['vessel_name'] = df_clean['vessel_name'].apply(self.clean_vessel_name)

        # Parse years
        if 'year_built' in df_clean.columns:
            df_clean['year_built'] = df_clean['year_built'].apply(self.parse_year)

        # Parse dimensions
        dimension_cols = ['length_m', 'beam_m', 'depth_m', 'draft_m', 'water_depth_m']
        for col in dimension_cols:
            if col in df_clean.columns:
                df_clean[col] = df_clean[col].apply(self.parse_dimension)

        # Remove rows with no vessel name
        if 'vessel_name' in df_clean.columns:
            df_clean = df_clean[df_clean['vessel_name'].str.strip() != '']

        # Remove duplicate vessels
        if 'vessel_name' in df_clean.columns:
            df_clean = df_clean.drop_duplicates(subset=['vessel_name'], keep='first')

        self.logger.info(f"Validation: {len(df)} → {len(df_clean)} rows after cleaning")

        return df_clean

    def add_vessel_type(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Add vessel_type column if missing.

        Args:
            df: DataFrame

        Returns:
            DataFrame with vessel_type column
        """
        if 'vessel_type' not in df.columns:
            df['vessel_type'] = self.vessel_type

        return df

    def save_vessel_data(
        self,
        df: pd.DataFrame,
        filename: str,
        source_url: str,
        description: str
    ) -> Path:
        """
        Save vessel data with metadata.

        Args:
            df: Vessel DataFrame
            filename: Output filename
            source_url: Source URL
            description: Dataset description

        Returns:
            Path to saved CSV file
        """
        # Standardize and validate
        df = self.standardize_columns(df)
        df = self.add_vessel_type(df)
        df = self.validate_vessel_data(df)

        # Save CSV
        filepath = self.save_csv(df, filename, domain='vessels', subdomain='raw')

        # Save metadata
        additional_info = {
            "total_records": len(df),
            "vessel_type": self.vessel_type,
            "fields": list(df.columns),
            "year_range": {
                "min": int(df['year_built'].min()) if 'year_built' in df.columns and df['year_built'].notna().any() else None,
                "max": int(df['year_built'].max()) if 'year_built' in df.columns and df['year_built'].notna().any() else None
            } if 'year_built' in df.columns else None
        }

        self.save_metadata(filepath, source_url, description, additional_info)

        return filepath
