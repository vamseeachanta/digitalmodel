"""
CSV Parser for OrcaFlex dm_* summary files
"""

import re
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd

from data_models import (
    ComponentClassification,
    LoadingConditionInfo,
    PolarDataPoint,
    ParsedData,
)
from utils.logger import setup_logger

logger = setup_logger(__name__)


class OrcaFlexCSVParser:
    """Parser for OrcaFlex CSV output files"""
    
    # Heading points for polar data (0° to 345° in 15° increments)
    POLAR_HEADINGS = list(range(0, 360, 15))
    
    # Component type patterns
    COMPONENT_PATTERNS = {
        "fst1": re.compile(r"fst1|FST1|floater_1"),
        "fst2": re.compile(r"fst2|FST2|floater_2"),
        "strut": re.compile(r"strut|STRUT|diagonal"),
        "jacket": re.compile(r"jacket|JACKET|foundation"),
        "lngc": re.compile(r"lngc|LNGC|carrier"),
    }
    
    # Loading condition patterns
    LOADING_PATTERNS = {
        "water_level": re.compile(r"(hwl|lwl|HWL|LWL)"),
        "volume": re.compile(r"(125km3|180km3)"),
        "ballast": re.compile(r"(pb|sb|PB|SB)"),
    }
    
    def __init__(self, base_directory: Path):
        """
        Initialize parser with base directory
        
        Args:
            base_directory: Base directory containing CSV files
        """
        self.base_directory = Path(base_directory)
        if not self.base_directory.exists():
            raise ValueError(f"Directory does not exist: {base_directory}")
    
    def parse_file(self, file_path: Path) -> ParsedData:
        """
        Parse a single CSV file
        
        Args:
            file_path: Path to CSV file
        
        Returns:
            ParsedData object containing parsed information
        """
        logger.info(f"Parsing file: {file_path}")
        
        try:
            # Read CSV file
            df = pd.read_csv(file_path)
            
            # Extract metadata from filename
            metadata = self._extract_metadata(file_path.name)
            
            # Classify component
            component = self._classify_component(df, metadata)
            
            # Parse loading condition
            loading = self._parse_loading_condition(metadata)
            
            # Extract polar data if applicable
            polar_data = self._extract_polar_data(df)
            
            # Extract time trace data if applicable
            time_trace = self._extract_time_trace(df)
            
            # Calculate statistics
            statistics = self._calculate_statistics(df)
            
            return ParsedData(
                file_path=file_path,
                case_name=metadata.get("case", "unknown"),
                component=component,
                loading_condition=loading,
                polar_data=polar_data,
                time_trace_data=time_trace,
                statistics=statistics,
                raw_data=df,
            )
            
        except Exception as e:
            logger.error(f"Failed to parse {file_path}: {e}")
            raise
    
    def parse_directory(
        self,
        pattern: str = "dm_*.csv",
    ) -> List[ParsedData]:
        """
        Parse all matching CSV files in directory
        
        Args:
            pattern: File pattern to match
        
        Returns:
            List of ParsedData objects
        """
        files = list(self.base_directory.glob(pattern))
        logger.info(f"Found {len(files)} files matching pattern {pattern}")
        
        parsed_data = []
        for file_path in files:
            try:
                data = self.parse_file(file_path)
                parsed_data.append(data)
            except Exception as e:
                logger.warning(f"Skipping {file_path}: {e}")
        
        return parsed_data
    
    def _extract_metadata(self, filename: str) -> Dict[str, str]:
        """
        Extract metadata from filename
        
        Args:
            filename: Name of the file
        
        Returns:
            Dictionary of metadata
        """
        metadata = {}
        
        # Remove extension
        name = filename.rsplit(".", 1)[0]
        
        # Split by underscore
        parts = name.split("_")
        
        # Extract case name (usually first part after dm_)
        if len(parts) > 1:
            metadata["case"] = parts[1]
        
        # Extract other metadata
        metadata["full_name"] = name
        metadata["parts"] = parts
        
        return metadata
    
    def _classify_component(
        self,
        df: pd.DataFrame,
        metadata: Dict[str, str],
    ) -> ComponentClassification:
        """
        Classify component type based on data and metadata
        
        Args:
            df: DataFrame containing CSV data
            metadata: Metadata extracted from filename
        
        Returns:
            ComponentClassification object
        """
        # Check filename for component type
        full_name = metadata.get("full_name", "").lower()
        
        for comp_type, pattern in self.COMPONENT_PATTERNS.items():
            if pattern.search(full_name):
                return ComponentClassification(
                    type=comp_type,
                    name=metadata.get("case", "unknown"),
                    confidence=0.9,
                )
        
        # Check column names for hints
        columns = " ".join(df.columns).lower()
        for comp_type, pattern in self.COMPONENT_PATTERNS.items():
            if pattern.search(columns):
                return ComponentClassification(
                    type=comp_type,
                    name=metadata.get("case", "unknown"),
                    confidence=0.7,
                )
        
        # Default to unknown
        return ComponentClassification(
            type="unknown",
            name=metadata.get("case", "unknown"),
            confidence=0.0,
        )
    
    def _parse_loading_condition(
        self,
        metadata: Dict[str, str],
    ) -> LoadingConditionInfo:
        """
        Parse loading condition from metadata
        
        Args:
            metadata: Metadata dictionary
        
        Returns:
            LoadingConditionInfo object
        """
        full_name = metadata.get("full_name", "").lower()
        
        # Extract water level
        water_level = "unknown"
        if match := self.LOADING_PATTERNS["water_level"].search(full_name):
            water_level = match.group(1).lower()
        
        # Extract volume
        volume = "unknown"
        if match := self.LOADING_PATTERNS["volume"].search(full_name):
            volume = match.group(1).lower()
        
        # Extract ballast condition
        ballast = "unknown"
        if match := self.LOADING_PATTERNS["ballast"].search(full_name):
            ballast = match.group(1).lower()
        
        # Create combined condition string
        condition = f"{water_level}_{volume}_{ballast}"
        
        return LoadingConditionInfo(
            water_level=water_level,
            volume=volume,
            ballast=ballast,
            combined=condition,
        )
    
    def _extract_polar_data(
        self,
        df: pd.DataFrame,
    ) -> Optional[List[PolarDataPoint]]:
        """
        Extract polar data from DataFrame
        
        Args:
            df: DataFrame containing CSV data
        
        Returns:
            List of PolarDataPoint objects or None
        """
        # Check if we have heading columns
        heading_cols = [col for col in df.columns if "heading" in col.lower()]
        
        if not heading_cols:
            return None
        
        polar_data = []
        
        # Look for data at standard heading points
        for heading in self.POLAR_HEADINGS:
            # Find matching row
            heading_str = f"{heading:03d}"
            mask = df.iloc[:, 0].astype(str).str.contains(heading_str)
            
            if mask.any():
                row = df[mask].iloc[0]
                # Extract response value (usually in column 2 or 3)
                value_cols = [col for col in df.columns if any(
                    x in col.lower() for x in ["max", "force", "moment", "tension"]
                )]
                
                if value_cols:
                    value = float(row[value_cols[0]])
                    polar_data.append(
                        PolarDataPoint(
                            heading=heading,
                            value=value,
                            unit=self._extract_unit(value_cols[0]),
                        )
                    )
        
        return polar_data if polar_data else None
    
    def _extract_time_trace(
        self,
        df: pd.DataFrame,
    ) -> Optional[pd.DataFrame]:
        """
        Extract time trace data from DataFrame
        
        Args:
            df: DataFrame containing CSV data
        
        Returns:
            DataFrame with time trace data or None
        """
        # Check for time column
        time_cols = [col for col in df.columns if "time" in col.lower()]
        
        if not time_cols:
            return None
        
        # Get value columns
        value_cols = [col for col in df.columns if col not in time_cols]
        
        if not value_cols:
            return None
        
        # Create time trace DataFrame
        trace_df = pd.DataFrame()
        trace_df["time"] = df[time_cols[0]]
        
        for col in value_cols[:5]:  # Limit to first 5 value columns
            trace_df[col] = df[col]
        
        return trace_df
    
    def _calculate_statistics(
        self,
        df: pd.DataFrame,
    ) -> Dict[str, float]:
        """
        Calculate statistics from data
        
        Args:
            df: DataFrame containing CSV data
        
        Returns:
            Dictionary of statistics
        """
        stats = {}
        
        # Get numeric columns
        numeric_cols = df.select_dtypes(include=["float64", "int64"]).columns
        
        for col in numeric_cols:
            if "time" not in col.lower():
                stats[f"{col}_max"] = float(df[col].max())
                stats[f"{col}_min"] = float(df[col].min())
                stats[f"{col}_mean"] = float(df[col].mean())
                stats[f"{col}_std"] = float(df[col].std())
        
        return stats
    
    def _extract_unit(self, column_name: str) -> str:
        """
        Extract unit from column name
        
        Args:
            column_name: Name of the column
        
        Returns:
            Unit string
        """
        # Common unit patterns
        unit_patterns = [
            (r"\[([^\]]+)\]", 1),  # [unit]
            (r"\(([^\)]+)\)", 1),  # (unit)
            (r"_([A-Za-z]+)$", 1),  # _unit at end
        ]
        
        for pattern, group in unit_patterns:
            if match := re.search(pattern, column_name):
                return match.group(group)
        
        # Default units based on column content
        if "force" in column_name.lower():
            return "kN"
        elif "moment" in column_name.lower():
            return "kNm"
        elif "tension" in column_name.lower():
            return "kN"
        elif "angle" in column_name.lower():
            return "deg"
        
        return ""