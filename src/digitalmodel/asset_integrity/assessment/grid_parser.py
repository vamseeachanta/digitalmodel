"""Grid parser for FFS wall thickness inspection data.

Normalises any supported input format to a 2-D pandas DataFrame where:
  rows  = longitudinal (axial) stations
  cols  = circumferential stations
  values = absolute wall thickness in inches (internal unit system)

Supported inputs:
  - pandas DataFrame (direct or C-scan percentage format)
  - CSV file-like object
  - NumPy 2-D array
  - XLSX handled externally via the existing ExcelRead helper; pass result here

API 579-1 Part 4 / Part 5 assessments expect absolute thickness in inches.
All unit conversion is performed here so downstream modules stay unit-agnostic.
"""

from __future__ import annotations

from typing import IO, Optional, Union

import numpy as np
import pandas as pd

# Conversion constant — 1 inch = 25.4 mm
_MM_PER_INCH = 25.4
_SUPPORTED_UNITS = {"in", "inch", "inches", "mm", "millimeter", "millimeters"}


class GridParser:
    """Static helpers for normalising wall-thickness grid data."""

    @staticmethod
    def from_dataframe(
        df: pd.DataFrame,
        *,
        data_correction_factor: float = 1.0,
        input_units: str = "in",
        drop_all_nan_rows: bool = False,
    ) -> pd.DataFrame:
        """Normalise a DataFrame to absolute wall thickness in inches.

        Args:
            df: Source DataFrame.  Values may be absolute thickness (inches or
                mm) or C-scan percentages when data_correction_factor != 1.0.
            data_correction_factor: Multiplied onto every value.  Use 0.833 for
                C-scan percentage grids where 100 % corresponds to nominal WT.
            input_units: ``'in'`` / ``'inch'`` / ``'inches'`` (default) or
                ``'mm'`` / ``'millimeter'`` / ``'millimeters'``.  Applied
                *after* data_correction_factor.
            drop_all_nan_rows: When True, rows that are entirely NaN are
                removed.

        Returns:
            DataFrame of absolute wall thickness in inches.

        Raises:
            ValueError: If df is empty, or input_units is not recognised.
        """
        if df.empty:
            raise ValueError("Cannot parse an empty DataFrame.")

        normalised_units = input_units.lower().strip()
        if normalised_units not in _SUPPORTED_UNITS:
            raise ValueError(
                f"input_units '{input_units}' is not supported.  "
                f"Use one of: {sorted(_SUPPORTED_UNITS)}"
            )

        result = df.copy().astype(float)

        # Apply vendor data correction factor (e.g. C-scan %)
        if data_correction_factor != 1.0:
            result = result * data_correction_factor

        # Unit conversion
        if normalised_units in {"mm", "millimeter", "millimeters"}:
            result = result / _MM_PER_INCH

        if drop_all_nan_rows:
            result = result.dropna(how="all")

        return result

    @staticmethod
    def from_csv(
        source: Union[str, IO],
        *,
        header: Optional[int] = None,
        data_correction_factor: float = 1.0,
        input_units: str = "in",
        skip_rows: int = 0,
        skip_footer: int = 0,
        index_col: Optional[int] = None,
    ) -> pd.DataFrame:
        """Parse a CSV file or file-like object into a wall-thickness DataFrame.

        Args:
            source: File path string or file-like object.
            header: Row number(s) to use as column names.  None means no
                header row (default for raw thickness grids).
            data_correction_factor: Passed to :meth:`from_dataframe`.
            input_units: Passed to :meth:`from_dataframe`.
            skip_rows: Number of leading data rows to skip.
            skip_footer: Number of trailing data rows to skip.
            index_col: Column to use as row labels, or None.

        Returns:
            Normalised DataFrame in inches.
        """
        read_kwargs: dict = {
            "header": header,
            "skiprows": skip_rows,
            "skipfooter": skip_footer,
            "engine": "python" if skip_footer else "c",
            "index_col": index_col,
        }
        df = pd.read_csv(source, **read_kwargs)
        return GridParser.from_dataframe(
            df,
            data_correction_factor=data_correction_factor,
            input_units=input_units,
        )

    @staticmethod
    def from_numpy(
        array: np.ndarray,
        *,
        data_correction_factor: float = 1.0,
        input_units: str = "in",
    ) -> pd.DataFrame:
        """Convert a 2-D NumPy array to a wall-thickness DataFrame.

        Args:
            array: 2-D array of thickness values.
            data_correction_factor: Passed to :meth:`from_dataframe`.
            input_units: Passed to :meth:`from_dataframe`.

        Returns:
            Normalised DataFrame in inches.

        Raises:
            ValueError: If array is not 2-D.
        """
        if array.ndim != 2:
            raise ValueError(
                f"Expected a 2-D array; got ndim={array.ndim}.  "
                "Reshape the array before calling from_numpy()."
            )
        df = pd.DataFrame(array)
        return GridParser.from_dataframe(
            df,
            data_correction_factor=data_correction_factor,
            input_units=input_units,
        )

    # ------------------------------------------------------------------
    # Summary statistics
    # ------------------------------------------------------------------

    @staticmethod
    def min_thickness(df: pd.DataFrame) -> float:
        """Return the minimum (non-NaN) thickness value in the grid (inches)."""
        return float(df.min(skipna=True).min())

    @staticmethod
    def max_thickness(df: pd.DataFrame) -> float:
        """Return the maximum (non-NaN) thickness value in the grid (inches)."""
        return float(df.max(skipna=True).max())

    @staticmethod
    def mean_thickness(df: pd.DataFrame) -> float:
        """Return the area-averaged mean thickness over the grid (inches)."""
        return float(df.mean(skipna=True).mean())
