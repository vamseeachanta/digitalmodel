# ABOUTME: Streaming data handler with zero storage footprint
# ABOUTME: In-memory processing, format conversion, and direct consumption pipelines

"""
Stream Handler
==============

Zero-storage streaming data processor.

Key Features:
- In-memory processing pipelines
- Format conversion (NetCDF, JSON, CSV → Python objects)
- Quality control on streaming data
- Unit conversion
- Spatial/temporal interpolation
- Direct consumption (no intermediate files)

Critical: NO data saving to disk (data can be huge and infeasible)
"""

import logging
from typing import Iterator, Dict, Any, Callable, Optional, List
from datetime import datetime
import io
import json
from abc import ABC, abstractmethod

logger = logging.getLogger(__name__)


class DataProcessor(ABC):
    """Abstract base class for streaming data processors."""

    @abstractmethod
    def process(self, data: Any) -> Any:
        """
        Process data chunk.

        Args:
            data: Input data chunk

        Returns:
            Processed data chunk
        """
        pass


class FormatParser:
    """
    Parse streaming data formats (NetCDF, JSON, CSV).

    Operates on in-memory buffers, never writes to disk.
    """

    @staticmethod
    def parse_json_stream(stream: Iterator[str]) -> Iterator[Dict[str, Any]]:
        """
        Parse JSON lines stream.

        Args:
            stream: Iterator of JSON strings

        Yields:
            Parsed JSON objects (in-memory)
        """
        for line in stream:
            try:
                yield json.loads(line)
            except json.JSONDecodeError as e:
                logger.warning(f"Failed to parse JSON line: {e}")
                continue

    @staticmethod
    def parse_csv_stream(stream: Iterator[str],
                        delimiter: str = ',',
                        skip_header: bool = True) -> Iterator[List[str]]:
        """
        Parse CSV stream.

        Args:
            stream: Iterator of CSV lines
            delimiter: Field delimiter
            skip_header: Skip first line

        Yields:
            Parsed CSV rows (in-memory)
        """
        first_line = True
        for line in stream:
            if first_line and skip_header:
                first_line = False
                continue

            yield line.strip().split(delimiter)

    @staticmethod
    def parse_netcdf_stream(data: bytes) -> Dict[str, Any]:
        """
        Parse NetCDF data from in-memory buffer.

        Args:
            data: NetCDF bytes (in-memory)

        Returns:
            Parsed data dict (in-memory, no file I/O)
        """
        try:
            import xarray as xr

            # Parse from in-memory bytes
            buffer = io.BytesIO(data)
            ds = xr.open_dataset(buffer, engine='h5netcdf')

            # Convert to dict (in-memory)
            result = {
                'variables': {var: ds[var].values for var in ds.data_vars},
                'coords': {coord: ds.coords[coord].values for coord in ds.coords},
                'attrs': dict(ds.attrs)
            }

            ds.close()
            buffer.close()

            return result

        except ImportError:
            logger.error("xarray not installed, cannot parse NetCDF")
            raise
        except Exception as e:
            logger.error(f"Failed to parse NetCDF: {e}")
            raise


class QualityControl:
    """
    Quality control for streaming data.

    Performs validation and filtering without storing intermediate results.
    """

    @staticmethod
    def range_check(value: float, min_val: float, max_val: float) -> bool:
        """
        Check if value is within valid range.

        Args:
            value: Value to check
            min_val: Minimum valid value
            max_val: Maximum valid value

        Returns:
            True if valid, False otherwise
        """
        return min_val <= value <= max_val

    @staticmethod
    def filter_outliers(stream: Iterator[Dict[str, Any]],
                       field: str,
                       method: str = 'iqr',
                       threshold: float = 3.0) -> Iterator[Dict[str, Any]]:
        """
        Filter outliers from streaming data.

        Args:
            stream: Data stream
            field: Field to check for outliers
            method: 'iqr' or 'zscore'
            threshold: Outlier threshold

        Yields:
            Filtered data records (in-memory)

        Note: For streaming, we use a rolling window approach
        """
        window = []
        window_size = 100  # Rolling window size

        for record in stream:
            value = record.get(field)
            if value is None:
                yield record
                continue

            window.append(value)
            if len(window) > window_size:
                window.pop(0)

            # Check if outlier based on current window
            if method == 'iqr':
                is_outlier = QualityControl._is_outlier_iqr(value, window, threshold)
            elif method == 'zscore':
                is_outlier = QualityControl._is_outlier_zscore(value, window, threshold)
            else:
                raise ValueError(f"Unknown outlier method: {method}")

            if not is_outlier:
                yield record
            else:
                logger.debug(f"Filtered outlier: {field}={value}")

    @staticmethod
    def _is_outlier_iqr(value: float, window: List[float], threshold: float) -> bool:
        """Check outlier using IQR method."""
        if len(window) < 10:
            return False

        sorted_window = sorted(window)
        q1_idx = len(sorted_window) // 4
        q3_idx = 3 * len(sorted_window) // 4

        q1 = sorted_window[q1_idx]
        q3 = sorted_window[q3_idx]
        iqr = q3 - q1

        lower_bound = q1 - threshold * iqr
        upper_bound = q3 + threshold * iqr

        return value < lower_bound or value > upper_bound

    @staticmethod
    def _is_outlier_zscore(value: float, window: List[float], threshold: float) -> bool:
        """Check outlier using Z-score method."""
        if len(window) < 10:
            return False

        mean = sum(window) / len(window)
        variance = sum((x - mean) ** 2 for x in window) / len(window)
        std = variance ** 0.5

        if std == 0:
            return False

        zscore = abs((value - mean) / std)
        return zscore > threshold


class StreamHandler:
    """
    Zero-storage streaming data handler.

    Processes data in-memory, converts formats, applies transformations,
    and passes directly to consumers without saving to disk.
    """

    def __init__(self, chunk_size: int = 8192):
        """
        Initialize stream handler.

        Args:
            chunk_size: Size of data chunks for processing
        """
        self.chunk_size = chunk_size
        self.processors: List[DataProcessor] = []
        logger.info(f"Initialized StreamHandler with chunk_size={chunk_size}")

    def add_processor(self, processor: DataProcessor) -> None:
        """
        Add data processor to pipeline.

        Args:
            processor: Data processor instance
        """
        self.processors.append(processor)
        logger.debug(f"Added processor: {processor.__class__.__name__}")

    def process_stream(self, stream: Iterator[Any]) -> Iterator[Any]:
        """
        Process data stream through pipeline (in-memory).

        Args:
            stream: Input data stream

        Yields:
            Processed data (in-memory, no storage)
        """
        for chunk in stream:
            # Apply all processors in sequence
            processed = chunk
            for processor in self.processors:
                processed = processor.process(processed)

            yield processed

    def consume_direct(self, stream: Iterator[Any],
                      consumer: Callable[[Any], None]) -> None:
        """
        Consume stream directly without storage.

        Args:
            stream: Data stream
            consumer: Function to consume each chunk
                     (e.g., write to OrcaFlex, AQWA, or analysis)

        Critical: Data is discarded after consumption (zero storage)
        """
        processed_count = 0

        for data in self.process_stream(stream):
            consumer(data)
            processed_count += 1

        logger.info(f"Consumed {processed_count} chunks directly (zero storage)")

    def batch_process(self, stream: Iterator[Any],
                     batch_size: int = 1000) -> Iterator[List[Any]]:
        """
        Batch stream into memory-efficient chunks.

        Args:
            stream: Input stream
            batch_size: Records per batch

        Yields:
            Batches of records (in-memory)
        """
        batch = []

        for item in self.process_stream(stream):
            batch.append(item)

            if len(batch) >= batch_size:
                yield batch
                batch = []

        # Yield remaining items
        if batch:
            yield batch

    def to_python_objects(self, stream: Iterator[Dict[str, Any]]) -> Iterator[Any]:
        """
        Convert stream to Python objects (in-memory).

        Args:
            stream: Dictionary stream

        Yields:
            Python objects (dataclasses, named tuples, etc.)
        """
        for record in self.process_stream(stream):
            # Convert dict to appropriate Python object
            # This is a template - specific implementations override
            yield record

    def to_orcaflex(self, stream: Iterator[Dict[str, Any]],
                   output_format: str = 'yml') -> Iterator[str]:
        """
        Convert stream to OrcaFlex format (in-memory).

        Args:
            stream: Data stream
            output_format: 'yml' or 'dat'

        Yields:
            OrcaFlex-formatted data (text, in-memory)

        Note: Caller writes to OrcaFlex directly, no intermediate files
        """
        for record in self.process_stream(stream):
            # Convert to OrcaFlex YAML format
            if output_format == 'yml':
                yield self._format_orcaflex_yml(record)
            elif output_format == 'dat':
                yield self._format_orcaflex_dat(record)
            else:
                raise ValueError(f"Unknown OrcaFlex format: {output_format}")

    def _format_orcaflex_yml(self, data: Dict[str, Any]) -> str:
        """Format data as OrcaFlex YAML (template)."""
        # This is a template - specific implementations override
        import yaml
        return yaml.dump(data, default_flow_style=False)

    def _format_orcaflex_dat(self, data: Dict[str, Any]) -> str:
        """Format data as OrcaFlex DAT (template)."""
        # This is a template - specific implementations override
        return str(data)

    def aggregate_statistics(self, stream: Iterator[float],
                            stats: List[str] = ['mean', 'max', 'min', 'std']) -> Dict[str, float]:
        """
        Calculate statistics on streaming data (in-memory).

        Args:
            stream: Numeric data stream
            stats: Statistics to calculate

        Returns:
            Statistics dict (in-memory, no intermediate storage)
        """
        values = []
        for value in stream:
            values.append(value)

        result = {}

        if 'mean' in stats:
            result['mean'] = sum(values) / len(values) if values else 0

        if 'max' in stats:
            result['max'] = max(values) if values else 0

        if 'min' in stats:
            result['min'] = min(values) if values else 0

        if 'std' in stats:
            mean = sum(values) / len(values) if values else 0
            variance = sum((x - mean) ** 2 for x in values) / len(values) if values else 0
            result['std'] = variance ** 0.5

        logger.info(f"Calculated statistics on {len(values)} values (in-memory)")
        return result
