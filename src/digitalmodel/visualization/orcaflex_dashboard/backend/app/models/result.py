"""
Result Database Model

Represents analysis results data from OrcaFlex CSV files.
Stores processed data points, polar data, and time series results.
"""

from sqlalchemy import Column, Integer, String, DateTime, Float, JSON, Text, Boolean, ForeignKey, LargeBinary
from sqlalchemy.orm import relationship
from sqlalchemy.ext.declarative import declarative_base
from datetime import datetime
from typing import Dict, Any, Optional, List, Union
import numpy as np
import json
import pickle
import gzip

Base = declarative_base()


class Result(Base):
    """
    Result data model for storing OrcaFlex analysis results.
    
    Handles various result types:
    - Polar data (force/moment vs heading)
    - Time series data
    - Summary statistics
    - Component-specific results
    """
    
    __tablename__ = 'results'
    
    # Primary key and relationships
    id = Column(Integer, primary_key=True, autoincrement=True)
    analysis_id = Column(Integer, ForeignKey('analyses.id'), nullable=False, index=True)
    component_id = Column(Integer, ForeignKey('components.id'), nullable=True, index=True)
    
    # Result identification and metadata
    result_type = Column(String(50), nullable=False, index=True)  # polar, timeseries, summary, statistics
    parameter_name = Column(String(100), nullable=False, index=True)  # fx, my, displacement_z, etc.
    parameter_type = Column(String(50), nullable=True)  # force, moment, displacement, etc.
    units = Column(String(20), nullable=True)  # kN, kNm, m, deg
    
    # Data source information
    source_file = Column(String(255), nullable=True)
    source_column = Column(String(100), nullable=True)
    file_type = Column(String(20), nullable=True)  # dm_summary, time_trace, dm_inputs
    
    # Data dimensions and characteristics
    data_points = Column(Integer, nullable=True)  # Number of data points
    data_dimensions = Column(String(20), nullable=True)  # 1d, 2d, polar, timeseries
    
    # Statistical summary (always computed for quick access)
    min_value = Column(Float, nullable=True)
    max_value = Column(Float, nullable=True)
    mean_value = Column(Float, nullable=True)
    std_value = Column(Float, nullable=True)
    rms_value = Column(Float, nullable=True)
    
    # Time series specific fields
    time_start = Column(Float, nullable=True)
    time_end = Column(Float, nullable=True)
    time_step = Column(Float, nullable=True)
    sampling_rate = Column(Float, nullable=True)
    
    # Polar data specific fields
    heading_start = Column(Float, nullable=True)  # Usually 0°
    heading_end = Column(Float, nullable=True)    # Usually 345°
    heading_step = Column(Float, nullable=True)   # Usually 15°
    angular_resolution = Column(Integer, nullable=True)  # Number of angular points
    
    # Data quality and validation
    data_quality_score = Column(Float, nullable=True)  # 0.0 to 1.0
    completeness_ratio = Column(Float, nullable=True)  # Fraction of expected data present
    has_missing_data = Column(Boolean, default=False)
    has_outliers = Column(Boolean, default=False)
    
    # Compressed data storage (for large datasets)
    data_compressed = Column(LargeBinary, nullable=True)  # Pickled and gzipped numpy arrays
    data_format = Column(String(20), default='numpy_gzip')  # Format of stored data
    
    # JSON data storage (for smaller datasets and metadata)
    data_json = Column(JSON, nullable=True)
    metadata = Column(JSON, nullable=True)
    
    # Processing information
    processing_method = Column(String(50), nullable=True)  # Method used to process data
    processing_timestamp = Column(DateTime, default=datetime.utcnow)
    processing_duration_ms = Column(Float, nullable=True)
    
    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    analysis = relationship("Analysis", back_populates="results")
    component = relationship("Component", back_populates="results")
    
    def __init__(self, **kwargs):
        """Initialize result with default values"""
        super().__init__(**kwargs)
        if not self.metadata:
            self.metadata = {}
    
    def __repr__(self):
        return (f"<Result(id={self.id}, analysis_id={self.analysis_id}, "
                f"type='{self.result_type}', parameter='{self.parameter_name}')>")
    
    def store_data(self, data: Union[np.ndarray, List, Dict], 
                  compress: bool = True) -> None:
        """
        Store data in the result record.
        
        Args:
            data: Data to store (numpy array, list, or dictionary)
            compress: Whether to use compression for large data
        """
        try:
            if isinstance(data, (list, tuple)):
                data = np.array(data)
            
            # For small data or metadata, use JSON
            if isinstance(data, dict) or (hasattr(data, 'size') and data.size < 1000):
                if isinstance(data, np.ndarray):
                    self.data_json = data.tolist()
                else:
                    self.data_json = data
                self.data_format = 'json'
            
            # For large numerical data, use compression
            elif isinstance(data, np.ndarray) and compress:
                # Pickle and compress the data
                pickled_data = pickle.dumps(data)
                compressed_data = gzip.compress(pickled_data)
                self.data_compressed = compressed_data
                self.data_format = 'numpy_gzip'
                
                # Store basic info in JSON for quick access
                self.data_json = {
                    'shape': data.shape,
                    'dtype': str(data.dtype),
                    'size': int(data.size)
                }
            
            # Update data statistics
            if isinstance(data, np.ndarray) and data.size > 0:
                self._compute_statistics(data)
                self.data_points = int(data.size)
            
        except Exception as e:
            raise ValueError(f"Failed to store data: {str(e)}")
    
    def load_data(self) -> Union[np.ndarray, List, Dict, None]:
        """
        Load data from the result record.
        
        Returns:
            Stored data as numpy array, list, or dictionary
        """
        try:
            if self.data_format == 'json' and self.data_json is not None:
                data = self.data_json
                # Convert lists back to numpy arrays if they represent numerical data
                if isinstance(data, list) and len(data) > 0:
                    try:
                        return np.array(data)
                    except:
                        return data
                return data
            
            elif self.data_format == 'numpy_gzip' and self.data_compressed is not None:
                # Decompress and unpickle
                decompressed_data = gzip.decompress(self.data_compressed)
                return pickle.loads(decompressed_data)
            
            return None
            
        except Exception as e:
            raise ValueError(f"Failed to load data: {str(e)}")
    
    def _compute_statistics(self, data: np.ndarray) -> None:
        """Compute and store statistical summary of data"""
        try:
            # Handle complex data by taking magnitude
            if np.iscomplexobj(data):
                data = np.abs(data)
            
            # Remove NaN values for statistics
            clean_data = data[~np.isnan(data.flatten())]
            
            if len(clean_data) > 0:
                self.min_value = float(np.min(clean_data))
                self.max_value = float(np.max(clean_data))
                self.mean_value = float(np.mean(clean_data))
                self.std_value = float(np.std(clean_data))
                self.rms_value = float(np.sqrt(np.mean(clean_data**2)))
                
                # Data quality indicators
                self.completeness_ratio = len(clean_data) / data.size
                self.has_missing_data = len(clean_data) < data.size
                
                # Simple outlier detection using IQR
                if len(clean_data) > 4:
                    Q1 = np.percentile(clean_data, 25)
                    Q3 = np.percentile(clean_data, 75)
                    IQR = Q3 - Q1
                    outlier_threshold = 3.0
                    outliers = ((clean_data < (Q1 - outlier_threshold * IQR)) | 
                               (clean_data > (Q3 + outlier_threshold * IQR)))
                    self.has_outliers = np.any(outliers)
        
        except Exception as e:
            # Don't fail if statistics computation fails
            pass
    
    def store_polar_data(self, headings: np.ndarray, values: np.ndarray, 
                        units: Optional[str] = None) -> None:
        """
        Store polar data (values vs heading angles).
        
        Args:
            headings: Array of heading angles in degrees
            values: Array of corresponding values
            units: Engineering units
        """
        if len(headings) != len(values):
            raise ValueError("Headings and values must have same length")
        
        self.result_type = 'polar'
        self.data_dimensions = 'polar'
        
        # Store polar-specific metadata
        self.heading_start = float(np.min(headings))
        self.heading_end = float(np.max(headings))
        self.angular_resolution = len(headings)
        
        if len(headings) > 1:
            heading_diffs = np.diff(np.sort(headings))
            self.heading_step = float(np.mean(heading_diffs))
        
        if units:
            self.units = units
        
        # Store the data
        polar_data = {
            'headings': headings.tolist(),
            'values': values.tolist()
        }
        
        self.store_data(polar_data, compress=False)  # Polar data is usually small
        
        # Update metadata
        if not self.metadata:
            self.metadata = {}
        
        self.metadata['polar_info'] = {
            'heading_range': [self.heading_start, self.heading_end],
            'angular_points': self.angular_resolution,
            'mean_heading_step': self.heading_step
        }
    
    def store_time_series(self, time: np.ndarray, values: np.ndarray,
                         units: Optional[str] = None) -> None:
        """
        Store time series data.
        
        Args:
            time: Array of time values
            values: Array of corresponding values  
            units: Engineering units
        """
        if len(time) != len(values):
            raise ValueError("Time and values must have same length")
        
        self.result_type = 'timeseries'
        self.data_dimensions = 'timeseries'
        
        # Store time series specific metadata
        self.time_start = float(np.min(time))
        self.time_end = float(np.max(time))
        
        if len(time) > 1:
            time_diffs = np.diff(time)
            self.time_step = float(np.mean(time_diffs))
            self.sampling_rate = 1.0 / self.time_step if self.time_step > 0 else None
        
        if units:
            self.units = units
        
        # Store the data (use compression for large time series)
        timeseries_data = {
            'time': time,
            'values': values
        }
        
        compress = len(time) > 1000  # Compress large time series
        self.store_data(timeseries_data, compress=compress)
        
        # Update metadata
        if not self.metadata:
            self.metadata = {}
        
        self.metadata['timeseries_info'] = {
            'time_range': [self.time_start, self.time_end],
            'duration': self.time_end - self.time_start,
            'data_points': len(time),
            'sampling_rate': self.sampling_rate
        }
    
    def store_summary_statistics(self, stats: Dict[str, float]) -> None:
        """
        Store summary statistics.
        
        Args:
            stats: Dictionary with statistical measures
        """
        self.result_type = 'statistics'
        self.data_dimensions = '0d'
        
        # Extract standard statistics
        self.min_value = stats.get('min')
        self.max_value = stats.get('max') 
        self.mean_value = stats.get('mean')
        self.std_value = stats.get('std')
        self.rms_value = stats.get('rms')
        
        # Store all statistics as JSON
        self.data_json = stats
        self.data_format = 'json'
    
    def get_polar_data(self) -> Optional[Tuple[np.ndarray, np.ndarray]]:
        """
        Get polar data as (headings, values) arrays.
        
        Returns:
            Tuple of (headings, values) or None if not polar data
        """
        if self.result_type != 'polar':
            return None
        
        data = self.load_data()
        if isinstance(data, dict) and 'headings' in data and 'values' in data:
            return np.array(data['headings']), np.array(data['values'])
        
        return None
    
    def get_time_series(self) -> Optional[Tuple[np.ndarray, np.ndarray]]:
        """
        Get time series data as (time, values) arrays.
        
        Returns:
            Tuple of (time, values) or None if not time series data
        """
        if self.result_type != 'timeseries':
            return None
        
        data = self.load_data()
        if isinstance(data, dict) and 'time' in data and 'values' in data:
            return np.array(data['time']), np.array(data['values'])
        
        return None
    
    def get_statistics_summary(self) -> Dict[str, float]:
        """Get statistical summary of the result data"""
        stats = {}
        
        # Get stored statistics
        if self.min_value is not None:
            stats['min'] = self.min_value
        if self.max_value is not None:
            stats['max'] = self.max_value
        if self.mean_value is not None:
            stats['mean'] = self.mean_value
        if self.std_value is not None:
            stats['std'] = self.std_value
        if self.rms_value is not None:
            stats['rms'] = self.rms_value
        
        # Add range and other derived statistics
        if self.min_value is not None and self.max_value is not None:
            stats['range'] = self.max_value - self.min_value
        
        if self.completeness_ratio is not None:
            stats['completeness'] = self.completeness_ratio
        
        return stats
    
    def to_dict(self, include_data: bool = False) -> Dict[str, Any]:
        """
        Convert result to dictionary representation.
        
        Args:
            include_data: Whether to include the actual data arrays
            
        Returns:
            Dictionary representation of the result
        """
        data = {
            'id': self.id,
            'analysis_id': self.analysis_id,
            'component_id': self.component_id,
            
            'result_type': self.result_type,
            'parameter_name': self.parameter_name,
            'parameter_type': self.parameter_type,
            'units': self.units,
            
            'source_file': self.source_file,
            'source_column': self.source_column,
            'file_type': self.file_type,
            
            'data_points': self.data_points,
            'data_dimensions': self.data_dimensions,
            
            'statistics': self.get_statistics_summary(),
            
            'time_info': {
                'time_start': self.time_start,
                'time_end': self.time_end,
                'time_step': self.time_step,
                'sampling_rate': self.sampling_rate
            } if self.result_type == 'timeseries' else None,
            
            'polar_info': {
                'heading_start': self.heading_start,
                'heading_end': self.heading_end,
                'heading_step': self.heading_step,
                'angular_resolution': self.angular_resolution
            } if self.result_type == 'polar' else None,
            
            'quality': {
                'data_quality_score': self.data_quality_score,
                'completeness_ratio': self.completeness_ratio,
                'has_missing_data': self.has_missing_data,
                'has_outliers': self.has_outliers
            },
            
            'processing': {
                'method': self.processing_method,
                'timestamp': self.processing_timestamp.isoformat() if self.processing_timestamp else None,
                'duration_ms': self.processing_duration_ms
            },
            
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'metadata': self.metadata
        }
        
        if include_data:
            loaded_data = self.load_data()
            if loaded_data is not None:
                # Convert numpy arrays to lists for JSON serialization
                if isinstance(loaded_data, np.ndarray):
                    data['data'] = loaded_data.tolist()
                elif isinstance(loaded_data, dict):
                    data['data'] = {}
                    for k, v in loaded_data.items():
                        if isinstance(v, np.ndarray):
                            data['data'][k] = v.tolist()
                        else:
                            data['data'][k] = v
                else:
                    data['data'] = loaded_data
        
        return data
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'Result':
        """
        Create Result from dictionary data.
        
        Args:
            data: Dictionary with result data
            
        Returns:
            Result instance
        """
        # Convert datetime strings back to datetime objects
        datetime_fields = ['created_at', 'updated_at', 'processing_timestamp']
        for field in datetime_fields:
            if field in data and isinstance(data[field], str):
                data[field] = datetime.fromisoformat(data[field])
        
        # Extract nested data structures
        if 'statistics' in data:
            stats = data.pop('statistics')
            data.update({
                'min_value': stats.get('min'),
                'max_value': stats.get('max'),
                'mean_value': stats.get('mean'),
                'std_value': stats.get('std'),
                'rms_value': stats.get('rms')
            })
        
        if 'time_info' in data and data['time_info']:
            time_info = data.pop('time_info')
            data.update(time_info)
        
        if 'polar_info' in data and data['polar_info']:
            polar_info = data.pop('polar_info')
            data.update(polar_info)
        
        if 'quality' in data:
            quality = data.pop('quality')
            data.update(quality)
        
        if 'processing' in data:
            processing = data.pop('processing')
            data.update({
                'processing_method': processing.get('method'),
                'processing_timestamp': processing.get('timestamp'),
                'processing_duration_ms': processing.get('duration_ms')
            })
        
        # Remove data field if present (handled separately)
        data.pop('data', None)
        
        return cls(**data)
    
    def get_data_size_mb(self) -> float:
        """Get approximate size of stored data in MB"""
        size_bytes = 0
        
        if self.data_compressed:
            size_bytes += len(self.data_compressed)
        
        if self.data_json:
            # Rough estimate of JSON size
            import sys
            size_bytes += sys.getsizeof(str(self.data_json))
        
        return size_bytes / (1024 * 1024)
    
    def validate_data_integrity(self) -> Dict[str, Any]:
        """Validate integrity of stored data"""
        validation = {
            'is_valid': True,
            'issues': [],
            'data_accessible': False,
            'statistics_consistent': True
        }
        
        try:
            # Test data loading
            data = self.load_data()
            validation['data_accessible'] = data is not None
            
            if not validation['data_accessible']:
                validation['is_valid'] = False
                validation['issues'].append("Data cannot be loaded")
            
            # Validate statistics consistency if we have numerical data
            if isinstance(data, np.ndarray) and data.size > 0:
                computed_stats = {}
                clean_data = data[~np.isnan(data.flatten())]
                
                if len(clean_data) > 0:
                    computed_stats['min'] = float(np.min(clean_data))
                    computed_stats['max'] = float(np.max(clean_data))
                    computed_stats['mean'] = float(np.mean(clean_data))
                    
                    # Check if stored statistics match computed ones
                    tolerance = 1e-6
                    for stat, computed_val in computed_stats.items():
                        stored_val = getattr(self, f"{stat}_value")
                        if stored_val is not None:
                            if abs(stored_val - computed_val) > tolerance:
                                validation['statistics_consistent'] = False
                                validation['issues'].append(
                                    f"Stored {stat} ({stored_val}) doesn't match computed {stat} ({computed_val})"
                                )
        
        except Exception as e:
            validation['is_valid'] = False
            validation['issues'].append(f"Data validation failed: {str(e)}")
        
        return validation


# Index definitions for performance
def create_indexes(engine):
    """Create database indexes for performance optimization"""
    from sqlalchemy import Index
    
    indexes = [
        Index('idx_result_analysis', Result.analysis_id),
        Index('idx_result_component', Result.component_id),
        Index('idx_result_type_param', Result.result_type, Result.parameter_name),
        Index('idx_result_source', Result.source_file, Result.file_type),
        Index('idx_result_created', Result.created_at),
        Index('idx_result_quality', Result.data_quality_score, Result.completeness_ratio),
    ]
    
    for index in indexes:
        index.create(engine, checkfirst=True)