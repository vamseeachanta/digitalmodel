"""
Component Database Model

Represents classified OrcaFlex analysis components (fst1, fst2, strut, jacket, lngc).
Stores component hierarchy, parameters, and relationships to results.
"""

from sqlalchemy import Column, Integer, String, DateTime, Float, JSON, Text, Boolean, ForeignKey
from sqlalchemy.orm import relationship
from sqlalchemy.ext.declarative import declarative_base
from datetime import datetime
from typing import Dict, Any, Optional, List
import json

Base = declarative_base()


class Component(Base):
    """
    Component model representing classified elements from OrcaFlex analysis.
    
    Components represent physical or logical elements in the offshore system:
    - fst1, fst2: Floating platforms/hulls
    - strut: Connectors and structural elements
    - jacket: Foundation structures
    - lngc: LNG Carrier vessels
    """
    
    __tablename__ = 'components'
    
    # Primary key and relationships
    id = Column(Integer, primary_key=True, autoincrement=True)
    analysis_id = Column(Integer, ForeignKey('analyses.id'), nullable=False, index=True)
    
    # Component identification and classification
    name = Column(String(100), nullable=False)  # Component name (e.g., 'fst1', 'strut_1')
    component_type = Column(String(50), nullable=False, index=True)  # fst1, fst2, strut, jacket, lngc, unknown
    display_name = Column(String(150), nullable=True)  # Human-readable name
    description = Column(Text, nullable=True)
    
    # Classification metadata
    classification_confidence = Column(Float, nullable=True)  # 0.0 to 1.0
    classification_method = Column(String(50), default='pattern_matching')
    column_count = Column(Integer, default=0)  # Number of associated columns
    
    # Component hierarchy and relationships
    parent_component_id = Column(Integer, ForeignKey('components.id'), nullable=True)
    component_level = Column(Integer, default=0)  # 0=main, 1=sub-component, etc.
    sort_order = Column(Integer, default=0)  # For consistent ordering in UI
    
    # Engineering metadata
    parameter_types = Column(JSON, nullable=True)  # List of parameter types (forces, moments, etc.)
    primary_parameters = Column(JSON, nullable=True)  # Main parameters of interest
    units_used = Column(JSON, nullable=True)  # Engineering units found
    coordinate_system = Column(String(50), nullable=True)  # global, local, body
    
    # Data source information
    source_columns = Column(JSON, nullable=True)  # Original CSV column names
    source_file_types = Column(JSON, nullable=True)  # Types of files contributing data
    
    # Component characteristics
    has_polar_data = Column(Boolean, default=False)
    has_time_series_data = Column(Boolean, default=False)
    has_summary_data = Column(Boolean, default=False)
    
    # Statistical summary
    total_data_points = Column(Integer, default=0)
    result_count = Column(Integer, default=0)
    data_quality_score = Column(Float, nullable=True)
    
    # Processing status
    processing_status = Column(String(20), default='pending')  # pending, processed, error
    last_processed = Column(DateTime, nullable=True)
    
    # Validation and quality flags
    is_validated = Column(Boolean, default=False)
    has_anomalies = Column(Boolean, default=False)
    is_primary_component = Column(Boolean, default=True)  # Main components vs derived/calculated
    
    # Additional metadata (flexible JSON field)
    metadata = Column(JSON, nullable=True)
    
    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    analysis = relationship("Analysis", back_populates="components")
    results = relationship("Result", back_populates="component", cascade="all, delete-orphan")
    
    # Self-referential relationship for component hierarchy
    children = relationship("Component", 
                          foreign_keys=[parent_component_id],
                          backref="parent",
                          remote_side=[id])
    
    def __init__(self, **kwargs):
        """Initialize component with default values"""
        super().__init__(**kwargs)
        if not self.metadata:
            self.metadata = {}
        if not self.parameter_types:
            self.parameter_types = []
        if not self.source_columns:
            self.source_columns = []
    
    def __repr__(self):
        return (f"<Component(id={self.id}, name='{self.name}', "
                f"type='{self.component_type}', analysis_id={self.analysis_id})>")
    
    def to_dict(self, include_relationships: bool = False) -> Dict[str, Any]:
        """
        Convert component to dictionary representation.
        
        Args:
            include_relationships: Whether to include results and children
            
        Returns:
            Dictionary representation of the component
        """
        data = {
            'id': self.id,
            'analysis_id': self.analysis_id,
            'parent_component_id': self.parent_component_id,
            
            'name': self.name,
            'component_type': self.component_type,
            'display_name': self.display_name,
            'description': self.description,
            
            'classification': {
                'confidence': self.classification_confidence,
                'method': self.classification_method,
                'column_count': self.column_count
            },
            
            'hierarchy': {
                'level': self.component_level,
                'sort_order': self.sort_order,
                'has_children': len(self.children) > 0 if self.children else False
            },
            
            'engineering': {
                'parameter_types': self.parameter_types,
                'primary_parameters': self.primary_parameters,
                'units_used': self.units_used,
                'coordinate_system': self.coordinate_system
            },
            
            'data_sources': {
                'source_columns': self.source_columns,
                'source_file_types': self.source_file_types
            },
            
            'data_characteristics': {
                'has_polar_data': self.has_polar_data,
                'has_time_series_data': self.has_time_series_data,
                'has_summary_data': self.has_summary_data,
                'total_data_points': self.total_data_points,
                'result_count': self.result_count
            },
            
            'quality': {
                'data_quality_score': self.data_quality_score,
                'is_validated': self.is_validated,
                'has_anomalies': self.has_anomalies,
                'is_primary_component': self.is_primary_component
            },
            
            'processing': {
                'status': self.processing_status,
                'last_processed': self.last_processed.isoformat() if self.last_processed else None
            },
            
            'timestamps': {
                'created_at': self.created_at.isoformat() if self.created_at else None,
                'updated_at': self.updated_at.isoformat() if self.updated_at else None
            },
            
            'metadata': self.metadata
        }
        
        if include_relationships:
            data['results'] = [result.to_dict() for result in self.results]
            data['children'] = [child.to_dict() for child in self.children]
        
        return data
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'Component':
        """
        Create Component from dictionary data.
        
        Args:
            data: Dictionary with component data
            
        Returns:
            Component instance
        """
        # Convert datetime strings back to datetime objects
        datetime_fields = ['created_at', 'updated_at', 'last_processed']
        for field in datetime_fields:
            if field in data and isinstance(data[field], str):
                data[field] = datetime.fromisoformat(data[field])
        
        # Extract nested structures
        nested_fields = ['classification', 'hierarchy', 'engineering', 
                        'data_sources', 'data_characteristics', 'quality', 
                        'processing', 'timestamps']
        
        flat_data = {}
        for key, value in data.items():
            if key in nested_fields and isinstance(value, dict):
                # Flatten nested dictionaries
                for nested_key, nested_value in value.items():
                    if nested_key in ['confidence', 'method', 'column_count']:
                        flat_data[f'classification_{nested_key}'] = nested_value
                    elif nested_key in ['level', 'sort_order']:
                        flat_data[f'component_{nested_key}'] = nested_value
                    elif nested_key in ['parameter_types', 'primary_parameters', 'units_used', 'coordinate_system']:
                        flat_data[nested_key] = nested_value
                    elif nested_key in ['source_columns', 'source_file_types']:
                        flat_data[nested_key] = nested_value
                    elif nested_key in ['has_polar_data', 'has_time_series_data', 'has_summary_data', 
                                      'total_data_points', 'result_count']:
                        flat_data[nested_key] = nested_value
                    elif nested_key in ['data_quality_score', 'is_validated', 'has_anomalies', 'is_primary_component']:
                        flat_data[nested_key] = nested_value
                    elif nested_key in ['status', 'last_processed']:
                        flat_data[f'processing_{nested_key}'] = nested_value
            elif key not in ['results', 'children']:  # Skip relationships
                flat_data[key] = value
        
        # Handle special field mappings
        if 'classification_confidence' in flat_data:
            flat_data['classification_confidence'] = flat_data.pop('classification_confidence')
        if 'processing_status' not in flat_data and 'processing_status' in data.get('processing', {}):
            flat_data['processing_status'] = data['processing']['status']
        
        return cls(**flat_data)
    
    @classmethod
    def from_component_info(cls, component_info, analysis_id: int) -> 'Component':
        """
        Create Component from ComponentInfo object.
        
        Args:
            component_info: ComponentInfo object from classifier
            analysis_id: ID of the parent analysis
            
        Returns:
            Component instance
        """
        # Extract parameter types
        parameter_types = list(set(param_type.value for param_type in component_info.parameters.values()))
        
        # Determine primary parameters (most common ones)
        param_counts = {}
        for param_type in component_info.parameters.values():
            param_counts[param_type.value] = param_counts.get(param_type.value, 0) + 1
        
        primary_parameters = [param for param, count in param_counts.items() if count >= 2]
        
        component = cls(
            analysis_id=analysis_id,
            name=component_info.name,
            component_type=component_info.component_type.value,
            classification_confidence=component_info.confidence_score,
            column_count=len(component_info.columns),
            parameter_types=parameter_types,
            primary_parameters=primary_parameters,
            source_columns=component_info.columns,
            metadata=component_info.metadata or {}
        )
        
        return component
    
    def update_from_results(self, results: List['Result']) -> None:
        """
        Update component statistics from associated results.
        
        Args:
            results: List of Result objects associated with this component
        """
        if not results:
            return
        
        # Update counts
        self.result_count = len(results)
        self.total_data_points = sum(r.data_points or 0 for r in results)
        
        # Update data type flags
        self.has_polar_data = any(r.result_type == 'polar' for r in results)
        self.has_time_series_data = any(r.result_type == 'timeseries' for r in results)
        self.has_summary_data = any(r.result_type in ['summary', 'statistics'] for r in results)
        
        # Calculate average data quality score
        quality_scores = [r.data_quality_score for r in results if r.data_quality_score is not None]
        if quality_scores:
            self.data_quality_score = sum(quality_scores) / len(quality_scores)
        
        # Update processing status
        if all(r.processing_timestamp is not None for r in results):
            self.processing_status = 'processed'
            self.is_validated = True
        
        # Update metadata with result summary
        if not self.metadata:
            self.metadata = {}
        
        self.metadata['result_summary'] = {
            'total_results': len(results),
            'result_types': list(set(r.result_type for r in results if r.result_type)),
            'parameter_names': list(set(r.parameter_name for r in results if r.parameter_name)),
            'units_found': list(set(r.units for r in results if r.units)),
            'file_types': list(set(r.file_type for r in results if r.file_type))
        }
    
    def add_child_component(self, child_component: 'Component') -> None:
        """
        Add a child component to this component.
        
        Args:
            child_component: Child Component instance
        """
        child_component.parent_component_id = self.id
        child_component.component_level = self.component_level + 1
    
    def get_hierarchy_path(self) -> List[str]:
        """
        Get the hierarchical path from root to this component.
        
        Returns:
            List of component names from root to current
        """
        path = [self.name]
        current = self
        
        while current.parent_component_id is not None:
            # This would need to be implemented with proper parent loading
            # For now, just return current component
            break
        
        return list(reversed(path))
    
    def get_parameter_summary(self) -> Dict[str, Any]:
        """
        Get summary of parameters associated with this component.
        
        Returns:
            Dictionary with parameter information
        """
        return {
            'parameter_types': self.parameter_types or [],
            'primary_parameters': self.primary_parameters or [],
            'units_used': self.units_used or [],
            'coordinate_system': self.coordinate_system,
            'total_parameters': len(self.source_columns or [])
        }
    
    def get_data_summary(self) -> Dict[str, Any]:
        """
        Get summary of data characteristics.
        
        Returns:
            Dictionary with data summary
        """
        return {
            'has_polar_data': self.has_polar_data,
            'has_time_series_data': self.has_time_series_data,
            'has_summary_data': self.has_summary_data,
            'total_data_points': self.total_data_points,
            'result_count': self.result_count,
            'data_quality_score': self.data_quality_score,
            'column_count': self.column_count
        }
    
    def validate_consistency(self) -> Dict[str, Any]:
        """
        Validate consistency of component data and relationships.
        
        Returns:
            Dictionary with validation results
        """
        validation = {
            'is_consistent': True,
            'issues': [],
            'warnings': []
        }
        
        # Check that we have source columns
        if not self.source_columns or len(self.source_columns) == 0:
            validation['issues'].append("No source columns associated with component")
            validation['is_consistent'] = False
        
        # Check column count consistency
        if self.source_columns and len(self.source_columns) != self.column_count:
            validation['warnings'].append(
                f"Column count mismatch: {len(self.source_columns)} vs {self.column_count}"
            )
        
        # Check parameter types
        if self.parameter_types and len(self.parameter_types) == 0:
            validation['warnings'].append("No parameter types identified")
        
        # Check classification confidence
        if self.classification_confidence is not None and self.classification_confidence < 0.5:
            validation['warnings'].append(
                f"Low classification confidence: {self.classification_confidence:.2f}"
            )
        
        # Check data quality
        if self.data_quality_score is not None and self.data_quality_score < 0.7:
            validation['warnings'].append(
                f"Low data quality score: {self.data_quality_score:.2f}"
            )
        
        # Check processing status
        if self.processing_status == 'error':
            validation['issues'].append("Component processing failed")
            validation['is_consistent'] = False
        
        return validation
    
    def get_display_info(self) -> Dict[str, Any]:
        """
        Get information suitable for UI display.
        
        Returns:
            Dictionary with display-friendly information
        """
        # Generate display name if not set
        display_name = self.display_name
        if not display_name:
            type_names = {
                'fst1': 'Floater 1',
                'fst2': 'Floater 2', 
                'strut': 'Strut/Connector',
                'jacket': 'Foundation/Jacket',
                'lngc': 'LNG Carrier',
                'unknown': 'Unknown Component'
            }
            display_name = type_names.get(self.component_type, self.component_type.title())
        
        return {
            'id': self.id,
            'name': self.name,
            'display_name': display_name,
            'type': self.component_type,
            'confidence': self.classification_confidence,
            'parameter_count': len(self.parameter_types or []),
            'column_count': self.column_count,
            'data_quality': self.data_quality_score,
            'has_data': {
                'polar': self.has_polar_data,
                'timeseries': self.has_time_series_data,
                'summary': self.has_summary_data
            },
            'status': self.processing_status,
            'is_validated': self.is_validated,
            'level': self.component_level
        }
    
    @property
    def confidence_grade(self) -> str:
        """Get confidence grade (A-F) based on classification confidence"""
        if not self.classification_confidence:
            return 'Unknown'
        elif self.classification_confidence >= 0.9:
            return 'A'
        elif self.classification_confidence >= 0.8:
            return 'B'
        elif self.classification_confidence >= 0.7:
            return 'C'
        elif self.classification_confidence >= 0.6:
            return 'D'
        else:
            return 'F'
    
    @property
    def quality_grade(self) -> str:
        """Get quality grade based on data quality score"""
        if not self.data_quality_score:
            return 'Unknown'
        elif self.data_quality_score >= 0.9:
            return 'A'
        elif self.data_quality_score >= 0.8:
            return 'B'
        elif self.data_quality_score >= 0.7:
            return 'C'
        elif self.data_quality_score >= 0.6:
            return 'D'
        else:
            return 'F'


# Index definitions for performance
def create_indexes(engine):
    """Create database indexes for performance optimization"""
    from sqlalchemy import Index
    
    indexes = [
        Index('idx_component_analysis', Component.analysis_id),
        Index('idx_component_type', Component.component_type),
        Index('idx_component_parent', Component.parent_component_id),
        Index('idx_component_level', Component.component_level, Component.sort_order),
        Index('idx_component_name', Component.name),
        Index('idx_component_status', Component.processing_status),
        Index('idx_component_quality', Component.data_quality_score, Component.classification_confidence),
        Index('idx_component_created', Component.created_at),
    ]
    
    for index in indexes:
        index.create(engine, checkfirst=True)