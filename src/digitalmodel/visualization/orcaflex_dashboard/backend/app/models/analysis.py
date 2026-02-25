"""
Analysis Database Model

Represents an OrcaFlex analysis case with metadata and relationships.
"""

from sqlalchemy import Column, Integer, String, DateTime, Float, JSON, Text, Boolean, ForeignKey
from sqlalchemy.orm import relationship
from sqlalchemy.ext.declarative import declarative_base
from datetime import datetime
from typing import Dict, Any, Optional, List
import uuid

Base = declarative_base()


class Analysis(Base):
    """
    Analysis case model representing a complete OrcaFlex analysis.
    
    An analysis contains multiple result files and components, representing
    a single design case or environmental condition scenario.
    """
    
    __tablename__ = 'analyses'
    
    # Primary key and identification
    id = Column(Integer, primary_key=True, autoincrement=True)
    uuid = Column(String(36), unique=True, nullable=False, default=lambda: str(uuid.uuid4()))
    name = Column(String(255), nullable=False)
    description = Column(Text, nullable=True)
    
    # Analysis metadata
    analysis_type = Column(String(50), nullable=False, default='orcaflex')  # orcaflex, ansys, etc.
    version = Column(String(50), nullable=True)  # Software version
    case_id = Column(String(100), nullable=True, index=True)  # External case identifier
    
    # File and directory information
    source_directory = Column(String(500), nullable=True)
    primary_file = Column(String(255), nullable=True)  # Main analysis file
    file_count = Column(Integer, default=0)
    total_file_size_mb = Column(Float, default=0.0)
    
    # Loading conditions
    water_level = Column(String(20), nullable=True)  # hwl, lwl
    volume_condition = Column(String(20), nullable=True)  # 125km3, 180km3, ballast
    side_configuration = Column(String(20), nullable=True)  # pb, sb, both
    loading_phase = Column(String(20), nullable=True)  # connected, approach, departure
    
    # Environmental conditions (JSON field for flexibility)
    environmental_conditions = Column(JSON, nullable=True)
    
    # Processing status and quality
    processing_status = Column(String(20), default='pending')  # pending, processing, completed, failed
    validation_score = Column(Float, nullable=True)  # 0.0 to 1.0
    data_quality_score = Column(Float, nullable=True)  # 0.0 to 1.0
    
    # Performance and statistics
    processing_time_seconds = Column(Float, nullable=True)
    memory_usage_mb = Column(Float, nullable=True)
    component_count = Column(Integer, default=0)
    result_count = Column(Integer, default=0)
    
    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow, nullable=False)
    analysis_timestamp = Column(DateTime, nullable=True)  # When analysis was run
    
    # Flags and configuration
    is_active = Column(Boolean, default=True)
    is_baseline = Column(Boolean, default=False)  # Mark as baseline case for comparisons
    
    # Additional metadata (flexible JSON field)
    metadata = Column(JSON, nullable=True)
    
    # Relationships
    components = relationship("Component", back_populates="analysis", cascade="all, delete-orphan")
    results = relationship("Result", back_populates="analysis", cascade="all, delete-orphan")
    
    def __init__(self, **kwargs):
        """Initialize analysis with default values"""
        super().__init__(**kwargs)
        if not self.uuid:
            self.uuid = str(uuid.uuid4())
        if not self.metadata:
            self.metadata = {}
    
    def __repr__(self):
        return f"<Analysis(id={self.id}, name='{self.name}', status='{self.processing_status}')>"
    
    def to_dict(self, include_relationships: bool = False) -> Dict[str, Any]:
        """
        Convert analysis to dictionary representation.
        
        Args:
            include_relationships: Whether to include components and results
            
        Returns:
            Dictionary representation of the analysis
        """
        data = {
            'id': self.id,
            'uuid': self.uuid,
            'name': self.name,
            'description': self.description,
            'analysis_type': self.analysis_type,
            'version': self.version,
            'case_id': self.case_id,
            
            'source_directory': self.source_directory,
            'primary_file': self.primary_file,
            'file_count': self.file_count,
            'total_file_size_mb': self.total_file_size_mb,
            
            'water_level': self.water_level,
            'volume_condition': self.volume_condition,
            'side_configuration': self.side_configuration,
            'loading_phase': self.loading_phase,
            'environmental_conditions': self.environmental_conditions,
            
            'processing_status': self.processing_status,
            'validation_score': self.validation_score,
            'data_quality_score': self.data_quality_score,
            
            'processing_time_seconds': self.processing_time_seconds,
            'memory_usage_mb': self.memory_usage_mb,
            'component_count': self.component_count,
            'result_count': self.result_count,
            
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'analysis_timestamp': self.analysis_timestamp.isoformat() if self.analysis_timestamp else None,
            
            'is_active': self.is_active,
            'is_baseline': self.is_baseline,
            'metadata': self.metadata
        }
        
        if include_relationships:
            data['components'] = [comp.to_dict() for comp in self.components]
            data['results'] = [result.to_dict() for result in self.results]
        
        return data
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'Analysis':
        """
        Create Analysis from dictionary data.
        
        Args:
            data: Dictionary with analysis data
            
        Returns:
            Analysis instance
        """
        # Convert datetime strings back to datetime objects
        datetime_fields = ['created_at', 'updated_at', 'analysis_timestamp']
        for field in datetime_fields:
            if field in data and isinstance(data[field], str):
                data[field] = datetime.fromisoformat(data[field])
        
        # Remove relationships from data (handled separately)
        filtered_data = {k: v for k, v in data.items() 
                        if k not in ['components', 'results']}
        
        return cls(**filtered_data)
    
    def update_loading_conditions(self, loading_conditions) -> None:
        """
        Update analysis with decoded loading conditions.
        
        Args:
            loading_conditions: LoadingConditions object from decoder
        """
        self.water_level = loading_conditions.water_level.value if loading_conditions.water_level else None
        self.volume_condition = loading_conditions.volume_condition.value if loading_conditions.volume_condition else None
        self.side_configuration = loading_conditions.side_config.value if loading_conditions.side_config else None
        self.loading_phase = loading_conditions.loading_phase.value if loading_conditions.loading_phase else None
        
        # Store environmental conditions as JSON
        if loading_conditions.environmental:
            env = loading_conditions.environmental
            self.environmental_conditions = {
                'wave_height': env.wave_height,
                'wave_period': env.wave_period,
                'wind_speed': env.wind_speed,
                'current_speed': env.current_speed,
                'wave_direction': env.wave_direction,
                'wind_direction': env.wind_direction,
                'current_direction': env.current_direction
            }
        
        # Update metadata
        if not self.metadata:
            self.metadata = {}
        
        self.metadata['loading_conditions'] = {
            'source': loading_conditions.source,
            'confidence_score': loading_conditions.confidence_score,
            'additional_info': loading_conditions.additional_info
        }
    
    def update_validation_results(self, validation_result) -> None:
        """
        Update analysis with validation results.
        
        Args:
            validation_result: ValidationResult object from validator
        """
        self.validation_score = validation_result.overall_score
        self.data_quality_score = validation_result.overall_score  # Same for now
        
        # Update metadata with validation details
        if not self.metadata:
            self.metadata = {}
        
        self.metadata['validation'] = {
            'total_checks': validation_result.total_checks,
            'passed_checks': validation_result.passed_checks,
            'category_scores': {k.value: v for k, v in validation_result.category_scores.items()},
            'issue_count': len(validation_result.issues),
            'validation_level': validation_result.validation_level.value,
            'statistics': validation_result.statistics
        }
    
    def get_loading_summary(self) -> str:
        """
        Get human-readable loading condition summary.
        
        Returns:
            String description of loading conditions
        """
        parts = []
        
        if self.water_level:
            parts.append(self.water_level.upper())
        
        if self.volume_condition:
            parts.append(self.volume_condition)
        
        if self.side_configuration:
            parts.append(self.side_configuration.upper())
        
        if self.loading_phase:
            parts.append(self.loading_phase)
        
        base_summary = " / ".join(parts) if parts else "Unknown conditions"
        
        # Add environmental conditions if available
        if self.environmental_conditions:
            env_parts = []
            env = self.environmental_conditions
            
            if env.get('wave_height'):
                env_parts.append(f"Hs={env['wave_height']}m")
            if env.get('wave_period'):
                env_parts.append(f"Tp={env['wave_period']}s")
            if env.get('wind_speed'):
                env_parts.append(f"Wind={env['wind_speed']}m/s")
            if env.get('current_speed'):
                env_parts.append(f"Current={env['current_speed']}m/s")
            
            if env_parts:
                base_summary += f" ({', '.join(env_parts)})"
        
        return base_summary
    
    def is_valid(self, min_score: float = 0.7) -> bool:
        """
        Check if analysis meets validation requirements.
        
        Args:
            min_score: Minimum validation score required
            
        Returns:
            True if analysis is valid
        """
        return (self.processing_status == 'completed' and
                self.validation_score is not None and
                self.validation_score >= min_score)
    
    def get_file_size_mb(self) -> float:
        """Get total file size in MB"""
        return self.total_file_size_mb or 0.0
    
    def get_processing_efficiency(self) -> Optional[float]:
        """
        Calculate processing efficiency (MB/second).
        
        Returns:
            Processing speed in MB/s or None if insufficient data
        """
        if (self.total_file_size_mb and self.processing_time_seconds and 
            self.processing_time_seconds > 0):
            return self.total_file_size_mb / self.processing_time_seconds
        return None
    
    def add_component(self, component_info) -> None:
        """
        Add component information to the analysis.
        
        Args:
            component_info: ComponentInfo object from classifier
        """
        # This would be implemented when Component model is created
        pass
    
    def add_results(self, results_data: List[Dict[str, Any]]) -> None:
        """
        Add multiple result records to the analysis.
        
        Args:
            results_data: List of result dictionaries
        """
        # This would be implemented when Result model is created
        pass
    
    @property
    def status_display(self) -> str:
        """Get display-friendly status"""
        status_map = {
            'pending': 'Pending',
            'processing': 'Processing',
            'completed': 'Completed',
            'failed': 'Failed',
            'error': 'Error'
        }
        return status_map.get(self.processing_status, self.processing_status.title())
    
    @property
    def quality_grade(self) -> str:
        """Get quality grade based on validation score"""
        if not self.validation_score:
            return 'Unknown'
        elif self.validation_score >= 0.9:
            return 'A'
        elif self.validation_score >= 0.8:
            return 'B'
        elif self.validation_score >= 0.7:
            return 'C'
        elif self.validation_score >= 0.6:
            return 'D'
        else:
            return 'F'


# Index definitions for performance
def create_indexes(engine):
    """Create database indexes for performance optimization"""
    from sqlalchemy import Index
    
    # Common query indexes
    indexes = [
        Index('idx_analysis_case_id', Analysis.case_id),
        Index('idx_analysis_status', Analysis.processing_status),
        Index('idx_analysis_created_at', Analysis.created_at),
        Index('idx_analysis_loading_conditions', 
              Analysis.water_level, Analysis.volume_condition, Analysis.side_configuration),
        Index('idx_analysis_quality', Analysis.validation_score, Analysis.data_quality_score),
        Index('idx_analysis_active', Analysis.is_active),
    ]
    
    for index in indexes:
        index.create(engine, checkfirst=True)