"""
Database models for OrcaFlex Dashboard
"""

from datetime import datetime
from uuid import uuid4

from sqlalchemy import Column, DateTime, Float, ForeignKey, Integer, String, Text, JSON
from sqlalchemy.dialects.postgresql import UUID, ARRAY
from sqlalchemy.orm import relationship

from services.database import Base


class AnalysisCase(Base):
    """Analysis case model"""
    __tablename__ = "analysis_cases"
    
    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid4)
    name = Column(String(255), nullable=False, unique=True)
    description = Column(Text)
    status = Column(String(50), default="pending")
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    metadata = Column(JSON)
    
    # Relationships
    components = relationship("Component", back_populates="case", cascade="all, delete-orphan")


class Component(Base):
    """Component model"""
    __tablename__ = "components"
    
    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid4)
    case_id = Column(UUID(as_uuid=True), ForeignKey("analysis_cases.id"), nullable=False)
    type = Column(String(50), nullable=False)  # fst1, fst2, strut, jacket, lngc
    name = Column(String(255), nullable=False)
    properties = Column(JSON)
    created_at = Column(DateTime, default=datetime.utcnow)
    
    # Relationships
    case = relationship("AnalysisCase", back_populates="components")
    polar_data = relationship("PolarData", back_populates="component", cascade="all, delete-orphan")
    time_traces = relationship("TimeTrace", back_populates="component", cascade="all, delete-orphan")


class PolarData(Base):
    """Polar data model"""
    __tablename__ = "polar_data"
    
    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid4)
    component_id = Column(UUID(as_uuid=True), ForeignKey("components.id"), nullable=False)
    loading_condition = Column(String(100), nullable=False)
    heading = Column(Float, nullable=False)
    value = Column(Float, nullable=False)
    unit = Column(String(20))
    timestamp = Column(DateTime, default=datetime.utcnow)
    
    # Relationships
    component = relationship("Component", back_populates="polar_data")


class TimeTrace(Base):
    """Time trace model"""
    __tablename__ = "time_traces"
    
    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid4)
    component_id = Column(UUID(as_uuid=True), ForeignKey("components.id"), nullable=False)
    heading = Column(Float)
    time_points = Column(ARRAY(Float), nullable=False)
    values = Column(ARRAY(Float), nullable=False)
    unit = Column(String(20))
    metadata = Column(JSON)
    created_at = Column(DateTime, default=datetime.utcnow)
    
    # Relationships
    component = relationship("Component", back_populates="time_traces")