"""
ABOUTME: Configuration data models for digitalmodel
ABOUTME: ORM models for storing and managing configurations in database
"""

from datetime import datetime
from typing import Any, Dict, Optional
from sqlalchemy import Column, String, DateTime, Text, Integer
from sqlalchemy.dialects.postgresql import JSON

# Assumes BaseModel with mixins is available from Phase 1
# Import structure: from digitalmodel.infrastructure.base_configs.models import BaseModel, AuditMixin, MetadataMixin, StatusMixin
try:
    from digitalmodel.base_configs.models import (
        BaseModel,
        AuditMixin,
        MetadataMixin,
        StatusMixin
    )
except ImportError:
    # Fallback for testing without full Phase 1 integration
    from sqlalchemy.orm import declarative_base
    from sqlalchemy.ext.declarative import DeclarativeMeta

    Base = declarative_base()

    class BaseModel(Base):
        """Temporary BaseModel for testing"""
        __abstract__ = True

        # Define primary key for SQLAlchemy mapper
        id = Column(Integer, primary_key=True)

    class AuditMixin:
        """Temporary AuditMixin for testing"""
        pass

    class MetadataMixin:
        """Temporary MetadataMixin for testing"""
        pass

    class StatusMixin:
        """Temporary StatusMixin for testing"""
        pass


class ConfigModel(BaseModel, AuditMixin, MetadataMixin, StatusMixin):
    """
    Configuration model for storing configurations in digitalmodel.

    Inherits from:
    - BaseModel: Base ORM functionality
    - AuditMixin: Tracks created_by, updated_by, created_at, updated_at
    - MetadataMixin: Tracks metadata fields (name, version, description)
    - StatusMixin: Tracks status field (active, inactive, archived)
    """

    __tablename__ = 'digitalmodel_configurations'

    # Primary configuration attributes
    config_name = Column(String(255), unique=True, nullable=False, index=True)
    config_type = Column(String(100), nullable=False, index=True)
    config_data = Column(JSON, nullable=False)

    # Configuration metadata
    environment = Column(String(50), nullable=True, index=True)  # dev, staging, prod
    category = Column(String(100), nullable=True, index=True)  # database, logging, performance, etc.

    # Configuration content details
    description = Column(Text, nullable=True)
    notes = Column(Text, nullable=True)

    # Version tracking
    version = Column(Integer, default=1, nullable=False)
    is_active = Column(String(20), default='active', nullable=False, index=True)  # active, deprecated, archived

    # Validation and schema tracking
    schema_version = Column(String(20), nullable=True)  # e.g., "1.0.0"
    validation_status = Column(String(50), default='pending', nullable=False)  # pending, valid, invalid
    validation_errors = Column(JSON, nullable=True)  # List of validation errors if any

    # Parent configuration (for inheritance/composition)
    parent_config_id = Column(Integer, nullable=True, index=True)

    # Last validation timestamp
    last_validated_at = Column(DateTime, nullable=True)

    def __init__(
        self,
        config_name: str,
        config_type: str,
        config_data: Dict[str, Any],
        environment: Optional[str] = None,
        category: Optional[str] = None,
        description: Optional[str] = None,
        **kwargs
    ):
        """
        Initialize configuration model.

        Args:
            config_name: Unique name for configuration
            config_type: Type of configuration (yaml, json, etc.)
            config_data: Configuration data as dictionary
            environment: Target environment (dev, staging, prod)
            category: Configuration category (database, logging, etc.)
            description: Human-readable description
        """
        super().__init__(**kwargs)
        self.config_name = config_name
        self.config_type = config_type
        self.config_data = config_data
        self.environment = environment
        self.category = category
        self.description = description
        self.last_validated_at = datetime.utcnow()

    def get_config_value(self, key: str, default: Any = None) -> Any:
        """
        Get configuration value by dot-separated key.

        Args:
            key: Dot-separated key (e.g., "database.host")
            default: Default value if key not found

        Returns:
            Configuration value or default
        """
        keys = key.split('.')
        value = self.config_data

        for k in keys:
            if isinstance(value, dict):
                value = value.get(k)
            else:
                return default

            if value is None:
                return default

        return value

    def set_config_value(self, key: str, value: Any) -> None:
        """
        Set configuration value by dot-separated key.

        Args:
            key: Dot-separated key (creates nested dicts as needed)
            value: Value to set
        """
        keys = key.split('.')
        config = self.config_data

        for k in keys[:-1]:
            if k not in config:
                config[k] = {}
            config = config[k]

        config[keys[-1]] = value

    def mark_validated(self, is_valid: bool = True, errors: Optional[list] = None) -> None:
        """
        Mark configuration as validated.

        Args:
            is_valid: Whether configuration is valid
            errors: List of validation errors if any
        """
        self.last_validated_at = datetime.utcnow()
        self.validation_status = 'valid' if is_valid else 'invalid'
        self.validation_errors = errors or []

    def __repr__(self) -> str:
        """String representation."""
        return (
            f"ConfigModel(name={self.config_name}, type={self.config_type}, "
            f"env={self.environment}, status={self.validation_status})"
        )
