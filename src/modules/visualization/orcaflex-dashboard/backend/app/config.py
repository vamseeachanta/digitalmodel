"""
Configuration management for OrcaFlex Visualization Dashboard.
"""

import os
from functools import lru_cache
from typing import List, Optional

from pydantic import BaseSettings, validator


class Settings(BaseSettings):
    """Application settings with environment variable support."""
    
    # Application settings
    app_name: str = "OrcaFlex Visualization Dashboard"
    environment: str = "development"
    debug: bool = True
    host: str = "0.0.0.0"
    port: int = 8000
    log_level: str = "INFO"
    
    # Security settings
    secret_key: str = "your-secret-key-change-this-in-production"
    algorithm: str = "HS256"
    access_token_expire_minutes: int = 30
    allowed_hosts: List[str] = ["*"]
    cors_origins: List[str] = ["http://localhost:3000", "http://localhost:3001"]
    
    # Database settings
    database_url: Optional[str] = None
    redis_url: Optional[str] = None
    
    # OrcaFlex settings
    orcaflex_license_path: Optional[str] = None
    orcaflex_temp_dir: str = "/tmp/orcaflex"
    max_file_size: int = 100 * 1024 * 1024  # 100MB
    
    # File storage settings
    upload_dir: str = "uploads"
    results_dir: str = "results"
    max_concurrent_analyses: int = 5
    
    # Monitoring and observability
    enable_metrics: bool = True
    enable_tracing: bool = False
    
    @validator("cors_origins", pre=True)
    def parse_cors_origins(cls, v):
        """Parse CORS origins from string or list."""
        if isinstance(v, str):
            return [origin.strip() for origin in v.split(",")]
        return v
    
    @validator("allowed_hosts", pre=True)
    def parse_allowed_hosts(cls, v):
        """Parse allowed hosts from string or list."""
        if isinstance(v, str):
            return [host.strip() for host in v.split(",")]
        return v
    
    @validator("environment")
    def validate_environment(cls, v):
        """Validate environment setting."""
        allowed = ["development", "testing", "staging", "production"]
        if v not in allowed:
            raise ValueError(f"Environment must be one of: {allowed}")
        return v
    
    class Config:
        env_file = ".env"
        env_prefix = "ORCAFLEX_"
        case_sensitive = False


@lru_cache()
def get_settings() -> Settings:
    """Get cached application settings."""
    return Settings()


def get_database_url() -> str:
    """Get database URL with fallback."""
    settings = get_settings()
    if settings.database_url:
        return settings.database_url
    
    # Default SQLite for development
    if settings.environment == "development":
        return "sqlite:///./orcaflex_dashboard.db"
    
    raise ValueError("DATABASE_URL must be set for non-development environments")


def get_redis_url() -> Optional[str]:
    """Get Redis URL if configured."""
    settings = get_settings()
    return settings.redis_url