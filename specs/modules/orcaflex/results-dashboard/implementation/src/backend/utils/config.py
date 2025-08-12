"""
Configuration management using Pydantic Settings
"""

from pathlib import Path
from typing import List, Optional

from pydantic import Field, field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """Application settings"""
    
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=False,
    )
    
    # Environment
    environment: str = Field(default="development")
    debug: bool = Field(default=False)
    
    # Database
    database_url: str = Field(
        default="postgresql+asyncpg://orcaflex:changeme@localhost:5432/orcaflex_dashboard"
    )
    
    # Redis
    redis_url: str = Field(default="redis://localhost:6379")
    cache_ttl: int = Field(default=3600)
    
    # Security
    secret_key: str = Field(default="your-secret-key-here")
    
    # CORS
    cors_origins: List[str] = Field(
        default=["http://localhost:3000", "http://localhost:8000"]
    )
    
    # File Monitoring
    watch_directory: Path = Field(default=Path("/data/orcaflex/results"))
    watch_patterns: List[str] = Field(default=["dm_*.csv", "*.sim"])
    
    # Performance
    max_workers: int = Field(default=4)
    max_upload_size: int = Field(default=1073741824)  # 1GB
    
    # Export
    export_directory: Path = Field(default=Path("/data/exports"))
    max_export_rows: int = Field(default=1000000)
    
    # Monitoring
    enable_metrics: bool = Field(default=True)
    metrics_port: int = Field(default=9090)
    
    @field_validator("cors_origins", mode="before")
    @classmethod
    def parse_cors_origins(cls, v: str | List[str]) -> List[str]:
        """Parse CORS origins from comma-separated string or list"""
        if isinstance(v, str):
            return [origin.strip() for origin in v.split(",")]
        return v
    
    @field_validator("watch_patterns", mode="before")
    @classmethod
    def parse_watch_patterns(cls, v: str | List[str]) -> List[str]:
        """Parse watch patterns from comma-separated string or list"""
        if isinstance(v, str):
            return [pattern.strip() for pattern in v.split(",")]
        return v


settings = Settings()