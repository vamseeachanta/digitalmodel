"""
Health check endpoints for OrcaFlex Visualization Dashboard.
"""

import psutil
import time
from datetime import datetime
from typing import Dict, Any

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from app.config import get_settings

router = APIRouter()


class HealthStatus(BaseModel):
    """Health status response model."""
    
    status: str
    timestamp: datetime
    version: str
    environment: str
    uptime_seconds: float
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat()
        }


class DetailedHealthStatus(HealthStatus):
    """Detailed health status with system metrics."""
    
    system: Dict[str, Any]
    services: Dict[str, str]
    
    class Config:
        json_encoders = {
            datetime: lambda v: v.isoformat()
        }


# Track application start time
_start_time = time.time()


@router.get("/", response_model=HealthStatus)
async def health_check(settings = Depends(get_settings)):
    """Basic health check endpoint."""
    
    uptime = time.time() - _start_time
    
    return HealthStatus(
        status="healthy",
        timestamp=datetime.utcnow(),
        version="1.0.0",
        environment=settings.environment,
        uptime_seconds=uptime
    )


@router.get("/detailed", response_model=DetailedHealthStatus)
async def detailed_health_check(settings = Depends(get_settings)):
    """Detailed health check with system metrics."""
    
    uptime = time.time() - _start_time
    
    # System metrics
    memory = psutil.virtual_memory()
    disk = psutil.disk_usage('/')
    cpu_percent = psutil.cpu_percent(interval=1)
    
    system_metrics = {
        "cpu": {
            "usage_percent": cpu_percent,
            "count": psutil.cpu_count()
        },
        "memory": {
            "total_gb": round(memory.total / (1024**3), 2),
            "available_gb": round(memory.available / (1024**3), 2),
            "usage_percent": memory.percent
        },
        "disk": {
            "total_gb": round(disk.total / (1024**3), 2),
            "free_gb": round(disk.free / (1024**3), 2),
            "usage_percent": round((disk.used / disk.total) * 100, 2)
        }
    }
    
    # Service status checks
    services_status = {
        "api": "healthy",
        "database": await _check_database_health(),
        "redis": await _check_redis_health(),
        "orcaflex": await _check_orcaflex_health()
    }
    
    return DetailedHealthStatus(
        status="healthy",
        timestamp=datetime.utcnow(),
        version="1.0.0",
        environment=settings.environment,
        uptime_seconds=uptime,
        system=system_metrics,
        services=services_status
    )


@router.get("/ready")
async def readiness_check():
    """Kubernetes readiness probe endpoint."""
    
    # Check if all required services are available
    services_healthy = [
        await _check_database_health() == "healthy",
        await _check_orcaflex_health() == "healthy"
    ]
    
    if all(services_healthy):
        return {"status": "ready"}
    else:
        from fastapi import HTTPException
        raise HTTPException(status_code=503, detail="Service not ready")


@router.get("/live")
async def liveness_check():
    """Kubernetes liveness probe endpoint."""
    return {"status": "alive"}


async def _check_database_health() -> str:
    """Check database connectivity."""
    try:
        # TODO: Implement actual database health check
        # For now, return healthy as placeholder
        return "healthy"
    except Exception:
        return "unhealthy"


async def _check_redis_health() -> str:
    """Check Redis connectivity."""
    try:
        # TODO: Implement actual Redis health check
        # For now, return healthy as placeholder
        return "healthy"
    except Exception:
        return "unhealthy"


async def _check_orcaflex_health() -> str:
    """Check OrcaFlex availability."""
    try:
        # TODO: Implement actual OrcaFlex health check
        # This would check if OrcaFlex Python API is available
        return "healthy"
    except Exception:
        return "unhealthy"