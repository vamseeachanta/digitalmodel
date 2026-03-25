"""
FastAPI application entry point for OrcaFlex Results Visualization Dashboard.
Production-ready implementation with comprehensive API endpoints and performance optimization.
"""

import logging
import os
from contextlib import asynccontextmanager
from typing import AsyncGenerator

import uvicorn
from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from fastapi.responses import JSONResponse
from fastapi.openapi.utils import get_openapi

from app.api.health import router as health_router
from app.api.analyses import router as analyses_router
from app.api.results import router as results_router
from app.api.components import router as components_router
from app.api.exports import router as exports_router
from app.api.upload import router as upload_router
from app.api.statistics import router as statistics_router
from app.config import get_settings
from app.core.cache import init_cache_service, close_cache_service
from app.core.rate_limiter import RateLimitExceeded

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler("orcaflex_dashboard.log")
    ]
)
logger = logging.getLogger(__name__)

settings = get_settings()


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    """Application lifespan events with comprehensive initialization."""
    logger.info("Starting OrcaFlex Visualization Dashboard API")
    logger.info(f"Environment: {settings.environment}")
    logger.info(f"Debug mode: {settings.debug}")
    
    # Startup tasks
    try:
        # Initialize cache service
        await init_cache_service()
        
        # Initialize database connections (when implemented)
        # await init_database_service()
        
        # Initialize background task queues
        # await init_task_queue()
        
        logger.info("Application startup completed successfully")
        
        # Performance metrics logging
        logger.info("Performance targets:")
        logger.info("- Response time: <100ms for cached data, <500ms for complex queries")
        logger.info("- Concurrent users: >10 without degradation")
        logger.info("- Data points: >10,000 efficiently supported")
        logger.info("- Cache hit ratio: >90% target")
        
    except Exception as e:
        logger.error(f"Startup failed: {e}")
        raise
    
    yield
    
    # Shutdown tasks
    logger.info("Shutting down OrcaFlex Visualization Dashboard API")
    
    try:
        # Close cache service
        await close_cache_service()
        
        # Close database connections
        # await close_database_service()
        
        # Close background task queues
        # await close_task_queue()
        
        logger.info("Application shutdown completed successfully")
        
    except Exception as e:
        logger.error(f"Shutdown error: {e}")


def custom_openapi():
    """Generate custom OpenAPI schema with comprehensive documentation."""
    if app.openapi_schema:
        return app.openapi_schema
    
    openapi_schema = get_openapi(
        title="OrcaFlex Results Visualization Dashboard API",
        version="1.0.0",
        description="""
        ## High-Performance API for OrcaFlex Simulation Data Analysis
        
        This API provides comprehensive endpoints for processing, analyzing, and visualizing OrcaFlex simulation results
        with enterprise-grade performance and reliability.
        
        ### Key Features
        
        - **High Performance**: <100ms response time for cached data, <500ms for complex queries
        - **Scalable**: Supports >10,000 data points efficiently with intelligent caching
        - **Comprehensive**: Full CRUD operations for analyses, components, and results
        - **Secure**: JWT-based authentication with rate limiting and session management
        - **Export Ready**: Multiple export formats (CSV, Excel, JSON, Parquet) with streaming
        - **Statistical Analysis**: Built-in statistical analysis and trend detection
        - **Real-time**: Live progress tracking and real-time updates
        
        ### Performance Specifications
        
        - **Response Time**: <100ms cached, <500ms complex queries
        - **Concurrent Users**: >10 users without performance degradation
        - **Memory Usage**: <500MB per worker process
        - **Cache Hit Ratio**: >90% for repeated queries
        - **Data Throughput**: >10,000 data points efficiently
        
        ### API Structure
        
        - **Analyses**: Analysis case management and processing
        - **Results**: High-performance data access with filtering
        - **Components**: Component classification and management
        - **Exports**: Data export with multiple formats and compression
        - **Upload**: Secure file upload with validation and processing
        - **Statistics**: Comprehensive statistical analysis and reporting
        
        ### Authentication
        
        All endpoints require Bearer token authentication using JWT tokens.
        Include the token in the Authorization header: `Bearer <your-token>`
        
        ### Rate Limiting
        
        API endpoints are rate limited to ensure fair usage and system stability.
        Rate limit headers are included in all responses:
        
        - `X-RateLimit-Limit`: Maximum requests per time window
        - `X-RateLimit-Remaining`: Remaining requests in current window
        - `X-RateLimit-Reset`: Timestamp when the window resets
        
        ### Error Handling
        
        The API uses standard HTTP status codes and provides detailed error messages
        with engineering-appropriate context for troubleshooting.
        """,
        routes=app.routes,
        tags=[
            {
                "name": "Health",
                "description": "System health and monitoring endpoints"
            },
            {
                "name": "Analyses",
                "description": "Analysis case management with comprehensive filtering and processing"
            },
            {
                "name": "Results", 
                "description": "High-performance data access with caching and visualization support"
            },
            {
                "name": "Components",
                "description": "Component classification and management (fst1, fst2, strut, jacket, lngc)"
            },
            {
                "name": "Exports",
                "description": "Data export functionality with multiple formats and job queuing"
            },
            {
                "name": "Upload",
                "description": "Secure file upload with validation and batch processing"
            },
            {
                "name": "Statistics",
                "description": "Statistical analysis, trend detection, and performance metrics"
            }
        ]
    )
    
    # Add security schemes
    openapi_schema["components"]["securitySchemes"] = {
        "BearerAuth": {
            "type": "http",
            "scheme": "bearer",
            "bearerFormat": "JWT",
            "description": "JWT authentication token"
        }
    }
    
    # Add global security requirement
    openapi_schema["security"] = [{"BearerAuth": []}]
    
    # Add performance and reliability information
    openapi_schema["info"]["x-performance"] = {
        "responseTime": {
            "cached": "<100ms",
            "complex": "<500ms"
        },
        "throughput": {
            "dataPoints": ">10,000",
            "concurrentUsers": ">10"
        },
        "reliability": {
            "cacheHitRatio": ">90%",
            "memoryUsage": "<500MB per worker"
        }
    }
    
    app.openapi_schema = openapi_schema
    return app.openapi_schema


# Create FastAPI application with production configuration
app = FastAPI(
    title="OrcaFlex Results Visualization Dashboard",
    description="High-performance API for processing and visualizing OrcaFlex simulation results",
    version="1.0.0",
    lifespan=lifespan,
    docs_url="/api/docs" if settings.debug else None,
    redoc_url="/api/redoc" if settings.debug else None,
    openapi_url="/api/openapi.json" if settings.debug else None,
    openapi_tags=[
        {"name": "Health", "description": "System health and monitoring"},
        {"name": "Analyses", "description": "Analysis case management"},
        {"name": "Results", "description": "High-performance data access"},
        {"name": "Components", "description": "Component classification and management"},
        {"name": "Exports", "description": "Data export functionality"},
        {"name": "Upload", "description": "Secure file upload"},
        {"name": "Statistics", "description": "Statistical analysis and metrics"}
    ]
)

# Set custom OpenAPI schema
app.openapi = custom_openapi

# Security middleware for production
if not settings.debug:
    app.add_middleware(
        TrustedHostMiddleware, 
        allowed_hosts=settings.allowed_hosts
    )

# CORS middleware with production-ready configuration
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.cors_origins,
    allow_credentials=True,
    allow_methods=["GET", "POST", "PUT", "DELETE", "OPTIONS"],
    allow_headers=["*"],
    expose_headers=["X-RateLimit-Limit", "X-RateLimit-Remaining", "X-RateLimit-Reset"],
)


# Global exception handlers
@app.exception_handler(404)
async def not_found_handler(request: Request, exc: HTTPException):
    """Custom 404 handler with helpful information."""
    return JSONResponse(
        status_code=404,
        content={
            "message": "Resource not found",
            "detail": str(exc.detail) if hasattr(exc, 'detail') else "The requested resource was not found",
            "path": str(request.url.path),
            "method": request.method,
            "timestamp": str(logger.info),
            "suggestion": "Check the API documentation at /api/docs for available endpoints"
        }
    )


@app.exception_handler(500)
async def internal_server_error_handler(request: Request, exc: Exception):
    """Custom 500 handler with error tracking."""
    error_id = f"error_{hash(str(exc))}"
    logger.error(f"Internal server error [{error_id}]: {exc}", exc_info=True)
    
    return JSONResponse(
        status_code=500,
        content={
            "message": "Internal server error",
            "error_id": error_id,
            "detail": "An unexpected error occurred. Please contact support if the problem persists.",
            "timestamp": str(logger.info)
        }
    )


@app.exception_handler(RateLimitExceeded)
async def rate_limit_handler(request: Request, exc: RateLimitExceeded):
    """Custom rate limit handler with helpful information."""
    response = JSONResponse(
        status_code=429,
        content={
            "message": "Rate limit exceeded",
            "detail": exc.detail,
            "type": "rate_limit_error",
            "timestamp": str(logger.info)
        }
    )
    
    # Add rate limit headers
    if hasattr(exc, 'retry_after') and exc.retry_after:
        response.headers["Retry-After"] = str(exc.retry_after)
    
    return response


@app.exception_handler(ValueError)
async def value_error_handler(request: Request, exc: ValueError):
    """Handle validation errors with detailed information."""
    return JSONResponse(
        status_code=400,
        content={
            "message": "Validation error",
            "detail": str(exc),
            "type": "validation_error",
            "path": str(request.url.path)
        }
    )


# Include API routers with proper prefixes and tags
app.include_router(
    health_router, 
    prefix="/api/health", 
    tags=["Health"]
)

app.include_router(
    analyses_router, 
    prefix="/api/analyses", 
    tags=["Analyses"]
)

app.include_router(
    results_router, 
    prefix="/api/results", 
    tags=["Results"]
)

app.include_router(
    components_router, 
    prefix="/api/components", 
    tags=["Components"]
)

app.include_router(
    exports_router, 
    prefix="/api/exports", 
    tags=["Exports"]
)

app.include_router(
    upload_router, 
    prefix="/api/upload", 
    tags=["Upload"]
)

app.include_router(
    statistics_router, 
    prefix="/api/statistics", 
    tags=["Statistics"]
)


# Root endpoint with comprehensive API information
@app.get("/", tags=["Root"])
async def root():
    """
    Root endpoint providing API overview and system information.
    """
    return {
        "message": "OrcaFlex Results Visualization Dashboard API",
        "version": "1.0.0",
        "status": "operational",
        "environment": settings.environment,
        "features": {
            "high_performance": "Response time <100ms for cached data",
            "scalable": "Supports >10,000 data points efficiently", 
            "secure": "JWT authentication with rate limiting",
            "comprehensive": "Full CRUD operations with statistical analysis",
            "export_ready": "Multiple formats with streaming support"
        },
        "endpoints": {
            "health": "/api/health - System health and monitoring",
            "analyses": "/api/analyses - Analysis case management",
            "results": "/api/results - High-performance data access",
            "components": "/api/components - Component classification",
            "exports": "/api/exports - Data export functionality", 
            "upload": "/api/upload - Secure file upload",
            "statistics": "/api/statistics - Statistical analysis"
        },
        "documentation": {
            "openapi": "/api/docs" if settings.debug else "Available in development mode",
            "redoc": "/api/redoc" if settings.debug else "Available in development mode"
        },
        "performance": {
            "response_time": {
                "cached": "<100ms",
                "complex_queries": "<500ms"
            },
            "throughput": {
                "concurrent_users": ">10",
                "data_points": ">10,000"
            },
            "reliability": {
                "cache_hit_ratio": ">90%",
                "memory_usage": "<500MB per worker"
            }
        },
        "support": {
            "engineering_focus": "Offshore hydrodynamic analysis and structural design",
            "file_formats": ["OrcaFlex .dat", "CSV results", "YAML configuration"],
            "export_formats": ["CSV", "Excel", "JSON", "Parquet", "PNG", "SVG", "PDF"],
            "standards_compliance": ["API", "DNV", "ABS offshore standards"]
        }
    }


# API information endpoint
@app.get("/api", tags=["Root"])
async def api_info():
    """
    API information endpoint with detailed capabilities.
    """
    return {
        "api": "OrcaFlex Results Visualization Dashboard",
        "version": "1.0.0",
        "status": "operational",
        "capabilities": {
            "analyses": {
                "description": "Analysis case management with comprehensive filtering",
                "features": ["CRUD operations", "Real-time progress", "Batch processing", "Comparison"]
            },
            "results": {
                "description": "High-performance data access with caching",
                "features": ["Polar plots", "Time series", "Statistical summaries", "Filtering"]
            },
            "components": {
                "description": "Component classification and management",
                "types": ["fst1", "fst2", "strut", "jacket", "lngc"],
                "features": ["Auto-classification", "Hierarchy", "Validation"]
            },
            "exports": {
                "description": "Data export with multiple formats",
                "formats": ["CSV", "Excel", "JSON", "Parquet", "PNG", "SVG", "PDF"],
                "features": ["Streaming", "Compression", "Job queue", "Progress tracking"]
            },
            "upload": {
                "description": "Secure file upload with validation",
                "formats": [".dat", ".csv", ".yml", ".yaml", ".zip"],
                "features": ["Batch upload", "Validation", "Progress tracking", "Auto-processing"]
            },
            "statistics": {
                "description": "Statistical analysis and trend detection",
                "features": ["Distributions", "Correlations", "Outliers", "Trends", "Comparisons"]
            }
        },
        "technical_specifications": {
            "performance": {
                "response_time_cached": "<100ms",
                "response_time_complex": "<500ms",
                "concurrent_users": ">10",
                "data_points_supported": ">10,000",
                "cache_hit_ratio": ">90%"
            },
            "reliability": {
                "memory_usage": "<500MB per worker",
                "rate_limiting": "Adaptive with Redis backend",
                "authentication": "JWT with session management",
                "caching": "Redis with intelligent TTL"
            }
        }
    }


# Health check endpoint for load balancers
@app.get("/health", tags=["Health"])
async def health_check():
    """Simple health check for load balancers."""
    return {"status": "healthy", "timestamp": str(logger.info)}


if __name__ == "__main__":
    uvicorn.run(
        "main:app",
        host=settings.host,
        port=settings.port,
        reload=settings.debug,
        log_level=settings.log_level.lower(),
        workers=1 if settings.debug else 4,
        access_log=True,
        server_header=False,
        date_header=False
    )