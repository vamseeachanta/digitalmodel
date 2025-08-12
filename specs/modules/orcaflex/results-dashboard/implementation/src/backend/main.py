"""
OrcaFlex Dashboard Backend API
Main FastAPI application entry point
"""

from contextlib import asynccontextmanager
from typing import Any

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse

from api.routes import analysis, data, export, websocket
from services.cache import init_redis
from services.database import init_db
from utils.config import settings
from utils.logger import setup_logger

logger = setup_logger(__name__)


@asynccontextmanager
async def lifespan(app: FastAPI) -> Any:
    """Application lifespan manager"""
    logger.info("Starting OrcaFlex Dashboard Backend")
    
    # Initialize database
    await init_db()
    
    # Initialize Redis cache
    await init_redis()
    
    logger.info("Backend initialization complete")
    
    yield
    
    logger.info("Shutting down OrcaFlex Dashboard Backend")


app = FastAPI(
    title="OrcaFlex Dashboard API",
    description="API for OrcaFlex simulation results visualization",
    version="1.0.0",
    lifespan=lifespan,
)

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/")
async def root() -> dict[str, str]:
    """Root endpoint"""
    return {
        "name": "OrcaFlex Dashboard API",
        "version": "1.0.0",
        "status": "operational",
    }


@app.get("/health")
async def health_check() -> JSONResponse:
    """Health check endpoint"""
    try:
        # TODO: Add database and Redis health checks
        return JSONResponse(
            status_code=200,
            content={
                "status": "healthy",
                "service": "orcaflex-dashboard",
                "version": "1.0.0",
            },
        )
    except Exception as e:
        logger.error(f"Health check failed: {e}")
        return JSONResponse(
            status_code=503,
            content={"status": "unhealthy", "error": str(e)},
        )


# Include routers
app.include_router(data.router, prefix="/api/data", tags=["data"])
app.include_router(analysis.router, prefix="/api/analysis", tags=["analysis"])
app.include_router(export.router, prefix="/api/export", tags=["export"])
app.include_router(websocket.router, prefix="/ws", tags=["websocket"])


if __name__ == "__main__":
    import uvicorn
    
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        reload=settings.debug,
        log_level="info",
    )