"""
Data Export API endpoints for OrcaFlex visualization dashboard.
Provides high-performance export functionality with job queuing and progress tracking.
"""

import logging
import asyncio
from typing import List, Optional, Dict, Any, Union
from uuid import UUID, uuid4
from datetime import datetime
from io import BytesIO, StringIO

from fastapi import APIRouter, Depends, HTTPException, Query, BackgroundTasks
from fastapi.responses import StreamingResponse, FileResponse
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field

from app.services.export_service import get_export_service, ExportService
from app.services.auth_service import verify_token
from app.core.cache import get_cache_service, CacheService
from app.core.rate_limiter import rate_limit

router = APIRouter()
logger = logging.getLogger(__name__)
security = HTTPBearer()


class ExportFormat(str):
    """Supported export formats."""
    CSV = "csv"
    EXCEL = "excel"
    JSON = "json"
    PARQUET = "parquet"
    HDF5 = "hdf5"
    PNG = "png"
    SVG = "svg"
    PDF = "pdf"


class ExportRequest(BaseModel):
    """Base export request model."""
    analysis_ids: List[UUID] = Field(..., min_items=1, max_items=50, description="Analysis IDs to export")
    format: ExportFormat = Field(..., description="Export format")
    include_metadata: bool = Field(True, description="Include analysis metadata")
    include_components: bool = Field(True, description="Include component data")
    include_results: bool = Field(True, description="Include result data")
    compression: Optional[str] = Field(None, regex="^(zip|gzip|bz2)$", description="Compression method")
    filename: Optional[str] = Field(None, description="Custom filename")


class CSVExportRequest(ExportRequest):
    """CSV export specific parameters."""
    format: ExportFormat = Field(ExportFormat.CSV, const=True)
    delimiter: str = Field(",", description="CSV delimiter")
    include_headers: bool = Field(True, description="Include column headers")
    date_format: str = Field("%Y-%m-%d %H:%M:%S", description="Date format string")
    decimal_places: int = Field(6, ge=0, le=15, description="Decimal precision")
    max_rows: Optional[int] = Field(None, ge=1, le=1000000, description="Maximum rows per file")


class ExcelExportRequest(ExportRequest):
    """Excel export specific parameters."""
    format: ExportFormat = Field(ExportFormat.EXCEL, const=True)
    include_charts: bool = Field(False, description="Include embedded charts")
    sheet_per_analysis: bool = Field(True, description="Create separate sheet per analysis")
    include_summary_sheet: bool = Field(True, description="Include summary sheet")
    freeze_headers: bool = Field(True, description="Freeze header rows")


class ChartExportRequest(BaseModel):
    """Chart export parameters."""
    analysis_ids: List[UUID] = Field(..., min_items=1, max_items=10)
    chart_type: str = Field(..., regex="^(polar|timeseries|comparison|statistics)$")
    format: ExportFormat = Field(..., regex="^(png|svg|pdf)$")
    width: int = Field(1200, ge=400, le=4000, description="Image width in pixels")
    height: int = Field(800, ge=300, le=3000, description="Image height in pixels")
    dpi: int = Field(300, ge=72, le=600, description="Image resolution")
    component_types: Optional[List[str]] = Field(None, description="Filter by component types")
    parameters: Optional[List[str]] = Field(None, description="Parameters to include")
    title: Optional[str] = Field(None, description="Chart title")
    theme: str = Field("default", regex="^(default|dark|light|publication)$")


class ExportJob(BaseModel):
    """Export job status model."""
    job_id: UUID
    status: str = Field(..., regex="^(pending|processing|completed|failed|cancelled)$")
    progress: float = Field(0.0, ge=0.0, le=100.0)
    created_at: datetime
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    file_size: Optional[int] = None
    download_url: Optional[str] = None
    error_message: Optional[str] = None
    export_request: Dict[str, Any]


@router.post("/csv", response_model=ExportJob)
@rate_limit(max_calls=20, time_window=300)
async def export_csv(
    export_request: CSVExportRequest,
    background_tasks: BackgroundTasks,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Export data as CSV with advanced formatting options.
    Supports large datasets with streaming and compression.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Create export job
        job_id = uuid4()
        job = ExportJob(
            job_id=job_id,
            status="pending",
            created_at=datetime.utcnow(),
            export_request=export_request.dict()
        )
        
        # Start export in background
        background_tasks.add_task(
            export_service.export_csv_background,
            job_id,
            export_request,
            user_id
        )
        
        logger.info(f"Started CSV export job {job_id} for user {user_id}")
        
        return job
        
    except Exception as e:
        logger.error(f"Error starting CSV export: {e}")
        raise HTTPException(status_code=500, detail="Failed to start CSV export")


@router.post("/excel", response_model=ExportJob)
@rate_limit(max_calls=10, time_window=300)
async def export_excel(
    export_request: ExcelExportRequest,
    background_tasks: BackgroundTasks,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Export data as Excel with metadata and formatting.
    Includes charts, summary sheets, and professional formatting.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Create export job
        job_id = uuid4()
        job = ExportJob(
            job_id=job_id,
            status="pending",
            created_at=datetime.utcnow(),
            export_request=export_request.dict()
        )
        
        # Start export in background
        background_tasks.add_task(
            export_service.export_excel_background,
            job_id,
            export_request,
            user_id
        )
        
        logger.info(f"Started Excel export job {job_id} for user {user_id}")
        
        return job
        
    except Exception as e:
        logger.error(f"Error starting Excel export: {e}")
        raise HTTPException(status_code=500, detail="Failed to start Excel export")


@router.post("/charts", response_model=ExportJob)
@rate_limit(max_calls=30, time_window=300)
async def export_charts(
    export_request: ChartExportRequest,
    background_tasks: BackgroundTasks,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Export charts as images with high-quality rendering.
    Supports PNG, SVG, and PDF formats with customizable styling.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Create export job
        job_id = uuid4()
        job = ExportJob(
            job_id=job_id,
            status="pending",
            created_at=datetime.utcnow(),
            export_request=export_request.dict()
        )
        
        # Start export in background
        background_tasks.add_task(
            export_service.export_charts_background,
            job_id,
            export_request,
            user_id
        )
        
        logger.info(f"Started chart export job {job_id} for user {user_id}")
        
        return job
        
    except Exception as e:
        logger.error(f"Error starting chart export: {e}")
        raise HTTPException(status_code=500, detail="Failed to start chart export")


@router.post("/custom", response_model=ExportJob)
@rate_limit(max_calls=10, time_window=300)
async def export_custom(
    analysis_ids: List[UUID] = Field(..., min_items=1, max_items=20),
    format: ExportFormat = Field(..., description="Export format"),
    custom_query: Optional[str] = Field(None, description="Custom SQL query"),
    parameters: Optional[Dict[str, Any]] = Field(None, description="Custom parameters"),
    template_id: Optional[str] = Field(None, description="Export template ID"),
    background_tasks: BackgroundTasks,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Custom export with advanced query capabilities.
    Supports custom SQL queries and export templates.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Create custom export request
        export_request = ExportRequest(
            analysis_ids=analysis_ids,
            format=format
        )
        
        # Create export job
        job_id = uuid4()
        job = ExportJob(
            job_id=job_id,
            status="pending",
            created_at=datetime.utcnow(),
            export_request={
                **export_request.dict(),
                "custom_query": custom_query,
                "parameters": parameters,
                "template_id": template_id
            }
        )
        
        # Start export in background
        background_tasks.add_task(
            export_service.export_custom_background,
            job_id,
            export_request,
            custom_query,
            parameters,
            template_id,
            user_id
        )
        
        logger.info(f"Started custom export job {job_id} for user {user_id}")
        
        return job
        
    except Exception as e:
        logger.error(f"Error starting custom export: {e}")
        raise HTTPException(status_code=500, detail="Failed to start custom export")


@router.get("/{job_id}/status", response_model=ExportJob)
@rate_limit(max_calls=500, time_window=60)
async def get_export_status(
    job_id: UUID,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get export job status with real-time progress updates."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        job = await export_service.get_export_job_status(job_id, user_id)
        if not job:
            raise HTTPException(status_code=404, detail="Export job not found")
        
        return job
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching export job status {job_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch export status")


@router.get("/{job_id}/download")
@rate_limit(max_calls=100, time_window=300)
async def download_export(
    job_id: UUID,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Download completed export file."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Get export job and verify completion
        job = await export_service.get_export_job_status(job_id, user_id)
        if not job:
            raise HTTPException(status_code=404, detail="Export job not found")
        
        if job.status != "completed":
            raise HTTPException(
                status_code=400,
                detail=f"Export not completed. Current status: {job.status}"
            )
        
        # Get file info and stream
        file_info = await export_service.get_export_file_info(job_id)
        if not file_info:
            raise HTTPException(status_code=404, detail="Export file not found")
        
        # Determine media type
        media_types = {
            "csv": "text/csv",
            "excel": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            "json": "application/json",
            "parquet": "application/octet-stream",
            "hdf5": "application/octet-stream",
            "png": "image/png",
            "svg": "image/svg+xml",
            "pdf": "application/pdf"
        }
        
        format_ext = file_info["format"].lower()
        media_type = media_types.get(format_ext, "application/octet-stream")
        
        # Stream file
        file_stream = await export_service.get_export_file_stream(job_id)
        
        logger.info(f"Downloading export file {job_id} for user {user_id}")
        
        return StreamingResponse(
            file_stream,
            media_type=media_type,
            headers={
                "Content-Disposition": f"attachment; filename={file_info['filename']}",
                "Content-Length": str(file_info["size"])
            }
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error downloading export {job_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to download export file")


@router.delete("/{job_id}")
@rate_limit(max_calls=50, time_window=60)
async def cancel_export(
    job_id: UUID,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Cancel a running export job."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        success = await export_service.cancel_export_job(job_id, user_id)
        if not success:
            raise HTTPException(status_code=404, detail="Export job not found or cannot be cancelled")
        
        logger.info(f"Cancelled export job {job_id} by user {user_id}")
        
        return {
            "message": "Export job cancelled successfully",
            "job_id": job_id
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error cancelling export job {job_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to cancel export job")


@router.get("/jobs", response_model=List[ExportJob])
@rate_limit(max_calls=100, time_window=60)
async def get_export_jobs(
    status: Optional[str] = Query(None, regex="^(pending|processing|completed|failed|cancelled)$"),
    limit: int = Query(50, ge=1, le=200),
    offset: int = Query(0, ge=0),
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get list of export jobs for the current user."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        jobs = await export_service.get_user_export_jobs(
            user_id,
            status=status,
            limit=limit,
            offset=offset
        )
        
        return jobs
        
    except Exception as e:
        logger.error(f"Error fetching export jobs: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch export jobs")


@router.get("/templates", response_model=List[Dict[str, Any]])
@rate_limit(max_calls=50, time_window=60)
async def get_export_templates(
    format: Optional[ExportFormat] = Query(None, description="Filter by format"),
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get available export templates."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        templates = await export_service.get_export_templates(format=format)
        
        return templates
        
    except Exception as e:
        logger.error(f"Error fetching export templates: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch export templates")


@router.post("/bulk")
@rate_limit(max_calls=5, time_window=300)
async def export_bulk(
    export_requests: List[Union[CSVExportRequest, ExcelExportRequest, ChartExportRequest]] = Field(..., min_items=1, max_items=10),
    create_archive: bool = Query(True, description="Create single archive file"),
    background_tasks: BackgroundTasks,
    export_service: ExportService = Depends(get_export_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Bulk export multiple formats/analyses as a single operation.
    Creates individual jobs and optionally packages them into an archive.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Create bulk export job
        bulk_job_id = uuid4()
        individual_job_ids = []
        
        # Start individual export jobs
        for export_request in export_requests:
            job_id = uuid4()
            individual_job_ids.append(job_id)
            
            # Start appropriate export based on format
            if export_request.format == ExportFormat.CSV:
                background_tasks.add_task(
                    export_service.export_csv_background,
                    job_id,
                    export_request,
                    user_id,
                    bulk_job_id
                )
            elif export_request.format == ExportFormat.EXCEL:
                background_tasks.add_task(
                    export_service.export_excel_background,
                    job_id,
                    export_request,
                    user_id,
                    bulk_job_id
                )
            elif export_request.format in [ExportFormat.PNG, ExportFormat.SVG, ExportFormat.PDF]:
                background_tasks.add_task(
                    export_service.export_charts_background,
                    job_id,
                    export_request,
                    user_id,
                    bulk_job_id
                )
        
        # If creating archive, start archive job
        if create_archive:
            background_tasks.add_task(
                export_service.create_bulk_archive,
                bulk_job_id,
                individual_job_ids,
                user_id
            )
        
        logger.info(f"Started bulk export {bulk_job_id} with {len(export_requests)} jobs for user {user_id}")
        
        return {
            "message": "Bulk export started successfully",
            "bulk_job_id": bulk_job_id,
            "individual_job_ids": individual_job_ids,
            "create_archive": create_archive,
            "total_exports": len(export_requests)
        }
        
    except Exception as e:
        logger.error(f"Error starting bulk export: {e}")
        raise HTTPException(status_code=500, detail="Failed to start bulk export")


@router.get("/formats", response_model=Dict[str, Any])
@rate_limit(max_calls=100, time_window=60)
async def get_supported_formats(
    include_capabilities: bool = Query(True, description="Include format capabilities"),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get supported export formats and their capabilities."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        formats = {
            "csv": {
                "name": "Comma-Separated Values",
                "extension": "csv",
                "mime_type": "text/csv",
                "supports_compression": True,
                "supports_large_datasets": True,
                "max_size_mb": 1000,
                "capabilities": ["streaming", "custom_delimiters", "date_formatting"]
            },
            "excel": {
                "name": "Microsoft Excel",
                "extension": "xlsx",
                "mime_type": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                "supports_compression": False,
                "supports_large_datasets": True,
                "max_size_mb": 500,
                "capabilities": ["multiple_sheets", "charts", "formatting", "metadata"]
            },
            "json": {
                "name": "JavaScript Object Notation",
                "extension": "json",
                "mime_type": "application/json",
                "supports_compression": True,
                "supports_large_datasets": False,
                "max_size_mb": 100,
                "capabilities": ["nested_data", "custom_queries"]
            },
            "parquet": {
                "name": "Apache Parquet",
                "extension": "parquet",
                "mime_type": "application/octet-stream",
                "supports_compression": True,
                "supports_large_datasets": True,
                "max_size_mb": 2000,
                "capabilities": ["columnar", "high_performance", "analytics_optimized"]
            },
            "png": {
                "name": "Portable Network Graphics",
                "extension": "png",
                "mime_type": "image/png",
                "supports_compression": False,
                "supports_large_datasets": False,
                "max_size_mb": 50,
                "capabilities": ["charts", "high_quality", "transparency"]
            },
            "svg": {
                "name": "Scalable Vector Graphics",
                "extension": "svg",
                "mime_type": "image/svg+xml",
                "supports_compression": True,
                "supports_large_datasets": False,
                "max_size_mb": 20,
                "capabilities": ["charts", "vector", "scalable", "interactive"]
            },
            "pdf": {
                "name": "Portable Document Format",
                "extension": "pdf",
                "mime_type": "application/pdf",
                "supports_compression": True,
                "supports_large_datasets": False,
                "max_size_mb": 100,
                "capabilities": ["charts", "reports", "publication_ready"]
            }
        }
        
        if not include_capabilities:
            for format_info in formats.values():
                format_info.pop("capabilities", None)
        
        return {
            "supported_formats": formats,
            "recommendations": {
                "large_datasets": ["csv", "parquet"],
                "visualization": ["png", "svg", "pdf"],
                "analysis": ["excel", "parquet"],
                "web_integration": ["json", "svg"],
                "publication": ["pdf", "svg", "excel"]
            }
        }
        
    except Exception as e:
        logger.error(f"Error fetching supported formats: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch supported formats")