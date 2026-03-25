"""
File Upload API endpoints for OrcaFlex visualization dashboard.
Provides secure, high-performance file upload with validation and processing.
"""

import logging
import os
import asyncio
from typing import List, Optional, Dict, Any
from uuid import UUID, uuid4
from datetime import datetime
from pathlib import Path

from fastapi import APIRouter, Depends, HTTPException, File, UploadFile, Form, BackgroundTasks
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field, validator

from app.services.upload_service import get_upload_service, UploadService
from app.services.auth_service import verify_token
from app.core.cache import get_cache_service, CacheService
from app.core.rate_limiter import rate_limit
from app.config import get_settings

router = APIRouter()
logger = logging.getLogger(__name__)
security = HTTPBearer()
settings = get_settings()


class UploadValidation(BaseModel):
    """Upload validation configuration."""
    allowed_extensions: List[str] = Field(default=['.dat', '.yml', '.yaml', '.csv', '.sim', '.zip'])
    max_file_size_mb: int = Field(default=100, le=500)
    max_files_per_batch: int = Field(default=10, le=50)
    scan_for_malware: bool = Field(default=True)
    validate_file_structure: bool = Field(default=True)
    extract_archives: bool = Field(default=True)


class UploadJob(BaseModel):
    """Upload job status model."""
    job_id: UUID
    status: str = Field(..., regex="^(pending|uploading|validating|processing|completed|failed)$")
    progress: float = Field(0.0, ge=0.0, le=100.0)
    created_at: datetime
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    total_files: int = 0
    processed_files: int = 0
    failed_files: int = 0
    total_size_mb: float = 0.0
    upload_path: Optional[str] = None
    analysis_id: Optional[UUID] = None
    error_message: Optional[str] = None
    validation_results: Optional[Dict[str, Any]] = None


class FileMetadata(BaseModel):
    """Uploaded file metadata."""
    filename: str
    size_bytes: int
    size_mb: float
    extension: str
    mime_type: str
    checksum: str
    upload_path: str
    created_at: datetime
    is_valid: bool
    validation_errors: List[str] = []
    file_type: Optional[str] = None  # orcaflex, csv_results, configuration, etc.


class BatchUploadRequest(BaseModel):
    """Batch upload configuration."""
    create_analysis: bool = Field(True, description="Automatically create analysis")
    analysis_name: Optional[str] = Field(None, description="Name for new analysis")
    analysis_description: Optional[str] = Field(None, description="Analysis description")
    auto_process: bool = Field(True, description="Automatically start processing")
    processing_priority: str = Field("normal", regex="^(low|normal|high)$")
    validation_config: Optional[UploadValidation] = None


@router.post("/single", response_model=UploadJob)
@rate_limit(max_calls=50, time_window=300)
async def upload_single_file(
    file: UploadFile = File(..., description="File to upload"),
    create_analysis: bool = Form(True, description="Create analysis from file"),
    analysis_name: Optional[str] = Form(None, description="Name for new analysis"),
    analysis_description: Optional[str] = Form(None, description="Analysis description"),
    auto_process: bool = Form(True, description="Start processing automatically"),
    processing_priority: str = Form("normal", regex="^(low|normal|high)$"),
    background_tasks: BackgroundTasks,
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Upload a single file with comprehensive validation and processing.
    Supports OrcaFlex .dat files, CSV results, and configuration files.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Validate file
        if not file.filename:
            raise HTTPException(status_code=400, detail="Filename is required")
        
        # Check file size
        if file.size and file.size > settings.max_file_size:
            raise HTTPException(
                status_code=413,
                detail=f"File size exceeds {settings.max_file_size / (1024*1024):.0f}MB limit"
            )
        
        # Create upload job
        job_id = uuid4()
        upload_job = UploadJob(
            job_id=job_id,
            status="pending",
            created_at=datetime.utcnow(),
            total_files=1,
            total_size_mb=file.size / (1024 * 1024) if file.size else 0.0
        )
        
        # Start upload in background
        background_tasks.add_task(
            upload_service.upload_single_file_background,
            job_id,
            file,
            user_id,
            create_analysis,
            analysis_name,
            analysis_description,
            auto_process,
            processing_priority
        )
        
        logger.info(f"Started single file upload {job_id} for user {user_id}: {file.filename}")
        
        return upload_job
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error uploading single file: {e}")
        raise HTTPException(status_code=500, detail="Failed to upload file")


@router.post("/batch", response_model=UploadJob)
@rate_limit(max_calls=10, time_window=300)
async def upload_batch_files(
    files: List[UploadFile] = File(..., description="Files to upload"),
    batch_config: BatchUploadRequest = Form(..., description="Batch upload configuration"),
    background_tasks: BackgroundTasks,
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Upload multiple files as a batch with coordinated processing.
    Ideal for uploading complete analysis sets or multiple scenarios.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Validate batch size
        if len(files) > (batch_config.validation_config.max_files_per_batch if batch_config.validation_config else 10):
            raise HTTPException(
                status_code=400,
                detail=f"Too many files. Maximum {batch_config.validation_config.max_files_per_batch if batch_config.validation_config else 10} files per batch"
            )
        
        # Calculate total size
        total_size = sum(file.size or 0 for file in files)
        total_size_mb = total_size / (1024 * 1024)
        
        # Check total size limit
        max_batch_size_mb = 500  # 500MB max per batch
        if total_size_mb > max_batch_size_mb:
            raise HTTPException(
                status_code=413,
                detail=f"Batch size exceeds {max_batch_size_mb}MB limit"
            )
        
        # Validate filenames
        for file in files:
            if not file.filename:
                raise HTTPException(status_code=400, detail="All files must have filenames")
        
        # Create upload job
        job_id = uuid4()
        upload_job = UploadJob(
            job_id=job_id,
            status="pending",
            created_at=datetime.utcnow(),
            total_files=len(files),
            total_size_mb=total_size_mb
        )
        
        # Start batch upload in background
        background_tasks.add_task(
            upload_service.upload_batch_files_background,
            job_id,
            files,
            batch_config,
            user_id
        )
        
        logger.info(f"Started batch upload {job_id} for user {user_id}: {len(files)} files, {total_size_mb:.2f}MB")
        
        return upload_job
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error uploading batch files: {e}")
        raise HTTPException(status_code=500, detail="Failed to upload batch files")


@router.post("/url", response_model=UploadJob)
@rate_limit(max_calls=20, time_window=300)
async def upload_from_url(
    url: str = Form(..., description="URL to download file from"),
    create_analysis: bool = Form(True, description="Create analysis from file"),
    analysis_name: Optional[str] = Form(None, description="Name for new analysis"),
    filename: Optional[str] = Form(None, description="Custom filename"),
    background_tasks: BackgroundTasks,
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Upload file from URL with validation and security checks.
    Supports HTTP/HTTPS URLs with proper authentication handling.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Validate URL
        if not url.startswith(('http://', 'https://')):
            raise HTTPException(status_code=400, detail="Only HTTP/HTTPS URLs are supported")
        
        # Create upload job
        job_id = uuid4()
        upload_job = UploadJob(
            job_id=job_id,
            status="pending",
            created_at=datetime.utcnow(),
            total_files=1
        )
        
        # Start URL upload in background
        background_tasks.add_task(
            upload_service.upload_from_url_background,
            job_id,
            url,
            user_id,
            create_analysis,
            analysis_name,
            filename
        )
        
        logger.info(f"Started URL upload {job_id} for user {user_id}: {url}")
        
        return upload_job
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error uploading from URL: {e}")
        raise HTTPException(status_code=500, detail="Failed to upload from URL")


@router.get("/{job_id}/status", response_model=UploadJob)
@rate_limit(max_calls=500, time_window=60)
async def get_upload_status(
    job_id: UUID,
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get upload job status with real-time progress updates."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        job = await upload_service.get_upload_job_status(job_id, user_id)
        if not job:
            raise HTTPException(status_code=404, detail="Upload job not found")
        
        return job
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching upload job status {job_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch upload status")


@router.get("/{job_id}/files", response_model=List[FileMetadata])
@rate_limit(max_calls=100, time_window=60)
async def get_upload_files(
    job_id: UUID,
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get detailed information about uploaded files."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        files = await upload_service.get_upload_job_files(job_id, user_id)
        if files is None:
            raise HTTPException(status_code=404, detail="Upload job not found")
        
        return files
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching upload files {job_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch upload files")


@router.delete("/{job_id}")
@rate_limit(max_calls=50, time_window=60)
async def cancel_upload(
    job_id: UUID,
    delete_files: bool = Form(True, description="Delete uploaded files"),
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Cancel an upload job and optionally delete files."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        success = await upload_service.cancel_upload_job(job_id, user_id, delete_files)
        if not success:
            raise HTTPException(status_code=404, detail="Upload job not found or cannot be cancelled")
        
        logger.info(f"Cancelled upload job {job_id} by user {user_id}")
        
        return {
            "message": "Upload job cancelled successfully",
            "job_id": job_id,
            "files_deleted": delete_files
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error cancelling upload job {job_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to cancel upload job")


@router.post("/validate")
@rate_limit(max_calls=100, time_window=300)
async def validate_files(
    files: List[UploadFile] = File(..., description="Files to validate"),
    validation_config: Optional[UploadValidation] = Form(None, description="Validation configuration"),
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Validate files without uploading them.
    Useful for pre-upload validation and file type detection.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Use default validation config if not provided
        if not validation_config:
            validation_config = UploadValidation()
        
        validation_results = []
        
        for file in files:
            result = await upload_service.validate_file(file, validation_config)
            validation_results.append({
                "filename": file.filename,
                "size_mb": file.size / (1024 * 1024) if file.size else 0.0,
                "is_valid": result["is_valid"],
                "file_type": result.get("file_type"),
                "errors": result.get("errors", []),
                "warnings": result.get("warnings", []),
                "metadata": result.get("metadata", {})
            })
        
        # Calculate summary
        total_files = len(files)
        valid_files = sum(1 for r in validation_results if r["is_valid"])
        total_size_mb = sum(r["size_mb"] for r in validation_results)
        
        logger.info(f"Validated {total_files} files for user {user_id}: {valid_files} valid")
        
        return {
            "summary": {
                "total_files": total_files,
                "valid_files": valid_files,
                "invalid_files": total_files - valid_files,
                "total_size_mb": total_size_mb,
                "validation_passed": valid_files == total_files
            },
            "files": validation_results
        }
        
    except Exception as e:
        logger.error(f"Error validating files: {e}")
        raise HTTPException(status_code=500, detail="Failed to validate files")


@router.get("/jobs", response_model=List[UploadJob])
@rate_limit(max_calls=100, time_window=60)
async def get_upload_jobs(
    status: Optional[str] = Query(None, regex="^(pending|uploading|validating|processing|completed|failed)$"),
    limit: int = Query(50, ge=1, le=200),
    offset: int = Query(0, ge=0),
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get list of upload jobs for the current user."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        jobs = await upload_service.get_user_upload_jobs(
            user_id,
            status=status,
            limit=limit,
            offset=offset
        )
        
        return jobs
        
    except Exception as e:
        logger.error(f"Error fetching upload jobs: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch upload jobs")


@router.get("/templates", response_model=Dict[str, Any])
@rate_limit(max_calls=50, time_window=60)
async def get_upload_templates(
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get upload templates and examples for different file types."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        templates = {
            "orcaflex_data": {
                "description": "OrcaFlex simulation data file",
                "extensions": [".dat"],
                "example_filename": "analysis_case_001.dat",
                "requirements": [
                    "Valid OrcaFlex format",
                    "Contains simulation results",
                    "Maximum 100MB file size"
                ],
                "validation_checks": [
                    "File format validation",
                    "Data completeness check",
                    "Component classification"
                ]
            },
            "csv_results": {
                "description": "CSV results exported from OrcaFlex",
                "extensions": [".csv"],
                "example_filename": "results_polar.csv",
                "requirements": [
                    "Standard CSV format",
                    "Contains numerical data",
                    "Header row required"
                ],
                "validation_checks": [
                    "CSV format validation",
                    "Column header detection",
                    "Data type validation"
                ]
            },
            "configuration": {
                "description": "Analysis configuration files",
                "extensions": [".yml", ".yaml"],
                "example_filename": "analysis_config.yml",
                "requirements": [
                    "Valid YAML format",
                    "Contains configuration parameters",
                    "Matches expected schema"
                ],
                "validation_checks": [
                    "YAML syntax validation",
                    "Schema validation",
                    "Parameter completeness"
                ]
            },
            "archive": {
                "description": "Compressed archive containing multiple files",
                "extensions": [".zip"],
                "example_filename": "analysis_batch.zip",
                "requirements": [
                    "Standard ZIP format",
                    "Contains supported file types",
                    "Maximum 200MB compressed size"
                ],
                "validation_checks": [
                    "Archive integrity check",
                    "Individual file validation",
                    "Directory structure validation"
                ]
            }
        }
        
        return {
            "templates": templates,
            "general_requirements": {
                "max_file_size_mb": 100,
                "max_batch_size_mb": 500,
                "max_files_per_batch": 10,
                "allowed_extensions": [".dat", ".csv", ".yml", ".yaml", ".zip", ".sim"],
                "encoding": "UTF-8 recommended",
                "security": "All files are scanned for malware"
            },
            "best_practices": [
                "Use descriptive filenames with analysis information",
                "Include metadata in YAML configuration files",
                "Compress large batches into ZIP archives",
                "Validate files before uploading large batches",
                "Use consistent naming conventions across analyses"
            ]
        }
        
    except Exception as e:
        logger.error(f"Error fetching upload templates: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch upload templates")


@router.get("/disk-usage", response_model=Dict[str, Any])
@rate_limit(max_calls=100, time_window=60)
async def get_disk_usage(
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get disk usage statistics for the current user."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        usage_stats = await upload_service.get_user_disk_usage(user_id)
        
        return usage_stats
        
    except Exception as e:
        logger.error(f"Error fetching disk usage: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch disk usage")


@router.post("/cleanup")
@rate_limit(max_calls=10, time_window=300)
async def cleanup_old_uploads(
    days_older_than: int = Query(30, ge=1, le=365, description="Delete uploads older than X days"),
    dry_run: bool = Query(True, description="Preview cleanup without deleting"),
    upload_service: UploadService = Depends(get_upload_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Clean up old upload files to free disk space."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        cleanup_result = await upload_service.cleanup_old_uploads(
            user_id,
            days_older_than=days_older_than,
            dry_run=dry_run
        )
        
        logger.info(f"Cleanup operation for user {user_id}: {cleanup_result}")
        
        return cleanup_result
        
    except Exception as e:
        logger.error(f"Error during cleanup: {e}")
        raise HTTPException(status_code=500, detail="Failed to cleanup old uploads")