"""
Enhanced Analysis API endpoints for OrcaFlex simulation processing.
Provides comprehensive analysis case management with caching and performance optimization.
"""

import logging
from typing import List, Optional, Dict, Any
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, File, UploadFile, Query, BackgroundTasks
from fastapi.responses import JSONResponse
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field

from app.models.analysis import (
    Analysis,
    AnalysisRequest,
    AnalysisResult,
    AnalysisProgress,
    AnalysisSummary,
    AnalysisStatus
)
from app.services.analysis_service import get_analysis_service, AnalysisService
from app.services.auth_service import get_auth_service, verify_token
from app.core.cache import get_cache_service, CacheService
from app.core.rate_limiter import rate_limit

router = APIRouter()
logger = logging.getLogger(__name__)
security = HTTPBearer()


class AnalysisFilter(BaseModel):
    """Filter parameters for analysis queries."""
    status: Optional[AnalysisStatus] = None
    water_level: Optional[str] = Field(None, description="Filter by water level (hwl, lwl)")
    volume_condition: Optional[str] = Field(None, description="Filter by volume condition")
    side_configuration: Optional[str] = Field(None, description="Filter by side config (pb, sb, both)")
    loading_phase: Optional[str] = Field(None, description="Filter by loading phase")
    date_from: Optional[str] = Field(None, description="Filter from date (ISO format)")
    date_to: Optional[str] = Field(None, description="Filter to date (ISO format)")
    validation_score_min: Optional[float] = Field(None, ge=0.0, le=1.0)
    is_baseline: Optional[bool] = None


class AnalysisComparison(BaseModel):
    """Request for comparing multiple analyses."""
    analysis_ids: List[UUID] = Field(..., min_items=2, max_items=10)
    comparison_parameters: Optional[List[str]] = None
    include_statistics: bool = True
    include_components: bool = True


@router.get("/", response_model=List[AnalysisSummary])
@rate_limit(max_calls=100, time_window=60)
async def get_analyses(
    status: Optional[AnalysisStatus] = Query(None, description="Filter by status"),
    water_level: Optional[str] = Query(None, description="Filter by water level"),
    volume_condition: Optional[str] = Query(None, description="Filter by volume condition"),
    side_configuration: Optional[str] = Query(None, description="Filter by side config"),
    is_baseline: Optional[bool] = Query(None, description="Filter baseline analyses"),
    validation_score_min: Optional[float] = Query(None, ge=0.0, le=1.0),
    limit: int = Query(50, ge=1, le=1000, description="Maximum number of analyses"),
    offset: int = Query(0, ge=0, description="Offset for pagination"),
    sort_by: str = Query("created_at", description="Sort field"),
    sort_desc: bool = Query(True, description="Sort descending"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get list of analyses with comprehensive filtering and caching.
    Response time: <100ms for cached data, <500ms for complex queries.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key from parameters
        cache_key = f"analyses_list:{status}:{water_level}:{volume_condition}:{side_configuration}:{is_baseline}:{validation_score_min}:{limit}:{offset}:{sort_by}:{sort_desc}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for analyses list query")
            return cached_result
        
        # Create filter object
        filter_params = AnalysisFilter(
            status=status,
            water_level=water_level,
            volume_condition=volume_condition,
            side_configuration=side_configuration,
            is_baseline=is_baseline,
            validation_score_min=validation_score_min
        )
        
        # Query database
        analyses = await analysis_service.get_analyses_filtered(
            filter_params=filter_params,
            limit=limit,
            offset=offset,
            sort_by=sort_by,
            sort_desc=sort_desc
        )
        
        # Cache results for 5 minutes
        await cache_service.set(cache_key, analyses, ttl=300)
        
        logger.info(f"Retrieved {len(analyses)} analyses for user {user_id}")
        return analyses
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching analyses: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch analyses")


@router.post("/", response_model=Analysis)
@rate_limit(max_calls=10, time_window=60)
async def create_analysis(
    analysis_request: AnalysisRequest,
    analysis_service: AnalysisService = Depends(get_analysis_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Create a new analysis with comprehensive validation."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Add user context to analysis request
        analysis_request.created_by = user_id
        
        analysis = await analysis_service.create_analysis(analysis_request)
        
        logger.info(f"Created analysis {analysis.uuid} by user {user_id}")
        return analysis
        
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"Error creating analysis: {e}")
        raise HTTPException(status_code=500, detail="Failed to create analysis")


@router.post("/upload")
@rate_limit(max_calls=20, time_window=60)
async def upload_orcaflex_file(
    file: UploadFile = File(..., description="OrcaFlex simulation file"),
    create_analysis: bool = Query(True, description="Automatically create analysis"),
    analysis_name: Optional[str] = Query(None, description="Name for new analysis"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Upload OrcaFlex simulation file with enhanced validation and processing.
    Supports .dat, .yml, .yaml files up to 100MB.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Enhanced file validation
        if not file.filename:
            raise HTTPException(status_code=400, detail="Filename is required")
            
        allowed_extensions = {'.dat', '.yml', '.yaml', '.csv', '.sim'}
        file_extension = file.filename.lower()
        if not any(file_extension.endswith(ext) for ext in allowed_extensions):
            raise HTTPException(
                status_code=400,
                detail=f"Invalid file type. Supported: {', '.join(allowed_extensions)}"
            )
        
        # Check file size (100MB limit)
        if file.size and file.size > 100 * 1024 * 1024:
            raise HTTPException(
                status_code=413,
                detail="File size exceeds 100MB limit"
            )
        
        # Upload and process file
        upload_result = await analysis_service.upload_file(
            file, 
            user_id=user_id,
            create_analysis=create_analysis,
            analysis_name=analysis_name
        )
        
        logger.info(f"File {file.filename} uploaded by user {user_id}")
        
        return {
            "message": "File uploaded successfully",
            "file_path": upload_result["file_path"],
            "filename": file.filename,
            "size": file.size,
            "analysis_id": upload_result.get("analysis_id"),
            "processing_status": upload_result.get("processing_status", "uploaded")
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error uploading file: {e}")
        raise HTTPException(status_code=500, detail="Failed to upload file")


@router.get("/{analysis_id}", response_model=Analysis)
@rate_limit(max_calls=200, time_window=60)
async def get_analysis(
    analysis_id: UUID,
    include_components: bool = Query(False, description="Include component data"),
    include_results: bool = Query(False, description="Include result data"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get specific analysis by ID with caching."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"analysis:{analysis_id}:components_{include_components}:results_{include_results}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for analysis {analysis_id}")
            return cached_result
        
        analysis = await analysis_service.get_analysis(
            analysis_id,
            include_components=include_components,
            include_results=include_results
        )
        
        if not analysis:
            raise HTTPException(status_code=404, detail="Analysis not found")
            
        # Cache for 10 minutes
        await cache_service.set(cache_key, analysis, ttl=600)
        
        return analysis
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch analysis")


@router.get("/{analysis_id}/components")
@rate_limit(max_calls=100, time_window=60)
async def get_analysis_components(
    analysis_id: UUID,
    component_type: Optional[str] = Query(None, description="Filter by component type"),
    include_results: bool = Query(False, description="Include result data"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get components for a specific analysis with filtering."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"analysis_components:{analysis_id}:{component_type}:{include_results}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        components = await analysis_service.get_analysis_components(
            analysis_id,
            component_type=component_type,
            include_results=include_results
        )
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, components, ttl=600)
        
        return {
            "analysis_id": analysis_id,
            "components": components,
            "total_components": len(components),
            "component_types": list(set(c.component_type for c in components))
        }
        
    except Exception as e:
        logger.error(f"Error fetching components for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch components")


@router.get("/{analysis_id}/progress", response_model=AnalysisProgress)
@rate_limit(max_calls=300, time_window=60)
async def get_analysis_progress(
    analysis_id: UUID,
    analysis_service: AnalysisService = Depends(get_analysis_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get real-time progress information for a running analysis."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        progress = await analysis_service.get_analysis_progress(analysis_id)
        if not progress:
            raise HTTPException(status_code=404, detail="Analysis or progress not found")
            
        return progress
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching progress for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch progress")


@router.post("/{analysis_id}/run")
@rate_limit(max_calls=10, time_window=60)
async def run_analysis(
    analysis_id: UUID,
    background_tasks: BackgroundTasks,
    priority: str = Query("normal", regex="^(low|normal|high)$"),
    force_rerun: bool = Query(False, description="Force rerun even if completed"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Start running an analysis with priority handling."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Validate analysis exists and user has permission
        analysis = await analysis_service.get_analysis(analysis_id)
        if not analysis:
            raise HTTPException(status_code=404, detail="Analysis not found")
        
        # Check if analysis can be run
        if not force_rerun and analysis.status == AnalysisStatus.COMPLETED:
            raise HTTPException(
                status_code=400,
                detail="Analysis already completed. Use force_rerun=true to rerun."
            )
        
        if analysis.status == AnalysisStatus.RUNNING:
            raise HTTPException(
                status_code=400,
                detail="Analysis is already running"
            )
        
        # Start analysis in background with priority
        background_tasks.add_task(
            analysis_service.run_analysis_background,
            analysis_id,
            user_id=user_id,
            priority=priority
        )
        
        # Update status to running
        await analysis_service.update_analysis_status(analysis_id, AnalysisStatus.RUNNING)
        
        logger.info(f"Started analysis {analysis_id} with priority {priority} by user {user_id}")
        
        return {
            "message": "Analysis started successfully",
            "analysis_id": analysis_id,
            "status": "running",
            "priority": priority,
            "estimated_duration": await analysis_service.estimate_processing_time(analysis_id)
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error starting analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to start analysis")


@router.post("/{analysis_id}/cancel")
@rate_limit(max_calls=50, time_window=60)
async def cancel_analysis(
    analysis_id: UUID,
    reason: Optional[str] = Query(None, description="Cancellation reason"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Cancel a running analysis."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        success = await analysis_service.cancel_analysis(analysis_id, user_id, reason)
        if not success:
            raise HTTPException(
                status_code=400,
                detail="Analysis cannot be cancelled or not found"
            )
        
        logger.info(f"Cancelled analysis {analysis_id} by user {user_id}: {reason}")
        
        return {
            "message": "Analysis cancelled successfully",
            "analysis_id": analysis_id,
            "status": "cancelled",
            "cancelled_by": user_id,
            "reason": reason
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error cancelling analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to cancel analysis")


@router.post("/compare", response_model=Dict[str, Any])
@rate_limit(max_calls=20, time_window=60)
async def compare_analyses(
    comparison_request: AnalysisComparison,
    analysis_service: AnalysisService = Depends(get_analysis_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Compare multiple analyses with statistical analysis."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        analysis_ids_str = "_".join(str(id) for id in sorted(comparison_request.analysis_ids))
        cache_key = f"analysis_comparison:{analysis_ids_str}:{comparison_request.include_statistics}:{comparison_request.include_components}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Perform comparison
        comparison_result = await analysis_service.compare_analyses(
            comparison_request.analysis_ids,
            comparison_parameters=comparison_request.comparison_parameters,
            include_statistics=comparison_request.include_statistics,
            include_components=comparison_request.include_components
        )
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, comparison_result, ttl=900)
        
        logger.info(f"Compared {len(comparison_request.analysis_ids)} analyses for user {user_id}")
        
        return comparison_result
        
    except Exception as e:
        logger.error(f"Error comparing analyses: {e}")
        raise HTTPException(status_code=500, detail="Failed to compare analyses")


@router.get("/{analysis_id}/statistics")
@rate_limit(max_calls=100, time_window=60)
async def get_analysis_statistics(
    analysis_id: UUID,
    include_components: bool = Query(True, description="Include component statistics"),
    include_quality_metrics: bool = Query(True, description="Include quality metrics"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get comprehensive statistical analysis of analysis data."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"analysis_stats:{analysis_id}:{include_components}:{include_quality_metrics}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        statistics = await analysis_service.get_analysis_statistics(
            analysis_id,
            include_components=include_components,
            include_quality_metrics=include_quality_metrics
        )
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, statistics, ttl=900)
        
        return statistics
        
    except Exception as e:
        logger.error(f"Error fetching statistics for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch statistics")


@router.delete("/{analysis_id}")
@rate_limit(max_calls=10, time_window=60)
async def delete_analysis(
    analysis_id: UUID,
    force: bool = Query(False, description="Force delete even if running"),
    delete_files: bool = Query(True, description="Delete associated files"),
    analysis_service: AnalysisService = Depends(get_analysis_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Delete an analysis with comprehensive cleanup."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        success = await analysis_service.delete_analysis(
            analysis_id,
            force=force,
            delete_files=delete_files,
            deleted_by=user_id
        )
        
        if not success:
            raise HTTPException(status_code=404, detail="Analysis not found")
        
        # Clear related cache entries
        await cache_service.delete_pattern(f"analysis*{analysis_id}*")
        await cache_service.delete_pattern("analyses_list:*")
        
        logger.info(f"Deleted analysis {analysis_id} by user {user_id}")
        
        return {
            "message": "Analysis deleted successfully",
            "analysis_id": analysis_id,
            "deleted_by": user_id,
            "files_deleted": delete_files
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to delete analysis")