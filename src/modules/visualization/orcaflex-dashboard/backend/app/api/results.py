"""
Enhanced Results API endpoints for OrcaFlex simulation data.
Provides high-performance data access with caching, filtering, and visualization support.
"""

import logging
from typing import List, Optional, Dict, Any, Tuple
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, Query
from fastapi.responses import StreamingResponse, JSONResponse
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field

from app.models.results import (
    SimulationResult,
    ResultsQuery,
    ResultsMetadata,
    TimeSeriesData,
    StatisticalSummary,
    PolarData,
    ComparisonResult
)
from app.services.results_service import get_results_service, ResultsService
from app.services.auth_service import verify_token
from app.core.cache import get_cache_service, CacheService
from app.core.rate_limiter import rate_limit

router = APIRouter()
logger = logging.getLogger(__name__)
security = HTTPBearer()


class ResultsFilter(BaseModel):
    """Advanced filtering parameters for results queries."""
    analysis_id: Optional[UUID] = None
    component_ids: Optional[List[int]] = None
    component_types: Optional[List[str]] = None
    result_types: Optional[List[str]] = Field(None, description="polar, timeseries, summary, statistics")
    parameter_names: Optional[List[str]] = None
    data_quality_min: Optional[float] = Field(None, ge=0.0, le=1.0)
    time_range: Optional[Tuple[float, float]] = None
    value_range: Optional[Tuple[float, float]] = None
    has_anomalies: Optional[bool] = None
    is_validated: Optional[bool] = None


class PolarQuery(BaseModel):
    """Polar plot data query parameters."""
    analysis_id: UUID
    component_ids: Optional[List[int]] = None
    parameter_names: Optional[List[str]] = None
    heading_range: Optional[Tuple[float, float]] = Field(None, description="Heading range in degrees (0-360)")
    normalize_by_max: bool = Field(False, description="Normalize values by maximum")
    angular_resolution: float = Field(5.0, ge=0.1, le=45.0, description="Angular resolution in degrees")
    include_statistics: bool = Field(True, description="Include statistical summaries")


class TimeSeriesQuery(BaseModel):
    """Time series data query parameters."""
    analysis_id: UUID
    component_ids: Optional[List[int]] = None
    parameter_names: Optional[List[str]] = None
    time_range: Optional[Tuple[float, float]] = None
    sampling_rate: Optional[float] = Field(None, gt=0, description="Resample to specific rate (Hz)")
    max_points: int = Field(10000, ge=100, le=100000, description="Maximum data points to return")
    include_filtered: bool = Field(False, description="Include filtered/smoothed data")
    filter_type: Optional[str] = Field(None, regex="^(lowpass|highpass|bandpass|butterworth)$")


class ComparisonQuery(BaseModel):
    """Multi-analysis comparison parameters."""
    baseline_analysis_id: UUID
    comparison_analysis_ids: List[UUID] = Field(..., min_items=1, max_items=10)
    parameter_names: List[str] = Field(..., min_items=1)
    component_types: Optional[List[str]] = None
    comparison_method: str = Field("absolute", regex="^(absolute|relative|normalized)$")
    include_statistics: bool = Field(True, description="Include statistical comparison")
    confidence_level: float = Field(0.95, ge=0.8, le=0.99, description="Statistical confidence level")


@router.get("/", response_model=List[SimulationResult])
@rate_limit(max_calls=200, time_window=60)
async def get_results(
    analysis_id: Optional[UUID] = Query(None, description="Filter by analysis ID"),
    component_ids: Optional[List[int]] = Query(None, description="Filter by component IDs"),
    component_types: Optional[List[str]] = Query(None, description="Filter by component types"),
    result_types: Optional[List[str]] = Query(None, description="Filter by result types"),
    parameter_names: Optional[List[str]] = Query(None, description="Filter by parameter names"),
    data_quality_min: Optional[float] = Query(None, ge=0.0, le=1.0, description="Minimum data quality"),
    is_validated: Optional[bool] = Query(None, description="Filter by validation status"),
    limit: int = Query(100, ge=1, le=1000, description="Maximum number of results"),
    offset: int = Query(0, ge=0, description="Offset for pagination"),
    sort_by: str = Query("created_at", description="Sort field"),
    sort_desc: bool = Query(True, description="Sort descending"),
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get list of simulation results with comprehensive filtering and caching.
    Optimized for >10,000 data points with <100ms response time for cached data.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key from parameters
        cache_key = (f"results_list:{analysis_id}:{component_ids}:{component_types}:"
                    f"{result_types}:{parameter_names}:{data_quality_min}:{is_validated}:"
                    f"{limit}:{offset}:{sort_by}:{sort_desc}")
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug("Cache hit for results list query")
            return cached_result
        
        # Create filter object
        filter_params = ResultsFilter(
            analysis_id=analysis_id,
            component_ids=component_ids,
            component_types=component_types,
            result_types=result_types,
            parameter_names=parameter_names,
            data_quality_min=data_quality_min,
            is_validated=is_validated
        )
        
        # Query database
        results = await results_service.get_results_filtered(
            filter_params=filter_params,
            limit=limit,
            offset=offset,
            sort_by=sort_by,
            sort_desc=sort_desc
        )
        
        # Cache results for 5 minutes
        await cache_service.set(cache_key, results, ttl=300)
        
        logger.info(f"Retrieved {len(results)} results for user {user_id}")
        return results
        
    except Exception as e:
        logger.error(f"Error fetching results: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch results")


@router.get("/{result_id}", response_model=SimulationResult)
@rate_limit(max_calls=300, time_window=60)
async def get_result(
    result_id: UUID,
    include_data: bool = Query(True, description="Include full data arrays"),
    include_metadata: bool = Query(True, description="Include result metadata"),
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get specific simulation result by ID with caching optimization."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"result:{result_id}:data_{include_data}:metadata_{include_metadata}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for result {result_id}")
            return cached_result
        
        result = await results_service.get_result(
            result_id,
            include_data=include_data,
            include_metadata=include_metadata
        )
        
        if not result:
            raise HTTPException(status_code=404, detail="Result not found")
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, result, ttl=600)
        
        return result
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch result")


@router.post("/polar", response_model=List[PolarData])
@rate_limit(max_calls=100, time_window=60)
async def get_polar_data(
    query: PolarQuery,
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get polar plot data with heading filtering and optimization.
    Supports efficient rendering of >10,000 data points.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"polar_data:{hash(str(query.dict()))}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get polar data
        polar_data = await results_service.get_polar_data(query)
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, polar_data, ttl=600)
        
        logger.info(f"Retrieved polar data for analysis {query.analysis_id} for user {user_id}")
        return polar_data
        
    except Exception as e:
        logger.error(f"Error fetching polar data: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch polar data")


@router.post("/timeseries", response_model=List[TimeSeriesData])
@rate_limit(max_calls=150, time_window=60)
async def get_timeseries_data(
    query: TimeSeriesQuery,
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get time series data with component grouping and filtering.
    Optimized for large datasets with intelligent sampling.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"timeseries_data:{hash(str(query.dict()))}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get time series data
        timeseries_data = await results_service.get_timeseries_data(query)
        
        # Cache for 8 minutes (shorter for large datasets)
        cache_ttl = 300 if query.max_points > 5000 else 480
        await cache_service.set(cache_key, timeseries_data, ttl=cache_ttl)
        
        logger.info(f"Retrieved time series data for analysis {query.analysis_id} for user {user_id}")
        return timeseries_data
        
    except Exception as e:
        logger.error(f"Error fetching time series data: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch time series data")


@router.get("/summary/{analysis_id}", response_model=List[StatisticalSummary])
@rate_limit(max_calls=100, time_window=60)
async def get_statistical_summary(
    analysis_id: UUID,
    component_types: Optional[List[str]] = Query(None, description="Filter by component types"),
    parameter_names: Optional[List[str]] = Query(None, description="Filter by parameter names"),
    include_percentiles: bool = Query(True, description="Include percentile calculations"),
    include_extremes: bool = Query(True, description="Include min/max values"),
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get statistical summary data with comprehensive metrics."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        components_str = "_".join(sorted(component_types or []))
        params_str = "_".join(sorted(parameter_names or []))
        cache_key = f"summary:{analysis_id}:{components_str}:{params_str}:{include_percentiles}:{include_extremes}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get statistical summary
        summary = await results_service.get_statistical_summary(
            analysis_id,
            component_types=component_types,
            parameter_names=parameter_names,
            include_percentiles=include_percentiles,
            include_extremes=include_extremes
        )
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, summary, ttl=900)
        
        return summary
        
    except Exception as e:
        logger.error(f"Error fetching statistical summary: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch statistical summary")


@router.post("/compare", response_model=ComparisonResult)
@rate_limit(max_calls=30, time_window=300)
async def compare_analyses(
    query: ComparisonQuery,
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Multi-analysis comparison with statistical analysis.
    Supports comprehensive comparison of results across multiple analyses.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        all_ids = [query.baseline_analysis_id] + query.comparison_analysis_ids
        ids_str = "_".join(str(id) for id in sorted(all_ids))
        params_str = "_".join(sorted(query.parameter_names))
        components_str = "_".join(sorted(query.component_types or []))
        cache_key = f"compare:{ids_str}:{params_str}:{components_str}:{query.comparison_method}:{query.confidence_level}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Perform comparison
        comparison = await results_service.compare_analyses(query)
        
        # Cache for 20 minutes
        await cache_service.set(cache_key, comparison, ttl=1200)
        
        logger.info(f"Performed analysis comparison for user {user_id}")
        return comparison
        
    except Exception as e:
        logger.error(f"Error comparing analyses: {e}")
        raise HTTPException(status_code=500, detail="Failed to compare analyses")


@router.get("/{result_id}/metadata", response_model=ResultsMetadata)
@rate_limit(max_calls=200, time_window=60)
async def get_result_metadata(
    result_id: UUID,
    include_quality_metrics: bool = Query(True, description="Include data quality metrics"),
    include_processing_info: bool = Query(True, description="Include processing information"),
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get comprehensive metadata for a specific result."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"result_metadata:{result_id}:{include_quality_metrics}:{include_processing_info}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        metadata = await results_service.get_result_metadata(
            result_id,
            include_quality_metrics=include_quality_metrics,
            include_processing_info=include_processing_info
        )
        
        if not metadata:
            raise HTTPException(status_code=404, detail="Result not found")
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, metadata, ttl=900)
        
        return metadata
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching metadata for result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch metadata")


@router.post("/{result_id}/query", response_model=SimulationResult)
@rate_limit(max_calls=100, time_window=60)
async def query_result_data(
    result_id: UUID,
    query: ResultsQuery,
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Query specific data from a simulation result with advanced filtering."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"result_query:{result_id}:{hash(str(query.dict()))}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        result = await results_service.query_result_data(result_id, query)
        if not result:
            raise HTTPException(status_code=404, detail="Result not found")
        
        # Cache for 8 minutes
        await cache_service.set(cache_key, result, ttl=480)
        
        return result
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error querying result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to query result data")


@router.get("/analysis/{analysis_id}/components")
@rate_limit(max_calls=100, time_window=60)
async def get_analysis_results_by_component(
    analysis_id: UUID,
    group_by_component: bool = Query(True, description="Group results by component"),
    include_statistics: bool = Query(True, description="Include component statistics"),
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get results organized by component for an analysis."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"analysis_results_components:{analysis_id}:{group_by_component}:{include_statistics}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get results by component
        component_results = await results_service.get_analysis_results_by_component(
            analysis_id,
            group_by_component=group_by_component,
            include_statistics=include_statistics
        )
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, component_results, ttl=600)
        
        return component_results
        
    except Exception as e:
        logger.error(f"Error fetching component results for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch component results")


@router.get("/export/{result_id}")
@rate_limit(max_calls=50, time_window=300)
async def export_result(
    result_id: UUID,
    format: str = Query("csv", regex="^(csv|json|excel|parquet)$", description="Export format"),
    include_metadata: bool = Query(True, description="Include result metadata"),
    compress: bool = Query(False, description="Compress export file"),
    variables: Optional[List[str]] = Query(None, description="Variables to include"),
    results_service: ResultsService = Depends(get_results_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Export result data in specified format with streaming response."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Generate export
        export_stream = await results_service.export_result(
            result_id=result_id,
            format=format,
            include_metadata=include_metadata,
            compress=compress,
            variables=variables,
            user_id=user_id
        )
        
        if not export_stream:
            raise HTTPException(status_code=404, detail="Result not found")
        
        # Determine content type and filename
        content_types = {
            "csv": "text/csv",
            "json": "application/json",
            "excel": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            "parquet": "application/octet-stream"
        }
        
        extensions = {
            "csv": "csv",
            "json": "json",
            "excel": "xlsx",
            "parquet": "parquet"
        }
        
        content_type = content_types[format]
        extension = extensions[format]
        if compress and format in ["csv", "json"]:
            extension += ".gz"
            content_type = "application/gzip"
        
        filename = f"result_{result_id}.{extension}"
        
        logger.info(f"Exporting result {result_id} as {format} for user {user_id}")
        
        return StreamingResponse(
            export_stream,
            media_type=content_type,
            headers={"Content-Disposition": f"attachment; filename={filename}"}
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error exporting result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to export result")


@router.delete("/{result_id}")
@rate_limit(max_calls=20, time_window=60)
async def delete_result(
    result_id: UUID,
    force: bool = Query(False, description="Force delete even if referenced"),
    results_service: ResultsService = Depends(get_results_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Delete a simulation result with cache cleanup."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        success = await results_service.delete_result(
            result_id,
            force=force,
            deleted_by=user_id
        )
        
        if not success:
            raise HTTPException(status_code=404, detail="Result not found")
        
        # Clear related cache entries
        await cache_service.delete_pattern(f"result*{result_id}*")
        await cache_service.delete_pattern("results_list:*")
        
        logger.info(f"Deleted result {result_id} by user {user_id}")
        
        return {
            "message": "Result deleted successfully",
            "result_id": result_id,
            "deleted_by": user_id
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to delete result")