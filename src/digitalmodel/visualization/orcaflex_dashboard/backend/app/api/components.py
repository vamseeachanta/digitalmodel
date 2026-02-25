"""
Component Management API endpoints for OrcaFlex visualization.
Provides component classification, grouping, and data access with performance optimization.
"""

import logging
from typing import List, Optional, Dict, Any
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, Query
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field

from app.models.component import Component, ComponentSummary, ComponentHierarchy
from app.services.component_service import get_component_service, ComponentService
from app.services.auth_service import verify_token
from app.core.cache import get_cache_service, CacheService
from app.core.rate_limiter import rate_limit

router = APIRouter()
logger = logging.getLogger(__name__)
security = HTTPBearer()


class ComponentFilter(BaseModel):
    """Filter parameters for component queries."""
    component_type: Optional[str] = Field(None, description="Filter by component type (fst1, fst2, strut, jacket, lngc)")
    analysis_id: Optional[UUID] = Field(None, description="Filter by analysis ID")
    classification_confidence_min: Optional[float] = Field(None, ge=0.0, le=1.0)
    data_quality_score_min: Optional[float] = Field(None, ge=0.0, le=1.0)
    has_polar_data: Optional[bool] = None
    has_time_series_data: Optional[bool] = None
    has_summary_data: Optional[bool] = None
    is_validated: Optional[bool] = None
    is_primary_component: Optional[bool] = None
    component_level: Optional[int] = Field(None, ge=0, le=5)


class ComponentGrouping(BaseModel):
    """Component grouping configuration."""
    group_by: str = Field(..., description="Group by field (type, analysis, level, quality)")
    include_statistics: bool = True
    include_hierarchy: bool = False


@router.get("/", response_model=List[ComponentSummary])
@rate_limit(max_calls=200, time_window=60)
async def get_components(
    component_type: Optional[str] = Query(None, description="Filter by component type"),
    analysis_id: Optional[UUID] = Query(None, description="Filter by analysis ID"),
    classification_confidence_min: Optional[float] = Query(None, ge=0.0, le=1.0),
    data_quality_score_min: Optional[float] = Query(None, ge=0.0, le=1.0),
    has_polar_data: Optional[bool] = Query(None, description="Filter by polar data availability"),
    has_time_series_data: Optional[bool] = Query(None, description="Filter by time series data availability"),
    is_validated: Optional[bool] = Query(None, description="Filter by validation status"),
    is_primary_component: Optional[bool] = Query(None, description="Filter by primary component flag"),
    limit: int = Query(100, ge=1, le=1000, description="Maximum number of components"),
    offset: int = Query(0, ge=0, description="Offset for pagination"),
    sort_by: str = Query("name", description="Sort field"),
    sort_desc: bool = Query(False, description="Sort descending"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get list of components with comprehensive filtering and caching.
    Optimized for component classification and grouping workflows.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = (f"components_list:{component_type}:{analysis_id}:"
                    f"{classification_confidence_min}:{data_quality_score_min}:"
                    f"{has_polar_data}:{has_time_series_data}:{is_validated}:"
                    f"{is_primary_component}:{limit}:{offset}:{sort_by}:{sort_desc}")
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for components list query")
            return cached_result
        
        # Create filter object
        filter_params = ComponentFilter(
            component_type=component_type,
            analysis_id=analysis_id,
            classification_confidence_min=classification_confidence_min,
            data_quality_score_min=data_quality_score_min,
            has_polar_data=has_polar_data,
            has_time_series_data=has_time_series_data,
            is_validated=is_validated,
            is_primary_component=is_primary_component
        )
        
        # Query database
        components = await component_service.get_components_filtered(
            filter_params=filter_params,
            limit=limit,
            offset=offset,
            sort_by=sort_by,
            sort_desc=sort_desc
        )
        
        # Cache results for 5 minutes
        await cache_service.set(cache_key, components, ttl=300)
        
        logger.info(f"Retrieved {len(components)} components for user {user_id}")
        return components
        
    except Exception as e:
        logger.error(f"Error fetching components: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch components")


@router.get("/{component_id}", response_model=Component)
@rate_limit(max_calls=300, time_window=60)
async def get_component(
    component_id: int,
    include_results: bool = Query(False, description="Include result data"),
    include_children: bool = Query(False, description="Include child components"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get specific component by ID with caching."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"component:{component_id}:results_{include_results}:children_{include_children}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for component {component_id}")
            return cached_result
        
        component = await component_service.get_component(
            component_id,
            include_results=include_results,
            include_children=include_children
        )
        
        if not component:
            raise HTTPException(status_code=404, detail="Component not found")
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, component, ttl=600)
        
        return component
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching component {component_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch component")


@router.get("/{component_id}/data")
@rate_limit(max_calls=200, time_window=60)
async def get_component_data(
    component_id: int,
    data_type: Optional[str] = Query(None, regex="^(polar|timeseries|summary|statistics)$", description="Filter by data type"),
    parameter_names: Optional[List[str]] = Query(None, description="Filter by parameter names"),
    time_range_start: Optional[float] = Query(None, description="Start time for time series"),
    time_range_end: Optional[float] = Query(None, description="End time for time series"),
    heading_range_start: Optional[float] = Query(None, ge=0, le=360, description="Start heading for polar data"),
    heading_range_end: Optional[float] = Query(None, ge=0, le=360, description="End heading for polar data"),
    limit: int = Query(10000, ge=1, le=50000, description="Maximum data points"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get component-specific data with filtering and optimization.
    Supports >10,000 data points efficiently with intelligent caching.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        params_str = "_".join(parameter_names or ["all"])
        cache_key = (f"component_data:{component_id}:{data_type}:{params_str}:"
                    f"{time_range_start}_{time_range_end}:{heading_range_start}_{heading_range_end}:{limit}")
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for component {component_id} data")
            return cached_result
        
        # Prepare data query parameters
        query_params = {
            'data_type': data_type,
            'parameter_names': parameter_names,
            'limit': limit
        }
        
        # Add time range if specified
        if time_range_start is not None and time_range_end is not None:
            query_params['time_range'] = (time_range_start, time_range_end)
        
        # Add heading range if specified
        if heading_range_start is not None and heading_range_end is not None:
            query_params['heading_range'] = (heading_range_start, heading_range_end)
        
        # Query component data
        component_data = await component_service.get_component_data(component_id, query_params)
        
        if not component_data:
            raise HTTPException(status_code=404, detail="Component or data not found")
        
        # Cache for 10 minutes (shorter for large datasets)
        cache_ttl = 300 if limit > 1000 else 600
        await cache_service.set(cache_key, component_data, ttl=cache_ttl)
        
        return component_data
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching data for component {component_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch component data")


@router.get("/groups", response_model=Dict[str, Any])
@rate_limit(max_calls=100, time_window=60)
async def get_component_groups(
    group_by: str = Query("type", regex="^(type|analysis|level|quality|validation)$", description="Group by field"),
    analysis_id: Optional[UUID] = Query(None, description="Filter by analysis ID"),
    include_statistics: bool = Query(True, description="Include group statistics"),
    include_hierarchy: bool = Query(False, description="Include component hierarchy"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get component grouping (struts vs jackets vs floaters) with statistics.
    Optimized for dashboard overview and classification workflows.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"component_groups:{group_by}:{analysis_id}:{include_statistics}:{include_hierarchy}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug(f"Cache hit for component groups query")
            return cached_result
        
        # Create grouping configuration
        grouping_config = ComponentGrouping(
            group_by=group_by,
            include_statistics=include_statistics,
            include_hierarchy=include_hierarchy
        )
        
        # Get component groups
        groups = await component_service.get_component_groups(
            grouping_config,
            analysis_id=analysis_id
        )
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, groups, ttl=900)
        
        logger.info(f"Retrieved component groups by {group_by} for user {user_id}")
        return groups
        
    except Exception as e:
        logger.error(f"Error fetching component groups: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch component groups")


@router.get("/hierarchy/{analysis_id}", response_model=List[ComponentHierarchy])
@rate_limit(max_calls=100, time_window=60)
async def get_component_hierarchy(
    analysis_id: UUID,
    max_depth: int = Query(3, ge=1, le=5, description="Maximum hierarchy depth"),
    include_data_summary: bool = Query(True, description="Include data availability summary"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get hierarchical component structure for an analysis."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"component_hierarchy:{analysis_id}:{max_depth}:{include_data_summary}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get component hierarchy
        hierarchy = await component_service.get_component_hierarchy(
            analysis_id,
            max_depth=max_depth,
            include_data_summary=include_data_summary
        )
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, hierarchy, ttl=600)
        
        return hierarchy
        
    except Exception as e:
        logger.error(f"Error fetching component hierarchy for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch component hierarchy")


@router.get("/classification/summary")
@rate_limit(max_calls=50, time_window=60)
async def get_classification_summary(
    analysis_id: Optional[UUID] = Query(None, description="Filter by analysis ID"),
    confidence_threshold: float = Query(0.7, ge=0.0, le=1.0, description="Minimum confidence threshold"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get component classification quality summary and statistics."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        cache_key = f"classification_summary:{analysis_id}:{confidence_threshold}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get classification summary
        summary = await component_service.get_classification_summary(
            analysis_id=analysis_id,
            confidence_threshold=confidence_threshold
        )
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, summary, ttl=900)
        
        return summary
        
    except Exception as e:
        logger.error(f"Error fetching classification summary: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch classification summary")


@router.post("/{component_id}/reclassify")
@rate_limit(max_calls=10, time_window=60)
async def reclassify_component(
    component_id: int,
    new_component_type: str = Query(..., regex="^(fst1|fst2|strut|jacket|lngc|unknown)$"),
    confidence_override: Optional[float] = Query(None, ge=0.0, le=1.0),
    reason: Optional[str] = Query(None, description="Reason for reclassification"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Manually reclassify a component with audit logging."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Reclassify component
        result = await component_service.reclassify_component(
            component_id,
            new_component_type,
            confidence_override=confidence_override,
            reclassified_by=user_id,
            reason=reason
        )
        
        if not result:
            raise HTTPException(status_code=404, detail="Component not found")
        
        # Clear related cache entries
        await cache_service.delete_pattern(f"component*{component_id}*")
        await cache_service.delete_pattern("component_groups:*")
        await cache_service.delete_pattern("classification_summary:*")
        
        logger.info(f"Reclassified component {component_id} to {new_component_type} by user {user_id}")
        
        return {
            "message": "Component reclassified successfully",
            "component_id": component_id,
            "old_type": result["old_type"],
            "new_type": new_component_type,
            "confidence": confidence_override,
            "reclassified_by": user_id,
            "reason": reason
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error reclassifying component {component_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to reclassify component")


@router.get("/{component_id}/validation")
@rate_limit(max_calls=100, time_window=60)
async def validate_component(
    component_id: int,
    include_recommendations: bool = Query(True, description="Include validation recommendations"),
    component_service: ComponentService = Depends(get_component_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Validate component data quality and classification."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        validation_result = await component_service.validate_component(
            component_id,
            include_recommendations=include_recommendations
        )
        
        if not validation_result:
            raise HTTPException(status_code=404, detail="Component not found")
        
        return validation_result
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error validating component {component_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to validate component")


@router.delete("/{component_id}")
@rate_limit(max_calls=10, time_window=60)
async def delete_component(
    component_id: int,
    cascade_delete: bool = Query(False, description="Delete child components and results"),
    component_service: ComponentService = Depends(get_component_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Delete a component with optional cascade deletion."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        success = await component_service.delete_component(
            component_id,
            cascade_delete=cascade_delete,
            deleted_by=user_id
        )
        
        if not success:
            raise HTTPException(status_code=404, detail="Component not found")
        
        # Clear related cache entries
        await cache_service.delete_pattern(f"component*{component_id}*")
        await cache_service.delete_pattern("component_groups:*")
        await cache_service.delete_pattern("classification_summary:*")
        
        logger.info(f"Deleted component {component_id} by user {user_id}")
        
        return {
            "message": "Component deleted successfully",
            "component_id": component_id,
            "cascade_delete": cascade_delete,
            "deleted_by": user_id
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting component {component_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to delete component")