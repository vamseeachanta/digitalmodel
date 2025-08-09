"""
Results API endpoints for OrcaFlex simulation data.
"""

import logging
from typing import List, Optional
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, Query
from fastapi.responses import StreamingResponse

from app.models.results import (
    SimulationResult,
    ResultsQuery,
    ResultsMetadata,
    TimeSeriesData,
    StatisticalSummary
)
from app.services.results_service import get_results_service, ResultsService

router = APIRouter()
logger = logging.getLogger(__name__)


@router.get("/", response_model=List[SimulationResult])
async def get_results(
    analysis_id: Optional[UUID] = Query(None, description="Filter by analysis ID"),
    limit: int = Query(50, ge=1, le=1000, description="Maximum number of results"),
    offset: int = Query(0, ge=0, description="Offset for pagination"),
    results_service: ResultsService = Depends(get_results_service)
):
    """Get list of simulation results with optional filtering."""
    
    try:
        results = await results_service.get_results(
            analysis_id=analysis_id,
            limit=limit,
            offset=offset
        )
        return results
        
    except Exception as e:
        logger.error(f"Error fetching results: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch results")


@router.get("/{result_id}", response_model=SimulationResult)
async def get_result(
    result_id: UUID,
    results_service: ResultsService = Depends(get_results_service)
):
    """Get specific simulation result by ID."""
    
    try:
        result = await results_service.get_result(result_id)
        if not result:
            raise HTTPException(status_code=404, detail="Result not found")
        return result
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch result")


@router.get("/{result_id}/metadata", response_model=ResultsMetadata)
async def get_result_metadata(
    result_id: UUID,
    results_service: ResultsService = Depends(get_results_service)
):
    """Get metadata for a specific result."""
    
    try:
        metadata = await results_service.get_result_metadata(result_id)
        if not metadata:
            raise HTTPException(status_code=404, detail="Result not found")
        return metadata
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching metadata for result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch metadata")


@router.post("/{result_id}/query", response_model=SimulationResult)
async def query_result_data(
    result_id: UUID,
    query: ResultsQuery,
    results_service: ResultsService = Depends(get_results_service)
):
    """Query specific data from a simulation result."""
    
    try:
        result = await results_service.query_result_data(result_id, query)
        if not result:
            raise HTTPException(status_code=404, detail="Result not found")
        return result
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error querying result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to query result data")


@router.get("/{result_id}/timeseries", response_model=List[TimeSeriesData])
async def get_timeseries_data(
    result_id: UUID,
    variable_names: Optional[List[str]] = Query(None, description="Variable names to fetch"),
    object_names: Optional[List[str]] = Query(None, description="Object names to filter"),
    time_start: Optional[float] = Query(None, description="Start time for data range"),
    time_end: Optional[float] = Query(None, description="End time for data range"),
    results_service: ResultsService = Depends(get_results_service)
):
    """Get time series data for specific variables."""
    
    try:
        time_range = None
        if time_start is not None and time_end is not None:
            time_range = (time_start, time_end)
            
        timeseries = await results_service.get_timeseries_data(
            result_id=result_id,
            variable_names=variable_names,
            object_names=object_names,
            time_range=time_range
        )
        
        return timeseries
        
    except Exception as e:
        logger.error(f"Error fetching timeseries for result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch timeseries data")


@router.get("/{result_id}/statistics", response_model=List[StatisticalSummary])
async def get_statistics(
    result_id: UUID,
    variable_names: Optional[List[str]] = Query(None, description="Variable names to fetch"),
    results_service: ResultsService = Depends(get_results_service)
):
    """Get statistical summaries for result variables."""
    
    try:
        statistics = await results_service.get_statistics(
            result_id=result_id,
            variable_names=variable_names
        )
        return statistics
        
    except Exception as e:
        logger.error(f"Error fetching statistics for result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch statistics")


@router.get("/{result_id}/export")
async def export_result(
    result_id: UUID,
    format: str = Query("csv", regex="^(csv|json|excel)$", description="Export format"),
    variables: Optional[List[str]] = Query(None, description="Variables to include"),
    results_service: ResultsService = Depends(get_results_service)
):
    """Export result data in specified format."""
    
    try:
        export_data = await results_service.export_result(
            result_id=result_id,
            format=format,
            variables=variables
        )
        
        if not export_data:
            raise HTTPException(status_code=404, detail="Result not found")
        
        filename = f"result_{result_id}.{format}"
        media_type = {
            "csv": "text/csv",
            "json": "application/json",
            "excel": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        }[format]
        
        return StreamingResponse(
            export_data,
            media_type=media_type,
            headers={"Content-Disposition": f"attachment; filename={filename}"}
        )
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error exporting result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to export result")


@router.delete("/{result_id}")
async def delete_result(
    result_id: UUID,
    results_service: ResultsService = Depends(get_results_service)
):
    """Delete a simulation result."""
    
    try:
        success = await results_service.delete_result(result_id)
        if not success:
            raise HTTPException(status_code=404, detail="Result not found")
        
        return {"message": "Result deleted successfully"}
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting result {result_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to delete result")