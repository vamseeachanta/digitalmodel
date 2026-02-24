"""
Data access API endpoints
"""

from typing import List, Optional

from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy.ext.asyncio import AsyncSession

from models.schemas import (
    DataPoint,
    DataQuery,
    DataResponse,
    PolarData,
    TimeTraceData,
)
from services.cache import get_cache, set_cache
from services.database import get_db
from services.data_processor import DataProcessor
from utils.logger import setup_logger

logger = setup_logger(__name__)
router = APIRouter()

data_processor = DataProcessor()


@router.get("/cases", response_model=List[str])
async def get_analysis_cases(
    db: AsyncSession = Depends(get_db),
) -> List[str]:
    """Get list of available analysis cases"""
    cache_key = "analysis_cases"
    
    # Check cache
    cached = await get_cache(cache_key)
    if cached:
        return cached
    
    try:
        cases = await data_processor.get_analysis_cases(db)
        await set_cache(cache_key, cases, ttl=300)
        return cases
    except Exception as e:
        logger.error(f"Failed to get analysis cases: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/components", response_model=List[str])
async def get_components(
    case: Optional[str] = Query(None),
    db: AsyncSession = Depends(get_db),
) -> List[str]:
    """Get list of components for a case"""
    cache_key = f"components:{case or 'all'}"
    
    # Check cache
    cached = await get_cache(cache_key)
    if cached:
        return cached
    
    try:
        components = await data_processor.get_components(db, case)
        await set_cache(cache_key, components, ttl=300)
        return components
    except Exception as e:
        logger.error(f"Failed to get components: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/polar", response_model=PolarData)
async def get_polar_data(
    query: DataQuery,
    db: AsyncSession = Depends(get_db),
) -> PolarData:
    """Get polar plot data for specified parameters"""
    cache_key = f"polar:{query.case}:{query.component}:{query.loading_condition}"
    
    # Check cache
    cached = await get_cache(cache_key)
    if cached:
        return PolarData(**cached)
    
    try:
        data = await data_processor.get_polar_data(db, query)
        await set_cache(cache_key, data.dict(), ttl=600)
        return data
    except Exception as e:
        logger.error(f"Failed to get polar data: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/time-trace", response_model=TimeTraceData)
async def get_time_trace(
    query: DataQuery,
    db: AsyncSession = Depends(get_db),
) -> TimeTraceData:
    """Get time trace data for specified parameters"""
    cache_key = f"trace:{query.case}:{query.component}:{query.heading}"
    
    # Check cache
    cached = await get_cache(cache_key)
    if cached:
        return TimeTraceData(**cached)
    
    try:
        data = await data_processor.get_time_trace(db, query)
        await set_cache(cache_key, data.dict(), ttl=600)
        return data
    except Exception as e:
        logger.error(f"Failed to get time trace: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/statistics", response_model=DataResponse)
async def get_statistics(
    query: DataQuery,
    db: AsyncSession = Depends(get_db),
) -> DataResponse:
    """Get statistical analysis for specified data"""
    try:
        stats = await data_processor.calculate_statistics(db, query)
        return DataResponse(
            success=True,
            data=stats,
            message="Statistics calculated successfully",
        )
    except Exception as e:
        logger.error(f"Failed to calculate statistics: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.delete("/cache")
async def clear_cache(
    pattern: str = Query("*", description="Cache key pattern to clear"),
) -> DataResponse:
    """Clear cache entries matching pattern"""
    try:
        from services.cache import delete_cache
        
        count = await delete_cache(pattern)
        return DataResponse(
            success=True,
            data={"deleted": count},
            message=f"Cleared {count} cache entries",
        )
    except Exception as e:
        logger.error(f"Failed to clear cache: {e}")
        raise HTTPException(status_code=500, detail=str(e))