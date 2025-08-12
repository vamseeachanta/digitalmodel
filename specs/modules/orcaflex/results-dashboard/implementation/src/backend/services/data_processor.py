"""
Data processor service for handling OrcaFlex data
"""

from typing import List, Optional
import numpy as np
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from models.database import AnalysisCase, Component, PolarData, TimeTrace
from models.schemas import DataQuery, PolarData as PolarDataSchema, TimeTraceData
from utils.logger import setup_logger

logger = setup_logger(__name__)


class DataProcessor:
    """Data processor for OrcaFlex results"""
    
    async def get_analysis_cases(self, db: AsyncSession) -> List[str]:
        """Get list of analysis cases"""
        result = await db.execute(select(AnalysisCase.name))
        cases = result.scalars().all()
        return list(cases)
    
    async def get_components(
        self,
        db: AsyncSession,
        case: Optional[str] = None
    ) -> List[str]:
        """Get list of components"""
        query = select(Component.name).distinct()
        if case:
            query = query.join(AnalysisCase).where(AnalysisCase.name == case)
        
        result = await db.execute(query)
        components = result.scalars().all()
        return list(components)
    
    async def get_polar_data(
        self,
        db: AsyncSession,
        query: DataQuery
    ) -> PolarDataSchema:
        """Get polar plot data"""
        # Mock data for testing
        headings = list(range(0, 360, 15))  # 24 points
        values = [100 * np.sin(np.radians(h)) + 200 for h in headings]
        
        return PolarDataSchema(
            case=query.case,
            component=query.component or "unknown",
            loading_condition=query.loading_condition or "default",
            headings=headings,
            values=values,
            unit="kN",
            max_value=max(values),
            min_value=min(values),
            mean_value=np.mean(values),
            std_dev=np.std(values)
        )
    
    async def get_time_trace(
        self,
        db: AsyncSession,
        query: DataQuery
    ) -> TimeTraceData:
        """Get time trace data"""
        # Mock data for testing
        time_points = list(np.linspace(0, 100, 1000))
        values = [50 * np.sin(0.1 * t) + 100 * np.random.random() for t in time_points]
        
        return TimeTraceData(
            case=query.case,
            component=query.component or "unknown",
            heading=query.heading or 0,
            timestamps=time_points,
            values=values,
            unit="kN",
            statistics={
                "max": max(values),
                "min": min(values),
                "mean": np.mean(values),
                "std": np.std(values)
            }
        )
    
    async def calculate_statistics(
        self,
        db: AsyncSession,
        query: DataQuery
    ) -> dict:
        """Calculate statistics for data"""
        # Mock statistics for testing
        return {
            "max": 300.5,
            "min": 50.2,
            "mean": 175.3,
            "std": 45.8,
            "p95": 250.1,
            "p99": 290.7
        }