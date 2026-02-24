"""
Analysis API endpoints
"""

from typing import List, Optional

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession

from models.schemas import (
    AnalysisRequest,
    AnalysisResult,
    ComparisonRequest,
    ComparisonResult,
    DataResponse,
)
from services.analysis import AnalysisService
from services.database import get_db
from utils.logger import setup_logger

logger = setup_logger(__name__)
router = APIRouter()

analysis_service = AnalysisService()


@router.post("/correlation", response_model=AnalysisResult)
async def calculate_correlation(
    request: AnalysisRequest,
    db: AsyncSession = Depends(get_db),
) -> AnalysisResult:
    """Calculate correlation between variables"""
    try:
        result = await analysis_service.calculate_correlation(db, request)
        return result
    except Exception as e:
        logger.error(f"Correlation analysis failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/regression", response_model=AnalysisResult)
async def perform_regression(
    request: AnalysisRequest,
    db: AsyncSession = Depends(get_db),
) -> AnalysisResult:
    """Perform regression analysis"""
    try:
        result = await analysis_service.perform_regression(db, request)
        return result
    except Exception as e:
        logger.error(f"Regression analysis failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/compare", response_model=ComparisonResult)
async def compare_cases(
    request: ComparisonRequest,
    db: AsyncSession = Depends(get_db),
) -> ComparisonResult:
    """Compare multiple analysis cases"""
    try:
        result = await analysis_service.compare_cases(db, request)
        return result
    except Exception as e:
        logger.error(f"Case comparison failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/sensitivity", response_model=AnalysisResult)
async def sensitivity_analysis(
    request: AnalysisRequest,
    db: AsyncSession = Depends(get_db),
) -> AnalysisResult:
    """Perform sensitivity analysis"""
    try:
        result = await analysis_service.sensitivity_analysis(db, request)
        return result
    except Exception as e:
        logger.error(f"Sensitivity analysis failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/anomalies", response_model=DataResponse)
async def detect_anomalies(
    request: AnalysisRequest,
    db: AsyncSession = Depends(get_db),
) -> DataResponse:
    """Detect anomalies in data"""
    try:
        anomalies = await analysis_service.detect_anomalies(db, request)
        return DataResponse(
            success=True,
            data=anomalies,
            message=f"Found {len(anomalies)} anomalies",
        )
    except Exception as e:
        logger.error(f"Anomaly detection failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/trends", response_model=AnalysisResult)
async def analyze_trends(
    request: AnalysisRequest,
    db: AsyncSession = Depends(get_db),
) -> AnalysisResult:
    """Analyze trends in time series data"""
    try:
        result = await analysis_service.analyze_trends(db, request)
        return result
    except Exception as e:
        logger.error(f"Trend analysis failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))