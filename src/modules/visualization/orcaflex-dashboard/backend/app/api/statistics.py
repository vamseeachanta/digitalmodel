"""
Statistical Analysis API endpoints for OrcaFlex visualization dashboard.
Provides comprehensive statistical analysis, trend detection, and performance metrics.
"""

import logging
from typing import List, Optional, Dict, Any, Tuple
from uuid import UUID
from datetime import datetime, timedelta

from fastapi import APIRouter, Depends, HTTPException, Query
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field

from app.services.statistics_service import get_statistics_service, StatisticsService
from app.services.auth_service import verify_token
from app.core.cache import get_cache_service, CacheService
from app.core.rate_limiter import rate_limit

router = APIRouter()
logger = logging.getLogger(__name__)
security = HTTPBearer()


class StatisticalQuery(BaseModel):
    """Statistical analysis query parameters."""
    analysis_ids: Optional[List[UUID]] = Field(None, description="Specific analyses to include")
    component_types: Optional[List[str]] = Field(None, description="Filter by component types")
    parameters: Optional[List[str]] = Field(None, description="Specific parameters to analyze")
    time_range: Optional[Tuple[float, float]] = Field(None, description="Time range for analysis")
    heading_range: Optional[Tuple[float, float]] = Field(None, description="Heading range for polar analysis")
    confidence_threshold: float = Field(0.95, ge=0.5, le=0.999, description="Statistical confidence level")
    include_outliers: bool = Field(True, description="Include outlier analysis")
    include_trends: bool = Field(True, description="Include trend analysis")
    include_correlations: bool = Field(False, description="Include correlation analysis")


class ComparisonQuery(BaseModel):
    """Multi-analysis comparison parameters."""
    baseline_analysis_id: UUID = Field(..., description="Baseline analysis for comparison")
    comparison_analysis_ids: List[UUID] = Field(..., min_items=1, max_items=20, description="Analyses to compare")
    comparison_parameters: Optional[List[str]] = Field(None, description="Parameters to compare")
    statistical_tests: List[str] = Field(["t_test", "ks_test"], description="Statistical tests to perform")
    significance_level: float = Field(0.05, ge=0.001, le=0.1, description="Statistical significance level")
    normalize_data: bool = Field(True, description="Normalize data for comparison")


class TrendAnalysisRequest(BaseModel):
    """Trend analysis configuration."""
    analysis_ids: List[UUID] = Field(..., min_items=2, max_items=50)
    time_series_parameter: str = Field(..., description="Parameter for time series analysis")
    trend_methods: List[str] = Field(["linear", "polynomial", "seasonal"], description="Trend detection methods")
    forecast_periods: int = Field(0, ge=0, le=100, description="Number of periods to forecast")
    seasonality_detection: bool = Field(True, description="Detect seasonal patterns")
    anomaly_detection: bool = Field(True, description="Detect anomalies in trends")


@router.get("/summary", response_model=Dict[str, Any])
@rate_limit(max_calls=100, time_window=60)
async def get_statistics_summary(
    analysis_ids: Optional[List[UUID]] = Query(None, description="Filter by analysis IDs"),
    component_types: Optional[List[str]] = Query(None, description="Filter by component types"),
    include_performance: bool = Query(True, description="Include performance metrics"),
    include_quality: bool = Query(True, description="Include data quality metrics"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Get comprehensive statistical summary across analyses and components.
    Provides high-level metrics for dashboard overview.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        analysis_ids_str = "_".join(str(id) for id in sorted(analysis_ids or []))
        component_types_str = "_".join(sorted(component_types or []))
        cache_key = f"stats_summary:{analysis_ids_str}:{component_types_str}:{include_performance}:{include_quality}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            logger.debug("Cache hit for statistics summary")
            return cached_result
        
        # Get statistics summary
        summary = await statistics_service.get_statistics_summary(
            analysis_ids=analysis_ids,
            component_types=component_types,
            include_performance=include_performance,
            include_quality=include_quality
        )
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, summary, ttl=600)
        
        logger.info(f"Retrieved statistics summary for user {user_id}")
        return summary
        
    except Exception as e:
        logger.error(f"Error fetching statistics summary: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch statistics summary")


@router.post("/analyze", response_model=Dict[str, Any])
@rate_limit(max_calls=50, time_window=300)
async def perform_statistical_analysis(
    query: StatisticalQuery,
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Perform comprehensive statistical analysis on specified data.
    Includes descriptive statistics, distribution analysis, and outlier detection.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key from query parameters
        cache_key = f"stats_analysis:{hash(str(query.dict()))}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Perform statistical analysis
        analysis_result = await statistics_service.perform_statistical_analysis(query)
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, analysis_result, ttl=900)
        
        logger.info(f"Performed statistical analysis for user {user_id}")
        return analysis_result
        
    except Exception as e:
        logger.error(f"Error performing statistical analysis: {e}")
        raise HTTPException(status_code=500, detail="Failed to perform statistical analysis")


@router.post("/compare", response_model=Dict[str, Any])
@rate_limit(max_calls=30, time_window=300)
async def compare_analyses_statistical(
    query: ComparisonQuery,
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Perform statistical comparison between multiple analyses.
    Includes hypothesis testing, effect size calculation, and significance analysis.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        all_ids = [query.baseline_analysis_id] + query.comparison_analysis_ids
        ids_str = "_".join(str(id) for id in sorted(all_ids))
        params_str = "_".join(sorted(query.comparison_parameters or []))
        tests_str = "_".join(sorted(query.statistical_tests))
        cache_key = f"stats_compare:{ids_str}:{params_str}:{tests_str}:{query.significance_level}:{query.normalize_data}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Perform comparison
        comparison_result = await statistics_service.compare_analyses_statistical(query)
        
        # Cache for 20 minutes
        await cache_service.set(cache_key, comparison_result, ttl=1200)
        
        logger.info(f"Performed statistical comparison for user {user_id}")
        return comparison_result
        
    except Exception as e:
        logger.error(f"Error performing statistical comparison: {e}")
        raise HTTPException(status_code=500, detail="Failed to perform statistical comparison")


@router.post("/trends", response_model=Dict[str, Any])
@rate_limit(max_calls=20, time_window=300)
async def analyze_trends(
    request: TrendAnalysisRequest,
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """
    Perform trend analysis across multiple analyses.
    Includes time series analysis, forecasting, and anomaly detection.
    """
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        ids_str = "_".join(str(id) for id in sorted(request.analysis_ids))
        methods_str = "_".join(sorted(request.trend_methods))
        cache_key = f"stats_trends:{ids_str}:{request.time_series_parameter}:{methods_str}:{request.forecast_periods}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Perform trend analysis
        trend_result = await statistics_service.analyze_trends(request)
        
        # Cache for 30 minutes
        await cache_service.set(cache_key, trend_result, ttl=1800)
        
        logger.info(f"Performed trend analysis for user {user_id}")
        return trend_result
        
    except Exception as e:
        logger.error(f"Error performing trend analysis: {e}")
        raise HTTPException(status_code=500, detail="Failed to perform trend analysis")


@router.get("/distributions/{analysis_id}")
@rate_limit(max_calls=100, time_window=60)
async def get_parameter_distributions(
    analysis_id: UUID,
    parameters: Optional[List[str]] = Query(None, description="Specific parameters to analyze"),
    component_types: Optional[List[str]] = Query(None, description="Filter by component types"),
    bins: int = Query(50, ge=10, le=200, description="Number of histogram bins"),
    include_kde: bool = Query(True, description="Include kernel density estimation"),
    include_normality_tests: bool = Query(True, description="Include normality tests"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get parameter distribution analysis for a specific analysis."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        params_str = "_".join(sorted(parameters or []))
        components_str = "_".join(sorted(component_types or []))
        cache_key = f"stats_distributions:{analysis_id}:{params_str}:{components_str}:{bins}:{include_kde}:{include_normality_tests}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get distributions
        distributions = await statistics_service.get_parameter_distributions(
            analysis_id,
            parameters=parameters,
            component_types=component_types,
            bins=bins,
            include_kde=include_kde,
            include_normality_tests=include_normality_tests
        )
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, distributions, ttl=900)
        
        return distributions
        
    except Exception as e:
        logger.error(f"Error fetching parameter distributions: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch parameter distributions")


@router.get("/correlations/{analysis_id}")
@rate_limit(max_calls=50, time_window=60)
async def get_correlation_analysis(
    analysis_id: UUID,
    parameters: Optional[List[str]] = Query(None, description="Parameters to correlate"),
    correlation_method: str = Query("pearson", regex="^(pearson|spearman|kendall)$"),
    min_correlation: float = Query(0.3, ge=0.0, le=1.0, description="Minimum correlation to report"),
    include_p_values: bool = Query(True, description="Include statistical significance"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get correlation analysis between parameters."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        params_str = "_".join(sorted(parameters or []))
        cache_key = f"stats_correlations:{analysis_id}:{params_str}:{correlation_method}:{min_correlation}:{include_p_values}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get correlations
        correlations = await statistics_service.get_correlation_analysis(
            analysis_id,
            parameters=parameters,
            method=correlation_method,
            min_correlation=min_correlation,
            include_p_values=include_p_values
        )
        
        # Cache for 15 minutes
        await cache_service.set(cache_key, correlations, ttl=900)
        
        return correlations
        
    except Exception as e:
        logger.error(f"Error fetching correlation analysis: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch correlation analysis")


@router.get("/outliers/{analysis_id}")
@rate_limit(max_calls=100, time_window=60)
async def detect_outliers(
    analysis_id: UUID,
    parameters: Optional[List[str]] = Query(None, description="Parameters to analyze for outliers"),
    method: str = Query("iqr", regex="^(iqr|zscore|isolation_forest|lof)$", description="Outlier detection method"),
    threshold: float = Query(2.0, ge=1.0, le=5.0, description="Outlier detection threshold"),
    include_context: bool = Query(True, description="Include contextual information"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Detect outliers in analysis data using various methods."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        params_str = "_".join(sorted(parameters or []))
        cache_key = f"stats_outliers:{analysis_id}:{params_str}:{method}:{threshold}:{include_context}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Detect outliers
        outliers = await statistics_service.detect_outliers(
            analysis_id,
            parameters=parameters,
            method=method,
            threshold=threshold,
            include_context=include_context
        )
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, outliers, ttl=600)
        
        return outliers
        
    except Exception as e:
        logger.error(f"Error detecting outliers: {e}")
        raise HTTPException(status_code=500, detail="Failed to detect outliers")


@router.get("/performance")
@rate_limit(max_calls=100, time_window=60)
async def get_performance_statistics(
    analysis_ids: Optional[List[UUID]] = Query(None, description="Filter by analysis IDs"),
    time_period: str = Query("last_30_days", regex="^(last_24_hours|last_7_days|last_30_days|last_90_days|all_time)$"),
    include_trends: bool = Query(True, description="Include performance trends"),
    include_benchmarks: bool = Query(True, description="Include benchmark comparisons"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get system performance statistics and metrics."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        ids_str = "_".join(str(id) for id in sorted(analysis_ids or []))
        cache_key = f"stats_performance:{ids_str}:{time_period}:{include_trends}:{include_benchmarks}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get performance statistics
        performance_stats = await statistics_service.get_performance_statistics(
            analysis_ids=analysis_ids,
            time_period=time_period,
            include_trends=include_trends,
            include_benchmarks=include_benchmarks
        )
        
        # Cache for 5 minutes (shorter for performance data)
        await cache_service.set(cache_key, performance_stats, ttl=300)
        
        return performance_stats
        
    except Exception as e:
        logger.error(f"Error fetching performance statistics: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch performance statistics")


@router.get("/quality")
@rate_limit(max_calls=100, time_window=60)
async def get_data_quality_statistics(
    analysis_ids: Optional[List[UUID]] = Query(None, description="Filter by analysis IDs"),
    component_types: Optional[List[str]] = Query(None, description="Filter by component types"),
    quality_threshold: float = Query(0.7, ge=0.0, le=1.0, description="Quality score threshold"),
    include_recommendations: bool = Query(True, description="Include quality improvement recommendations"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get data quality statistics and analysis."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        ids_str = "_".join(str(id) for id in sorted(analysis_ids or []))
        components_str = "_".join(sorted(component_types or []))
        cache_key = f"stats_quality:{ids_str}:{components_str}:{quality_threshold}:{include_recommendations}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get quality statistics
        quality_stats = await statistics_service.get_data_quality_statistics(
            analysis_ids=analysis_ids,
            component_types=component_types,
            quality_threshold=quality_threshold,
            include_recommendations=include_recommendations
        )
        
        # Cache for 10 minutes
        await cache_service.set(cache_key, quality_stats, ttl=600)
        
        return quality_stats
        
    except Exception as e:
        logger.error(f"Error fetching data quality statistics: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch data quality statistics")


@router.get("/benchmarks")
@rate_limit(max_calls=50, time_window=60)
async def get_benchmark_statistics(
    benchmark_type: str = Query("processing_time", regex="^(processing_time|data_quality|file_size|component_count)$"),
    percentiles: List[float] = Query([50, 75, 90, 95, 99], description="Percentiles to calculate"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    cache_service: CacheService = Depends(get_cache_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Get benchmark statistics for performance comparison."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Build cache key
        percentiles_str = "_".join(str(p) for p in sorted(percentiles))
        cache_key = f"stats_benchmarks:{benchmark_type}:{percentiles_str}"
        
        # Try cache first
        cached_result = await cache_service.get(cache_key)
        if cached_result:
            return cached_result
        
        # Get benchmark statistics
        benchmarks = await statistics_service.get_benchmark_statistics(
            benchmark_type=benchmark_type,
            percentiles=percentiles
        )
        
        # Cache for 1 hour (benchmarks change slowly)
        await cache_service.set(cache_key, benchmarks, ttl=3600)
        
        return benchmarks
        
    except Exception as e:
        logger.error(f"Error fetching benchmark statistics: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch benchmark statistics")


@router.post("/report")
@rate_limit(max_calls=10, time_window=300)
async def generate_statistical_report(
    analysis_ids: List[UUID] = Field(..., min_items=1, max_items=20),
    report_sections: List[str] = Field(["summary", "distributions", "correlations", "outliers", "quality"], description="Report sections to include"),
    format: str = Query("json", regex="^(json|html|pdf)$", description="Report format"),
    include_charts: bool = Query(True, description="Include statistical charts"),
    statistics_service: StatisticsService = Depends(get_statistics_service),
    credentials: HTTPAuthorizationCredentials = Depends(security)
):
    """Generate comprehensive statistical report for multiple analyses."""
    
    try:
        # Verify authentication
        user_id = await verify_token(credentials.credentials)
        
        # Generate report
        report = await statistics_service.generate_statistical_report(
            analysis_ids=analysis_ids,
            sections=report_sections,
            format=format,
            include_charts=include_charts,
            generated_by=user_id
        )
        
        logger.info(f"Generated statistical report for user {user_id}: {len(analysis_ids)} analyses")
        
        return report
        
    except Exception as e:
        logger.error(f"Error generating statistical report: {e}")
        raise HTTPException(status_code=500, detail="Failed to generate statistical report")