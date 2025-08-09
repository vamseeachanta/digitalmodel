"""
Results service for OrcaFlex simulation data processing.
"""

import csv
import io
import json
import logging
import numpy as np
import pandas as pd
from pathlib import Path
from typing import List, Optional, Tuple, AsyncGenerator
from uuid import UUID

from app.config import get_settings
from app.models.results import (
    SimulationResult,
    ResultsQuery,
    ResultsMetadata,
    TimeSeriesData,
    TimeSeriesPoint,
    StatisticalSummary,
    FrequencyDomainData
)

logger = logging.getLogger(__name__)


class ResultsService:
    """Service for managing simulation results."""
    
    def __init__(self):
        self.settings = get_settings()
        self._results = {}  # In-memory storage for demo
        self._generate_mock_data()
    
    def _generate_mock_data(self):
        """Generate mock simulation results for demonstration."""
        
        # Create sample time series data
        time_points = np.linspace(0, 100, 1000)
        
        # Mock tension data with some noise
        base_tension = 1200
        tension_variation = 50 * np.sin(0.1 * time_points) + 20 * np.random.normal(0, 1, len(time_points))
        tension_data = [
            TimeSeriesPoint(time=t, value=base_tension + v)
            for t, v in zip(time_points, tension_variation)
        ]
        
        # Mock displacement data
        displacement_data = [
            TimeSeriesPoint(time=t, value=10 * np.sin(0.05 * t) + 2 * np.random.normal())
            for t in time_points
        ]
        
        # Mock analysis IDs for testing
        mock_analysis_id = UUID("123e4567-e89b-12d3-a456-426614174000")
        mock_result_id = UUID("987fcdeb-51a2-43d1-b234-567890123456")
        
        # Create mock result
        mock_result = SimulationResult(
            analysis_id=mock_analysis_id,
            result_id=mock_result_id,
            name="Dynamic Analysis Results",
            description="Mock results for demonstration",
            time_series=[
                TimeSeriesData(
                    variable_name="Top Tension",
                    unit="kN",
                    object_name="Mooring Line 1",
                    data_type="tension",
                    data=tension_data,
                    sample_rate=10.0,
                    duration=100.0
                ),
                TimeSeriesData(
                    variable_name="Platform Displacement",
                    unit="m",
                    object_name="Platform",
                    data_type="displacement",
                    data=displacement_data,
                    sample_rate=10.0,
                    duration=100.0
                )
            ],
            statistics=[
                StatisticalSummary(
                    variable_name="Top Tension",
                    count=len(tension_data),
                    mean=float(np.mean([p.value for p in tension_data])),
                    std=float(np.std([p.value for p in tension_data])),
                    min=float(np.min([p.value for p in tension_data])),
                    max=float(np.max([p.value for p in tension_data])),
                    percentile_25=float(np.percentile([p.value for p in tension_data], 25)),
                    percentile_50=float(np.percentile([p.value for p in tension_data], 50)),
                    percentile_75=float(np.percentile([p.value for p in tension_data], 75)),
                    percentile_95=float(np.percentile([p.value for p in tension_data], 95)),
                    percentile_99=float(np.percentile([p.value for p in tension_data], 99))
                )
            ],
            simulation_time=100.0,
            time_step=0.1,
            solver_info={
                "solver": "OrcaFlex Dynamic",
                "version": "11.3a",
                "convergence_tolerance": 1e-6
            }
        )
        
        self._results[mock_result_id] = mock_result
    
    async def get_results(
        self,
        analysis_id: Optional[UUID] = None,
        limit: int = 50,
        offset: int = 0
    ) -> List[SimulationResult]:
        """Get list of simulation results."""
        
        results = list(self._results.values())
        
        # Filter by analysis ID if provided
        if analysis_id:
            results = [r for r in results if r.analysis_id == analysis_id]
        
        # Sort by creation date
        results.sort(key=lambda x: x.created_at, reverse=True)
        
        # Apply pagination
        return results[offset:offset + limit]
    
    async def get_result(self, result_id: UUID) -> Optional[SimulationResult]:
        """Get specific result by ID."""
        return self._results.get(result_id)
    
    async def get_result_metadata(self, result_id: UUID) -> Optional[ResultsMetadata]:
        """Get metadata for a result."""
        
        result = self._results.get(result_id)
        if not result:
            return None
        
        # Extract metadata from result
        variables = [ts.variable_name for ts in result.time_series]
        objects = list(set(ts.object_name for ts in result.time_series if ts.object_name))
        data_types = list(set(ts.data_type for ts in result.time_series))
        
        # Calculate time range
        all_times = []
        for ts in result.time_series:
            if ts.data:
                all_times.extend([p.time for p in ts.data])
        
        time_range = (min(all_times), max(all_times)) if all_times else (0.0, 0.0)
        
        return ResultsMetadata(
            analysis_id=result.analysis_id,
            total_variables=len(variables),
            available_variables=variables,
            available_objects=objects,
            data_types=data_types,
            time_range=time_range,
            sample_rate=result.time_series[0].sample_rate if result.time_series else 1.0,
            file_size=1024  # Mock file size
        )
    
    async def query_result_data(
        self, 
        result_id: UUID, 
        query: ResultsQuery
    ) -> Optional[SimulationResult]:
        """Query specific data from a result."""
        
        result = self._results.get(result_id)
        if not result:
            return None
        
        # Filter time series based on query
        filtered_ts = result.time_series
        
        if query.variable_names:
            filtered_ts = [ts for ts in filtered_ts if ts.variable_name in query.variable_names]
        
        if query.object_names:
            filtered_ts = [ts for ts in filtered_ts if ts.object_name in query.object_names]
        
        if query.data_types:
            filtered_ts = [ts for ts in filtered_ts if ts.data_type in query.data_types]
        
        # Apply time range filter
        if query.time_range:
            start_time, end_time = query.time_range
            for ts in filtered_ts:
                ts.data = [
                    p for p in ts.data 
                    if start_time <= p.time <= end_time
                ]
        
        # Create filtered result
        filtered_result = SimulationResult(
            analysis_id=result.analysis_id,
            result_id=result.result_id,
            name=result.name,
            description=result.description,
            time_series=filtered_ts,
            statistics=result.statistics if query.include_statistics else [],
            frequency_domain=result.frequency_domain if query.include_frequency_domain else [],
            simulation_time=result.simulation_time,
            time_step=result.time_step,
            solver_info=result.solver_info,
            created_at=result.created_at
        )
        
        return filtered_result
    
    async def get_timeseries_data(
        self,
        result_id: UUID,
        variable_names: Optional[List[str]] = None,
        object_names: Optional[List[str]] = None,
        time_range: Optional[Tuple[float, float]] = None
    ) -> List[TimeSeriesData]:
        """Get time series data for specific variables."""
        
        result = self._results.get(result_id)
        if not result:
            return []
        
        # Filter time series
        timeseries = result.time_series
        
        if variable_names:
            timeseries = [ts for ts in timeseries if ts.variable_name in variable_names]
        
        if object_names:
            timeseries = [ts for ts in timeseries if ts.object_name in object_names]
        
        # Apply time range filter
        if time_range:
            start_time, end_time = time_range
            filtered_timeseries = []
            for ts in timeseries:
                filtered_data = [
                    p for p in ts.data 
                    if start_time <= p.time <= end_time
                ]
                if filtered_data:
                    filtered_ts = TimeSeriesData(
                        variable_name=ts.variable_name,
                        unit=ts.unit,
                        object_name=ts.object_name,
                        data_type=ts.data_type,
                        data=filtered_data,
                        sample_rate=ts.sample_rate,
                        duration=ts.duration
                    )
                    filtered_timeseries.append(filtered_ts)
            timeseries = filtered_timeseries
        
        return timeseries
    
    async def get_statistics(
        self,
        result_id: UUID,
        variable_names: Optional[List[str]] = None
    ) -> List[StatisticalSummary]:
        """Get statistical summaries."""
        
        result = self._results.get(result_id)
        if not result:
            return []
        
        statistics = result.statistics
        
        if variable_names:
            statistics = [stat for stat in statistics if stat.variable_name in variable_names]
        
        return statistics
    
    async def export_result(
        self,
        result_id: UUID,
        format: str,
        variables: Optional[List[str]] = None
    ) -> Optional[AsyncGenerator[bytes, None]]:
        """Export result data in specified format."""
        
        result = self._results.get(result_id)
        if not result:
            return None
        
        # Filter variables if specified
        timeseries = result.time_series
        if variables:
            timeseries = [ts for ts in timeseries if ts.variable_name in variables]
        
        if format == "csv":
            return self._export_csv(timeseries)
        elif format == "json":
            return self._export_json(result, timeseries)
        elif format == "excel":
            return self._export_excel(timeseries)
        
        return None
    
    async def _export_csv(self, timeseries: List[TimeSeriesData]) -> AsyncGenerator[bytes, None]:
        """Export time series data as CSV."""
        
        output = io.StringIO()
        
        if not timeseries:
            yield b""
            return
        
        # Prepare data for CSV
        all_times = set()
        for ts in timeseries:
            all_times.update([p.time for p in ts.data])
        
        sorted_times = sorted(all_times)
        
        # Create header
        header = ["Time"]
        for ts in timeseries:
            header.append(f"{ts.variable_name} ({ts.unit})")
        
        writer = csv.writer(output)
        writer.writerow(header)
        
        # Write data rows
        for time_val in sorted_times:
            row = [time_val]
            for ts in timeseries:
                # Find value for this time
                value = None
                for point in ts.data:
                    if abs(point.time - time_val) < 1e-6:  # Floating point comparison
                        value = point.value
                        break
                row.append(value if value is not None else "")
            
            writer.writerow(row)
        
        csv_data = output.getvalue().encode('utf-8')
        yield csv_data
    
    async def _export_json(
        self, 
        result: SimulationResult, 
        timeseries: List[TimeSeriesData]
    ) -> AsyncGenerator[bytes, None]:
        """Export result as JSON."""
        
        export_data = {
            "result_id": str(result.result_id),
            "analysis_id": str(result.analysis_id),
            "name": result.name,
            "description": result.description,
            "time_series": [ts.dict() for ts in timeseries],
            "statistics": [stat.dict() for stat in result.statistics],
            "metadata": {
                "simulation_time": result.simulation_time,
                "time_step": result.time_step,
                "solver_info": result.solver_info,
                "created_at": result.created_at.isoformat()
            }
        }
        
        json_data = json.dumps(export_data, indent=2).encode('utf-8')
        yield json_data
    
    async def _export_excel(self, timeseries: List[TimeSeriesData]) -> AsyncGenerator[bytes, None]:
        """Export time series data as Excel."""
        
        # For demo purposes, export as CSV with Excel-like formatting
        # In production, you'd use xlsxwriter or openpyxl
        async for chunk in self._export_csv(timeseries):
            yield chunk
    
    async def delete_result(self, result_id: UUID) -> bool:
        """Delete a simulation result."""
        
        if result_id not in self._results:
            return False
        
        del self._results[result_id]
        logger.info(f"Deleted result: {result_id}")
        return True


# Dependency injection
_results_service = None

def get_results_service() -> ResultsService:
    """Get results service instance."""
    global _results_service
    if _results_service is None:
        _results_service = ResultsService()
    return _results_service