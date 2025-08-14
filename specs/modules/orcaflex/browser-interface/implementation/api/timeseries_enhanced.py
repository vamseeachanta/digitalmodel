"""
Enhanced Time Series API Endpoints for OrcaFlex Browser
Provides comprehensive time trace analysis and visualization data
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field
from typing import List, Dict, Any, Optional
from pathlib import Path
import pandas as pd
import numpy as np
from datetime import datetime
import json

# Create router for time series endpoints
router = APIRouter(prefix="/api/v2/timeseries", tags=["Time Series Enhanced"])

# Pydantic models
class TimePoint(BaseModel):
    time: float
    value: float

class StrutTimeTrace(BaseModel):
    strut_id: str
    max_value: float
    min_value: float
    mean_value: float
    std_dev: float
    max_time: float
    min_time: float
    data: List[TimePoint]

class MultiStrutResponse(BaseModel):
    fe_file: str
    time_range: Dict[str, float]
    sample_rate: float
    struts: List[StrutTimeTrace]
    statistics: Dict[str, Any]

class JacketDataPoint(BaseModel):
    time: float
    fx: float
    fy: float
    fz: float
    mx: float
    my: float
    mz: float

class JacketTimeTrace(BaseModel):
    fe_file: str
    location: str
    time_range: Dict[str, float]
    forces: Dict[str, Dict[str, float]]  # fx, fy, fz statistics
    moments: Dict[str, Dict[str, float]]  # mx, my, mz statistics
    data: List[JacketDataPoint]

class ComparativeTrace(BaseModel):
    parameter: str
    fe_files: List[str]
    time_range: Dict[str, float]
    traces: List[Dict[str, Any]]
    statistics: Dict[str, Any]

class TimeSeriesStatistics(BaseModel):
    parameter: str
    peak_value: float
    peak_time: float
    mean: float
    std_dev: float
    rms: float
    range: float
    percentiles: Dict[str, float]
    zero_crossings: int
    dominant_frequency: Optional[float]
    fatigue_damage: Optional[float]

class ExportFormat(BaseModel):
    format: str = Field(default="csv", pattern="^(csv|json|excel|matlab)$")
    include_statistics: bool = True
    downsample_factor: Optional[int] = None
    time_range: Optional[Dict[str, float]] = None


@router.get("/struts/{fe_file}/all", response_model=MultiStrutResponse)
async def get_all_strut_traces(
    fe_file: str,
    start_time: Optional[float] = Query(0, description="Start time in seconds"),
    end_time: Optional[float] = Query(None, description="End time in seconds"),
    downsample: Optional[int] = Query(100, description="Downsample to N points per trace")
):
    """Get time traces for all struts from an FE file"""
    
    try:
        # Import necessary modules
        import sys
        import os
        sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
        from time_trace_viewer import TimeTraceViewer
        
        # Get base path from config
        base_path = Path("D:/1522/ctr7/orcaflex/rev_a08/output/csv").parent
        
        # Create viewer
        viewer = TimeTraceViewer(fe_file, str(base_path))
        
        if viewer.strut_timeseries is None:
            raise HTTPException(status_code=404, detail="No strut time series data available")
        
        # Get time vector
        time_vector = viewer.time_vector
        if end_time is None:
            end_time = time_vector[-1]
        
        # Apply time filter
        mask = (time_vector >= start_time) & (time_vector <= end_time)
        filtered_time = time_vector[mask]
        
        # Process all strut columns
        struts = []
        all_max_values = []
        all_min_values = []
        
        for col in viewer.strut_timeseries.columns:
            if "Strut" in col and "Tension" in col:
                values = viewer.strut_timeseries[col].values[mask]
                
                # Downsample if needed
                if len(filtered_time) > downsample:
                    indices = np.linspace(0, len(filtered_time)-1, downsample, dtype=int)
                    sampled_time = filtered_time[indices]
                    sampled_values = values[indices]
                else:
                    sampled_time = filtered_time
                    sampled_values = values
                
                # Calculate statistics
                max_idx = np.argmax(values)
                min_idx = np.argmin(values)
                
                all_max_values.append(float(values.max()))
                all_min_values.append(float(values.min()))
                
                struts.append(StrutTimeTrace(
                    strut_id=col,
                    max_value=float(values.max()),
                    min_value=float(values.min()),
                    mean_value=float(values.mean()),
                    std_dev=float(values.std()),
                    max_time=float(filtered_time[max_idx]),
                    min_time=float(filtered_time[min_idx]),
                    data=[TimePoint(time=t, value=v) 
                          for t, v in zip(sampled_time.tolist(), sampled_values.tolist())]
                ))
        
        # Overall statistics
        statistics = {
            "total_struts": len(struts),
            "global_max": max(all_max_values) if all_max_values else 0,
            "global_min": min(all_min_values) if all_min_values else 0,
            "critical_strut": struts[np.argmax(all_max_values)].strut_id if struts else None,
            "time_points": len(sampled_time),
            "original_points": len(filtered_time)
        }
        
        return MultiStrutResponse(
            fe_file=fe_file,
            time_range={"start": float(filtered_time[0]), "end": float(filtered_time[-1])},
            sample_rate=float(time_vector[1] - time_vector[0]) if len(time_vector) > 1 else 0.1,
            struts=struts,
            statistics=statistics
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/jacket/{fe_file}", response_model=JacketTimeTrace)
async def get_jacket_traces(
    fe_file: str,
    location: str = Query("base", description="Jacket location (base, top, mid)"),
    start_time: Optional[float] = Query(0, description="Start time in seconds"),
    end_time: Optional[float] = Query(None, description="End time in seconds"),
    downsample: Optional[int] = Query(100, description="Downsample to N points")
):
    """Get jacket forces and moments time traces"""
    
    try:
        # Generate sample jacket data for demonstration
        # In production, this would read actual jacket CSV files
        
        import numpy as np
        
        # Generate time vector
        time = np.linspace(start_time or 0, end_time or 3600, 1000)
        
        # Generate sample forces and moments with realistic patterns
        np.random.seed(42)
        base_freq = 0.1  # Hz
        
        fx = 5000 * np.sin(2 * np.pi * base_freq * time) + 1000 * np.random.randn(len(time))
        fy = 3000 * np.sin(2 * np.pi * base_freq * time + np.pi/4) + 800 * np.random.randn(len(time))
        fz = 10000 + 2000 * np.sin(2 * np.pi * base_freq * time + np.pi/2) + 500 * np.random.randn(len(time))
        
        mx = 50000 * np.sin(2 * np.pi * base_freq * time + np.pi/3) + 10000 * np.random.randn(len(time))
        my = 40000 * np.sin(2 * np.pi * base_freq * time + np.pi/6) + 8000 * np.random.randn(len(time))
        mz = 20000 * np.sin(2 * np.pi * base_freq * time) + 5000 * np.random.randn(len(time))
        
        # Downsample if needed
        if downsample and len(time) > downsample:
            indices = np.linspace(0, len(time)-1, downsample, dtype=int)
            time = time[indices]
            fx = fx[indices]
            fy = fy[indices]
            fz = fz[indices]
            mx = mx[indices]
            my = my[indices]
            mz = mz[indices]
        
        # Calculate statistics
        forces_stats = {
            "fx": {
                "max": float(fx.max()),
                "min": float(fx.min()),
                "mean": float(fx.mean()),
                "std": float(fx.std()),
                "rms": float(np.sqrt(np.mean(fx**2)))
            },
            "fy": {
                "max": float(fy.max()),
                "min": float(fy.min()),
                "mean": float(fy.mean()),
                "std": float(fy.std()),
                "rms": float(np.sqrt(np.mean(fy**2)))
            },
            "fz": {
                "max": float(fz.max()),
                "min": float(fz.min()),
                "mean": float(fz.mean()),
                "std": float(fz.std()),
                "rms": float(np.sqrt(np.mean(fz**2)))
            }
        }
        
        moments_stats = {
            "mx": {
                "max": float(mx.max()),
                "min": float(mx.min()),
                "mean": float(mx.mean()),
                "std": float(mx.std()),
                "rms": float(np.sqrt(np.mean(mx**2)))
            },
            "my": {
                "max": float(my.max()),
                "min": float(my.min()),
                "mean": float(my.mean()),
                "std": float(my.std()),
                "rms": float(np.sqrt(np.mean(my**2)))
            },
            "mz": {
                "max": float(mz.max()),
                "min": float(mz.min()),
                "mean": float(mz.mean()),
                "std": float(mz.std()),
                "rms": float(np.sqrt(np.mean(mz**2)))
            }
        }
        
        # Create data points
        data_points = [
            JacketDataPoint(
                time=float(t),
                fx=float(fx[i]),
                fy=float(fy[i]),
                fz=float(fz[i]),
                mx=float(mx[i]),
                my=float(my[i]),
                mz=float(mz[i])
            )
            for i, t in enumerate(time)
        ]
        
        return JacketTimeTrace(
            fe_file=fe_file,
            location=location,
            time_range={"start": float(time[0]), "end": float(time[-1])},
            forces=forces_stats,
            moments=moments_stats,
            data=data_points
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/compare", response_model=ComparativeTrace)
async def compare_time_traces(
    fe_files: List[str],
    parameter: str = "strut7_tension",
    start_time: Optional[float] = 0,
    end_time: Optional[float] = None,
    normalize: bool = False
):
    """Compare time traces from multiple FE files"""
    
    try:
        traces = []
        all_max_values = []
        all_min_values = []
        
        for fe_file in fe_files:
            # Generate sample data for each file
            # In production, this would read actual data
            
            time = np.linspace(start_time, end_time or 3600, 500)
            
            # Create different patterns for each file
            seed_offset = hash(fe_file) % 100
            np.random.seed(42 + seed_offset)
            
            amplitude = 5000 + seed_offset * 50
            frequency = 0.1 + seed_offset * 0.001
            phase = seed_offset * np.pi / 50
            
            values = amplitude * np.sin(2 * np.pi * frequency * time + phase) + \
                    1000 * np.random.randn(len(time))
            
            if normalize:
                values = (values - values.min()) / (values.max() - values.min())
            
            all_max_values.append(float(values.max()))
            all_min_values.append(float(values.min()))
            
            traces.append({
                "fe_file": fe_file,
                "max_value": float(values.max()),
                "min_value": float(values.min()),
                "mean_value": float(values.mean()),
                "data": [{"time": float(t), "value": float(v)} 
                        for t, v in zip(time[::10], values[::10])]  # Downsample for response
            })
        
        statistics = {
            "parameter": parameter,
            "num_files": len(fe_files),
            "global_max": max(all_max_values),
            "global_min": min(all_min_values),
            "normalized": normalize
        }
        
        return ComparativeTrace(
            parameter=parameter,
            fe_files=fe_files,
            time_range={"start": float(time[0]), "end": float(time[-1])},
            traces=traces,
            statistics=statistics
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/statistics/{fe_file}/{parameter}", response_model=TimeSeriesStatistics)
async def get_time_series_statistics(
    fe_file: str,
    parameter: str,
    start_time: Optional[float] = 0,
    end_time: Optional[float] = None
):
    """Get detailed statistical analysis for a time series parameter"""
    
    try:
        # Generate sample data
        time = np.linspace(start_time, end_time or 3600, 10000)
        values = 5000 * np.sin(2 * np.pi * 0.1 * time) + 1000 * np.random.randn(len(time))
        
        # Calculate statistics
        max_idx = np.argmax(values)
        
        # Calculate zero crossings
        zero_crossings = np.where(np.diff(np.sign(values - values.mean())))[0].shape[0]
        
        # Calculate dominant frequency using FFT
        from scipy import signal
        frequencies, power = signal.periodogram(values, fs=1/(time[1]-time[0]))
        dominant_freq_idx = np.argmax(power[1:]) + 1  # Skip DC component
        dominant_frequency = float(frequencies[dominant_freq_idx])
        
        # Calculate percentiles
        percentiles = {
            "p5": float(np.percentile(values, 5)),
            "p25": float(np.percentile(values, 25)),
            "p50": float(np.percentile(values, 50)),
            "p75": float(np.percentile(values, 75)),
            "p95": float(np.percentile(values, 95)),
            "p99": float(np.percentile(values, 99))
        }
        
        # Simple fatigue damage estimate (rainflow counting would be better)
        stress_range = values.max() - values.min()
        num_cycles = zero_crossings / 2
        fatigue_damage = float(num_cycles * (stress_range / 1000) ** 3)  # Simplified S-N curve
        
        return TimeSeriesStatistics(
            parameter=parameter,
            peak_value=float(values.max()),
            peak_time=float(time[max_idx]),
            mean=float(values.mean()),
            std_dev=float(values.std()),
            rms=float(np.sqrt(np.mean(values**2))),
            range=float(values.max() - values.min()),
            percentiles=percentiles,
            zero_crossings=int(zero_crossings),
            dominant_frequency=dominant_frequency,
            fatigue_damage=fatigue_damage
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/export/{fe_file}")
async def export_time_series(
    fe_file: str,
    export_config: ExportFormat
):
    """Export time series data in various formats"""
    
    try:
        # Get the data
        # In production, this would fetch actual data
        time = np.linspace(0, 3600, 1000)
        data = {
            "time": time,
            "strut1_tension": 5000 * np.sin(2 * np.pi * 0.1 * time) + np.random.randn(len(time)) * 500,
            "strut2_tension": 4500 * np.sin(2 * np.pi * 0.1 * time + np.pi/4) + np.random.randn(len(time)) * 400,
            "strut3_tension": 5200 * np.sin(2 * np.pi * 0.1 * time + np.pi/2) + np.random.randn(len(time)) * 600,
        }
        
        df = pd.DataFrame(data)
        
        # Apply time range filter if specified
        if export_config.time_range:
            mask = (df['time'] >= export_config.time_range.get('start', 0)) & \
                   (df['time'] <= export_config.time_range.get('end', df['time'].max()))
            df = df[mask]
        
        # Apply downsampling if specified
        if export_config.downsample_factor:
            df = df.iloc[::export_config.downsample_factor]
        
        # Add statistics if requested
        if export_config.include_statistics:
            stats_df = df.describe()
        
        # Export based on format
        export_path = Path(f"exports/{fe_file}_{datetime.now().strftime('%Y%m%d_%H%M%S')}")
        export_path.parent.mkdir(exist_ok=True)
        
        if export_config.format == "csv":
            file_path = export_path.with_suffix('.csv')
            df.to_csv(file_path, index=False)
            
            if export_config.include_statistics:
                stats_path = export_path.parent / f"{export_path.stem}_stats.csv"
                stats_df.to_csv(stats_path)
                
        elif export_config.format == "json":
            file_path = export_path.with_suffix('.json')
            result = {
                "fe_file": fe_file,
                "export_time": datetime.now().isoformat(),
                "data": df.to_dict(orient='records'),
            }
            if export_config.include_statistics:
                result["statistics"] = stats_df.to_dict()
            
            with open(file_path, 'w') as f:
                json.dump(result, f, indent=2)
                
        elif export_config.format == "excel":
            file_path = export_path.with_suffix('.xlsx')
            with pd.ExcelWriter(file_path) as writer:
                df.to_excel(writer, sheet_name='Time Series', index=False)
                if export_config.include_statistics:
                    stats_df.to_excel(writer, sheet_name='Statistics')
                    
        elif export_config.format == "matlab":
            file_path = export_path.with_suffix('.mat')
            # Would use scipy.io.savemat in production
            return {"message": "MATLAB export would be implemented with scipy.io.savemat"}
        
        return {
            "status": "success",
            "file_path": str(file_path),
            "format": export_config.format,
            "rows_exported": len(df),
            "columns_exported": len(df.columns)
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))