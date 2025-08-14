"""
OrcaFlex Browser API
FastAPI REST endpoints for OrcaFlex data analysis
"""

from fastapi import FastAPI, HTTPException, BackgroundTasks, WebSocket, Query
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field
from typing import List, Dict, Any, Optional
from pathlib import Path
import sys
import os
from datetime import datetime
import asyncio
import json

# Add parent directory to path for imports
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from backend.file_processor import FileProcessor, MetadataExtractor
from backend.orcaflex_analyzer import OrcaFlexAnalyzer
from time_trace_viewer import TimeTraceViewer

# Import enhanced time series router
from timeseries_enhanced import router as timeseries_router
from case_selection import router as case_router
from report_generator import router as report_router
from simple_pdf_endpoint import router as simple_pdf_router
from rich_pdf_endpoint import router as rich_pdf_router

# Initialize FastAPI app
app = FastAPI(
    title="OrcaFlex Browser API",
    description="REST API for browsing and analyzing OrcaFlex simulation data",
    version="1.0.0",
    docs_url="/docs",
    redoc_url="/redoc"
)

# Configure CORS for web frontend access
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Configure appropriately for production
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Global configuration
class Config:
    BASE_PATH = Path("D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr")
    CACHE_TTL = 300  # 5 minutes
    MAX_FILES = 100
    
config = Config()

# In-memory cache
cache = {
    "analysis_results": None,
    "last_updated": None,
    "file_list": None
}

# Initialize processors
file_processor = None
analyzer = OrcaFlexAnalyzer()
metadata_extractor = MetadataExtractor()

# Pydantic models for request/response
class AnalysisRequest(BaseModel):
    file_pattern: str = Field(default="dm_*_strut_dyn.csv", description="File pattern to search")
    max_files: Optional[int] = Field(default=20, description="Maximum files to process")
    force_refresh: bool = Field(default=False, description="Force refresh cache")

class FileInfo(BaseModel):
    filename: str
    path: str
    size_kb: float
    modified: str
    metadata: Dict[str, Any]

class StrutAnalysis(BaseModel):
    strut_id: str
    min_tension: float
    max_tension: float
    range: float
    status: str
    source_files: List[str]

class CriticalCase(BaseModel):
    value: float
    strut: str
    fe_filename: str
    fe_filename_stem: str
    metadata: Dict[str, Any]
    
class AnalysisResponse(BaseModel):
    timestamp: str
    files_analyzed: int
    struts_found: int
    absolute_max: float
    absolute_min: float
    critical_case: Optional[CriticalCase]
    strut_analysis: List[StrutAnalysis]

class TimeSeriesPoint(BaseModel):
    time: float
    value: float

class TimeSeriesResponse(BaseModel):
    fe_file: str
    strut_id: Optional[str]
    time_points: int
    sample_rate: float
    max_value: float
    min_value: float
    mean_value: float
    std_dev: float
    max_time: float
    data: List[TimeSeriesPoint]

class MetadataCategory(BaseModel):
    name: str
    options: List[Dict[str, Any]]

class BrowseRequest(BaseModel):
    lng_loading: Optional[List[str]] = None
    tide_levels: Optional[List[str]] = None
    directions: Optional[List[int]] = None
    min_tension: Optional[float] = None
    max_tension: Optional[float] = None

# Initialize on module load
def initialize_processor():
    """Initialize file processor"""
    global file_processor
    if config.BASE_PATH.exists():
        file_processor = FileProcessor(str(config.BASE_PATH))
        print(f"Initialized with base path: {config.BASE_PATH}")
    else:
        print(f"Warning: Base path does not exist: {config.BASE_PATH}")

# Initialize processor
initialize_processor()

# Include enhanced time series router
app.include_router(timeseries_router)
app.include_router(case_router)
app.include_router(report_router)
app.include_router(simple_pdf_router)
app.include_router(rich_pdf_router)

# Root endpoint
@app.get("/")
async def root():
    """API root endpoint"""
    return {
        "name": "OrcaFlex Browser API",
        "version": "1.0.0",
        "status": "online",
        "base_path": str(config.BASE_PATH),
        "docs": "/docs",
        "endpoints": {
            "files": "/api/files",
            "analyze": "/api/analyze",
            "critical": "/api/critical",
            "timeseries": "/api/timeseries/{fe_file}",
            "metadata": "/api/metadata/categories",
            "enhanced_timeseries": {
                "all_struts": "/api/v2/timeseries/struts/{fe_file}/all",
                "jacket_data": "/api/v2/timeseries/jacket/{fe_file}",
                "compare": "/api/v2/timeseries/compare",
                "statistics": "/api/v2/timeseries/statistics/{fe_file}/{parameter}",
                "export": "/api/v2/timeseries/export/{fe_file}"
            }
        }
    }

# File endpoints
@app.get("/api/files", response_model=List[FileInfo])
async def get_files(
    pattern: str = Query(default="dm_*_strut_dyn.csv", description="File search pattern"),
    limit: int = Query(default=50, description="Maximum files to return")
):
    """Get list of available CSV files"""
    if not file_processor:
        raise HTTPException(status_code=500, detail="File processor not initialized")
    
    try:
        files = file_processor.search_files(pattern)[:limit]
        
        file_list = []
        for file in files:
            if file.exists():
                metadata = metadata_extractor.extract_metadata(file.name, str(file))
                file_list.append(FileInfo(
                    filename=file.name,
                    path=str(file),
                    size_kb=file.stat().st_size / 1024,
                    modified=datetime.fromtimestamp(file.stat().st_mtime).isoformat(),
                    metadata=metadata
                ))
        
        # Cache the file list
        cache["file_list"] = file_list
        
        return file_list
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Analysis endpoints
@app.post("/api/analyze", response_model=AnalysisResponse)
async def analyze_files(request: AnalysisRequest, background_tasks: BackgroundTasks):
    """Analyze CSV files for tension data"""
    
    # Check cache
    if not request.force_refresh and cache["analysis_results"]:
        if cache["last_updated"]:
            time_diff = (datetime.now() - cache["last_updated"]).seconds
            if time_diff < config.CACHE_TTL:
                return cache["analysis_results"]
    
    if not file_processor:
        raise HTTPException(status_code=500, detail="File processor not initialized")
    
    try:
        # Search for files
        files = file_processor.search_files(request.file_pattern)[:request.max_files]
        
        if not files:
            raise HTTPException(status_code=404, detail="No files found matching pattern")
        
        # Load and analyze
        dataframes = file_processor.read_multiple_csvs_parallel(files, max_workers=4)
        results = analyzer.analyze_multiple_dataframes(dataframes)
        
        # Prepare response
        strut_analysis = []
        for strut_name, data in results['strut_analysis'].items():
            # Determine status
            max_val = data.get('max_tension', 0)
            if max_val > 5000:
                status = "CRITICAL"
            elif max_val > 3000:
                status = "HIGH"
            else:
                status = "NORMAL"
            
            strut_analysis.append(StrutAnalysis(
                strut_id=strut_name,
                min_tension=data.get('min_tension', 0),
                max_tension=max_val,
                range=max_val - data.get('min_tension', 0),
                status=status,
                source_files=data.get('source_files', [])
            ))
        
        # Sort by max tension
        strut_analysis.sort(key=lambda x: x.max_tension, reverse=True)
        
        # Prepare critical case
        critical_case = None
        if results.get('absolute_maximum'):
            abs_max = results['absolute_maximum']
            metadata = {}
            if 'fe_filename' in abs_max:
                metadata = metadata_extractor.extract_from_fe_filename(abs_max['fe_filename'])
            
            critical_case = CriticalCase(
                value=abs_max['value'],
                strut=abs_max['strut'],
                fe_filename=abs_max.get('fe_filename', ''),
                fe_filename_stem=abs_max.get('fe_filename_stem', ''),
                metadata=metadata
            )
        
        response = AnalysisResponse(
            timestamp=datetime.now().isoformat(),
            files_analyzed=len(files),
            struts_found=results['summary']['total_struts_found'],
            absolute_max=results['summary']['absolute_max_tension'],
            absolute_min=results['summary']['absolute_min_tension'],
            critical_case=critical_case,
            strut_analysis=strut_analysis
        )
        
        # Update cache
        cache["analysis_results"] = response
        cache["last_updated"] = datetime.now()
        
        return response
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/api/critical", response_model=Optional[CriticalCase])
async def get_critical_case():
    """Get the critical loading case"""
    
    # Try cache first
    if cache["analysis_results"] and cache["analysis_results"].critical_case:
        return cache["analysis_results"].critical_case
    
    # Run analysis if not cached
    request = AnalysisRequest()
    result = await analyze_files(request, BackgroundTasks())
    
    return result.critical_case

# Time series endpoints
@app.get("/api/timeseries/{fe_file}", response_model=TimeSeriesResponse)
async def get_timeseries(
    fe_file: str,
    strut_id: Optional[int] = Query(None, description="Strut number (1-8)"),
    start_time: Optional[float] = Query(0, description="Start time in seconds"),
    end_time: Optional[float] = Query(None, description="End time in seconds"),
    downsample: Optional[int] = Query(100, description="Downsample to N points")
):
    """Get time series data for an FE file"""
    
    try:
        # Create time trace viewer
        fe_base_path = config.BASE_PATH.parent
        viewer = TimeTraceViewer(fe_file, str(fe_base_path))
        
        if viewer.strut_timeseries is None:
            raise HTTPException(status_code=404, detail="No time series data available")
        
        # Get specific strut or default to strut 7
        if strut_id:
            col_name = f"Strut{strut_id}_Tension"
        else:
            col_name = "Strut7_Tension"  # Default to critical strut
        
        if col_name not in viewer.strut_timeseries.columns:
            raise HTTPException(status_code=404, detail=f"No data for {col_name}")
        
        # Get data
        values = viewer.strut_timeseries[col_name].values
        time_vector = viewer.time_vector
        
        # Apply time range filter
        if end_time is None:
            end_time = time_vector[-1]
        
        mask = (time_vector >= start_time) & (time_vector <= end_time)
        filtered_time = time_vector[mask]
        filtered_values = values[mask]
        
        # Downsample if needed
        if len(filtered_time) > downsample:
            indices = range(0, len(filtered_time), len(filtered_time) // downsample)
            filtered_time = filtered_time[list(indices)]
            filtered_values = filtered_values[list(indices)]
        
        # Calculate statistics
        import numpy as np
        max_idx = np.argmax(filtered_values)
        
        # Prepare data points
        data_points = [
            TimeSeriesPoint(time=t, value=v)
            for t, v in zip(filtered_time.tolist(), filtered_values.tolist())
        ]
        
        return TimeSeriesResponse(
            fe_file=fe_file,
            strut_id=col_name,
            time_points=len(data_points),
            sample_rate=viewer.time_vector[1] - viewer.time_vector[0] if len(viewer.time_vector) > 1 else 0.1,
            max_value=float(filtered_values.max()),
            min_value=float(filtered_values.min()),
            mean_value=float(filtered_values.mean()),
            std_dev=float(filtered_values.std()),
            max_time=float(filtered_time[max_idx]),
            data=data_points
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Metadata endpoints
@app.get("/api/metadata/categories", response_model=List[MetadataCategory])
async def get_metadata_categories():
    """Get available metadata categories for filtering"""
    
    categories = []
    
    # LNG Loading
    categories.append(MetadataCategory(
        name="lng_loading",
        options=[
            {"code": "l015", "display": "15% LNG", "value": "15"},
            {"code": "l095", "display": "95% LNG", "value": "95"}
        ]
    ))
    
    # Tide Levels
    categories.append(MetadataCategory(
        name="tide_levels",
        options=[
            {"code": "hwl", "display": "High Water Level (HWL)"},
            {"code": "lwl", "display": "Low Water Level (LWL)"},
            {"code": "mwl", "display": "Mean Water Level (MWL)"}
        ]
    ))
    
    # Environment Types
    categories.append(MetadataCategory(
        name="environment_types",
        options=[
            {"code": "cl", "display": "Colinear"},
            {"code": "ncl", "display": "Non-colinear"}
        ]
    ))
    
    # Wave Directions
    directions = []
    for deg in range(0, 360, 15):
        directions.append({
            "value": deg,
            "display": f"{deg}°",
            "code": f"{deg:03d}deg"
        })
    categories.append(MetadataCategory(
        name="wave_directions",
        options=directions
    ))
    
    return categories

@app.post("/api/browse")
async def browse_data(request: BrowseRequest):
    """Browse data with filters"""
    
    # Get all files
    if not cache["file_list"]:
        await get_files()
    
    filtered_files = cache["file_list"] or []
    
    # Apply filters
    if request.lng_loading:
        filtered_files = [f for f in filtered_files 
                         if any(lng in f.metadata.get('lng_loading', '') 
                               for lng in request.lng_loading)]
    
    if request.tide_levels:
        filtered_files = [f for f in filtered_files 
                         if any(tide in f.metadata.get('tide_level', '') 
                               for tide in request.tide_levels)]
    
    return {
        "total_files": len(cache["file_list"] or []),
        "filtered_files": len(filtered_files),
        "files": filtered_files
    }

# Statistics endpoint
@app.get("/api/stats")
async def get_statistics():
    """Get overall statistics"""
    
    stats = {
        "timestamp": datetime.now().isoformat(),
        "base_path": str(config.BASE_PATH),
        "cache_status": "valid" if cache["analysis_results"] else "empty",
        "last_analysis": cache["last_updated"].isoformat() if cache["last_updated"] else None
    }
    
    if cache["analysis_results"]:
        stats["summary"] = {
            "files_analyzed": cache["analysis_results"].files_analyzed,
            "struts_found": cache["analysis_results"].struts_found,
            "absolute_max": cache["analysis_results"].absolute_max,
            "absolute_min": cache["analysis_results"].absolute_min
        }
        
        if cache["analysis_results"].critical_case:
            stats["critical"] = {
                "value": cache["analysis_results"].critical_case.value,
                "strut": cache["analysis_results"].critical_case.strut,
                "fe_file": Path(cache["analysis_results"].critical_case.fe_filename).name
            }
    
    return stats

# WebSocket for real-time updates
@app.websocket("/ws/analysis")
async def websocket_analysis(websocket: WebSocket):
    """WebSocket endpoint for real-time analysis updates"""
    await websocket.accept()
    
    try:
        while True:
            # Send periodic updates
            if cache["analysis_results"]:
                await websocket.send_json({
                    "type": "update",
                    "timestamp": datetime.now().isoformat(),
                    "data": {
                        "absolute_max": cache["analysis_results"].absolute_max,
                        "critical_strut": cache["analysis_results"].critical_case.strut if cache["analysis_results"].critical_case else None
                    }
                })
            
            # Wait before next update
            await asyncio.sleep(5)
            
    except Exception as e:
        print(f"WebSocket error: {e}")
    finally:
        await websocket.close()

# Health check
@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "timestamp": datetime.now().isoformat(),
        "file_processor": file_processor is not None,
        "base_path_exists": config.BASE_PATH.exists()
    }

# Configuration endpoints
@app.get("/api/config/folders")
async def list_available_folders():
    """List available data folders in the OrcaFlex output directory"""
    base_dir = Path("D:/1522/ctr7/orcaflex/rev_a08/output/csv")
    folders = []
    
    if base_dir.exists():
        for folder in base_dir.iterdir():
            if folder.is_dir():
                # Check if folder contains CSV files
                csv_files = list(folder.glob("*.csv"))
                if csv_files:
                    folders.append({
                        "name": folder.name,
                        "path": str(folder),
                        "file_count": len(csv_files),
                        "description": get_folder_description(folder.name)
                    })
    
    return {
        "base_path": str(base_dir),
        "folders": sorted(folders, key=lambda x: x["name"]),
        "current": str(config.BASE_PATH)
    }

def get_folder_description(folder_name: str) -> str:
    """Get description based on folder name patterns"""
    if "01c" in folder_name:
        return "10-year storm conditions"
    elif "03c" in folder_name:
        return "100-year storm conditions"
    elif "10yr" in folder_name:
        return "10-year return period"
    elif "100yr" in folder_name:
        return "100-year return period"
    else:
        return "Simulation data"

@app.post("/api/config/folder")
async def change_folder(request: Dict[str, str]):
    """Change the base folder path for data files"""
    new_path = request.get("path")
    if not new_path:
        raise HTTPException(status_code=400, detail="Path is required")
    
    new_path_obj = Path(new_path)
    if not new_path_obj.exists():
        raise HTTPException(status_code=404, detail=f"Path does not exist: {new_path}")
    
    # Update configuration
    config.BASE_PATH = new_path_obj
    
    # Reinitialize file processor
    global file_processor
    file_processor = FileProcessor(str(new_path_obj))
    
    # Clear cache
    cache["analysis_results"] = None
    cache["last_updated"] = None
    cache["file_list"] = None
    
    # Get folder info
    csv_files = list(new_path_obj.glob("dm_*_strut_dyn.csv"))
    
    return {
        "status": "success",
        "new_path": str(new_path_obj),
        "folder_name": new_path_obj.name,
        "file_count": len(csv_files),
        "description": get_folder_description(new_path_obj.name),
        "message": "Folder path updated successfully"
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("main:app", host="0.0.0.0", port=8001, reload=False)