"""
Case Selection API Endpoints
Allows manual selection and exploration of any simulation case
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field
from typing import List, Dict, Any, Optional
from pathlib import Path
import sys
import os

# Add parent directory to path
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from backend.file_processor import FileProcessor, MetadataExtractor
from backend.orcaflex_analyzer import OrcaFlexAnalyzer

# Create router
router = APIRouter(prefix="/api/cases", tags=["Case Selection"])

# Configuration
BASE_PATH = Path("D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr")

# Initialize processors
file_processor = FileProcessor(str(BASE_PATH))
metadata_extractor = MetadataExtractor()
analyzer = OrcaFlexAnalyzer()

# Pydantic models
class SimulationCase(BaseModel):
    case_id: str
    fe_filename: str
    fe_filename_stem: str
    metadata: Dict[str, Any]
    max_tension: Optional[float] = None
    min_tension: Optional[float] = None
    critical_strut: Optional[str] = None
    is_critical: bool = False

class CaseListResponse(BaseModel):
    total_cases: int
    critical_case_id: Optional[str]
    cases: List[SimulationCase]

class CaseDetailsResponse(BaseModel):
    case_id: str
    fe_filename: str
    metadata: Dict[str, Any]
    strut_analysis: Dict[str, Dict[str, float]]
    summary: Dict[str, Any]
    time_series_available: bool

class CaseFilterRequest(BaseModel):
    lng_loading: Optional[List[str]] = Field(None, description="Filter by LNG loading (l015, l095)")
    tide_levels: Optional[List[str]] = Field(None, description="Filter by tide level (hwl, lwl, mwl)")
    environment: Optional[List[str]] = Field(None, description="Filter by environment (cl, ncl)")
    directions: Optional[List[int]] = Field(None, description="Filter by wave direction degrees")
    tension_min: Optional[float] = Field(None, description="Minimum tension threshold")
    tension_max: Optional[float] = Field(None, description="Maximum tension threshold")


@router.get("/available-filters")
async def get_available_filters():
    """Get all available filter options based on actual data"""
    
    try:
        # Search for CSV files
        csv_files = file_processor.search_files("dm_*_strut_dyn.csv")[:100]
        
        available_filters = {
            "lng_loading": set(),
            "tide_levels": set(),
            "environments": set(),
            "directions": {"colinear": set(), "non_colinear": set()}
        }
        
        # Process files to extract unique values
        for csv_file in csv_files:
            filename = csv_file.stem
            
            # Parse filename components
            parts = filename.split('_')
            
            # Extract LNG loading
            for part in parts:
                if part.startswith('l0'):
                    available_filters["lng_loading"].add(part)
            
            # Extract tide level
            for tide in ['hwl', 'lwl', 'mwl']:
                if tide in filename:
                    available_filters["tide_levels"].add(tide)
            
            # Check if colinear/non-colinear data exists
            import pandas as pd
            df = pd.read_csv(csv_file)
            if 'fe_filename' in df.columns:
                for fe_file in df['fe_filename'].unique():
                    if fe_file:
                        # Extract environment and direction
                        fe_parts = Path(fe_file).stem.split('_')
                        
                        for part in fe_parts:
                            if part == 'cl':
                                available_filters["environments"].add('cl')
                                # Extract direction for colinear
                                for p in fe_parts:
                                    if p.endswith('deg'):
                                        deg = p[:-3]
                                        if deg.isdigit():
                                            available_filters["directions"]["colinear"].add(int(deg))
                            elif part == 'ncl':
                                available_filters["environments"].add('ncl')
                                # Extract direction for non-colinear
                                for p in fe_parts:
                                    if p.endswith('deg'):
                                        deg = p[:-3]
                                        if deg.isdigit():
                                            available_filters["directions"]["non_colinear"].add(int(deg))
        
        # Convert sets to sorted lists
        return {
            "lng_loading": sorted(list(available_filters["lng_loading"])),
            "tide_levels": sorted(list(available_filters["tide_levels"])),
            "environments": sorted(list(available_filters["environments"])),
            "directions": {
                "colinear": sorted(list(available_filters["directions"]["colinear"])),
                "non_colinear": sorted(list(available_filters["directions"]["non_colinear"]))
            }
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/list", response_model=CaseListResponse)
async def list_all_cases(
    sort_by: str = Query("max_tension", description="Sort by: max_tension, direction, lng_loading, tide_level"),
    ascending: bool = Query(False, description="Sort order"),
    limit: int = Query(50, description="Maximum cases to return")
):
    """List all available simulation cases with their basic info"""
    
    try:
        # Search for CSV files
        csv_files = file_processor.search_files("dm_*_strut_dyn.csv")[:limit]
        
        if not csv_files:
            return CaseListResponse(total_cases=0, critical_case_id=None, cases=[])
        
        # Load and analyze all files
        dataframes = file_processor.read_multiple_csvs_parallel(csv_files, max_workers=4)
        results = analyzer.analyze_multiple_dataframes(dataframes)
        
        cases = []
        critical_case_id = None
        max_overall_tension = 0
        
        # Process each unique FE file
        fe_files_processed = set()
        
        for csv_file in csv_files:
            # Extract FE filename from the CSV
            df_key = csv_file.stem
            if df_key in dataframes:
                df = dataframes[df_key]
                
                # Get unique FE files from this CSV
                if 'fe_filename' in df.columns:
                    for fe_file in df['fe_filename'].unique():
                        if fe_file and fe_file not in fe_files_processed:
                            fe_files_processed.add(fe_file)
                            
                            # Extract metadata
                            metadata = metadata_extractor.extract_from_fe_filename(fe_file)
                            
                            # Get tensions for this FE file
                            fe_mask = df['fe_filename'] == fe_file
                            fe_data = df[fe_mask]
                            
                            max_tension = 0
                            min_tension = 0
                            critical_strut = None
                            
                            # Find max tension across all struts for this FE file
                            for col in fe_data.columns:
                                if 'strut' in col.lower() and 'eff_tension' in col.lower():
                                    col_max = fe_data[col].max()
                                    col_min = fe_data[col].min()
                                    
                                    if col_max > max_tension:
                                        max_tension = col_max
                                        critical_strut = col.split('_')[0] if '_' in col else col
                                    if col_min < min_tension:
                                        min_tension = col_min
                            
                            # Create case ID from FE stem
                            case_id = Path(fe_file).stem
                            
                            # Check if this is the critical case
                            is_critical = False
                            if max_tension > max_overall_tension:
                                max_overall_tension = max_tension
                                critical_case_id = case_id
                                is_critical = True
                            
                            cases.append(SimulationCase(
                                case_id=case_id,
                                fe_filename=fe_file,
                                fe_filename_stem=Path(fe_file).stem,
                                metadata=metadata,
                                max_tension=float(max_tension) if max_tension else None,
                                min_tension=float(min_tension) if min_tension else None,
                                critical_strut=critical_strut,
                                is_critical=is_critical
                            ))
        
        # Sort cases
        if sort_by == "max_tension":
            cases.sort(key=lambda x: x.max_tension or 0, reverse=not ascending)
        elif sort_by == "direction":
            cases.sort(key=lambda x: x.metadata.get('direction', ''), reverse=not ascending)
        elif sort_by == "lng_loading":
            cases.sort(key=lambda x: x.metadata.get('lng_loading', ''), reverse=not ascending)
        elif sort_by == "tide_level":
            cases.sort(key=lambda x: x.metadata.get('tide_level', ''), reverse=not ascending)
        
        return CaseListResponse(
            total_cases=len(cases),
            critical_case_id=critical_case_id,
            cases=cases
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{case_id}", response_model=CaseDetailsResponse)
async def get_case_details(case_id: str):
    """Get detailed analysis for a specific case"""
    
    try:
        # Search for CSV files
        csv_files = file_processor.search_files("dm_*_strut_dyn.csv")
        
        # Load CSVs and find the case
        found_case = False
        case_data = None
        
        for csv_file in csv_files:
            import pandas as pd
            df = pd.read_csv(csv_file)
            
            if 'fe_filename' in df.columns:
                # Look for matching FE file
                for fe_file in df['fe_filename'].unique():
                    if fe_file and Path(fe_file).stem == case_id:
                        found_case = True
                        
                        # Get data for this FE file
                        fe_mask = df['fe_filename'] == fe_file
                        case_data = df[fe_mask]
                        
                        # Extract metadata
                        metadata = metadata_extractor.extract_from_fe_filename(fe_file)
                        
                        # Analyze struts
                        strut_analysis = {}
                        summary = {
                            'max_tension': 0,
                            'min_tension': 0,
                            'critical_strut': None,
                            'total_data_points': len(case_data)
                        }
                        
                        for col in case_data.columns:
                            if 'strut' in col.lower() and 'eff_tension' in col.lower():
                                strut_name = col.split('_')[0] if '_' in col else col
                                
                                strut_analysis[strut_name] = {
                                    'max': float(case_data[col].max()),
                                    'min': float(case_data[col].min()),
                                    'mean': float(case_data[col].mean()),
                                    'std': float(case_data[col].std()),
                                    'range': float(case_data[col].max() - case_data[col].min())
                                }
                                
                                if strut_analysis[strut_name]['max'] > summary['max_tension']:
                                    summary['max_tension'] = strut_analysis[strut_name]['max']
                                    summary['critical_strut'] = strut_name
                                
                                if strut_analysis[strut_name]['min'] < summary['min_tension']:
                                    summary['min_tension'] = strut_analysis[strut_name]['min']
                        
                        # Check if time series files exist
                        time_series_available = False
                        ts_patterns = [
                            f"{case_id}_strut_timeseries.csv",
                            f"{case_id}_timeseries.csv",
                            f"{case_id}*.csv"
                        ]
                        
                        for pattern in ts_patterns:
                            if list(BASE_PATH.parent.glob(pattern)):
                                time_series_available = True
                                break
                        
                        return CaseDetailsResponse(
                            case_id=case_id,
                            fe_filename=fe_file,
                            metadata=metadata,
                            strut_analysis=strut_analysis,
                            summary=summary,
                            time_series_available=time_series_available
                        )
        
        if not found_case:
            raise HTTPException(status_code=404, detail=f"Case {case_id} not found")
            
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/filter", response_model=CaseListResponse)
async def filter_cases(request: CaseFilterRequest):
    """Filter cases based on metadata and tension criteria"""
    
    try:
        # Get all cases first
        all_cases_response = await list_all_cases(limit=100)
        filtered_cases = all_cases_response.cases
        
        # Apply filters
        if request.lng_loading:
            filtered_cases = [
                case for case in filtered_cases
                if any(lng in case.metadata.get('parsed_components', {}).get('lng', '') 
                      for lng in request.lng_loading)
            ]
        
        if request.tide_levels:
            filtered_cases = [
                case for case in filtered_cases
                if any(tide in case.metadata.get('parsed_components', {}).get('tide', '')
                      for tide in request.tide_levels)
            ]
        
        if request.environment:
            filtered_cases = [
                case for case in filtered_cases
                if any(env in case.metadata.get('parsed_components', {}).get('env', '')
                      for env in request.environment)
            ]
        
        if request.directions:
            filtered_cases = [
                case for case in filtered_cases
                if any(abs(int(case.metadata.get('parsed_components', {}).get('direction', '0deg')[:-3]) - dir) < 15
                      for dir in request.directions if case.metadata.get('parsed_components', {}).get('direction'))
            ]
        
        if request.tension_min is not None:
            filtered_cases = [
                case for case in filtered_cases
                if case.max_tension and case.max_tension >= request.tension_min
            ]
        
        if request.tension_max is not None:
            filtered_cases = [
                case for case in filtered_cases
                if case.max_tension and case.max_tension <= request.tension_max
            ]
        
        return CaseListResponse(
            total_cases=len(filtered_cases),
            critical_case_id=all_cases_response.critical_case_id,
            cases=filtered_cases
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/{case_id}/compare/{other_case_id}")
async def compare_two_cases(case_id: str, other_case_id: str):
    """Compare two specific cases side by side"""
    
    try:
        # Get details for both cases
        case1 = await get_case_details(case_id)
        case2 = await get_case_details(other_case_id)
        
        # Calculate differences
        comparison = {
            "case1": {
                "id": case1.case_id,
                "max_tension": case1.summary['max_tension'],
                "critical_strut": case1.summary['critical_strut'],
                "metadata": case1.metadata
            },
            "case2": {
                "id": case2.case_id,
                "max_tension": case2.summary['max_tension'],
                "critical_strut": case2.summary['critical_strut'],
                "metadata": case2.metadata
            },
            "differences": {
                "max_tension_diff": case1.summary['max_tension'] - case2.summary['max_tension'],
                "max_tension_diff_pct": ((case1.summary['max_tension'] - case2.summary['max_tension']) / 
                                         case2.summary['max_tension'] * 100) if case2.summary['max_tension'] else 0,
                "same_critical_strut": case1.summary['critical_strut'] == case2.summary['critical_strut']
            },
            "strut_comparison": {}
        }
        
        # Compare each strut
        for strut_name in case1.strut_analysis.keys():
            if strut_name in case2.strut_analysis:
                comparison["strut_comparison"][strut_name] = {
                    "case1_max": case1.strut_analysis[strut_name]['max'],
                    "case2_max": case2.strut_analysis[strut_name]['max'],
                    "difference": case1.strut_analysis[strut_name]['max'] - case2.strut_analysis[strut_name]['max']
                }
        
        return comparison
        
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/select/{case_id}")
async def select_case_as_active(case_id: str):
    """Select a case as the active case for analysis"""
    
    try:
        # Verify case exists
        case_details = await get_case_details(case_id)
        
        # Store as active case (in production, this would be stored in session/database)
        # For now, we return the selection confirmation
        return {
            "status": "success",
            "active_case": {
                "case_id": case_id,
                "fe_filename": case_details.fe_filename,
                "max_tension": case_details.summary['max_tension'],
                "critical_strut": case_details.summary['critical_strut'],
                "metadata": case_details.metadata
            },
            "message": f"Case {case_id} is now the active case"
        }
        
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))