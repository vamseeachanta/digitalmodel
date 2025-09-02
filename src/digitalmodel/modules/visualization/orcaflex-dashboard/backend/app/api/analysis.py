"""
Analysis API endpoints for OrcaFlex simulation processing.
"""

import logging
from typing import List, Optional
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, File, UploadFile, Query, BackgroundTasks
from fastapi.responses import JSONResponse

from app.models.analysis import (
    Analysis,
    AnalysisRequest,
    AnalysisResult,
    AnalysisProgress,
    AnalysisSummary,
    AnalysisStatus
)
from app.services.analysis_service import get_analysis_service, AnalysisService

router = APIRouter()
logger = logging.getLogger(__name__)


@router.get("/", response_model=List[AnalysisSummary])
async def get_analyses(
    status: Optional[AnalysisStatus] = Query(None, description="Filter by status"),
    limit: int = Query(50, ge=1, le=1000, description="Maximum number of analyses"),
    offset: int = Query(0, ge=0, description="Offset for pagination"),
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Get list of analyses with optional filtering."""
    
    try:
        analyses = await analysis_service.get_analyses(
            status=status,
            limit=limit,
            offset=offset
        )
        return analyses
        
    except Exception as e:
        logger.error(f"Error fetching analyses: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch analyses")


@router.post("/", response_model=Analysis)
async def create_analysis(
    analysis_request: AnalysisRequest,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Create a new analysis."""
    
    try:
        analysis = await analysis_service.create_analysis(analysis_request)
        return analysis
        
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"Error creating analysis: {e}")
        raise HTTPException(status_code=500, detail="Failed to create analysis")


@router.post("/upload")
async def upload_orcaflex_file(
    file: UploadFile = File(..., description="OrcaFlex simulation file"),
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Upload OrcaFlex simulation file."""
    
    try:
        # Validate file type
        if not file.filename or not file.filename.lower().endswith(('.dat', '.yml', '.yaml')):
            raise HTTPException(
                status_code=400, 
                detail="Invalid file type. Only .dat, .yml, and .yaml files are supported"
            )
        
        file_path = await analysis_service.upload_file(file)
        
        return {
            "message": "File uploaded successfully",
            "file_path": file_path,
            "filename": file.filename,
            "size": file.size
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error uploading file: {e}")
        raise HTTPException(status_code=500, detail="Failed to upload file")


@router.get("/{analysis_id}", response_model=Analysis)
async def get_analysis(
    analysis_id: UUID,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Get specific analysis by ID."""
    
    try:
        analysis = await analysis_service.get_analysis(analysis_id)
        if not analysis:
            raise HTTPException(status_code=404, detail="Analysis not found")
        return analysis
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch analysis")


@router.put("/{analysis_id}", response_model=Analysis)
async def update_analysis(
    analysis_id: UUID,
    analysis_request: AnalysisRequest,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Update an existing analysis."""
    
    try:
        analysis = await analysis_service.update_analysis(analysis_id, analysis_request)
        if not analysis:
            raise HTTPException(status_code=404, detail="Analysis not found")
        return analysis
        
    except HTTPException:
        raise
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.error(f"Error updating analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to update analysis")


@router.delete("/{analysis_id}")
async def delete_analysis(
    analysis_id: UUID,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Delete an analysis."""
    
    try:
        success = await analysis_service.delete_analysis(analysis_id)
        if not success:
            raise HTTPException(status_code=404, detail="Analysis not found")
        
        return {"message": "Analysis deleted successfully"}
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error deleting analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to delete analysis")


@router.post("/{analysis_id}/run")
async def run_analysis(
    analysis_id: UUID,
    background_tasks: BackgroundTasks,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Start running an analysis."""
    
    try:
        # Validate analysis exists and is ready to run
        analysis = await analysis_service.get_analysis(analysis_id)
        if not analysis:
            raise HTTPException(status_code=404, detail="Analysis not found")
        
        if analysis.status not in [AnalysisStatus.PENDING, AnalysisStatus.FAILED]:
            raise HTTPException(
                status_code=400, 
                detail=f"Analysis cannot be run in status: {analysis.status}"
            )
        
        # Start analysis in background
        background_tasks.add_task(
            analysis_service.run_analysis_background, 
            analysis_id
        )
        
        # Update status to running
        await analysis_service.update_analysis_status(analysis_id, AnalysisStatus.RUNNING)
        
        return {
            "message": "Analysis started successfully",
            "analysis_id": analysis_id,
            "status": "running"
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error starting analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to start analysis")


@router.post("/{analysis_id}/cancel")
async def cancel_analysis(
    analysis_id: UUID,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Cancel a running analysis."""
    
    try:
        success = await analysis_service.cancel_analysis(analysis_id)
        if not success:
            raise HTTPException(
                status_code=400, 
                detail="Analysis cannot be cancelled or not found"
            )
        
        return {
            "message": "Analysis cancelled successfully",
            "analysis_id": analysis_id,
            "status": "cancelled"
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error cancelling analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to cancel analysis")


@router.get("/{analysis_id}/progress", response_model=AnalysisProgress)
async def get_analysis_progress(
    analysis_id: UUID,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Get progress information for a running analysis."""
    
    try:
        progress = await analysis_service.get_analysis_progress(analysis_id)
        if not progress:
            raise HTTPException(status_code=404, detail="Analysis or progress not found")
        return progress
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching progress for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch progress")


@router.get("/{analysis_id}/results", response_model=AnalysisResult)
async def get_analysis_results(
    analysis_id: UUID,
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Get results for a completed analysis."""
    
    try:
        result = await analysis_service.get_analysis_results(analysis_id)
        if not result:
            raise HTTPException(status_code=404, detail="Analysis results not found")
        return result
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching results for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch results")


@router.get("/{analysis_id}/logs")
async def get_analysis_logs(
    analysis_id: UUID,
    lines: int = Query(100, ge=1, le=10000, description="Number of log lines"),
    analysis_service: AnalysisService = Depends(get_analysis_service)
):
    """Get log output for an analysis."""
    
    try:
        logs = await analysis_service.get_analysis_logs(analysis_id, lines)
        if logs is None:
            raise HTTPException(status_code=404, detail="Analysis or logs not found")
        
        return {
            "analysis_id": analysis_id,
            "logs": logs,
            "lines": len(logs.split('\n')) if logs else 0
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error fetching logs for analysis {analysis_id}: {e}")
        raise HTTPException(status_code=500, detail="Failed to fetch logs")