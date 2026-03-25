"""
Analysis service for OrcaFlex simulation processing.
"""

import asyncio
import logging
import os
import shutil
from datetime import datetime
from pathlib import Path
from typing import List, Optional
from uuid import UUID, uuid4

from fastapi import UploadFile

from app.config import get_settings
from app.models.analysis import (
    Analysis,
    AnalysisRequest,
    AnalysisResult,
    AnalysisProgress,
    AnalysisSummary,
    AnalysisStatus,
    AnalysisType
)

logger = logging.getLogger(__name__)


class AnalysisService:
    """Service for managing OrcaFlex analyses."""
    
    def __init__(self):
        self.settings = get_settings()
        self._analyses = {}  # In-memory storage for demo
        self._progress = {}
        self._results = {}
        self._running_tasks = {}
        
        # Ensure upload directories exist
        Path(self.settings.upload_dir).mkdir(parents=True, exist_ok=True)
        Path(self.settings.results_dir).mkdir(parents=True, exist_ok=True)
    
    async def get_analyses(
        self, 
        status: Optional[AnalysisStatus] = None,
        limit: int = 50,
        offset: int = 0
    ) -> List[AnalysisSummary]:
        """Get list of analyses with optional filtering."""
        
        analyses = list(self._analyses.values())
        
        # Filter by status if provided
        if status:
            analyses = [a for a in analyses if a.status == status]
        
        # Sort by creation date (newest first)
        analyses.sort(key=lambda x: x.created_at, reverse=True)
        
        # Apply pagination
        analyses = analyses[offset:offset + limit]
        
        # Convert to summary format
        summaries = []
        for analysis in analyses:
            execution_time = None
            if analysis.started_at and analysis.completed_at:
                execution_time = (analysis.completed_at - analysis.started_at).total_seconds()
            
            summaries.append(AnalysisSummary(
                id=analysis.id,
                name=analysis.name,
                analysis_type=analysis.analysis_type,
                status=analysis.status,
                created_at=analysis.created_at,
                completed_at=analysis.completed_at,
                execution_time=execution_time,
                has_results=analysis.id in self._results
            ))
        
        return summaries
    
    async def create_analysis(self, request: AnalysisRequest) -> Analysis:
        """Create a new analysis."""
        
        # Validate OrcaFlex file exists
        file_path = Path(self.settings.upload_dir) / request.orcaflex_file
        if not file_path.exists():
            raise ValueError(f"OrcaFlex file not found: {request.orcaflex_file}")
        
        analysis = Analysis(
            name=request.name,
            description=request.description,
            analysis_type=request.analysis_type,
            orcaflex_file=request.orcaflex_file,
            configuration=request.configuration,
            tags=request.tags
        )
        
        self._analyses[analysis.id] = analysis
        logger.info(f"Created analysis: {analysis.id}")
        
        return analysis
    
    async def get_analysis(self, analysis_id: UUID) -> Optional[Analysis]:
        """Get analysis by ID."""
        return self._analyses.get(analysis_id)
    
    async def update_analysis(
        self, 
        analysis_id: UUID, 
        request: AnalysisRequest
    ) -> Optional[Analysis]:
        """Update an existing analysis."""
        
        analysis = self._analyses.get(analysis_id)
        if not analysis:
            return None
        
        # Only allow updates if analysis is pending or failed
        if analysis.status not in [AnalysisStatus.PENDING, AnalysisStatus.FAILED]:
            raise ValueError(f"Cannot update analysis in status: {analysis.status}")
        
        # Update fields
        analysis.name = request.name
        analysis.description = request.description
        analysis.analysis_type = request.analysis_type
        analysis.orcaflex_file = request.orcaflex_file
        analysis.configuration = request.configuration
        analysis.tags = request.tags
        
        logger.info(f"Updated analysis: {analysis_id}")
        return analysis
    
    async def delete_analysis(self, analysis_id: UUID) -> bool:
        """Delete an analysis."""
        
        analysis = self._analyses.get(analysis_id)
        if not analysis:
            return False
        
        # Cancel if running
        if analysis.status == AnalysisStatus.RUNNING:
            await self.cancel_analysis(analysis_id)
        
        # Clean up files
        try:
            file_path = Path(self.settings.upload_dir) / analysis.orcaflex_file
            if file_path.exists():
                file_path.unlink()
        except Exception as e:
            logger.warning(f"Failed to delete file for analysis {analysis_id}: {e}")
        
        # Remove from storage
        del self._analyses[analysis_id]
        self._progress.pop(analysis_id, None)
        self._results.pop(analysis_id, None)
        
        logger.info(f"Deleted analysis: {analysis_id}")
        return True
    
    async def upload_file(self, file: UploadFile) -> str:
        """Upload and store OrcaFlex file."""
        
        # Generate unique filename
        file_extension = Path(file.filename).suffix
        unique_filename = f"{uuid4()}{file_extension}"
        file_path = Path(self.settings.upload_dir) / unique_filename
        
        # Save file
        try:
            with open(file_path, "wb") as buffer:
                shutil.copyfileobj(file.file, buffer)
            
            logger.info(f"Uploaded file: {unique_filename}")
            return unique_filename
            
        except Exception as e:
            logger.error(f"Failed to upload file: {e}")
            raise
    
    async def run_analysis_background(self, analysis_id: UUID):
        """Run analysis in background task."""
        
        analysis = self._analyses.get(analysis_id)
        if not analysis:
            logger.error(f"Analysis not found: {analysis_id}")
            return
        
        try:
            # Update analysis status
            analysis.status = AnalysisStatus.RUNNING
            analysis.started_at = datetime.utcnow()
            
            # Initialize progress
            self._progress[analysis_id] = AnalysisProgress(
                analysis_id=analysis_id,
                progress=0.0,
                current_step="Initializing",
                total_steps=5,
                completed_steps=0
            )
            
            # Simulate analysis steps
            steps = [
                ("Loading OrcaFlex model", 20),
                ("Running simulation", 40),
                ("Processing results", 60),
                ("Generating statistics", 80),
                ("Finalizing output", 100)
            ]
            
            for i, (step_name, progress_percent) in enumerate(steps):
                # Check if cancelled
                if analysis.status == AnalysisStatus.CANCELLED:
                    logger.info(f"Analysis cancelled: {analysis_id}")
                    return
                
                # Update progress
                self._progress[analysis_id].current_step = step_name
                self._progress[analysis_id].progress = progress_percent
                self._progress[analysis_id].completed_steps = i + 1
                
                # Simulate work
                await asyncio.sleep(2)
            
            # Complete analysis
            analysis.status = AnalysisStatus.COMPLETED
            analysis.completed_at = datetime.utcnow()
            
            # Generate mock results
            self._results[analysis_id] = AnalysisResult(
                analysis_id=analysis_id,
                status=AnalysisStatus.COMPLETED,
                results={
                    "max_tension": 1250.5,
                    "min_tension": 890.2,
                    "mean_displacement": 12.3
                },
                metrics={
                    "simulation_time": 1800.0,
                    "time_step": 0.1,
                    "convergence_iterations": 45
                },
                output_files=[f"results_{analysis_id}.csv"],
                execution_time=(analysis.completed_at - analysis.started_at).total_seconds()
            )
            
            logger.info(f"Analysis completed successfully: {analysis_id}")
            
        except Exception as e:
            # Handle analysis failure
            analysis.status = AnalysisStatus.FAILED
            analysis.completed_at = datetime.utcnow()
            
            self._results[analysis_id] = AnalysisResult(
                analysis_id=analysis_id,
                status=AnalysisStatus.FAILED,
                error_message=str(e),
                error_details={"exception_type": type(e).__name__}
            )
            
            logger.error(f"Analysis failed: {analysis_id}, error: {e}")
    
    async def cancel_analysis(self, analysis_id: UUID) -> bool:
        """Cancel a running analysis."""
        
        analysis = self._analyses.get(analysis_id)
        if not analysis or analysis.status != AnalysisStatus.RUNNING:
            return False
        
        analysis.status = AnalysisStatus.CANCELLED
        analysis.completed_at = datetime.utcnow()
        
        # Cancel background task if exists
        task = self._running_tasks.get(analysis_id)
        if task and not task.done():
            task.cancel()
        
        logger.info(f"Cancelled analysis: {analysis_id}")
        return True
    
    async def update_analysis_status(
        self, 
        analysis_id: UUID, 
        status: AnalysisStatus
    ):
        """Update analysis status."""
        
        analysis = self._analyses.get(analysis_id)
        if analysis:
            analysis.status = status
            if status == AnalysisStatus.RUNNING:
                analysis.started_at = datetime.utcnow()
            elif status in [AnalysisStatus.COMPLETED, AnalysisStatus.FAILED, AnalysisStatus.CANCELLED]:
                analysis.completed_at = datetime.utcnow()
    
    async def get_analysis_progress(self, analysis_id: UUID) -> Optional[AnalysisProgress]:
        """Get progress for a running analysis."""
        return self._progress.get(analysis_id)
    
    async def get_analysis_results(self, analysis_id: UUID) -> Optional[AnalysisResult]:
        """Get results for a completed analysis."""
        return self._results.get(analysis_id)
    
    async def get_analysis_logs(self, analysis_id: UUID, lines: int = 100) -> Optional[str]:
        """Get log output for an analysis."""
        
        # In a real implementation, this would read from log files
        # For demo, return mock logs
        if analysis_id not in self._analyses:
            return None
        
        analysis = self._analyses[analysis_id]
        
        mock_logs = f"""
Analysis Log for {analysis.name} ({analysis_id})
=================================================

[{datetime.utcnow().isoformat()}] Starting OrcaFlex analysis
[{datetime.utcnow().isoformat()}] Loading model file: {analysis.orcaflex_file}
[{datetime.utcnow().isoformat()}] Model validation completed
[{datetime.utcnow().isoformat()}] Beginning simulation run
[{datetime.utcnow().isoformat()}] Simulation progress: 25%
[{datetime.utcnow().isoformat()}] Simulation progress: 50%
[{datetime.utcnow().isoformat()}] Simulation progress: 75%
[{datetime.utcnow().isoformat()}] Simulation completed successfully
[{datetime.utcnow().isoformat()}] Processing results
[{datetime.utcnow().isoformat()}] Analysis completed
        """.strip()
        
        return mock_logs


# Dependency injection
_analysis_service = None

def get_analysis_service() -> AnalysisService:
    """Get analysis service instance."""
    global _analysis_service
    if _analysis_service is None:
        _analysis_service = AnalysisService()
    return _analysis_service