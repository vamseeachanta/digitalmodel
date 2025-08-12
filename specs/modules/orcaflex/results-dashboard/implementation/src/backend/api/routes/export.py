"""
Export API endpoints
"""

import io
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Query
from fastapi.responses import FileResponse, StreamingResponse
from sqlalchemy.ext.asyncio import AsyncSession

from models.schemas import ExportRequest, ExportResponse
from services.database import get_db
from services.export import ExportService
from utils.logger import setup_logger

logger = setup_logger(__name__)
router = APIRouter()

export_service = ExportService()


@router.post("/chart", response_model=ExportResponse)
async def export_chart(
    request: ExportRequest,
    format: str = Query("png", regex="^(png|svg|pdf)$"),
    db: AsyncSession = Depends(get_db),
) -> ExportResponse:
    """Export chart in specified format"""
    try:
        file_path = await export_service.export_chart(db, request, format)
        return ExportResponse(
            success=True,
            file_path=str(file_path),
            format=format,
            message=f"Chart exported as {format}",
        )
    except Exception as e:
        logger.error(f"Chart export failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/data")
async def export_data(
    request: ExportRequest,
    format: str = Query("csv", regex="^(csv|xlsx|json)$"),
    db: AsyncSession = Depends(get_db),
) -> StreamingResponse:
    """Export data in specified format"""
    try:
        data_buffer = await export_service.export_data(db, request, format)
        
        # Set appropriate content type
        content_types = {
            "csv": "text/csv",
            "xlsx": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            "json": "application/json",
        }
        
        return StreamingResponse(
            io.BytesIO(data_buffer),
            media_type=content_types[format],
            headers={
                "Content-Disposition": f"attachment; filename=orcaflex_data.{format}"
            },
        )
    except Exception as e:
        logger.error(f"Data export failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/report", response_model=ExportResponse)
async def generate_report(
    request: ExportRequest,
    template: Optional[str] = Query(None),
    db: AsyncSession = Depends(get_db),
) -> ExportResponse:
    """Generate PDF report from template"""
    try:
        file_path = await export_service.generate_report(db, request, template)
        return ExportResponse(
            success=True,
            file_path=str(file_path),
            format="pdf",
            message="Report generated successfully",
        )
    except Exception as e:
        logger.error(f"Report generation failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/batch", response_model=ExportResponse)
async def batch_export(
    requests: List[ExportRequest],
    format: str = Query("zip"),
    db: AsyncSession = Depends(get_db),
) -> ExportResponse:
    """Batch export multiple items"""
    try:
        archive_path = await export_service.batch_export(db, requests, format)
        return ExportResponse(
            success=True,
            file_path=str(archive_path),
            format=format,
            message=f"Exported {len(requests)} items",
        )
    except Exception as e:
        logger.error(f"Batch export failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/download/{file_id}")
async def download_export(file_id: str) -> FileResponse:
    """Download exported file by ID"""
    try:
        file_path = await export_service.get_export_path(file_id)
        if not file_path.exists():
            raise HTTPException(status_code=404, detail="File not found")
        
        return FileResponse(
            path=file_path,
            filename=file_path.name,
            media_type="application/octet-stream",
        )
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Download failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))