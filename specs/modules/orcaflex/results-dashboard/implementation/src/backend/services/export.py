"""
Export service for generating reports and exports
"""

import io
import json
from pathlib import Path
from typing import List, Optional
import uuid

import pandas as pd
from sqlalchemy.ext.asyncio import AsyncSession

from models.schemas import ExportRequest
from utils.config import settings
from utils.logger import setup_logger

logger = setup_logger(__name__)


class ExportService:
    """Service for handling exports"""
    
    async def export_chart(
        self,
        db: AsyncSession,
        request: ExportRequest,
        format: str
    ) -> Path:
        """Export chart in specified format"""
        # Mock chart export
        export_id = str(uuid.uuid4())
        export_path = settings.export_directory / f"chart_{export_id}.{format}"
        
        # Create mock file
        export_path.parent.mkdir(parents=True, exist_ok=True)
        export_path.write_text(f"Mock {format} chart for {request.data_type}")
        
        logger.info(f"Exported chart to {export_path}")
        return export_path
    
    async def export_data(
        self,
        db: AsyncSession,
        request: ExportRequest,
        format: str
    ) -> bytes:
        """Export data in specified format"""
        # Mock data export
        data = {
            "case": request.query.case,
            "component": request.query.component,
            "data": [
                {"heading": i * 15, "value": 100 + i * 10}
                for i in range(24)
            ]
        }
        
        if format == "csv":
            df = pd.DataFrame(data["data"])
            buffer = io.StringIO()
            df.to_csv(buffer, index=False)
            return buffer.getvalue().encode()
        
        elif format == "json":
            return json.dumps(data, indent=2).encode()
        
        elif format == "xlsx":
            df = pd.DataFrame(data["data"])
            buffer = io.BytesIO()
            df.to_excel(buffer, index=False)
            return buffer.getvalue()
        
        else:
            raise ValueError(f"Unsupported format: {format}")
    
    async def generate_report(
        self,
        db: AsyncSession,
        request: ExportRequest,
        template: Optional[str] = None
    ) -> Path:
        """Generate PDF report"""
        # Mock report generation
        export_id = str(uuid.uuid4())
        report_path = settings.export_directory / f"report_{export_id}.pdf"
        
        # Create mock PDF
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(f"Mock PDF report for {request.query.case}")
        
        logger.info(f"Generated report at {report_path}")
        return report_path
    
    async def batch_export(
        self,
        db: AsyncSession,
        requests: List[ExportRequest],
        format: str
    ) -> Path:
        """Batch export multiple items"""
        # Mock batch export
        export_id = str(uuid.uuid4())
        archive_path = settings.export_directory / f"batch_{export_id}.{format}"
        
        # Create mock archive
        archive_path.parent.mkdir(parents=True, exist_ok=True)
        archive_path.write_text(f"Mock batch export with {len(requests)} items")
        
        logger.info(f"Batch exported to {archive_path}")
        return archive_path
    
    async def get_export_path(self, file_id: str) -> Path:
        """Get path for exported file"""
        # Try different extensions
        for ext in ["png", "svg", "pdf", "csv", "xlsx", "zip"]:
            path = settings.export_directory / f"*_{file_id}.{ext}"
            matches = list(settings.export_directory.glob(path.name))
            if matches:
                return matches[0]
        
        # Default path if not found
        return settings.export_directory / f"export_{file_id}.dat"