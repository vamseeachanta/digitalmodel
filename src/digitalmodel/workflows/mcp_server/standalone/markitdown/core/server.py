"""
MarkItDown MCP Server Implementation
"""

import asyncio
import logging
from typing import Any, Dict, List, Optional

from .converter import DocumentConverter

logger = logging.getLogger(__name__)


class MarkItDownMCPServer:
    """MCP Server for MarkItDown document conversion"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize the MarkItDown MCP server
        
        Args:
            config: Configuration dictionary
        """
        self.config = config or self._default_config()
        self.converter = DocumentConverter(self.config)
        self.conversion_history: List[Dict[str, Any]] = []
        
    def _default_config(self) -> Dict[str, Any]:
        """Return default configuration
        
        Returns:
            Default configuration dictionary
        """
        return {
            "server": {
                "name": "markitdown",
                "port": 3106,
                "host": "localhost",
                "description": "Document to Markdown conversion service"
            },
            "markitdown": {
                "processing": {
                    "batch_size": 10,
                    "timeout": 300,
                    "preserve_formatting": True
                },
                "output": {
                    "format": "markdown",
                    "table_format": "github"
                }
            }
        }
    
    async def convert_document(
        self,
        file_path: str,
        output_path: Optional[str] = None,
        **kwargs
    ) -> Dict[str, Any]:
        """Convert a document to Markdown
        
        Args:
            file_path: Path to input document
            output_path: Optional output path
            **kwargs: Additional conversion options
            
        Returns:
            Conversion result
        """
        result = self.converter.convert_file(file_path, output_path, **kwargs)
        
        # Track in history
        self.conversion_history.append({
            "file": file_path,
            "output": output_path,
            "success": result["success"],
            "error": result.get("error")
        })
        
        return result
    
    async def batch_convert(
        self,
        input_directory: str,
        output_directory: str,
        pattern: str = "*",
        recursive: bool = False,
        **kwargs
    ) -> Dict[str, Any]:
        """Convert multiple documents
        
        Args:
            input_directory: Input directory
            output_directory: Output directory
            pattern: File pattern
            recursive: Process subdirectories
            **kwargs: Additional options
            
        Returns:
            Batch conversion results
        """
        return self.converter.batch_convert(
            input_directory,
            output_directory,
            pattern,
            recursive,
            **kwargs
        )
    
    async def extract_tables(self, file_path: str) -> Dict[str, Any]:
        """Extract tables from a document
        
        Args:
            file_path: Path to document
            
        Returns:
            Extracted tables
        """
        return self.converter.extract_tables(file_path)
    
    async def describe_image(self, image_path: str) -> Dict[str, Any]:
        """Generate description for an image
        
        Args:
            image_path: Path to image
            
        Returns:
            Image description
        """
        return self.converter.describe_image(image_path)
    
    def get_capabilities(self) -> Dict[str, Any]:
        """Get server capabilities
        
        Returns:
            Capabilities dictionary
        """
        return {
            "supported_formats": [
                "pdf", "docx", "pptx", "xlsx",
                "html", "xml", "csv", "json",
                "jpg", "jpeg", "png", "gif",
                "txt", "md", "rst"
            ],
            "features": {
                "ocr": True,
                "tables": True,
                "images": True,
                "batch_processing": True,
                "llm_description": False  # Requires API key
            },
            "server_version": "1.0.0"
        }
    
    def get_history(self, limit: int = 100) -> List[Dict[str, Any]]:
        """Get conversion history
        
        Args:
            limit: Maximum number of entries
            
        Returns:
            Conversion history
        """
        return self.conversion_history[-limit:]
    
    def get_config(self) -> Dict[str, Any]:
        """Get current configuration
        
        Returns:
            Configuration dictionary
        """
        return self.config