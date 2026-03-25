"""
Tests for MarkItDown converter
"""

import pytest
from pathlib import Path
from unittest.mock import Mock, patch

from digitalmodel.workflows.mcp_server.standalone.markitdown.core.converter import DocumentConverter


class TestDocumentConverter:
    """Test document converter functionality"""
    
    def test_converter_initialization(self):
        """Test converter initialization"""
        converter = DocumentConverter()
        assert converter is not None
        assert converter.config == {}
    
    def test_converter_with_config(self):
        """Test converter with configuration"""
        config = {
            "markitdown": {
                "processing": {
                    "timeout": 600
                }
            }
        }
        converter = DocumentConverter(config)
        assert converter.config == config
    
    @patch('markitdown.MarkItDown')
    def test_convert_file_success(self, mock_markitdown):
        """Test successful file conversion"""
        # Setup mock
        mock_instance = Mock()
        mock_result = Mock()
        mock_result.text_content = "# Test Content"
        mock_instance.convert.return_value = mock_result
        mock_markitdown.return_value = mock_instance
        
        # Test conversion
        converter = DocumentConverter()
        result = converter.convert_file("test.pdf")
        
        assert result["success"] is True
        assert result["content"] == "# Test Content"
        assert result["output_path"] is None
    
    @patch('markitdown.MarkItDown')
    def test_convert_file_with_output(self, mock_markitdown, tmp_path):
        """Test file conversion with output path"""
        # Setup mock
        mock_instance = Mock()
        mock_result = Mock()
        mock_result.text_content = "# Test Content"
        mock_instance.convert.return_value = mock_result
        mock_markitdown.return_value = mock_instance
        
        # Test conversion with output
        converter = DocumentConverter()
        output_path = tmp_path / "output.md"
        result = converter.convert_file("test.pdf", str(output_path))
        
        assert result["success"] is True
        assert result["output_path"] == str(output_path)
    
    @patch('markitdown.MarkItDown')
    def test_convert_file_failure(self, mock_markitdown):
        """Test file conversion failure"""
        # Setup mock to raise exception
        mock_instance = Mock()
        mock_instance.convert.side_effect = Exception("Conversion failed")
        mock_markitdown.return_value = mock_instance
        
        # Test conversion failure
        converter = DocumentConverter()
        result = converter.convert_file("test.pdf")
        
        assert result["success"] is False
        assert "Conversion failed" in result["error"]
        assert result["file_path"] == "test.pdf"
    
    def test_extract_tables(self):
        """Test table extraction from markdown content"""
        converter = DocumentConverter()
        
        # Mock convert_file to return markdown with tables
        def mock_convert(file_path):
            return {
                "success": True,
                "content": """
# Document

Some text here.

| Column 1 | Column 2 |
|----------|----------|
| Value 1  | Value 2  |
| Value 3  | Value 4  |

More text.

| Another | Table |
|---------|-------|
| Data 1  | Data 2|
"""
            }
        
        converter.convert_file = mock_convert
        
        result = converter.extract_tables("test.md")
        assert result["success"] is True
        assert result["count"] == 2
        assert len(result["tables"]) == 2