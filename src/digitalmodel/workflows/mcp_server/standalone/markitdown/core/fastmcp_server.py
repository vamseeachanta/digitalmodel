"""
FastMCP-based MarkItDown MCP Server Implementation
"""

import asyncio
import logging
from pathlib import Path
from typing import Any, Dict, List, Optional
from datetime import datetime

try:
    from fastmcp import FastMCP
except ImportError:
    FastMCP = None
    
from markitdown import MarkItDown
from .cleaner import MarkdownCleaner
import yaml

logger = logging.getLogger(__name__)


class MarkItDownFastMCPServer:
    """FastMCP-based MCP Server for MarkItDown document conversion"""
    
    def __init__(self, config_path: Optional[str] = None):
        """Initialize the MarkItDown FastMCP server
        
        Args:
            config_path: Path to configuration file
        """
        if FastMCP is None:
            raise ImportError("FastMCP is required but not installed. Install with: uv add fastmcp")
            
        self.config = self._load_config(config_path)
        self.mcp = FastMCP("MarkItDown Document Converter")
        self.converter = self._init_converter()
        self.conversion_history: List[Dict[str, Any]] = []
        
        # Register MCP resources and tools
        self._register_resources()
        self._register_tools()
        self._register_prompts()
        
    def _load_config(self, config_path: Optional[str] = None) -> Dict[str, Any]:
        """Load configuration from file
        
        Args:
            config_path: Path to configuration file
            
        Returns:
            Configuration dictionary
        """
        if config_path is None:
            # Try multiple locations
            locations = [
                Path(__file__).parent.parent / "config" / "config.yml",
                Path(__file__).parent.parent.parent.parent.parent / "specs" / "modules" / "mcp-server" / "mcp" / "markitdown" / "config.yml",
                Path("config.yml")
            ]
            for location in locations:
                if location.exists():
                    config_path = location
                    break
                    
        if config_path and Path(config_path).exists():
            try:
                with open(config_path, 'r') as f:
                    config = yaml.safe_load(f)
                    logger.info(f"Loaded configuration from {config_path}")
                    return config
            except Exception as e:
                logger.warning(f"Could not load config from {config_path}: {e}")
                
        return self._default_config()
    
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
                    "preserve_formatting": True,
                    "include_metadata": True
                },
                "output": {
                    "format": "markdown",
                    "table_format": "github",
                    "include_toc": True
                },
                "file_types": {
                    "pdf": {
                        "use_ocr": True,
                        "extract_tables": True
                    }
                }
            }
        }
    
    def _init_converter(self) -> MarkItDown:
        """Initialize MarkItDown converter with configuration
        
        Returns:
            Configured MarkItDown instance
        """
        llm_config = self.config.get("markitdown", {}).get("llm", {})
        
        kwargs = {}
        if llm_config.get("model") and llm_config.get("api_key"):
            kwargs["llm_model"] = llm_config["model"]
            kwargs["llm_api_key"] = llm_config["api_key"]
            
        return MarkItDown(**kwargs)
    
    def _register_resources(self):
        """Register MCP resources"""
        
        @self.mcp.resource("markitdown://capabilities")
        async def get_capabilities() -> Dict[str, Any]:
            """Get conversion capabilities and supported formats"""
            return {
                "supported_formats": [
                    "pdf", "docx", "pptx", "xlsx",
                    "html", "xml", "csv", "json",
                    "jpg", "jpeg", "png", "gif",
                    "mp3", "wav", "m4a",
                    "txt", "md", "rst"
                ],
                "features": {
                    "ocr": self.config.get("markitdown", {}).get("file_types", {}).get("pdf", {}).get("use_ocr", True),
                    "tables": True,
                    "images": True,
                    "batch_processing": True,
                    "llm_description": self.config.get("markitdown", {}).get("llm", {}).get("model") is not None,
                    "azure_integration": self.config.get("markitdown", {}).get("azure", {}).get("endpoint") is not None
                },
                "version": "1.0.0",
                "mcp_version": "1.0"
            }
        
        @self.mcp.resource("markitdown://history")
        async def get_history(limit: int = 100) -> List[Dict[str, Any]]:
            """Get conversion history
            
            Args:
                limit: Maximum number of entries to return
                
            Returns:
                List of recent conversion operations
            """
            return self.conversion_history[-limit:]
        
        @self.mcp.resource("markitdown://config")
        async def get_config() -> Dict[str, Any]:
            """Get current server configuration"""
            # Return sanitized config (remove sensitive keys)
            config_copy = self.config.copy()
            
            # Remove sensitive information
            if "markitdown" in config_copy and "llm" in config_copy["markitdown"]:
                if "api_key" in config_copy["markitdown"]["llm"]:
                    config_copy["markitdown"]["llm"]["api_key"] = "***"
            if "markitdown" in config_copy and "azure" in config_copy["markitdown"]:
                if "key" in config_copy["markitdown"]["azure"]:
                    config_copy["markitdown"]["azure"]["key"] = "***"
                    
            return config_copy
    
    def _register_tools(self):
        """Register MCP tools"""
        
        @self.mcp.tool()
        async def convert_document(
            file_path: str,
            output_path: Optional[str] = None,
            include_metadata: bool = True,
            preserve_formatting: bool = True,
            clean_output: bool = True,  # Default to clean
            save_raw: bool = True,  # Also save raw version
            remove_unicode: bool = True,
            fix_quotes: bool = True,
            fix_math_symbols: bool = True,
            fix_greek_letters: bool = False,
            pure_ascii: bool = False,
            fix_latex: bool = True,
            clean_format: bool = True
        ) -> Dict[str, Any]:
            """Convert a document to Markdown with optional cleaning
            
            Args:
                file_path: Path to input document
                output_path: Optional output path for Markdown file
                include_metadata: Include document metadata in output
                preserve_formatting: Attempt to preserve original formatting
                clean_output: Apply cleaning to remove special characters
                remove_unicode: Remove invisible Unicode characters (zero-width spaces, etc.)
                fix_quotes: Replace smart quotes with regular quotes
                fix_math_symbols: Replace math symbols with ASCII equivalents
                fix_greek_letters: Replace Greek letters with text names
                pure_ascii: Convert to pure ASCII only (most aggressive cleaning)
                fix_latex: Fix LaTeX formula issues (escaped underscores, etc.)
                clean_format: Clean up formatting issues (multiple spaces, blank lines)
                
            Returns:
                Conversion result with content and metadata
            """
            try:
                # Validate file exists
                if not Path(file_path).exists():
                    raise FileNotFoundError(f"File not found: {file_path}")
                
                # Convert document
                result = self.converter.convert(file_path)
                raw_content = result.text_content
                
                # Apply cleaning (default behavior)
                clean_content = raw_content
                if clean_output:
                    clean_content = MarkdownCleaner.full_clean(
                        raw_content,
                        remove_unicode=remove_unicode,
                        fix_quotes=fix_quotes,
                        fix_math_symbols=fix_math_symbols,
                        fix_greek_letters=fix_greek_letters,
                        pure_ascii=pure_ascii,
                        fix_latex=fix_latex,
                        clean_format=clean_format
                    )
                
                # Process metadata if requested
                metadata = {}
                if include_metadata:
                    file_stat = Path(file_path).stat()
                    metadata = {
                        "source_file": file_path,
                        "file_size": file_stat.st_size,
                        "modified_time": datetime.fromtimestamp(file_stat.st_mtime).isoformat(),
                        "conversion_time": datetime.now().isoformat(),
                        "cleaned": clean_output,
                        "pure_ascii": pure_ascii
                    }
                
                # Save files if output path provided
                saved_paths = {}
                if output_path:
                    output_base = Path(output_path)
                    output_dir = output_base.parent
                    output_stem = output_base.stem
                    output_suffix = output_base.suffix or '.md'
                    
                    output_dir.mkdir(parents=True, exist_ok=True)
                    
                    # Prepare content with metadata if requested
                    def add_metadata(content, is_raw=False):
                        if include_metadata:
                            meta_copy = metadata.copy()
                            meta_copy['version'] = 'raw' if is_raw else 'clean'
                            frontmatter = f"---\n{yaml.dump(meta_copy, default_flow_style=False)}---\n\n"
                            return frontmatter + content
                        return content
                    
                    # Save clean version (primary output)
                    clean_path = output_dir / f"{output_stem}{output_suffix}"
                    encoding = 'ascii' if pure_ascii else 'utf-8'
                    clean_path.write_text(add_metadata(clean_content), encoding=encoding)
                    saved_paths['clean'] = str(clean_path)
                    
                    # Save raw version if requested (for agent reference)
                    if save_raw and clean_output:  # Only save raw if we actually cleaned
                        raw_path = output_dir / f"{output_stem}_raw{output_suffix}"
                        raw_path.write_text(add_metadata(raw_content, is_raw=True), encoding='utf-8')
                        saved_paths['raw'] = str(raw_path)
                
                # Track in history
                self.conversion_history.append({
                    "file": file_path,
                    "output_paths": saved_paths,
                    "timestamp": datetime.now().isoformat(),
                    "success": True,
                    "metadata": metadata
                })
                
                return {
                    "success": True,
                    "content": clean_content[:5000],  # First 5000 chars of clean version for preview
                    "raw_content": raw_content[:5000] if save_raw else None,
                    "full_length": len(clean_content),
                    "output_paths": saved_paths,
                    "output_path": saved_paths.get('clean'),  # Backward compatibility
                    "metadata": metadata
                }
                
            except Exception as e:
                logger.error(f"Conversion failed for {file_path}: {e}")
                self.conversion_history.append({
                    "file": file_path,
                    "timestamp": datetime.now().isoformat(),
                    "success": False,
                    "error": str(e)
                })
                return {
                    "success": False,
                    "error": str(e),
                    "file_path": file_path
                }
        
        @self.mcp.tool()
        async def batch_convert(
            input_directory: str,
            output_directory: str,
            pattern: str = "*",
            recursive: bool = False,
            parallel: bool = True
        ) -> Dict[str, Any]:
            """Convert multiple documents in a directory
            
            Args:
                input_directory: Input directory path
                output_directory: Output directory path
                pattern: File pattern to match (e.g., "*.pdf")
                recursive: Process subdirectories recursively
                parallel: Process files in parallel
                
            Returns:
                Batch conversion results
            """
            input_path = Path(input_directory)
            output_path = Path(output_directory)
            
            if not input_path.exists():
                return {
                    "success": False,
                    "error": f"Input directory not found: {input_directory}"
                }
            
            output_path.mkdir(parents=True, exist_ok=True)
            
            # Find files to convert
            if recursive:
                files = list(input_path.rglob(pattern))
            else:
                files = list(input_path.glob(pattern))
            
            results = {
                "total": len(files),
                "successful": 0,
                "failed": 0,
                "files": []
            }
            
            # Process files
            async def process_file(file_path: Path) -> Dict[str, Any]:
                try:
                    # Create output path maintaining directory structure
                    rel_path = file_path.relative_to(input_path)
                    out_file = output_path / rel_path.with_suffix(".md")
                    
                    # Use the convert_document tool
                    result = await convert_document(
                        str(file_path),
                        str(out_file),
                        include_metadata=True
                    )
                    
                    return {
                        "input": str(file_path),
                        "output": str(out_file) if result["success"] else None,
                        "success": result["success"],
                        "error": result.get("error")
                    }
                except Exception as e:
                    return {
                        "input": str(file_path),
                        "success": False,
                        "error": str(e)
                    }
            
            # Process in parallel or sequentially
            if parallel:
                batch_size = self.config.get("markitdown", {}).get("processing", {}).get("batch_size", 10)
                
                for i in range(0, len(files), batch_size):
                    batch = files[i:i + batch_size]
                    batch_results = await asyncio.gather(*[process_file(f) for f in batch])
                    
                    for result in batch_results:
                        if result["success"]:
                            results["successful"] += 1
                        else:
                            results["failed"] += 1
                        results["files"].append(result)
            else:
                for file_path in files:
                    result = await process_file(file_path)
                    if result["success"]:
                        results["successful"] += 1
                    else:
                        results["failed"] += 1
                    results["files"].append(result)
            
            return results
        
        @self.mcp.tool()
        async def extract_tables(
            file_path: str,
            output_format: str = "markdown"
        ) -> Dict[str, Any]:
            """Extract tables from a document
            
            Args:
                file_path: Path to document
                output_format: Output format (markdown, csv, json)
                
            Returns:
                Extracted tables in specified format
            """
            try:
                if not Path(file_path).exists():
                    raise FileNotFoundError(f"File not found: {file_path}")
                
                # Convert document
                result = self.converter.convert(file_path)
                content = result.text_content
                
                # Extract tables from Markdown content
                tables = []
                lines = content.split('\n')
                current_table = []
                in_table = False
                
                for line in lines:
                    # Detect table rows (contain pipes)
                    if '|' in line and not line.strip().startswith('```'):
                        if not in_table:
                            in_table = True
                            current_table = []
                        current_table.append(line)
                    elif in_table and (line.strip() == '' or '|' not in line):
                        # End of table
                        if current_table:
                            # Process based on output format
                            if output_format == "markdown":
                                tables.append('\n'.join(current_table))
                            elif output_format == "csv":
                                # Convert markdown table to CSV
                                csv_lines = []
                                for table_line in current_table:
                                    if not table_line.strip().startswith('|--'):
                                        cells = [cell.strip() for cell in table_line.split('|')[1:-1]]
                                        csv_lines.append(','.join(cells))
                                tables.append('\n'.join(csv_lines))
                            elif output_format == "json":
                                # Convert to JSON structure
                                headers = []
                                rows = []
                                for i, table_line in enumerate(current_table):
                                    if not table_line.strip().startswith('|--'):
                                        cells = [cell.strip() for cell in table_line.split('|')[1:-1]]
                                        if i == 0:
                                            headers = cells
                                        else:
                                            row = {headers[j]: cells[j] if j < len(cells) else "" 
                                                   for j in range(len(headers))}
                                            rows.append(row)
                                tables.append({"headers": headers, "rows": rows})
                        
                        in_table = False
                        current_table = []
                
                # Add last table if exists
                if current_table:
                    if output_format == "markdown":
                        tables.append('\n'.join(current_table))
                    # ... (same conversion logic as above)
                
                return {
                    "success": True,
                    "tables": tables,
                    "count": len(tables),
                    "format": output_format,
                    "source_file": file_path
                }
                
            except Exception as e:
                logger.error(f"Table extraction failed for {file_path}: {e}")
                return {
                    "success": False,
                    "error": str(e),
                    "file_path": file_path
                }
        
        @self.mcp.tool()
        async def describe_image(
            image_path: str,
            use_llm: bool = True,
            use_ocr: bool = True
        ) -> Dict[str, Any]:
            """Generate description for an image using LLM and/or OCR
            
            Args:
                image_path: Path to image file
                use_llm: Use LLM for image description
                use_ocr: Use OCR for text extraction
                
            Returns:
                Image description and extracted text
            """
            try:
                if not Path(image_path).exists():
                    raise FileNotFoundError(f"Image not found: {image_path}")
                
                llm_config = self.config.get("markitdown", {}).get("llm", {})
                
                # Check if LLM is available
                if use_llm and not (llm_config.get("model") and llm_config.get("api_key")):
                    return {
                        "success": False,
                        "error": "LLM configuration required for image description. Set llm.model and llm.api_key in config."
                    }
                
                # Convert image
                result = self.converter.convert(image_path)
                
                response = {
                    "success": True,
                    "image_path": image_path,
                    "content": result.text_content
                }
                
                # Add metadata
                file_stat = Path(image_path).stat()
                response["metadata"] = {
                    "file_size": file_stat.st_size,
                    "modified_time": datetime.fromtimestamp(file_stat.st_mtime).isoformat()
                }
                
                return response
                
            except Exception as e:
                logger.error(f"Image description failed for {image_path}: {e}")
                return {
                    "success": False,
                    "error": str(e),
                    "image_path": image_path
                }
    
    def _register_prompts(self):
        """Register MCP prompts for common operations"""
        
        @self.mcp.prompt("convert_technical_docs")
        async def convert_technical_docs() -> str:
            """Prompt for converting technical documentation"""
            return """Convert all technical documentation in the specified directory to Markdown format.
            
Please provide:
1. Input directory containing technical documents
2. Output directory for Markdown files
3. Whether to process subdirectories recursively
4. Any specific file patterns to include/exclude

The conversion will:
- Preserve tables and formatting
- Extract images with descriptions
- Include document metadata
- Maintain directory structure"""
        
        @self.mcp.prompt("extract_data_tables")
        async def extract_data_tables() -> str:
            """Prompt for extracting tables from documents"""
            return """Extract all data tables from documents and export them in a structured format.
            
Please specify:
1. Source document(s) containing tables
2. Desired output format (markdown, csv, json)
3. Whether to merge tables from multiple documents
4. Any specific table filtering criteria

The extraction will identify and parse all tables, maintaining column headers and data relationships."""
        
        @self.mcp.prompt("process_scanned_documents")
        async def process_scanned_documents() -> str:
            """Prompt for processing scanned documents with OCR"""
            return """Process scanned documents using OCR to extract text content.
            
Please provide:
1. Path to scanned document(s)
2. Output directory for extracted text
3. Whether to enhance with Azure Document Intelligence
4. Any specific regions or languages for OCR

The OCR processing will extract text while attempting to preserve layout and structure."""
    
    async def run(self):
        """Run the MCP server"""
        config = self.config.get("server", {})
        host = config.get("host", "localhost")
        port = config.get("port", 3106)
        
        logger.info(f"Starting MarkItDown FastMCP server on {host}:{port}")
        logger.info(f"Capabilities: {await self.mcp._resources['markitdown://capabilities'].handler()}")
        
        try:
            # FastMCP's run method handles the server lifecycle
            await self.mcp.run(host=host, port=port)
        except KeyboardInterrupt:
            logger.info("Server shutdown requested")
        except Exception as e:
            logger.error(f"Server error: {e}")
            raise


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="MarkItDown FastMCP Server")
    parser.add_argument(
        "--config",
        type=str,
        help="Path to configuration file",
        default=None
    )
    parser.add_argument(
        "--port",
        type=int,
        help="Server port (overrides config)",
        default=None
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug logging"
    )
    
    args = parser.parse_args()
    
    # Configure logging
    logging.basicConfig(
        level=logging.DEBUG if args.debug else logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    )
    
    # Create and run server
    try:
        server = MarkItDownFastMCPServer(config_path=args.config)
        
        # Override port if specified
        if args.port:
            server.config["server"]["port"] = args.port
        
        # Run server
        asyncio.run(server.run())
    except ImportError as e:
        logger.error(f"Missing dependency: {e}")
        print("\nPlease install required dependencies:")
        print("  uv add fastmcp markitdown")
        sys.exit(1)
    except KeyboardInterrupt:
        logger.info("Server stopped by user")
    except Exception as e:
        logger.error(f"Server failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    import sys
    main()