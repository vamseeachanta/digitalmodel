"""
Document converter wrapper for MarkItDown
"""

import logging
from pathlib import Path
from typing import Any, Dict, Optional

from markitdown import MarkItDown
from .cleaner import MarkdownCleaner

logger = logging.getLogger(__name__)


class DocumentConverter:
    """Enhanced document converter with additional features"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize document converter
        
        Args:
            config: Configuration dictionary
        """
        self.config = config or {}
        
        # Initialize MarkItDown with LLM config if available
        llm_config = self.config.get("markitdown", {}).get("llm", {})
        if llm_config.get("model") and llm_config.get("api_key"):
            self.converter = MarkItDown(
                llm_model=llm_config["model"],
                llm_api_key=llm_config["api_key"]
            )
        else:
            self.converter = MarkItDown()
    
    def convert_file(
        self,
        file_path: str,
        output_path: Optional[str] = None,
        clean_output: bool = True,  # Default to True for clean output
        save_raw: bool = True,  # Also save raw version
        remove_unicode: bool = True,
        fix_quotes: bool = True,
        fix_math_symbols: bool = True,
        fix_greek_letters: bool = False,
        pure_ascii: bool = False,
        fix_latex: bool = True,
        clean_format: bool = True,
        **kwargs
    ) -> Dict[str, Any]:
        """Convert a single file to Markdown with automatic cleaning
        
        Args:
            file_path: Path to input file
            output_path: Optional output path (will create both clean and raw versions)
            clean_output: Apply cleaning to remove special characters (default: True)
            save_raw: Also save raw unconverted version (default: True)
            remove_unicode: Remove invisible Unicode characters
            fix_quotes: Replace smart quotes with regular quotes
            fix_math_symbols: Replace math symbols with ASCII equivalents
            fix_greek_letters: Replace Greek letters with names
            pure_ascii: Convert to pure ASCII only (most aggressive)
            fix_latex: Fix LaTeX formula issues
            clean_format: Clean up formatting issues
            **kwargs: Additional conversion options
            
        Returns:
            Conversion result dictionary with both clean and raw content
        """
        try:
            # Convert file
            result = self.converter.convert(file_path, **kwargs)
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
            
            # Save files if output path provided
            saved_paths = {}
            if output_path:
                output_base = Path(output_path)
                output_dir = output_base.parent
                output_stem = output_base.stem
                output_suffix = output_base.suffix or '.md'
                
                output_dir.mkdir(parents=True, exist_ok=True)
                
                # Save clean version (primary output)
                clean_path = output_dir / f"{output_stem}{output_suffix}"
                encoding = 'ascii' if pure_ascii else 'utf-8'
                clean_path.write_text(clean_content, encoding=encoding)
                saved_paths['clean'] = str(clean_path)
                
                # Save raw version if requested (for agent reference)
                if save_raw and clean_output:  # Only save raw if we actually cleaned
                    raw_path = output_dir / f"{output_stem}_raw{output_suffix}"
                    raw_path.write_text(raw_content, encoding='utf-8')
                    saved_paths['raw'] = str(raw_path)
            
            return {
                "success": True,
                "content": clean_content,  # Return clean content by default
                "raw_content": raw_content,  # Also provide raw content
                "metadata": getattr(result, 'metadata', {}),
                "output_paths": saved_paths,
                "output_path": saved_paths.get('clean'),  # Backward compatibility
                "cleaned": clean_output
            }
            
        except Exception as e:
            logger.error(f"Failed to convert {file_path}: {e}")
            return {
                "success": False,
                "error": str(e),
                "file_path": file_path
            }
    
    def batch_convert(
        self,
        input_dir: str,
        output_dir: str,
        pattern: str = "*",
        recursive: bool = False,
        clean_output: bool = True,
        **kwargs
    ) -> Dict[str, Any]:
        """Convert multiple files in a directory
        
        Args:
            input_dir: Input directory path
            output_dir: Output directory path
            pattern: File pattern to match
            recursive: Process subdirectories
            **kwargs: Additional conversion options
            
        Returns:
            Batch conversion results
        """
        input_path = Path(input_dir)
        output_path = Path(output_dir)
        
        # Find files
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
        
        # Process each file
        for file_path in files:
            rel_path = file_path.relative_to(input_path)
            out_file = output_path / rel_path.with_suffix(".md")
            
            result = self.convert_file(
                str(file_path),
                str(out_file),
                clean_output=clean_output,
                **kwargs
            )
            
            if result["success"]:
                results["successful"] += 1
            else:
                results["failed"] += 1
            
            results["files"].append({
                "input": str(file_path),
                "output": str(out_file) if result["success"] else None,
                "success": result["success"],
                "error": result.get("error")
            })
        
        return results
    
    def extract_tables(self, file_path: str) -> Dict[str, Any]:
        """Extract tables from a document
        
        Args:
            file_path: Path to document
            
        Returns:
            Extracted tables
        """
        result = self.convert_file(file_path)
        
        if not result["success"]:
            return result
        
        # Parse tables from Markdown content
        # This is simplified - actual implementation would use
        # proper Markdown parsing to extract table structures
        content = result["content"]
        tables = []
        
        lines = content.split('\n')
        in_table = False
        current_table = []
        
        for line in lines:
            if '|' in line:
                if not in_table:
                    in_table = True
                    current_table = []
                current_table.append(line)
            elif in_table and line.strip() == '':
                # End of table
                if current_table:
                    tables.append('\n'.join(current_table))
                in_table = False
                current_table = []
        
        # Add last table if exists
        if current_table:
            tables.append('\n'.join(current_table))
        
        return {
            "success": True,
            "tables": tables,
            "count": len(tables)
        }
    
    def describe_image(self, image_path: str) -> Dict[str, Any]:
        """Generate description for an image using LLM
        
        Args:
            image_path: Path to image file
            
        Returns:
            Image description
        """
        llm_config = self.config.get("markitdown", {}).get("llm", {})
        
        if not (llm_config.get("model") and llm_config.get("api_key")):
            return {
                "success": False,
                "error": "LLM configuration required for image description"
            }
        
        try:
            result = self.converter.convert(image_path)
            return {
                "success": True,
                "description": result.text_content,
                "image_path": image_path
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "image_path": image_path
            }