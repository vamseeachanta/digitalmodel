"""
Text file minimizer for Create Go-By Folder Tool
"""

from pathlib import Path
from typing import Optional, Union
import logging

from .base import BaseMinimizer

logger = logging.getLogger(__name__)


class TextMinimizer(BaseMinimizer):
    """Minimizer for text-based files."""
    
    TEXT_EXTENSIONS = {
        '.txt', '.md', '.rst', '.log', '.csv', '.tsv', '.dat',
        '.text', '.asc', '.out', '.err', '.lst', '.rpt'
    }
    
    def __init__(self, max_size: int = 10240, lines_to_keep: int = 10):
        """
        Initialize text minimizer.
        
        Args:
            max_size: Maximum size for minimized files
            lines_to_keep: Number of lines to keep from start/end
        """
        super().__init__(max_size)
        self.lines_to_keep = lines_to_keep
    
    def can_handle(self, file_path: Path) -> bool:
        """
        Check if this minimizer can handle the file.
        
        Args:
            file_path: Path to file
            
        Returns:
            True if this is a text file
        """
        return file_path.suffix.lower() in self.TEXT_EXTENSIONS
    
    def minimize(self, file_path: Path, preserve: bool = False) -> Optional[str]:
        """
        Minimize text file content.
        
        Args:
            file_path: Path to file to minimize
            preserve: If True, return None to preserve original
            
        Returns:
            Minimized content as string, or None if preserving
        """
        if preserve:
            return None
        
        try:
            # Check file size
            file_size = file_path.stat().st_size
            
            # If already small enough, return as-is
            if file_size <= self.max_size:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    return f.read()
            
            # For larger files, keep first and last lines
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = []
                line_count = 0
                
                # Read all lines (memory efficient for reasonable files)
                for line in f:
                    lines.append(line)
                    line_count += 1
                    
                    # For very large files, limit memory usage
                    if len(lines) > self.lines_to_keep * 3:
                        # Keep first N lines
                        if len(lines) > self.lines_to_keep * 2:
                            # Start removing middle lines
                            lines = lines[:self.lines_to_keep] + lines[-self.lines_to_keep:]
                            break
            
            if line_count <= self.lines_to_keep * 2:
                # File is small enough to keep entirely
                return ''.join(lines)
            
            # Create minimized version
            minimized = []
            
            # Add header
            minimized.append(f"# Minimized version of: {file_path.name}\n")
            minimized.append(f"# Original size: {file_size} bytes, {line_count} lines\n")
            minimized.append(f"# Showing first {self.lines_to_keep} and last {self.lines_to_keep} lines\n")
            minimized.append("\n")
            
            # Add first lines
            minimized.extend(lines[:self.lines_to_keep])
            
            # Add separator
            minimized.append(f"\n... ({line_count - self.lines_to_keep * 2} lines omitted) ...\n\n")
            
            # Add last lines
            if len(lines) > self.lines_to_keep:
                minimized.extend(lines[-self.lines_to_keep:])
            
            result = ''.join(minimized)
            
            # Ensure we don't exceed max size
            if len(result.encode('utf-8')) > self.max_size:
                # Truncate to fit
                result = result[:self.max_size - 100] + "\n... (truncated to fit size limit) ..."
            
            return result
            
        except Exception as e:
            logger.warning(f"Error minimizing text file {file_path}: {e}")
            # Return stub as fallback
            return self.create_stub_metadata(file_path)
    
    def minimize_csv(self, file_path: Path) -> Optional[str]:
        """
        Special handling for CSV files to preserve structure.
        
        Args:
            file_path: Path to CSV file
            
        Returns:
            Minimized CSV content
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            if not lines:
                return ""
            
            # Always keep header
            result = [lines[0]]
            
            if len(lines) <= self.lines_to_keep * 2 + 1:
                # Small file, keep all
                return ''.join(lines)
            
            # Add first data rows
            result.extend(lines[1:self.lines_to_keep + 1])
            
            # Add comment about omitted rows
            result.append(f"# ... {len(lines) - self.lines_to_keep * 2 - 1} rows omitted ...\n")
            
            # Add last data rows
            result.extend(lines[-self.lines_to_keep:])
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error minimizing CSV {file_path}: {e}")
            return self.minimize(file_path)  # Fall back to regular text minimization
    
    def minimize_log(self, file_path: Path) -> Optional[str]:
        """
        Special handling for log files to preserve important entries.
        
        Args:
            file_path: Path to log file
            
        Returns:
            Minimized log content
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            if not lines:
                return ""
            
            # Look for error/warning lines
            important_lines = []
            regular_lines = []
            
            keywords = ['ERROR', 'CRITICAL', 'FATAL', 'WARNING', 'WARN', 'Exception', 'Traceback']
            
            for line in lines:
                if any(keyword in line.upper() for keyword in keywords):
                    important_lines.append(line)
                else:
                    regular_lines.append(line)
            
            # Build result
            result = []
            result.append(f"# Log file summary: {file_path.name}\n")
            result.append(f"# Total lines: {len(lines)}, Important lines: {len(important_lines)}\n\n")
            
            # Add first few regular lines for context
            if regular_lines:
                result.append("=== First entries ===\n")
                result.extend(regular_lines[:5])
            
            # Add important lines (limited)
            if important_lines:
                result.append("\n=== Important entries (errors/warnings) ===\n")
                result.extend(important_lines[:20])
                if len(important_lines) > 20:
                    result.append(f"\n... and {len(important_lines) - 20} more important entries ...\n")
            
            # Add last few regular lines
            if len(regular_lines) > 5:
                result.append("\n=== Last entries ===\n")
                result.extend(regular_lines[-5:])
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error minimizing log {file_path}: {e}")
            return self.minimize(file_path)  # Fall back to regular minimization