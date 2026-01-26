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

            # Use specialized handlers for certain file types
            if file_path.suffix.lower() == '.csv':
                return self.minimize_csv(file_path)
            if file_path.suffix.lower() == '.log':
                return self.minimize_log(file_path)

            # If already small enough, return as-is
            if file_size <= self.max_size:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    return f.read()
            
            # For larger files, keep first and last lines
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                first_lines = []
                last_lines = []
                line_count = 0

                # Read all lines, keeping track of first N and last N
                for line in f:
                    line_count += 1

                    # Keep first N lines
                    if len(first_lines) < self.lines_to_keep:
                        first_lines.append(line)
                    else:
                        # Circular buffer for last N lines
                        last_lines.append(line)
                        if len(last_lines) > self.lines_to_keep:
                            last_lines.pop(0)

                # Combine for further processing
                lines = first_lines + last_lines
            
            if line_count <= self.lines_to_keep * 2:
                # File is small enough to keep entirely
                return ''.join(lines)
            
            # Create minimized version - keep first and last lines with omitted marker
            first_lines = lines[:self.lines_to_keep]
            last_lines = lines[-self.lines_to_keep:] if len(lines) > self.lines_to_keep else []
            omitted_count = line_count - len(first_lines) - len(last_lines)

            # Build result with essential content first
            result_parts = []
            result_parts.extend(first_lines)
            if omitted_count > 0:
                result_parts.append(f"\n[... omitted ... ({omitted_count} lines)]\n\n")
            result_parts.extend(last_lines)

            result = ''.join(result_parts)

            # Only add header if we have space
            header = (
                f"# Minimized version of: {file_path.name}\n"
                f"# Original size: {file_size} bytes, {line_count} lines\n\n"
            )

            if len((header + result).encode('utf-8')) <= self.max_size:
                result = header + result
            elif len(result.encode('utf-8')) > self.max_size:
                # Still too big - reduce lines kept
                reduced_keep = max(2, self.lines_to_keep // 2)
                first_lines = lines[:reduced_keep]
                last_lines = lines[-reduced_keep:] if len(lines) > reduced_keep else []
                omitted_count = line_count - len(first_lines) - len(last_lines)
                result_parts = []
                result_parts.extend(first_lines)
                if omitted_count > 0:
                    result_parts.append(f"\n... ({omitted_count} lines omitted) ...\n\n")
                result_parts.extend(last_lines)
                result = ''.join(result_parts)

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