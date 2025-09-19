"""
Code file minimizer for Create Go-By Folder Tool
"""

import re
from pathlib import Path
from typing import Optional, List
import logging

from .base import BaseMinimizer

logger = logging.getLogger(__name__)


class CodeMinimizer(BaseMinimizer):
    """Minimizer for source code files."""
    
    CODE_EXTENSIONS = {
        # Python
        '.py', '.pyw', '.pyx', '.pyi',
        # JavaScript/TypeScript
        '.js', '.jsx', '.ts', '.tsx', '.mjs', '.cjs',
        # Java/Kotlin
        '.java', '.kt', '.kts',
        # C/C++
        '.c', '.h', '.cpp', '.hpp', '.cc', '.cxx', '.hxx',
        # C#
        '.cs', '.csx',
        # Go
        '.go',
        # Rust
        '.rs',
        # Ruby
        '.rb', '.erb',
        # PHP
        '.php', '.php3', '.php4', '.php5', '.phtml',
        # Shell
        '.sh', '.bash', '.zsh', '.fish',
        # PowerShell
        '.ps1', '.psm1', '.psd1',
        # Batch
        '.bat', '.cmd',
        # R
        '.r', '.R',
        # Swift
        '.swift',
        # Scala
        '.scala', '.sc',
        # MATLAB
        '.m',
        # SQL
        '.sql'
    }
    
    def __init__(self, max_size: int = 10240):
        """
        Initialize code minimizer.
        
        Args:
            max_size: Maximum size for minimized files
        """
        super().__init__(max_size)
    
    def can_handle(self, file_path: Path) -> bool:
        """
        Check if this minimizer can handle the file.
        
        Args:
            file_path: Path to file
            
        Returns:
            True if this is a code file
        """
        return file_path.suffix.lower() in self.CODE_EXTENSIONS
    
    def minimize(self, file_path: Path, preserve: bool = False) -> Optional[str]:
        """
        Minimize code file content.
        
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
            
            # Determine file type and use appropriate minimizer
            ext = file_path.suffix.lower()
            
            if ext in ['.py', '.pyw', '.pyx', '.pyi']:
                return self.minimize_python(file_path)
            elif ext in ['.js', '.jsx', '.ts', '.tsx', '.mjs', '.cjs']:
                return self.minimize_javascript(file_path)
            elif ext in ['.java', '.kt', '.kts']:
                return self.minimize_java(file_path)
            elif ext in ['.c', '.h', '.cpp', '.hpp', '.cc', '.cxx', '.hxx']:
                return self.minimize_c_cpp(file_path)
            else:
                return self.minimize_generic(file_path)
                
        except Exception as e:
            logger.warning(f"Error minimizing code file {file_path}: {e}")
            return self.create_stub_metadata(file_path)
    
    def minimize_python(self, file_path: Path) -> str:
        """
        Minimize Python code file.
        
        Args:
            file_path: Path to Python file
            
        Returns:
            Minimized Python code
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f"# Minimized version of: {file_path.name}\n")
            result.append(f"# Original: {len(lines)} lines\n\n")
            
            # Keep imports
            imports = []
            for line in lines:
                if line.strip().startswith(('import ', 'from ')):
                    imports.append(line)
            
            if imports:
                result.append("# === Imports ===\n")
                result.extend(imports[:20])  # Limit imports
                if len(imports) > 20:
                    result.append(f"# ... and {len(imports) - 20} more imports\n")
                result.append("\n")
            
            # Extract classes and functions
            in_class = False
            in_function = False
            indent_level = 0
            structures = []
            
            for i, line in enumerate(lines):
                stripped = line.lstrip()
                current_indent = len(line) - len(stripped)
                
                # Class definitions
                if stripped.startswith('class '):
                    in_class = True
                    indent_level = current_indent
                    structures.append(line)
                    # Get docstring if present
                    if i + 1 < len(lines) and '"""' in lines[i + 1]:
                        structures.append(lines[i + 1])
                
                # Function definitions
                elif stripped.startswith('def '):
                    in_function = True
                    indent_level = current_indent
                    structures.append(line)
                    # Get docstring if present
                    if i + 1 < len(lines) and '"""' in lines[i + 1]:
                        structures.append(lines[i + 1])
                
                # Reset when dedent
                elif (in_class or in_function) and current_indent <= indent_level and stripped:
                    in_class = False
                    in_function = False
            
            if structures:
                result.append("# === Structure ===\n")
                result.extend(structures[:30])  # Limit structures
                if len(structures) > 30:
                    result.append(f"\n# ... and {len(structures) - 30} more structures\n")
            
            # Add implementation placeholder
            result.append("\n# === Implementation ===\n")
            result.append("# ... implementation details omitted ...\n")
            result.append("# See original file for complete code\n")
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error in Python minimizer: {e}")
            return self.minimize_generic(file_path)
    
    def minimize_javascript(self, file_path: Path) -> str:
        """
        Minimize JavaScript/TypeScript code file.
        
        Args:
            file_path: Path to JS/TS file
            
        Returns:
            Minimized JavaScript code
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f"// Minimized version of: {file_path.name}\n")
            result.append(f"// Original: {len(lines)} lines\n\n")
            
            # Keep imports/requires
            imports = []
            for line in lines:
                if re.match(r'^\s*(import|export|require|from)\s+', line):
                    imports.append(line)
            
            if imports:
                result.append("// === Imports/Exports ===\n")
                result.extend(imports[:20])
                if len(imports) > 20:
                    result.append(f"// ... and {len(imports) - 20} more imports\n")
                result.append("\n")
            
            # Extract classes and functions
            structures = []
            for line in lines:
                if re.match(r'^\s*(class|function|const|let|var)\s+\w+\s*[=({]', line):
                    structures.append(line)
                elif re.match(r'^\s*(async\s+)?function\s+\w+', line):
                    structures.append(line)
                elif re.match(r'^\s*export\s+(default\s+)?(class|function)', line):
                    structures.append(line)
            
            if structures:
                result.append("// === Structure ===\n")
                result.extend(structures[:30])
                if len(structures) > 30:
                    result.append(f"\n// ... and {len(structures) - 30} more structures\n")
            
            result.append("\n// === Implementation ===\n")
            result.append("// ... implementation details omitted ...\n")
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error in JavaScript minimizer: {e}")
            return self.minimize_generic(file_path)
    
    def minimize_java(self, file_path: Path) -> str:
        """
        Minimize Java/Kotlin code file.
        
        Args:
            file_path: Path to Java/Kotlin file
            
        Returns:
            Minimized Java code
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f"// Minimized version of: {file_path.name}\n")
            result.append(f"// Original: {len(lines)} lines\n\n")
            
            # Keep package and imports
            for line in lines:
                if line.strip().startswith(('package ', 'import ')):
                    result.append(line)
            
            result.append("\n")
            
            # Extract class and method signatures
            structures = []
            for line in lines:
                if re.match(r'^\s*(public|private|protected)?\s*(static)?\s*(class|interface|enum)', line):
                    structures.append(line)
                elif re.match(r'^\s*(public|private|protected)?\s*(static)?\s*\w+\s+\w+\s*\(', line):
                    structures.append(line)
            
            if structures:
                result.append("// === Structure ===\n")
                result.extend(structures[:30])
                if len(structures) > 30:
                    result.append(f"\n// ... and {len(structures) - 30} more structures\n")
            
            result.append("\n// === Implementation ===\n")
            result.append("// ... implementation details omitted ...\n")
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error in Java minimizer: {e}")
            return self.minimize_generic(file_path)
    
    def minimize_c_cpp(self, file_path: Path) -> str:
        """
        Minimize C/C++ code file.
        
        Args:
            file_path: Path to C/C++ file
            
        Returns:
            Minimized C/C++ code
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f"// Minimized version of: {file_path.name}\n")
            result.append(f"// Original: {len(lines)} lines\n\n")
            
            # Keep includes
            for line in lines:
                if line.strip().startswith('#include'):
                    result.append(line)
            
            result.append("\n")
            
            # Extract function signatures and class definitions
            structures = []
            for line in lines:
                # Function signatures
                if re.match(r'^\s*\w+\s+\w+\s*\([^)]*\)\s*{?', line):
                    structures.append(line)
                # Class definitions
                elif re.match(r'^\s*class\s+\w+', line):
                    structures.append(line)
                # Struct definitions
                elif re.match(r'^\s*struct\s+\w+', line):
                    structures.append(line)
            
            if structures:
                result.append("// === Structure ===\n")
                result.extend(structures[:30])
                if len(structures) > 30:
                    result.append(f"\n// ... and {len(structures) - 30} more structures\n")
            
            result.append("\n// === Implementation ===\n")
            result.append("// ... implementation details omitted ...\n")
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error in C/C++ minimizer: {e}")
            return self.minimize_generic(file_path)
    
    def minimize_generic(self, file_path: Path) -> str:
        """
        Generic code minimizer for unrecognized languages.
        
        Args:
            file_path: Path to code file
            
        Returns:
            Minimized code
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f"# Minimized version of: {file_path.name}\n")
            result.append(f"# Original: {len(lines)} lines\n")
            result.append(f"# Language: {file_path.suffix}\n\n")
            
            # Keep first 20 lines for context
            result.append("# === First 20 lines ===\n")
            result.extend(lines[:20])
            
            if len(lines) > 40:
                result.append(f"\n# ... {len(lines) - 40} lines omitted ...\n\n")
                
                # Keep last 20 lines
                result.append("# === Last 20 lines ===\n")
                result.extend(lines[-20:])
            elif len(lines) > 20:
                result.append("\n# === Last lines ===\n")
                result.extend(lines[20:])
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error in generic minimizer: {e}")
            return self.create_stub_metadata(file_path)