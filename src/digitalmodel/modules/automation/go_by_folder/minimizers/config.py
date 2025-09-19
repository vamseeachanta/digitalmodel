"""
Configuration file minimizer for Create Go-By Folder Tool
"""

import json
import re
from pathlib import Path
from typing import Optional, Dict, Any
import logging

from .base import BaseMinimizer

logger = logging.getLogger(__name__)


class ConfigMinimizer(BaseMinimizer):
    """Minimizer for configuration files."""
    
    CONFIG_EXTENSIONS = {
        # Data formats
        '.json', '.yaml', '.yml', '.toml', '.ini', '.cfg', '.conf',
        # XML
        '.xml', '.xsl', '.xslt', '.xsd',
        # Properties
        '.properties', '.props',
        # Environment
        '.env', '.env.local', '.env.production',
        # Docker
        '.dockerfile', 'Dockerfile',
        # Build files
        '.gradle', '.maven', '.npmrc', '.yarnrc',
        # Git
        '.gitignore', '.gitconfig', '.gitmodules', '.gitattributes',
        # Editor
        '.editorconfig', '.prettierrc', '.eslintrc'
    }
    
    def __init__(self, max_size: int = 10240):
        """
        Initialize config minimizer.
        
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
            True if this is a config file
        """
        # Check extension
        if file_path.suffix.lower() in self.CONFIG_EXTENSIONS:
            return True
        
        # Check filename
        filename = file_path.name.lower()
        return filename in self.CONFIG_EXTENSIONS or filename in [
            'dockerfile', 'makefile', 'rakefile', 'gemfile',
            'package.json', 'tsconfig.json', 'webpack.config.js',
            'requirements.txt', 'setup.py', 'setup.cfg', 'pyproject.toml'
        ]
    
    def minimize(self, file_path: Path, preserve: bool = False) -> Optional[str]:
        """
        Minimize config file content.
        
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
            filename = file_path.name.lower()
            
            if ext == '.json' or filename.endswith('.json'):
                return self.minimize_json(file_path)
            elif ext in ['.yaml', '.yml']:
                return self.minimize_yaml(file_path)
            elif ext == '.xml':
                return self.minimize_xml(file_path)
            elif ext in ['.ini', '.cfg', '.conf']:
                return self.minimize_ini(file_path)
            elif ext == '.toml' or filename == 'pyproject.toml':
                return self.minimize_toml(file_path)
            else:
                return self.minimize_generic_config(file_path)
                
        except Exception as e:
            logger.warning(f"Error minimizing config file {file_path}: {e}")
            return self.create_stub_metadata(file_path)
    
    def minimize_json(self, file_path: Path) -> str:
        """
        Minimize JSON configuration file.
        
        Args:
            file_path: Path to JSON file
            
        Returns:
            Minimized JSON content
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            # Create minimized version
            minimized = self._minimize_json_object(data, max_depth=3)
            
            # Add header comment
            result = f'// Minimized version of: {file_path.name}\n'
            result += f'// Original file size: {file_path.stat().st_size} bytes\n'
            result += json.dumps(minimized, indent=2, default=str)
            
            return result
            
        except json.JSONDecodeError as e:
            logger.warning(f"Invalid JSON in {file_path}: {e}")
            return self.minimize_generic_config(file_path)
        except Exception as e:
            logger.warning(f"Error minimizing JSON: {e}")
            return self.minimize_generic_config(file_path)
    
    def _minimize_json_object(self, obj: Any, max_depth: int = 3, current_depth: int = 0) -> Any:
        """
        Recursively minimize JSON object.
        
        Args:
            obj: JSON object to minimize
            max_depth: Maximum depth to preserve
            current_depth: Current recursion depth
            
        Returns:
            Minimized object
        """
        if current_depth >= max_depth:
            # Replace deep structures with placeholders
            if isinstance(obj, dict):
                return {k: "..." for k in list(obj.keys())[:3]}
            elif isinstance(obj, list):
                return ["..." if len(obj) > 0 else None]
            else:
                return "..."
        
        if isinstance(obj, dict):
            result = {}
            for key in list(obj.keys())[:20]:  # Limit keys
                result[key] = self._minimize_json_object(
                    obj[key], max_depth, current_depth + 1
                )
            if len(obj) > 20:
                result["__truncated__"] = f"{len(obj) - 20} more keys"
            return result
            
        elif isinstance(obj, list):
            if len(obj) <= 5:
                return [
                    self._minimize_json_object(item, max_depth, current_depth + 1)
                    for item in obj
                ]
            else:
                # Keep first and last items
                result = [
                    self._minimize_json_object(obj[0], max_depth, current_depth + 1)
                ]
                if len(obj) > 2:
                    result.append(f"... {len(obj) - 2} items ...")
                if len(obj) > 1:
                    result.append(
                        self._minimize_json_object(obj[-1], max_depth, current_depth + 1)
                    )
                return result
        
        elif isinstance(obj, str) and len(obj) > 100:
            return obj[:100] + "..."
        
        else:
            return obj
    
    def minimize_yaml(self, file_path: Path) -> str:
        """
        Minimize YAML configuration file.
        
        Args:
            file_path: Path to YAML file
            
        Returns:
            Minimized YAML content
        """
        try:
            import yaml
            
            with open(file_path, 'r', encoding='utf-8') as f:
                data = yaml.safe_load(f)
            
            # Create minimized version
            minimized = self._minimize_json_object(data, max_depth=3)
            
            # Convert back to YAML
            result = f'# Minimized version of: {file_path.name}\n'
            result += f'# Original file size: {file_path.stat().st_size} bytes\n\n'
            result += yaml.dump(minimized, default_flow_style=False, sort_keys=False)
            
            return result
            
        except ImportError:
            logger.warning("PyYAML not available, using generic minimizer")
            return self.minimize_generic_config(file_path)
        except Exception as e:
            logger.warning(f"Error minimizing YAML: {e}")
            return self.minimize_generic_config(file_path)
    
    def minimize_xml(self, file_path: Path) -> str:
        """
        Minimize XML configuration file.
        
        Args:
            file_path: Path to XML file
            
        Returns:
            Minimized XML content
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f'<!-- Minimized version of: {file_path.name} -->\n')
            result.append(f'<!-- Original: {len(lines)} lines -->\n')
            
            # Keep XML declaration and root elements
            depth = 0
            max_depth = 2
            
            for line in lines[:100]:  # Limit lines
                # Count depth changes
                open_tags = len(re.findall(r'<[^/][^>]*>', line))
                close_tags = len(re.findall(r'</[^>]+>', line))
                
                if depth <= max_depth:
                    result.append(line)
                
                depth += open_tags - close_tags
                
                if depth < 0:
                    depth = 0
            
            if len(lines) > 100:
                result.append(f'<!-- ... {len(lines) - 100} lines omitted ... -->\n')
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error minimizing XML: {e}")
            return self.minimize_generic_config(file_path)
    
    def minimize_ini(self, file_path: Path) -> str:
        """
        Minimize INI/CFG configuration file.
        
        Args:
            file_path: Path to INI file
            
        Returns:
            Minimized INI content
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f'# Minimized version of: {file_path.name}\n')
            result.append(f'# Original: {len(lines)} lines\n\n')
            
            current_section = None
            section_lines = 0
            max_lines_per_section = 5
            
            for line in lines:
                stripped = line.strip()
                
                # Section header
                if stripped.startswith('[') and stripped.endswith(']'):
                    current_section = stripped
                    section_lines = 0
                    result.append(line)
                
                # Comments
                elif stripped.startswith(('#', ';')):
                    if section_lines < 2:  # Keep first couple comments
                        result.append(line)
                        section_lines += 1
                
                # Key-value pairs
                elif '=' in stripped or ':' in stripped:
                    if section_lines < max_lines_per_section:
                        # Replace values with placeholders for sensitive keys
                        key = stripped.split('=')[0].strip() if '=' in stripped else stripped.split(':')[0].strip()
                        if any(sensitive in key.lower() for sensitive in ['password', 'secret', 'key', 'token']):
                            result.append(f'{key} = <redacted>\n')
                        else:
                            result.append(line)
                        section_lines += 1
                    elif section_lines == max_lines_per_section:
                        result.append('# ... more settings ...\n')
                        section_lines += 1
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error minimizing INI: {e}")
            return self.minimize_generic_config(file_path)
    
    def minimize_toml(self, file_path: Path) -> str:
        """
        Minimize TOML configuration file.
        
        Args:
            file_path: Path to TOML file
            
        Returns:
            Minimized TOML content
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f'# Minimized version of: {file_path.name}\n')
            result.append(f'# Original: {len(lines)} lines\n\n')
            
            # Keep table headers and first few values
            in_table = False
            table_lines = 0
            max_table_lines = 5
            
            for line in lines:
                stripped = line.strip()
                
                # Table header
                if stripped.startswith('[') and stripped.endswith(']'):
                    in_table = True
                    table_lines = 0
                    result.append(line)
                
                # Keep first few lines per table
                elif in_table and table_lines < max_table_lines:
                    result.append(line)
                    table_lines += 1
                
                elif in_table and table_lines == max_table_lines:
                    result.append('# ... more entries ...\n')
                    table_lines += 1
                
                # Empty line resets table
                elif not stripped:
                    in_table = False
                    if len(result) < 100:  # Limit total lines
                        result.append(line)
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error minimizing TOML: {e}")
            return self.minimize_generic_config(file_path)
    
    def minimize_generic_config(self, file_path: Path) -> str:
        """
        Generic config file minimizer.
        
        Args:
            file_path: Path to config file
            
        Returns:
            Minimized config
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            result = []
            result.append(f'# Minimized version of: {file_path.name}\n')
            result.append(f'# Original: {len(lines)} lines\n')
            result.append(f'# Type: {file_path.suffix}\n\n')
            
            # Keep non-empty, non-comment lines (limited)
            kept = 0
            max_keep = 50
            
            for line in lines:
                stripped = line.strip()
                if stripped and not stripped.startswith('#'):
                    if kept < max_keep:
                        result.append(line)
                        kept += 1
                elif kept < 10:  # Keep some comments for context
                    result.append(line)
            
            if len(lines) > max_keep:
                result.append(f'\n# ... {len(lines) - max_keep} lines omitted ...\n')
            
            return ''.join(result)
            
        except Exception as e:
            logger.warning(f"Error in generic config minimizer: {e}")
            return self.create_stub_metadata(file_path)