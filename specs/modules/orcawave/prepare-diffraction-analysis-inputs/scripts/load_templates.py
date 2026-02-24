#!/usr/bin/env python3
"""
Load and parse OrcaWave go-by template files.
Identifies variable placeholders and creates template registry.
"""

import yaml
import re
from pathlib import Path
from typing import Dict, Any, List, Set
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class TemplateLoader:
    """Loads and manages OrcaWave template files."""
    
    def __init__(self, template_dir: Path = None):
        """Initialize template loader.
        
        Args:
            template_dir: Directory containing template files
        """
        if template_dir is None:
            template_dir = Path(__file__).parent.parent / "outputs" / "orcawave_configs" / "go-by"
        self.template_dir = Path(template_dir)
        self.templates = {}
        self.variable_patterns = {
            'simple': re.compile(r'\{\{(\w+)\}\}'),  # {{variable}}
            'nested': re.compile(r'\{\{(\w+)\.(\w+)\}\}'),  # {{object.property}}
            'function': re.compile(r'\{\{(\w+)\((.*?)\)\}\}'),  # {{function(args)}}
        }
        
    def load_template(self, template_name: str) -> Dict[str, Any]:
        """Load a template file by name.
        
        Args:
            template_name: Name of template file (with or without .yml extension)
            
        Returns:
            Dictionary containing template data
        """
        if not template_name.endswith('.yml'):
            template_name += '.yml'
            
        template_path = self.template_dir / template_name
        
        if not template_path.exists():
            raise FileNotFoundError(f"Template not found: {template_path}")
            
        logger.info(f"Loading template: {template_path}")
        
        with open(template_path, 'r', encoding='utf-8') as f:
            template_data = yaml.safe_load(f)
            
        self.templates[template_name] = template_data
        return template_data
        
    def identify_variables(self, template_data: Any, path: str = "") -> Set[str]:
        """Identify all variable placeholders in template.
        
        Args:
            template_data: Template data to scan
            path: Current path in template hierarchy
            
        Returns:
            Set of variable names found
        """
        variables = set()
        
        if isinstance(template_data, dict):
            for key, value in template_data.items():
                current_path = f"{path}.{key}" if path else key
                variables.update(self.identify_variables(value, current_path))
                
        elif isinstance(template_data, list):
            for i, item in enumerate(template_data):
                current_path = f"{path}[{i}]"
                variables.update(self.identify_variables(item, current_path))
                
        elif isinstance(template_data, str):
            # Check for variable patterns
            for pattern_type, pattern in self.variable_patterns.items():
                matches = pattern.findall(template_data)
                if matches:
                    if pattern_type == 'simple':
                        variables.update(matches)
                    elif pattern_type == 'nested':
                        variables.update([f"{m[0]}.{m[1]}" for m in matches])
                    elif pattern_type == 'function':
                        variables.update([m[0] for m in matches])
                        
        return variables
        
    def create_registry(self) -> Dict[str, Dict[str, Any]]:
        """Create a registry of all loaded templates.
        
        Returns:
            Dictionary mapping template names to their metadata
        """
        registry = {}
        
        for template_name, template_data in self.templates.items():
            variables = self.identify_variables(template_data)
            
            registry[template_name] = {
                'path': str(self.template_dir / template_name),
                'variables': sorted(list(variables)),
                'variable_count': len(variables),
                'sections': list(template_data.keys()) if isinstance(template_data, dict) else [],
            }
            
        return registry
        
    def load_all_templates(self) -> Dict[str, Dict[str, Any]]:
        """Load all template files from template directory.
        
        Returns:
            Dictionary of all loaded templates
        """
        template_files = list(self.template_dir.glob('*.yml'))
        
        for template_file in template_files:
            template_name = template_file.name
            self.load_template(template_name)
            
        logger.info(f"Loaded {len(self.templates)} templates")
        return self.templates
        
    def get_template_info(self, template_name: str) -> Dict[str, Any]:
        """Get information about a specific template.
        
        Args:
            template_name: Name of template
            
        Returns:
            Dictionary with template information
        """
        if template_name not in self.templates:
            self.load_template(template_name)
            
        template_data = self.templates[template_name]
        variables = self.identify_variables(template_data)
        
        info = {
            'name': template_name,
            'path': str(self.template_dir / template_name),
            'variables': sorted(list(variables)),
            'sections': list(template_data.keys()) if isinstance(template_data, dict) else [],
            'body_count': len(template_data.get('Bodies', [])) if 'Bodies' in template_data else 0,
            'has_wave_data': 'PeriodOrFrequency' in str(template_data),
            'has_qtf': 'QTF' in str(template_data),
        }
        
        return info


def main():
    """Test template loader functionality."""
    loader = TemplateLoader()
    
    # Load go-by template
    try:
        template = loader.load_template('go-by-template')
        print(f"Successfully loaded template with {len(template)} top-level keys")
        
        # Identify variables (if any)
        variables = loader.identify_variables(template)
        if variables:
            print(f"Found {len(variables)} variable placeholders:")
            for var in sorted(variables):
                print(f"  - {var}")
        else:
            print("No variable placeholders found in template")
            
        # Get template info
        info = loader.get_template_info('go-by-template.yml')
        print("\nTemplate Information:")
        for key, value in info.items():
            if isinstance(value, list) and len(value) > 5:
                print(f"  {key}: [{value[0]}, {value[1]}, ... ({len(value)} items)]")
            else:
                print(f"  {key}: {value}")
                
        # Create registry
        registry = loader.create_registry()
        print(f"\nTemplate Registry created with {len(registry)} templates")
        
    except FileNotFoundError as e:
        print(f"Error: {e}")
        print("Please ensure go-by-template.yml exists in the go-by directory")


if __name__ == "__main__":
    main()