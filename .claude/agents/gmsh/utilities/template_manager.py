"""
Template Manager for GMSH Agent
Manages parametric geometry templates
"""

import json
import yaml
from pathlib import Path
from typing import Dict, List, Any, Optional, Union
import re
import logging
from dataclasses import dataclass
from datetime import datetime


@dataclass
class TemplateParameter:
    """Represents a template parameter"""
    name: str
    description: str
    default_value: float
    min_value: Optional[float] = None
    max_value: Optional[float] = None
    unit: Optional[str] = None
    category: Optional[str] = None
    

@dataclass
class GeometryTemplate:
    """Represents a parametric geometry template"""
    name: str
    description: str
    file_path: Path
    parameters: List[TemplateParameter]
    category: str
    version: str = "1.0"
    author: str = "GMSH Agent"
    tags: List[str] = None
    

class TemplateManager:
    """Manages parametric geometry templates"""
    
    TEMPLATES_DIR = Path(__file__).parent.parent / 'templates'
    
    def __init__(self, templates_dir: Optional[Path] = None):
        """
        Initialize template manager
        
        Args:
            templates_dir: Custom templates directory
        """
        self.templates_dir = templates_dir or self.TEMPLATES_DIR
        self.logger = self._setup_logging()
        self.templates_cache = {}
        self._load_templates()
        
    def _setup_logging(self) -> logging.Logger:
        """Set up logging"""
        logger = logging.getLogger('TemplateManager')
        if not logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            )
            handler.setFormatter(formatter)
            logger.addHandler(handler)
            logger.setLevel(logging.INFO)
        return logger
        
    def _load_templates(self):
        """Load all available templates"""
        if not self.templates_dir.exists():
            self.logger.warning(f"Templates directory does not exist: {self.templates_dir}")
            return
            
        # Load .geo files
        for geo_file in self.templates_dir.glob("*.geo"):
            try:
                template = self._parse_geo_template(geo_file)
                self.templates_cache[template.name] = template
                self.logger.info(f"Loaded template: {template.name}")
            except Exception as e:
                self.logger.error(f"Failed to load template {geo_file}: {e}")
                
        # Load template metadata if exists
        metadata_file = self.templates_dir / "templates.json"
        if metadata_file.exists():
            with open(metadata_file, 'r') as f:
                metadata = json.load(f)
                self._update_template_metadata(metadata)
                
    def _parse_geo_template(self, geo_file: Path) -> GeometryTemplate:
        """
        Parse a .geo template file to extract parameters
        
        Args:
            geo_file: Path to .geo file
            
        Returns:
            GeometryTemplate object
        """
        parameters = []
        name = geo_file.stem
        description = ""
        category = "general"
        
        with open(geo_file, 'r') as f:
            content = f.read()
            
        # Extract description from header comments
        desc_match = re.search(r'^// (.+?)$', content, re.MULTILINE)
        if desc_match:
            description = desc_match.group(1)
            
        # Extract DefineNumber parameters
        pattern = r'(\w+)\s*=\s*DefineNumber\[([\d.-]+),\s*Name\s+"([^"]+)"(?:,\s*Choices\{[^}]+\})?\];'
        matches = re.findall(pattern, content)
        
        for match in matches:
            var_name, default_val, param_path = match
            
            # Parse parameter path (e.g., "Platform/Deck Length (m)")
            parts = param_path.split('/')
            if len(parts) > 1:
                param_category = parts[0]
                param_name = parts[1]
            else:
                param_category = "Parameters"
                param_name = param_path
                
            # Extract unit from name if present
            unit_match = re.search(r'\(([^)]+)\)', param_name)
            unit = unit_match.group(1) if unit_match else None
            if unit:
                param_name = param_name.replace(f" ({unit})", "")
                
            param = TemplateParameter(
                name=var_name,
                description=param_name,
                default_value=float(default_val),
                unit=unit,
                category=param_category
            )
            parameters.append(param)
            
        # Determine category from filename
        if "offshore" in name.lower() or "platform" in name.lower():
            category = "offshore"
        elif "mooring" in name.lower():
            category = "mooring"
        elif "seabed" in name.lower() or "terrain" in name.lower():
            category = "seabed"
            
        return GeometryTemplate(
            name=name,
            description=description,
            file_path=geo_file,
            parameters=parameters,
            category=category
        )
        
    def _update_template_metadata(self, metadata: Dict[str, Any]):
        """Update template metadata from JSON"""
        for name, data in metadata.items():
            if name in self.templates_cache:
                template = self.templates_cache[name]
                template.version = data.get('version', template.version)
                template.author = data.get('author', template.author)
                template.tags = data.get('tags', template.tags)
                
                # Update parameter constraints
                param_constraints = data.get('parameters', {})
                for param in template.parameters:
                    if param.name in param_constraints:
                        constraints = param_constraints[param.name]
                        param.min_value = constraints.get('min')
                        param.max_value = constraints.get('max')
                        
    def list_templates(self, category: Optional[str] = None) -> List[GeometryTemplate]:
        """
        List available templates
        
        Args:
            category: Filter by category
            
        Returns:
            List of templates
        """
        templates = list(self.templates_cache.values())
        
        if category:
            templates = [t for t in templates if t.category == category]
            
        return templates
        
    def get_template(self, name: str) -> Optional[GeometryTemplate]:
        """
        Get template by name
        
        Args:
            name: Template name
            
        Returns:
            Template or None if not found
        """
        return self.templates_cache.get(name)
        
    def generate_geometry(
        self,
        template_name: str,
        parameters: Optional[Dict[str, float]] = None,
        output_file: Optional[Union[str, Path]] = None
    ) -> Path:
        """
        Generate geometry from template with custom parameters
        
        Args:
            template_name: Name of template to use
            parameters: Custom parameter values
            output_file: Output file path
            
        Returns:
            Path to generated geometry file
        """
        template = self.get_template(template_name)
        if not template:
            raise ValueError(f"Template not found: {template_name}")
            
        parameters = parameters or {}
        
        # Validate parameters
        for param_name, value in parameters.items():
            param = next((p for p in template.parameters if p.name == param_name), None)
            if not param:
                self.logger.warning(f"Unknown parameter: {param_name}")
                continue
                
            if param.min_value is not None and value < param.min_value:
                raise ValueError(f"Parameter {param_name} = {value} is below minimum {param.min_value}")
            if param.max_value is not None and value > param.max_value:
                raise ValueError(f"Parameter {param_name} = {value} is above maximum {param.max_value}")
                
        # Read template file
        with open(template.file_path, 'r') as f:
            content = f.read()
            
        # Replace parameter values
        for param in template.parameters:
            value = parameters.get(param.name, param.default_value)
            # Replace DefineNumber statement
            pattern = rf'({param.name}\s*=\s*DefineNumber\[)[^,]+(,\s*Name\s+"[^"]+"[^]]*\];)'
            replacement = rf'\g<1>{value}\g<2>'
            content = re.sub(pattern, replacement, content)
            
        # Generate output filename if not provided
        if not output_file:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_file = Path(f"{template_name}_{timestamp}.geo")
        else:
            output_file = Path(output_file)
            
        # Write generated geometry
        with open(output_file, 'w') as f:
            f.write(content)
            
        self.logger.info(f"Generated geometry: {output_file}")
        return output_file
        
    def create_parameter_study(
        self,
        template_name: str,
        parameter_ranges: Dict[str, List[float]],
        base_parameters: Optional[Dict[str, float]] = None,
        output_dir: Optional[Path] = None
    ) -> List[Path]:
        """
        Create parameter study with multiple geometries
        
        Args:
            template_name: Template to use
            parameter_ranges: Parameter values to vary
            base_parameters: Base parameter values
            output_dir: Output directory
            
        Returns:
            List of generated geometry files
        """
        import itertools
        
        template = self.get_template(template_name)
        if not template:
            raise ValueError(f"Template not found: {template_name}")
            
        base_parameters = base_parameters or {}
        output_dir = output_dir or Path("parameter_study")
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create all parameter combinations
        param_names = list(parameter_ranges.keys())
        param_values = list(parameter_ranges.values())
        combinations = list(itertools.product(*param_values))
        
        generated_files = []
        
        for i, combo in enumerate(combinations):
            # Create parameter set
            params = base_parameters.copy()
            for j, param_name in enumerate(param_names):
                params[param_name] = combo[j]
                
            # Generate filename
            param_str = "_".join([f"{name}{value}" for name, value in zip(param_names, combo)])
            output_file = output_dir / f"{template_name}_case{i:03d}_{param_str}.geo"
            
            # Generate geometry
            try:
                geo_file = self.generate_geometry(template_name, params, output_file)
                generated_files.append(geo_file)
            except Exception as e:
                self.logger.error(f"Failed to generate case {i}: {e}")
                
        self.logger.info(f"Generated {len(generated_files)} geometries for parameter study")
        return generated_files
        
    def validate_template(self, template_name: str) -> Dict[str, Any]:
        """
        Validate a template by checking syntax and generating test geometry
        
        Args:
            template_name: Template to validate
            
        Returns:
            Validation results
        """
        template = self.get_template(template_name)
        if not template:
            return {'valid': False, 'error': f"Template not found: {template_name}"}
            
        results = {
            'valid': True,
            'template': template_name,
            'parameters': len(template.parameters),
            'warnings': [],
            'errors': []
        }
        
        # Check for duplicate parameters
        param_names = [p.name for p in template.parameters]
        duplicates = [name for name in param_names if param_names.count(name) > 1]
        if duplicates:
            results['warnings'].append(f"Duplicate parameters: {duplicates}")
            
        # Check for missing constraints
        unconstrained = [p.name for p in template.parameters 
                        if p.min_value is None or p.max_value is None]
        if unconstrained:
            results['warnings'].append(f"Parameters without constraints: {unconstrained}")
            
        # Try to generate with default parameters
        try:
            import tempfile
            with tempfile.NamedTemporaryFile(suffix='.geo', delete=True) as tmp:
                self.generate_geometry(template_name, output_file=tmp.name)
                results['test_generation'] = 'success'
        except Exception as e:
            results['valid'] = False
            results['errors'].append(f"Failed to generate test geometry: {e}")
            results['test_generation'] = 'failed'
            
        return results
        
    def export_template_docs(self, output_file: Union[str, Path], format: str = 'markdown'):
        """
        Export template documentation
        
        Args:
            output_file: Output file path
            format: Documentation format ('markdown', 'json', 'html')
        """
        output_file = Path(output_file)
        
        if format == 'markdown':
            self._export_markdown_docs(output_file)
        elif format == 'json':
            self._export_json_docs(output_file)
        elif format == 'html':
            self._export_html_docs(output_file)
        else:
            raise ValueError(f"Unsupported format: {format}")
            
    def _export_markdown_docs(self, output_file: Path):
        """Export documentation in Markdown format"""
        lines = ["# GMSH Agent Geometry Templates\n\n"]
        
        # Group templates by category
        categories = {}
        for template in self.templates_cache.values():
            if template.category not in categories:
                categories[template.category] = []
            categories[template.category].append(template)
            
        for category, templates in sorted(categories.items()):
            lines.append(f"## {category.title()} Templates\n\n")
            
            for template in sorted(templates, key=lambda t: t.name):
                lines.append(f"### {template.name}\n\n")
                lines.append(f"{template.description}\n\n")
                lines.append(f"**File:** `{template.file_path.name}`\n")
                lines.append(f"**Version:** {template.version}\n")
                lines.append(f"**Author:** {template.author}\n\n")
                
                if template.parameters:
                    lines.append("#### Parameters\n\n")
                    lines.append("| Parameter | Description | Default | Unit | Min | Max |\n")
                    lines.append("|-----------|-------------|---------|------|-----|-----|\n")
                    
                    for param in template.parameters:
                        min_val = param.min_value if param.min_value is not None else "-"
                        max_val = param.max_value if param.max_value is not None else "-"
                        unit = param.unit or "-"
                        lines.append(
                            f"| {param.name} | {param.description} | {param.default_value} | "
                            f"{unit} | {min_val} | {max_val} |\n"
                        )
                    lines.append("\n")
                    
        with open(output_file, 'w') as f:
            f.writelines(lines)
            
    def _export_json_docs(self, output_file: Path):
        """Export documentation in JSON format"""
        docs = {}
        
        for name, template in self.templates_cache.items():
            docs[name] = {
                'description': template.description,
                'category': template.category,
                'version': template.version,
                'author': template.author,
                'file': str(template.file_path),
                'parameters': [
                    {
                        'name': p.name,
                        'description': p.description,
                        'default': p.default_value,
                        'min': p.min_value,
                        'max': p.max_value,
                        'unit': p.unit,
                        'category': p.category
                    }
                    for p in template.parameters
                ]
            }
            
        with open(output_file, 'w') as f:
            json.dump(docs, f, indent=2)
            
    def _export_html_docs(self, output_file: Path):
        """Export documentation in HTML format"""
        html = """<!DOCTYPE html>
<html>
<head>
    <title>GMSH Agent Geometry Templates</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        h1 { color: #333; }
        h2 { color: #666; border-bottom: 2px solid #ddd; }
        h3 { color: #888; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f4f4f4; }
        .template { margin-bottom: 40px; }
        .metadata { color: #666; font-size: 0.9em; }
    </style>
</head>
<body>
    <h1>GMSH Agent Geometry Templates</h1>
"""
        
        # Group templates by category
        categories = {}
        for template in self.templates_cache.values():
            if template.category not in categories:
                categories[template.category] = []
            categories[template.category].append(template)
            
        for category, templates in sorted(categories.items()):
            html += f"    <h2>{category.title()} Templates</h2>\n"
            
            for template in sorted(templates, key=lambda t: t.name):
                html += f"""    <div class="template">
        <h3>{template.name}</h3>
        <p>{template.description}</p>
        <div class="metadata">
            <strong>File:</strong> {template.file_path.name}<br>
            <strong>Version:</strong> {template.version}<br>
            <strong>Author:</strong> {template.author}
        </div>
"""
                
                if template.parameters:
                    html += """        <h4>Parameters</h4>
        <table>
            <tr>
                <th>Parameter</th>
                <th>Description</th>
                <th>Default</th>
                <th>Unit</th>
                <th>Min</th>
                <th>Max</th>
            </tr>
"""
                    for param in template.parameters:
                        min_val = param.min_value if param.min_value is not None else "-"
                        max_val = param.max_value if param.max_value is not None else "-"
                        unit = param.unit or "-"
                        html += f"""            <tr>
                <td>{param.name}</td>
                <td>{param.description}</td>
                <td>{param.default_value}</td>
                <td>{unit}</td>
                <td>{min_val}</td>
                <td>{max_val}</td>
            </tr>
"""
                    html += "        </table>\n"
                    
                html += "    </div>\n"
                
        html += """</body>
</html>"""
        
        with open(output_file, 'w') as f:
            f.write(html)


if __name__ == "__main__":
    # Example usage
    manager = TemplateManager()
    
    # List available templates
    print("Available templates:")
    for template in manager.list_templates():
        print(f"  - {template.name}: {template.description}")
        
    # Generate documentation
    manager.export_template_docs("template_docs.md", format="markdown")
    print("\nTemplate documentation exported to template_docs.md")
    
    # Example: Generate offshore platform with custom parameters
    if manager.get_template("offshore_platform"):
        params = {
            'deck_length': 50.0,
            'deck_width': 40.0,
            'water_depth': 150.0,
            'num_legs': 6
        }
        
        geo_file = manager.generate_geometry("offshore_platform", params)
        print(f"\nGenerated geometry: {geo_file}")
        
    # Example: Create parameter study
    if manager.get_template("mooring_line"):
        param_ranges = {
            'total_length': [1000, 1500, 2000],
            'pretension': [300, 500, 700]
        }
        
        files = manager.create_parameter_study(
            "mooring_line",
            param_ranges,
            output_dir=Path("mooring_study")
        )
        print(f"\nGenerated {len(files)} geometries for parameter study")