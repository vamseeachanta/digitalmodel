"""
ABOUTME: OrcaFlex Model Generator - Component-based model assembly from lookup tables
         and templates for automated OrcaFlex model generation.
"""

from pathlib import Path
from typing import Dict, Any, List, Optional, Union
import pandas as pd
import yaml
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class ComponentNotFoundError(Exception):
    """Raised when a component cannot be found in lookup tables."""
    pass


class ValidationError(Exception):
    """Raised when model validation fails."""
    pass


class OrcaFlexModelGenerator:
    """
    Generate OrcaFlex models by assembling components from lookup tables.

    This class implements the Component Assembly pattern where models are built
    by combining pre-defined, validated components (vessels, lines, materials, etc.)
    rather than modifying template files directly.

    Features:
    - Component lookup from CSV databases
    - Model assembly from configuration
    - Validation against engineering standards
    - Integration with converter, runner, post-processor
    - Template-based generation

    Example:
        >>> generator = OrcaFlexModelGenerator()
        >>>
        >>> # List available vessels
        >>> vessels = generator.list_components("vessels")
        >>> print(vessels)  # ['FPSO_P50', 'FPSO_P70', ...]
        >>>
        >>> # Generate model from template
        >>> model = generator.generate_from_template(
        ...     template="risers/scr_catenary",
        ...     config="my_config.yml",
        ...     output="my_model.yml"
        ... )
    """

    def __init__(self,
                 components_dir: Optional[Path] = None,
                 templates_dir: Optional[Path] = None):
        """
        Initialize the model generator.

        Args:
            components_dir: Path to component library (CSV files)
            templates_dir: Path to template library
        """
        self.repo_root = Path(__file__).parent.parent.parent.parent.parent

        # Set default paths
        if components_dir is None:
            self.components_dir = self.repo_root / "docs" / "modules" / "orcaflex" / "templates" / "components"
        else:
            self.components_dir = Path(components_dir)

        if templates_dir is None:
            self.templates_dir = self.repo_root / "docs" / "modules" / "orcaflex" / "templates"
        else:
            self.templates_dir = Path(templates_dir)

        # Component cache
        self._component_cache: Dict[str, pd.DataFrame] = {}

        logger.info(f"OrcaFlexModelGenerator initialized")
        logger.info(f"  Components: {self.components_dir}")
        logger.info(f"  Templates: {self.templates_dir}")

    def list_components(self, category: str) -> List[str]:
        """
        List available components in a category.

        Args:
            category: Component category (e.g., "vessels", "lines/risers", "materials/steel")

        Returns:
            List of component IDs

        Example:
            >>> generator.list_components("vessels")
            ['FPSO_P50', 'FPSO_P70', 'Drillship_DP3', ...]
        """
        components = []

        # Handle subcategories like "lines/risers"
        category_path = self.components_dir / category.replace("/", "\\")

        if not category_path.exists():
            logger.warning(f"Category not found: {category}")
            return []

        # Find all CSV files in category
        for csv_file in category_path.glob("*.csv"):
            df = self._load_component_csv(category, csv_file.stem)
            # First column is assumed to be the ID column
            if len(df.columns) > 0:
                id_col = df.columns[0]
                components.extend(df[id_col].tolist())

        return components

    def get_component(self, category: str, component_id: str) -> Dict[str, Any]:
        """
        Get component properties from lookup table.

        Args:
            category: Component category (e.g., "vessels", "lines/risers")
            component_id: Component ID to lookup

        Returns:
            Dictionary of component properties

        Raises:
            ComponentNotFoundError: If component not found in lookup tables

        Example:
            >>> vessel = generator.get_component("vessels", "FPSO_P50")
            >>> print(vessel['LOA'])  # 300.0
            >>> print(vessel['Displacement'])  # 200000.0
        """
        # Search all CSV files in category for this component
        category_path = self.components_dir / category.replace("/", "\\")

        if not category_path.exists():
            raise ComponentNotFoundError(f"Category not found: {category}")

        for csv_file in category_path.glob("*.csv"):
            df = self._load_component_csv(category, csv_file.stem)

            # First column is ID column
            id_col = df.columns[0]

            # Search for component
            matches = df[df[id_col] == component_id]

            if len(matches) > 0:
                # Return first match as dictionary
                return matches.iloc[0].to_dict()

        raise ComponentNotFoundError(
            f"Component '{component_id}' not found in category '{category}'"
        )

    def _load_component_csv(self, category: str, filename: str) -> pd.DataFrame:
        """Load component CSV file with caching."""
        cache_key = f"{category}/{filename}"

        if cache_key not in self._component_cache:
            csv_path = self.components_dir / category.replace("/", "\\") / f"{filename}.csv"

            if not csv_path.exists():
                raise FileNotFoundError(f"Component file not found: {csv_path}")

            self._component_cache[cache_key] = pd.read_csv(csv_path)

        return self._component_cache[cache_key]

    def generate_from_template(self,
                               template: str,
                               config: Union[str, Path, Dict[str, Any]],
                               output: Optional[Union[str, Path]] = None) -> Dict[str, Any]:
        """
        Generate OrcaFlex model from template and configuration.

        Args:
            template: Template name (e.g., "risers/scr_catenary")
            config: Configuration (YAML file path or dictionary)
            output: Output file path (optional)

        Returns:
            Generated model as dictionary

        Example:
            >>> model = generator.generate_from_template(
            ...     template="risers/scr_catenary",
            ...     config="my_config.yml",
            ...     output="my_model.yml"
            ... )
        """
        # Load configuration
        if isinstance(config, (str, Path)):
            with open(config, 'r') as f:
                config_dict = yaml.safe_load(f)
        else:
            config_dict = config

        # Load template
        template_path = self.templates_dir / template.replace("/", "\\")
        template_file = template_path / "model_template.yml"

        if not template_file.exists():
            raise FileNotFoundError(f"Template not found: {template_file}")

        with open(template_file, 'r') as f:
            model_template = yaml.safe_load(f)

        # Assemble model from components
        model = self._assemble_model(model_template, config_dict)

        # Add metadata
        model['_metadata'] = {
            'generated_by': 'OrcaFlexModelGenerator',
            'template': template,
            'generated_at': datetime.now().isoformat(),
            'generator_version': '1.0.0'
        }

        # Save if output specified
        if output:
            output_path = Path(output)
            output_path.parent.mkdir(parents=True, exist_ok=True)

            with open(output_path, 'w') as f:
                yaml.dump(model, f, default_flow_style=False, sort_keys=False)

            logger.info(f"Model saved to: {output_path}")

        return model

    def _assemble_model(self, template: Dict[str, Any], config: Dict[str, Any]) -> Dict[str, Any]:
        """
        Assemble model by looking up components and applying configuration.

        This is the core of the Component Assembly pattern.
        """
        model = template.copy()

        # Process vessel if specified with lookup
        if 'vessel' in config and 'lookup' in config['vessel']:
            vessel_id = config['vessel']['lookup']
            vessel_props = self.get_component("vessels", vessel_id)

            # Merge vessel properties into model
            if 'Vessel' not in model:
                model['Vessel'] = {}

            model['Vessel'].update(vessel_props)

            # Apply any overrides from config
            for key, value in config['vessel'].items():
                if key != 'lookup':
                    model['Vessel'][key] = value

        # Process riser/line if specified with lookup
        if 'riser' in config and 'lookup' in config['riser']:
            riser_id = config['riser']['lookup']
            riser_props = self.get_component("lines/risers", riser_id)

            # Merge riser properties into model
            if 'Line' not in model:
                model['Line'] = {}

            model['Line'].update(riser_props)

            # Apply overrides
            for key, value in config['riser'].items():
                if key != 'lookup':
                    model['Line'][key] = value

        # Process environment if specified with lookup
        if 'environment' in config and 'lookup' in config['environment']:
            env_id = config['environment']['lookup']
            env_props = self.get_component("environment", env_id)

            # Merge environment properties
            if 'Environment' not in model:
                model['Environment'] = {}

            model['Environment'].update(env_props)

            # Apply overrides
            for key, value in config['environment'].items():
                if key != 'lookup':
                    model['Environment'][key] = value

        # Apply model-level configuration
        if 'model' in config:
            for key, value in config['model'].items():
                model[key] = value

        # Apply analysis configuration
        if 'analysis' in config:
            if 'Analysis' not in model:
                model['Analysis'] = {}
            model['Analysis'].update(config['analysis'])

        return model

    def validate(self, model: Dict[str, Any]) -> Dict[str, Any]:
        """
        Validate generated model against engineering standards.

        Args:
            model: Model dictionary to validate

        Returns:
            Validation result with is_valid flag and any warnings/errors

        Example:
            >>> validation = generator.validate(model)
            >>> if validation['is_valid']:
            ...     print("Model is valid")
            >>> else:
            ...     print("Validation errors:", validation['errors'])
        """
        result = {
            'is_valid': True,
            'errors': [],
            'warnings': [],
            'checks_performed': []
        }

        # Basic structural validation
        result['checks_performed'].append('structure')

        # Check for required top-level keys
        if 'Vessel' not in model and 'Line' not in model:
            result['errors'].append("Model must contain at least a Vessel or Line")
            result['is_valid'] = False

        # Validate vessel if present
        if 'Vessel' in model:
            result['checks_performed'].append('vessel')
            vessel = model['Vessel']

            # Check required vessel properties
            required_props = ['LOA', 'Breadth', 'Draught', 'Displacement']
            for prop in required_props:
                if prop not in vessel:
                    result['warnings'].append(f"Vessel missing recommended property: {prop}")

        # Validate line if present
        if 'Line' in model:
            result['checks_performed'].append('line')
            line = model['Line']

            # Check required line properties
            required_props = ['OD', 'Mass', 'EI', 'EA']
            for prop in required_props:
                if prop not in line:
                    result['warnings'].append(f"Line missing recommended property: {prop}")

        # Validate environment if present
        if 'Environment' in model:
            result['checks_performed'].append('environment')
            env = model['Environment']

            # Check for wave parameters
            if 'Hs' in env and env['Hs'] > 20.0:
                result['warnings'].append(f"Significant wave height ({env['Hs']}m) is very high")

        return result

    def add_component(self,
                      category: str,
                      component_id: str,
                      properties: Dict[str, Any]) -> None:
        """
        Add a custom component to the library.

        Args:
            category: Component category (e.g., "vessels")
            component_id: Unique component identifier
            properties: Component properties

        Example:
            >>> generator.add_component(
            ...     category="vessels",
            ...     component_id="My_Custom_FPSO",
            ...     properties={
            ...         "LOA": 310.0,
            ...         "Breadth": 62.0,
            ...         "Displacement": 220000.0,
            ...         # ... more properties
            ...     }
            ... )
        """
        # Determine which CSV file to add to
        category_path = self.components_dir / category.replace("/", "\\")

        if not category_path.exists():
            raise ValueError(f"Category not found: {category}")

        # For simplicity, add to first CSV file found, or create new one
        csv_files = list(category_path.glob("*.csv"))

        if len(csv_files) == 0:
            # Create new custom.csv file
            csv_path = category_path / "custom.csv"
            df = pd.DataFrame([properties])
        else:
            # Add to existing file
            csv_path = csv_files[0]
            df = pd.read_csv(csv_path)

            # Check if component already exists
            id_col = df.columns[0]
            if component_id in df[id_col].values:
                raise ValueError(f"Component '{component_id}' already exists")

            # Append new component
            df = pd.concat([df, pd.DataFrame([properties])], ignore_index=True)

        # Save updated CSV
        df.to_csv(csv_path, index=False)

        # Clear cache for this category
        cache_keys_to_remove = [k for k in self._component_cache.keys() if k.startswith(category)]
        for key in cache_keys_to_remove:
            del self._component_cache[key]

        logger.info(f"Added component '{component_id}' to {category}")


# Convenience function
def generate_model(template: str,
                   config: Union[str, Path, Dict[str, Any]],
                   output: Optional[Union[str, Path]] = None) -> Dict[str, Any]:
    """
    Convenience function to generate a model in one line.

    Example:
        >>> from digitalmodel.modules.orcaflex.model_generator import generate_model
        >>> model = generate_model("risers/scr_catenary", "my_config.yml", "my_model.yml")
    """
    generator = OrcaFlexModelGenerator()
    return generator.generate_from_template(template, config, output)


__all__ = [
    'OrcaFlexModelGenerator',
    'ComponentNotFoundError',
    'ValidationError',
    'generate_model'
]
