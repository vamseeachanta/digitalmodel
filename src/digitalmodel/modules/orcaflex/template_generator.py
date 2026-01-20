"""
OrcaFlex Template Generator CLI Tool.

Provides functionality for discovering, generating, and validating OrcaFlex
models from hybrid templates (base + variations).

Example:
    # List available templates
    python -m digitalmodel.modules.orcaflex.template_generator list-templates

    # Generate a model from template + variation
    python -m digitalmodel.modules.orcaflex.template_generator generate \\
        --template calm_buoy_hybrid --variation deep_water_200m --output model.yml

    # Validate a generated model
    python -m digitalmodel.modules.orcaflex.template_generator validate model.yml
"""

import argparse
import sys
import yaml
import logging
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass, field
from copy import deepcopy

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)

# Default templates directory (relative to this module)
DEFAULT_TEMPLATES_DIR = (
    Path(__file__).parent.parent.parent.parent.parent
    / "docs" / "modules" / "orcaflex" / "templates"
)


@dataclass
class TemplateInfo:
    """Information about a discovered template."""
    name: str
    category: str
    path: Path
    base_files: List[Path] = field(default_factory=list)
    variations: List[Path] = field(default_factory=list)
    cases: List[Path] = field(default_factory=list)
    has_readme: bool = False

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary representation."""
        return {
            'name': self.name,
            'category': self.category,
            'path': self.path,
            'base_files': self.base_files,
            'variations': self.variations,
            'cases': self.cases,
            'has_readme': self.has_readme
        }


class TemplateManager:
    """
    Manages discovery and organization of OrcaFlex hybrid templates.

    Templates follow the structure:
        templates/
            category/
                template_name_hybrid/
                    base/
                        base_model.yml
                    variations/
                        variation1.yml
                        variation2.yml
                    cases/
                        case_variation1.yml
                    README.md
    """

    def __init__(self, templates_dir: Union[str, Path] = None):
        """
        Initialize the template manager.

        Args:
            templates_dir: Path to templates directory. Defaults to
                          docs/modules/orcaflex/templates/
        """
        if templates_dir is None:
            self.templates_dir = DEFAULT_TEMPLATES_DIR
        else:
            self.templates_dir = Path(templates_dir)

        self._templates: Dict[str, TemplateInfo] = {}
        self._discovered = False

    def discover_templates(self) -> List[Dict[str, Any]]:
        """
        Discover all hybrid templates in the templates directory.

        Returns:
            List of template information dictionaries.
        """
        if not self.templates_dir.exists():
            logger.warning(f"Templates directory not found: {self.templates_dir}")
            return []

        templates = []

        # Iterate through category directories
        for category_dir in self.templates_dir.iterdir():
            if not category_dir.is_dir():
                continue

            # Skip non-template directories
            if category_dir.name in ('components', 'include_structure'):
                continue

            # Find hybrid template directories
            for template_dir in category_dir.iterdir():
                if template_dir.is_dir() and template_dir.name.endswith('_hybrid'):
                    template_info = self._parse_template_directory(
                        template_dir, category_dir.name
                    )
                    templates.append(template_info.to_dict())
                    self._templates[template_info.name] = template_info

        self._discovered = True
        return templates

    def _parse_template_directory(self, template_dir: Path, category: str) -> TemplateInfo:
        """
        Parse a template directory and extract its components.

        Args:
            template_dir: Path to the template directory.
            category: Category name (e.g., 'mooring_systems', 'risers').

        Returns:
            TemplateInfo with discovered components.
        """
        info = TemplateInfo(
            name=template_dir.name,
            category=category,
            path=template_dir
        )

        # Find base files
        base_dir = template_dir / 'base'
        if base_dir.exists():
            info.base_files = list(base_dir.glob('*.yml'))

        # Find variations
        variations_dir = template_dir / 'variations'
        if variations_dir.exists():
            info.variations = list(variations_dir.glob('*.yml'))

        # Find cases
        cases_dir = template_dir / 'cases'
        if cases_dir.exists():
            info.cases = list(cases_dir.glob('*.yml'))

        # Check for README
        info.has_readme = (template_dir / 'README.md').exists()

        return info

    def get_template(self, name: str) -> Optional[Dict[str, Any]]:
        """
        Get a specific template by name.

        Args:
            name: Template name (e.g., 'calm_buoy_hybrid').

        Returns:
            Template information dictionary or None if not found.
        """
        if not self._discovered:
            self.discover_templates()

        template = self._templates.get(name)
        if template:
            return template.to_dict()
        return None

    def list_templates(self) -> List[str]:
        """
        Get a list of all template names.

        Returns:
            List of template names.
        """
        if not self._discovered:
            self.discover_templates()

        return list(self._templates.keys())


class TemplateGenerator:
    """
    Generates OrcaFlex models by combining base templates with variations.

    Supports two output modes:
    1. Merged YAML: Combines base and variation into a single flat YAML file.
    2. Reference format: Creates a case file with BaseFile + IncludeFile references.
    """

    def merge_yaml_files(
        self,
        base_file: Union[str, Path],
        variation_file: Union[str, Path]
    ) -> Dict[str, Any]:
        """
        Merge a base YAML file with a variation file.

        The variation file's values override the base file's values.
        For lists (like Lines), the variation replaces by object Name.

        Args:
            base_file: Path to base template YAML file.
            variation_file: Path to variation YAML file.

        Returns:
            Merged dictionary.
        """
        base_file = Path(base_file)
        variation_file = Path(variation_file)

        with open(base_file, 'r') as f:
            base = yaml.safe_load(f) or {}

        with open(variation_file, 'r') as f:
            variation = yaml.safe_load(f) or {}

        return self._deep_merge(base, variation)

    def _deep_merge(
        self,
        base: Dict[str, Any],
        override: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Deep merge two dictionaries with special handling for OrcaFlex objects.

        For lists of objects (identified by 'Name' key), objects are matched
        by name and the entire object is replaced (OrcaFlex behavior).

        Args:
            base: Base dictionary.
            override: Override dictionary (takes precedence).

        Returns:
            Merged dictionary.
        """
        result = deepcopy(base)

        for key, value in override.items():
            if key not in result:
                result[key] = deepcopy(value)
            elif isinstance(value, dict) and isinstance(result[key], dict):
                result[key] = self._deep_merge(result[key], value)
            elif isinstance(value, list) and isinstance(result[key], list):
                result[key] = self._merge_object_lists(result[key], value)
            else:
                result[key] = deepcopy(value)

        return result

    def _merge_object_lists(
        self,
        base_list: List[Any],
        override_list: List[Any]
    ) -> List[Any]:
        """
        Merge lists of objects by matching on 'Name' key.

        Objects in override_list replace matching objects in base_list
        entirely (matching OrcaFlex's behavior where entire objects are replaced).

        Args:
            base_list: Base list.
            override_list: Override list.

        Returns:
            Merged list.
        """
        # Check if lists contain dictionaries with Name keys
        has_names = (
            all(isinstance(item, dict) and 'Name' in item for item in base_list if item)
            and all(isinstance(item, dict) and 'Name' in item for item in override_list if item)
        )

        if not has_names:
            # Simple replacement for non-named lists
            return deepcopy(override_list) if override_list else deepcopy(base_list)

        # Build lookup by name
        result = []
        base_by_name = {item['Name']: item for item in base_list if item}
        override_by_name = {item['Name']: item for item in override_list if item}

        # Process base items, replacing with overrides where available
        seen_names = set()
        for item in base_list:
            if item and 'Name' in item:
                name = item['Name']
                seen_names.add(name)
                if name in override_by_name:
                    result.append(deepcopy(override_by_name[name]))
                else:
                    result.append(deepcopy(item))

        # Add new items from override that weren't in base
        for item in override_list:
            if item and 'Name' in item:
                if item['Name'] not in seen_names:
                    result.append(deepcopy(item))

        return result

    def generate(
        self,
        base_file: Union[str, Path],
        variation_file: Union[str, Path],
        output_file: Union[str, Path],
        as_reference: bool = False
    ) -> Dict[str, Any]:
        """
        Generate a model file from base and variation.

        Args:
            base_file: Path to base template.
            variation_file: Path to variation file.
            output_file: Path to output file.
            as_reference: If True, generate BaseFile + IncludeFile reference format
                         instead of merged content.

        Returns:
            Result dictionary with 'success' and optional 'error' keys.
        """
        base_file = Path(base_file)
        variation_file = Path(variation_file)
        output_file = Path(output_file)

        try:
            # Validate inputs
            if not base_file.exists():
                return {'success': False, 'error': f'Base file not found: {base_file}'}
            if not variation_file.exists():
                return {'success': False, 'error': f'Variation file not found: {variation_file}'}

            # Create output directory if needed
            output_file.parent.mkdir(parents=True, exist_ok=True)

            if as_reference:
                # Generate reference-style case file
                content = self._generate_reference(base_file, variation_file, output_file)
            else:
                # Generate merged content
                content = self.merge_yaml_files(base_file, variation_file)

            # Write output
            with open(output_file, 'w') as f:
                f.write("%YAML 1.1\n")
                f.write(f"# Generated from: {base_file.name} + {variation_file.name}\n")
                f.write("---\n")
                yaml.dump(content, f, default_flow_style=False, sort_keys=False)

            logger.info(f"Generated model: {output_file}")
            return {'success': True, 'output_file': str(output_file)}

        except Exception as e:
            logger.error(f"Generation failed: {e}")
            return {'success': False, 'error': str(e)}

    def _generate_reference(
        self,
        base_file: Path,
        variation_file: Path,
        output_file: Path
    ) -> Dict[str, str]:
        """
        Generate reference-style content with relative paths.

        Args:
            base_file: Path to base template.
            variation_file: Path to variation file.
            output_file: Path to output file.

        Returns:
            Dictionary with BaseFile and IncludeFile keys.
        """
        # Calculate relative paths from output location
        try:
            base_relative = base_file.relative_to(output_file.parent)
        except ValueError:
            # If not relative, use relative from a common ancestor
            base_relative = Path('..') / base_file.relative_to(base_file.parent.parent)

        try:
            variation_relative = variation_file.relative_to(output_file.parent)
        except ValueError:
            variation_relative = Path('..') / variation_file.relative_to(variation_file.parent.parent)

        return {
            'BaseFile': str(base_relative).replace('\\', '/'),
            'IncludeFile': str(variation_relative).replace('\\', '/')
        }


class ModelValidator:
    """
    Validates OrcaFlex model files for syntax and structure.

    Supports three validation levels:
    1. Syntax: Valid YAML.
    2. Structure: Contains required OrcaFlex sections.
    3. OrcaFlex: Load with OrcFxAPI and optionally run statics.
    """

    REQUIRED_SECTIONS = ['General', 'Environment']
    OPTIONAL_SECTIONS = ['Lines', 'LineTypes', '6DBuoys', 'Vessels', 'VesselTypes', 'Constraints']

    def validate_syntax(self, file_path: Union[str, Path]) -> Dict[str, Any]:
        """
        Validate YAML syntax.

        Args:
            file_path: Path to YAML file.

        Returns:
            Validation result with 'valid' and 'errors' keys.
        """
        file_path = Path(file_path)

        try:
            with open(file_path, 'r') as f:
                yaml.safe_load(f)
            return {'valid': True, 'errors': []}
        except yaml.YAMLError as e:
            return {'valid': False, 'errors': [str(e)]}
        except Exception as e:
            return {'valid': False, 'errors': [f'File error: {e}']}

    def validate_structure(self, file_path: Union[str, Path]) -> Dict[str, Any]:
        """
        Validate OrcaFlex model structure.

        Args:
            file_path: Path to YAML file.

        Returns:
            Validation result with 'valid', 'sections_found', and 'warnings' keys.
        """
        file_path = Path(file_path)

        result = {
            'valid': True,
            'sections_found': [],
            'warnings': [],
            'errors': []
        }

        try:
            with open(file_path, 'r') as f:
                content = yaml.safe_load(f)

            if content is None:
                result['valid'] = False
                result['errors'].append('Empty or invalid YAML file')
                return result

            # Check for BaseFile + IncludeFile format (case file)
            if 'BaseFile' in content:
                result['sections_found'].append('BaseFile')
                if 'IncludeFile' in content:
                    result['sections_found'].append('IncludeFile')
                # Case files are valid if they have BaseFile
                return result

            # Check required sections
            for section in self.REQUIRED_SECTIONS:
                if section in content:
                    result['sections_found'].append(section)
                else:
                    result['warnings'].append(f'Missing recommended section: {section}')

            # Check optional sections
            for section in self.OPTIONAL_SECTIONS:
                if section in content:
                    result['sections_found'].append(section)

            # Model needs at least General or Environment
            if not any(s in content for s in self.REQUIRED_SECTIONS):
                result['valid'] = False
                result['errors'].append(
                    f'Model must contain at least one of: {self.REQUIRED_SECTIONS}'
                )

        except Exception as e:
            result['valid'] = False
            result['errors'].append(str(e))

        return result

    def validate_with_orcaflex(
        self,
        file_path: Union[str, Path],
        run_statics: bool = False
    ) -> Dict[str, Any]:
        """
        Validate model by loading with OrcFxAPI.

        Args:
            file_path: Path to YAML file.
            run_statics: If True, also run static analysis.

        Returns:
            Validation result with 'valid', 'water_depth', and optional 'statics' keys.
        """
        file_path = Path(file_path)
        result = {
            'valid': False,
            'errors': [],
            'water_depth': None,
            'statics_converged': None
        }

        try:
            import OrcFxAPI
        except ImportError:
            result['errors'].append('OrcFxAPI not available')
            return result

        try:
            model = OrcFxAPI.Model(str(file_path))
            result['valid'] = True
            result['water_depth'] = model.environment.WaterDepth

            if run_statics:
                try:
                    model.CalculateStatics()
                    result['statics_converged'] = True
                except Exception as e:
                    result['statics_converged'] = False
                    result['errors'].append(f'Static analysis failed: {e}')

        except Exception as e:
            result['errors'].append(f'Model load failed: {e}')

        return result

    def validate(
        self,
        file_path: Union[str, Path],
        level: str = 'syntax'
    ) -> Dict[str, Any]:
        """
        Validate model at specified level.

        Args:
            file_path: Path to YAML file.
            level: Validation level - 'syntax', 'structure', or 'orcaflex'.

        Returns:
            Validation result dictionary.
        """
        file_path = Path(file_path)

        if not file_path.exists():
            return {'valid': False, 'errors': [f'File not found: {file_path}']}

        if level == 'syntax':
            return self.validate_syntax(file_path)
        elif level == 'structure':
            syntax_result = self.validate_syntax(file_path)
            if not syntax_result['valid']:
                return syntax_result
            return self.validate_structure(file_path)
        elif level == 'orcaflex':
            return self.validate_with_orcaflex(file_path, run_statics=True)
        else:
            return {'valid': False, 'errors': [f'Unknown validation level: {level}']}


def format_template_list(templates: List[Dict[str, Any]], verbose: bool = False) -> str:
    """
    Format template list for display.

    Args:
        templates: List of template dictionaries.
        verbose: If True, show detailed information.

    Returns:
        Formatted string.
    """
    if not templates:
        return "No templates found."

    lines = []
    lines.append(f"Found {len(templates)} hybrid template(s):\n")

    # Group by category
    by_category: Dict[str, List[Dict]] = {}
    for template in templates:
        category = template['category']
        if category not in by_category:
            by_category[category] = []
        by_category[category].append(template)

    for category, cat_templates in sorted(by_category.items()):
        lines.append(f"\n{category.upper().replace('_', ' ')}:")
        lines.append("-" * (len(category) + 1))

        for template in sorted(cat_templates, key=lambda t: t['name']):
            name = template['name']
            base_count = len(template['base_files'])
            var_count = len(template['variations'])

            if verbose:
                lines.append(f"  {name}")
                lines.append(f"    Base files: {base_count}")
                lines.append(f"    Variations: {var_count}")
                if template['variations']:
                    for var in template['variations']:
                        lines.append(f"      - {var.stem}")
            else:
                lines.append(f"  {name} ({base_count} base, {var_count} variations)")

    return "\n".join(lines)


def cmd_list_templates(args: argparse.Namespace) -> int:
    """Execute list-templates command."""
    manager = TemplateManager(args.templates_dir)
    templates = manager.discover_templates()

    output = format_template_list(templates, verbose=args.verbose)
    print(output)

    return 0


def cmd_generate(args: argparse.Namespace) -> int:
    """Execute generate command."""
    generator = TemplateGenerator()

    # If template name is provided, find base and variation automatically
    if args.template:
        manager = TemplateManager(args.templates_dir)
        template = manager.get_template(args.template)

        if template is None:
            print(f"Error: Template not found: {args.template}")
            return 1

        # Find base file
        if not template['base_files']:
            print(f"Error: No base files in template: {args.template}")
            return 1
        base_file = template['base_files'][0]

        # Find variation
        if args.variation:
            variation_file = None
            for var in template['variations']:
                if args.variation in var.stem:
                    variation_file = var
                    break
            if variation_file is None:
                print(f"Error: Variation not found: {args.variation}")
                print(f"Available: {[v.stem for v in template['variations']]}")
                return 1
        elif template['variations']:
            variation_file = template['variations'][0]
        else:
            print(f"Error: No variations available for template: {args.template}")
            return 1
    else:
        # Use explicit base and variation paths
        if not args.base:
            print("Error: --base is required when not using --template")
            return 1
        if not args.variation:
            print("Error: --variation is required when not using --template")
            return 1

        base_file = Path(args.base)
        variation_file = Path(args.variation)

    # Generate
    result = generator.generate(
        base_file=base_file,
        variation_file=variation_file,
        output_file=Path(args.output),
        as_reference=args.as_reference
    )

    if result['success']:
        print(f"Generated: {result['output_file']}")
        return 0
    else:
        print(f"Error: {result['error']}")
        return 1


def cmd_validate(args: argparse.Namespace) -> int:
    """Execute validate command."""
    validator = ModelValidator()

    model_file = Path(args.model_file)
    if not model_file.exists():
        print(f"Error: File not found: {model_file}")
        return 1

    # Determine validation level
    if args.orcaflex:
        level = 'orcaflex'
    elif args.structure:
        level = 'structure'
    else:
        level = 'syntax'

    result = validator.validate(model_file, level=level)

    if result['valid']:
        print(f"Valid: {model_file}")

        if 'sections_found' in result:
            print(f"  Sections: {', '.join(result['sections_found'])}")

        if 'water_depth' in result and result['water_depth'] is not None:
            print(f"  Water depth: {result['water_depth']}m")

        if result.get('statics_converged'):
            print("  Static analysis: Converged")
        elif result.get('statics_converged') is False:
            print("  Static analysis: FAILED")

        if result.get('warnings'):
            print("  Warnings:")
            for warning in result['warnings']:
                print(f"    - {warning}")

        return 0
    else:
        print(f"Invalid: {model_file}")
        for error in result.get('errors', []):
            print(f"  - {error}")
        return 1


def create_parser() -> argparse.ArgumentParser:
    """
    Create and configure the argument parser.

    Returns:
        Configured ArgumentParser instance.
    """
    # Create parent parser with common arguments
    parent_parser = argparse.ArgumentParser(add_help=False)
    parent_parser.add_argument(
        '--templates-dir',
        type=str,
        default=str(DEFAULT_TEMPLATES_DIR),
        help='Path to templates directory'
    )
    parent_parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose output'
    )

    parser = argparse.ArgumentParser(
        prog='template_generator',
        description='OrcaFlex Template Generator - Generate models from hybrid templates',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # List all available templates
  python -m digitalmodel.modules.orcaflex.template_generator list-templates

  # Generate a model from template + variation
  python -m digitalmodel.modules.orcaflex.template_generator generate \\
      --template calm_buoy_hybrid --variation deep_water --output model.yml

  # Generate from explicit files
  python -m digitalmodel.modules.orcaflex.template_generator generate \\
      --base base/model.yml --variation variations/deep.yml --output output.yml

  # Validate a model file
  python -m digitalmodel.modules.orcaflex.template_generator validate model.yml

  # Validate with OrcaFlex static analysis
  python -m digitalmodel.modules.orcaflex.template_generator validate model.yml --orcaflex
"""
    )

    subparsers = parser.add_subparsers(dest='command', help='Available commands')

    # list-templates command
    list_parser = subparsers.add_parser(
        'list-templates',
        parents=[parent_parser],
        help='List all available hybrid templates'
    )
    list_parser.set_defaults(func=cmd_list_templates)

    # generate command
    gen_parser = subparsers.add_parser(
        'generate',
        parents=[parent_parser],
        help='Generate a model from template + variation'
    )
    gen_parser.add_argument(
        '--template', '-t',
        help='Template name (e.g., calm_buoy_hybrid)'
    )
    gen_parser.add_argument(
        '--base', '-b',
        help='Path to base template file (alternative to --template)'
    )
    gen_parser.add_argument(
        '--variation', '-V',
        help='Variation name or path'
    )
    gen_parser.add_argument(
        '--output', '-o',
        required=True,
        help='Output file path'
    )
    gen_parser.add_argument(
        '--as-reference',
        action='store_true',
        help='Generate as BaseFile + IncludeFile reference instead of merged YAML'
    )
    gen_parser.set_defaults(func=cmd_generate)

    # validate command
    val_parser = subparsers.add_parser(
        'validate',
        parents=[parent_parser],
        help='Validate a model file'
    )
    val_parser.add_argument(
        'model_file',
        help='Path to model file to validate'
    )
    val_parser.add_argument(
        '--structure', '-s',
        action='store_true',
        help='Check OrcaFlex section structure'
    )
    val_parser.add_argument(
        '--orcaflex', '-x',
        action='store_true',
        help='Validate with OrcFxAPI and run static analysis'
    )
    val_parser.set_defaults(func=cmd_validate)

    return parser


def main(argv: Optional[List[str]] = None) -> int:
    """
    Main entry point for template generator CLI.

    Args:
        argv: Command line arguments (for testing).

    Returns:
        Exit code (0 for success).
    """
    parser = create_parser()

    # Handle errors gracefully for testing
    try:
        args = parser.parse_args(argv)
    except SystemExit as e:
        # Re-raise for help/version (exit code 0)
        if e.code == 0:
            raise
        # Return the exit code for errors
        return e.code if isinstance(e.code, int) else 1

    if args.command is None:
        parser.print_help()
        return 1

    # Execute command
    return args.func(args)


if __name__ == '__main__':
    sys.exit(main())
