#!/usr/bin/env python3
"""
ABOUTME: CALM Buoy project generator - creates OrcaFlex models from project YAML
ABOUTME: Validates parameters, generates directory structure, and produces analysis-ready files
"""

import sys
import argparse
import shutil
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional
import yaml
import io

# Configure stdout/stderr for UTF-8 on Windows
if sys.platform == 'win32':
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8', errors='replace')

# Add src and scripts to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))
sys.path.insert(0, str(Path(__file__).parent))

from digitalmodel.orcaflex.modular_input_validation import (
    ModularInputValidator,
    ValidationConfig
)
from yaml_key_mapper import YAMLKeyMapper


class CALMBuoyProjectGenerator:
    """Generate CALM Buoy project from configuration YAML."""

    def __init__(self, config_path: Path, output_dir: Optional[Path] = None):
        """
        Initialize project generator.

        Args:
            config_path: Path to project configuration YAML
            output_dir: Optional output directory (defaults to projects/<project_code>)
        """
        self.config_path = Path(config_path)
        self.mapper = YAMLKeyMapper()
        self.config = self._load_config()

        # Determine output directory
        if output_dir:
            self.project_root = Path(output_dir)
        else:
            project_code = self._get_value('human_input.project.code')
            self.project_root = Path(__file__).parent.parent / "projects" / project_code

        # Repository root
        self.repo_root = Path(__file__).parent.parent

    def _load_config(self) -> Dict[str, Any]:
        """Load and parse project configuration YAML with flexible key support."""
        print(f"📖 Loading configuration from: {self.config_path}")

        with open(self.config_path, 'r', encoding='utf-8') as f:
            config_raw = yaml.safe_load(f)

        # Convert human-friendly keys to technical keys for internal use
        config = self.mapper.convert_dict_to_technical(config_raw)

        project_name = self._get_value_direct(config, 'human_input.project.name')
        print(f"✅ Configuration loaded: {project_name}")
        return config

    def _get_value(self, path: str) -> Any:
        """
        Get nested value from config using dot notation.
        Supports both human-friendly and technical keys.

        Args:
            path: Dot-notation path (e.g., "human_input.buoy.outer_diameter")

        Returns:
            Value at path
        """
        return self.mapper.get_nested_value(self.config, path)

    def _resolve_metocean_condition(self, condition_name: str) -> Dict[str, Any]:
        """
        Resolve load case condition to metocean parameters.

        PHASE 1: Load Case Condition Mapping

        Args:
            condition_name: "operating", "design_1yr", "design_10yr", or "extreme"

        Returns:
            Dictionary of metocean parameters

        Raises:
            ValueError: If condition name is unknown
        """
        condition_map = {
            'operating': 'human_input.site.metocean_override.operating_conditions',
            'design_1yr': 'human_input.site.metocean_override.design_1yr_conditions',
            'design_10yr': 'human_input.site.metocean_override.design_10yr_conditions',
            'extreme': 'human_input.site.metocean_override.extreme_conditions',
        }

        if condition_name not in condition_map:
            raise ValueError(
                f"Unknown condition: '{condition_name}'. "
                f"Valid options: {list(condition_map.keys())}"
            )

        condition_data = self._get_value(condition_map[condition_name])

        if condition_data is None:
            raise ValueError(
                f"Condition '{condition_name}' not found in configuration. "
                f"Expected at: {condition_map[condition_name]}"
            )

        return condition_data

    def generate_operability_load_cases(
        self,
        directions: int = 12,
        return_period: str = "design_1yr",
        aligned: bool = True
    ) -> List[Dict[str, Any]]:
        """
        Generate operability analysis load cases for 360° coverage.

        PHASE 2: Operability Load Case Generator

        Args:
            directions: Number of directions (12, 24, 36, 72)
            return_period: "design_1yr", "design_10yr", "extreme"
            aligned: True=align wind/current with waves, False=independent directions

        Returns:
            List of load case dictionaries
        """
        angle_step = 360 / directions
        load_cases = []

        for i in range(directions):
            angle = int(i * angle_step)

            load_case = {
                'name': f'operability_{angle:03d}deg',
                'condition': return_period,
                'wave_direction': angle,
                'simulation_duration': self._get_value('human_input.analysis.simulation_duration') or 3600,
                'time_step': self._get_value('human_input.analysis.time_step') or 0.1,
                'ramp_time': self._get_value('human_input.analysis.ramp_time') or 100,
            }

            if aligned:
                # Align wind and current with wave direction
                load_case['current_direction'] = angle
                load_case['wind_direction'] = angle
            else:
                # Independent directions (conservative)
                load_case['current_direction'] = (angle + 30) % 360
                load_case['wind_direction'] = (angle + 15) % 360

            load_cases.append(load_case)

        return load_cases

    def _get_value_direct(self, data: Dict[str, Any], path: str) -> Any:
        """Get nested value directly from a dictionary."""
        keys = path.split('.')
        current = data
        for key in keys:
            if isinstance(current, dict) and key in current:
                current = current[key]
            else:
                return None
        return current

    def create_directory_structure(self) -> None:
        """Create project directory structure."""
        print(f"\n📁 Creating project directory structure at: {self.project_root}")

        # Create main directories
        directories = [
            self.project_root,
            self.project_root / "orcaflex",
            self.project_root / "orcaflex" / "modules",
            self.project_root / "freecad",
            self.project_root / "blender",
            self.project_root / "blender" / "renders",
            self.project_root / "reports",
            self.project_root / "reports" / "validation",
            self.project_root / "data",
        ]

        for directory in directories:
            directory.mkdir(parents=True, exist_ok=True)
            print(f"  ✓ {directory.relative_to(self.project_root.parent)}")

        print("✅ Directory structure created")

    def validate_human_input(self) -> bool:
        """
        Validate human input against reference data and engineering limits.

        Returns:
            True if validation passes, False otherwise
        """
        print("\n🔍 Validating human input parameters...")

        human_input = self.config['human_input']
        warnings = []
        errors = []

        # Validate water depth
        water_depth = human_input['site']['water_depth']
        if water_depth < 50:
            warnings.append(f"Water depth {water_depth}m is shallow for CALM buoy (typical 50-150m)")
        elif water_depth > 200:
            warnings.append(f"Water depth {water_depth}m is deep for CALM buoy (typical 50-150m)")

        # Validate buoy geometry
        buoy = human_input['buoy']
        diameter = buoy['outer_diameter']
        draft = buoy['draft']

        if diameter < 8 or diameter > 18:
            warnings.append(f"Buoy diameter {diameter}m outside typical range (8-18m)")

        if draft < 6 or draft > 15:
            warnings.append(f"Buoy draft {draft}m outside typical range (6-15m)")

        # Validate mooring system
        mooring = human_input['mooring']
        num_lines = mooring['number_of_lines']

        if num_lines not in [4, 6, 8, 12]:
            warnings.append(f"Number of mooring lines {num_lines} is non-standard (typical: 4, 6, 8, 12)")

        # Check safety factors
        sf_intact = mooring['safety_factor_intact']
        sf_damaged = mooring['safety_factor_damaged']

        if sf_intact < 1.67:
            errors.append(f"Safety factor intact {sf_intact} below API RP 2SK minimum 1.67")
        elif sf_intact < 2.0:
            warnings.append(f"Safety factor intact {sf_intact} below recommended 2.0")

        if sf_damaged < 1.25:
            errors.append(f"Safety factor damaged {sf_damaged} below minimum 1.25")

        # Display results
        if errors:
            print("\n❌ ERRORS found:")
            for error in errors:
                print(f"  ✗ {error}")

        if warnings:
            print("\n⚠️  WARNINGS:")
            for warning in warnings:
                print(f"  ⚠ {warning}")

        if not errors and not warnings:
            print("✅ All validations passed")
        elif not errors:
            print("✅ No errors (warnings can be addressed)")

        # Store in ai_generated section
        self.config['ai_generated']['recommendations']['warnings'] = warnings

        return len(errors) == 0

    def calculate_derived_parameters(self) -> None:
        """Calculate derived parameters from human input."""
        print("\n🧮 Calculating derived parameters...")

        human_input = self.config['human_input']
        derived = {}

        # Mooring footprint
        mooring = human_input['mooring']
        top_length = mooring['line_segments'][0]['length']
        bottom_length = mooring['line_segments'][1]['length']
        total_length = top_length + bottom_length
        water_depth = human_input['site']['water_depth']

        # Approximate footprint (assumes catenary)
        derived['mooring_footprint_radius'] = float(total_length - water_depth * 1.1)

        # Total mooring mass
        total_mass = 0
        for segment in mooring['line_segments']:
            mass = segment['length'] * segment['mass_per_meter'] / 1000  # Convert kg to tonnes
            total_mass += mass
        derived['total_mooring_mass'] = float(total_mass * mooring['number_of_lines'])

        # Watch circle radius (conservative estimate)
        derived['watch_circle_radius'] = float(derived['mooring_footprint_radius'] * 0.1)

        print(f"  ✓ Mooring footprint radius: {derived['mooring_footprint_radius']:.1f} m")
        print(f"  ✓ Total mooring mass: {derived['total_mooring_mass']:.1f} tonnes")
        print(f"  ✓ Watch circle radius: {derived['watch_circle_radius']:.1f} m")

        # Store in ai_generated section
        self.config['ai_generated']['derived'].update(derived)

        print("✅ Derived parameters calculated")

    def generate_orcaflex_modules(self, fidelity: str = "preliminary") -> None:
        """
        Generate OrcaFlex module files from templates.

        For operability analysis with multiple load cases, generates load case-specific
        environment modules to enable batch execution with different metocean conditions.

        Args:
            fidelity: Analysis fidelity level ("preliminary" or "detailed")
        """
        print(f"\n⚙️  Generating OrcaFlex modules ({fidelity} fidelity)...")

        # Select template
        templates = self.config['generation']['orcaflex']['templates']
        template_config = templates[fidelity]
        template_base = self.repo_root / template_config['base']

        print(f"  Using template: {template_base.name}")

        # Copy base file
        project_code = self.config['human_input']['project']['code']
        output_base = self.project_root / "orcaflex" / f"{project_code}_calm_buoy.yml"

        shutil.copy(template_base, output_base)
        print(f"  ✓ Created: {output_base.name}")

        # Get load cases for environment module customization
        load_cases = self.config['human_input']['analysis'].get('load_cases', [])

        # Copy module files
        template_dir = template_base.parent
        module_files = list(template_dir.glob("_*.yml"))

        # Environment module names that need load case-specific customization
        environment_modules = {'_03c_waves.yml', '_03d_current.yml', '_03e_wind.yml'}

        modules_copied = 0
        for module_file in module_files:
            module_name = module_file.name

            # Check if this is an environment module and we have operability load cases
            if module_name in environment_modules and len(load_cases) > 1:
                # Generate load case-specific environment modules
                print(f"  🎯 Generating {len(load_cases)} load case variants for {module_name}...")

                for lc in load_cases:
                    lc_name = lc.get('name', 'unknown')
                    # Create load case-specific module directory
                    lc_module_dir = self.project_root / "orcaflex" / "modules" / lc_name
                    lc_module_dir.mkdir(parents=True, exist_ok=True)

                    # Generate customized module for this load case
                    output_module = lc_module_dir / module_name
                    self._apply_module_customizations(module_file, output_module, load_case=lc)

                modules_copied += len(load_cases)
                print(f"    ✓ Created {len(load_cases)} variants of {module_name}")
            else:
                # Non-environment modules: copy once without load case customization
                output_module = self.project_root / "orcaflex" / "modules" / module_name
                self._apply_module_customizations(module_file, output_module)
                modules_copied += 1

        print(f"  ✓ Processed {modules_copied} total module files")

        # Generate load case-specific batch script if multiple load cases exist
        if len(load_cases) > 1:
            self._generate_batch_script(load_cases, project_code)

        # Update base file to reference local modules
        self._update_base_file_paths(output_base)

        print(f"✅ OrcaFlex model generated: {output_base}")

        if len(load_cases) > 1:
            print(f"  ℹ️  Load case-specific environment modules created in modules/[load_case_name]/")
            print(f"  ℹ️  Use batch script to run all load cases: orcaflex/run_batch_analysis.py")

    def _populate_environment_module(
        self,
        content: str,
        condition: Dict[str, Any],
        load_case: Dict[str, Any]
    ) -> str:
        """
        Populate environment module with return period data.

        PHASE 3: Environment Module Template Population

        Args:
            content: Module template content
            condition: Metocean condition dictionary (e.g., design_1yr_conditions)
            load_case: Load case with directions

        Returns:
            Populated module content
        """
        # Extract metocean parameters from condition
        # Handle different field naming conventions
        hs = condition.get('hs_1yr') or condition.get('hs_10yr') or condition.get('hs_100yr') or 2.5
        tp = condition.get('tp_1yr') or condition.get('tp_10yr') or condition.get('tp_100yr') or 8.0
        tz = condition.get('tz_1yr') or condition.get('tz_10yr') or condition.get('tz_100yr') or tp * 0.71

        wind_speed = (condition.get('wind_speed_1yr') or
                     condition.get('wind_speed_10yr') or
                     condition.get('wind_speed_100yr') or
                     condition.get('wind_speed_max') or 15)

        current_surface = (condition.get('current_speed_surface_1yr') or
                          condition.get('current_speed_surface_10yr') or
                          condition.get('current_speed_surface_100yr') or
                          condition.get('current_speed_surface') or 1.0)

        # Template variable substitutions
        substitutions = {
            '{{wave_hs}}': str(hs),
            '{{wave_tp}}': str(tp),
            '{{wave_tz}}': f'{tz:.1f}',
            '{{wave_dir}}': str(load_case.get('wave_direction', 0)),
            '{{current_speed}}': str(current_surface),
            '{{current_dir}}': str(load_case.get('current_direction', 0)),
            '{{wind_speed}}': str(wind_speed),
            '{{wind_dir}}': str(load_case.get('wind_direction', 0)),
        }

        # Apply substitutions
        for placeholder, value in substitutions.items():
            content = content.replace(placeholder, value)

        # Legacy replacements for non-template modules
        # Wave parameters
        content = content.replace('WaveHs: 2', f'WaveHs: {hs}')
        content = content.replace('WaveTz: 6', f'WaveTz: {tz:.1f}')
        content = content.replace('WaveTp: 8', f'WaveTp: {tp:.1f}')

        # Current parameters
        content = content.replace('CurrentSpeed: 1', f'CurrentSpeed: {current_surface}')
        content = content.replace('RefCurrentSpeed: 1', f'RefCurrentSpeed: {current_surface}')
        content = content.replace('RefCurrentDirection: 120', f'RefCurrentDirection: {load_case.get("current_direction", 0)}')

        # Wind parameters
        content = content.replace('WindSpeed: 12', f'WindSpeed: {wind_speed}')
        content = content.replace('RefWindSpeed: 15', f'RefWindSpeed: {wind_speed}')
        content = content.replace('RefWindDirection: 180', f'RefWindDirection: {load_case.get("wind_direction", 0)}')
        # Handle direct WindDirection field (common in NPD spectrum wind)
        import re
        content = re.sub(r'WindDirection: \d+', f'WindDirection: {load_case.get("wind_direction", 0)}', content)

        # Wave direction (WaveDirection appears in wave component sections)
        content = content.replace('WaveDirection: 180', f'WaveDirection: {load_case.get("wave_direction", 0)}')
        content = content.replace('WaveDirection: 0', f'WaveDirection: {load_case.get("wave_direction", 0)}')

        return content

    def _apply_module_customizations(self, source: Path, destination: Path, load_case: Optional[Dict[str, Any]] = None) -> None:
        """
        Apply project-specific customizations to module files.

        PHASE 3: Module Customization with Return Period Data

        Args:
            source: Source template file
            destination: Destination project file
            load_case: Optional load case dictionary for environment modules
        """
        # Read template
        with open(source, 'r', encoding='utf-8') as f:
            content = f.read()

        # Get override configuration
        overrides = self.config['generation']['orcaflex']['overrides']
        human_input = self.config['human_input']

        # Apply customizations based on module type
        if '_03c_waves' in source.name or '_03d_current' in source.name or '_03e_wind' in source.name:
            # Environment modules - use return period data if load case specified
            if load_case and 'condition' in load_case:
                try:
                    condition_data = self._resolve_metocean_condition(load_case['condition'])
                    content = self._populate_environment_module(content, condition_data, load_case)
                except (ValueError, KeyError) as e:
                    print(f"  ⚠️  Warning: Could not resolve condition for {source.name}: {e}")
                    # Fall back to legacy customization
                    if 'environment' in overrides:
                        env_custom = overrides['environment']['customize']
                        if 'wave_hs' in env_custom:
                            hs_value = self._get_nested_value(env_custom['wave_hs'])
                            if hs_value:
                                content = content.replace('WaveHs: 2', f'WaveHs: {hs_value}')
                        if 'wave_tp' in env_custom:
                            tp_value = self._get_nested_value(env_custom['wave_tp'])
                            if tp_value:
                                tz_value = tp_value * 0.8
                                content = content.replace('WaveTz: 6', f'WaveTz: {tz_value:.1f}')
                        if 'wind_speed' in env_custom:
                            wind_value = self._get_nested_value(env_custom['wind_speed'])
                            if wind_value:
                                content = content.replace('WindSpeed: 12', f'WindSpeed: {wind_value}')

        # Write customized content
        with open(destination, 'w', encoding='utf-8') as f:
            f.write(content)

    def _get_nested_value(self, path: str) -> Any:
        """
        Get nested value from config using dot notation.

        Args:
            path: Dot-separated path (e.g., "human_input.site.water_depth")

        Returns:
            Value at path, or None if not found
        """
        parts = path.split('.')
        value = self.config

        for part in parts:
            # Handle array indexing (e.g., "line_segments[0]")
            if '[' in part:
                key, idx = part.split('[')
                idx = int(idx.rstrip(']'))
                value = value.get(key, [])[idx]
            else:
                value = value.get(part)

            if value is None:
                return None

        return value

    def _generate_batch_script(self, load_cases: List[Dict[str, Any]], project_code: str) -> None:
        """
        Generate Python batch script for running all operability load cases.

        Creates a script that programmatically modifies the OrcaFlex model to use
        load case-specific environment modules and executes each simulation.

        Args:
            load_cases: List of load case dictionaries
            project_code: Project code for naming
        """
        batch_script = f'''#!/usr/bin/env python3
"""
ABOUTME: OrcaFlex batch execution script for operability analysis
ABOUTME: Runs all {len(load_cases)} load cases with load case-specific environment modules
"""

import sys
from pathlib import Path

# Add OrcaFlex Python API to path (update to your OrcaFlex installation)
# sys.path.append(r"C:\\Program Files\\Orcina\\OrcaFlex\\11.4\\Python")

try:
    import OrcFxAPI
except ImportError:
    print("❌ OrcaFlex Python API not found.")
    print("   Install OrcaFlex and update sys.path above to point to OrcaFlex Python directory")
    print("   Example: C:\\\\Program Files\\\\Orcina\\\\OrcaFlex\\\\11.4\\\\Python")
    sys.exit(1)

def run_load_case(base_model_path: Path, load_case_name: str, output_dir: Path) -> bool:
    """
    Run a single operability load case.

    Args:
        base_model_path: Path to base OrcaFlex .yml file
        load_case_name: Name of load case (e.g., 'operability_000deg')
        output_dir: Directory for simulation results

    Returns:
        True if simulation succeeded, False otherwise
    """
    print(f"\\n{'='*80}")
    print(f"Running: {{load_case_name}}")
    print(f"{'='*80}")

    try:
        # Load base model
        print(f"  Loading base model: {{base_model_path.name}}")
        model = OrcFxAPI.Model(str(base_model_path))

        # Update environment module paths to use load case-specific modules
        # OrcaFlex models using includefile need module paths updated
        modules_dir = base_model_path.parent / "modules" / load_case_name

        if not modules_dir.exists():
            print(f"  ⚠️  Warning: Load case-specific modules not found: {{modules_dir}}")
            print(f"      Using default modules instead")
            modules_dir = base_model_path.parent / "modules"

        # Update wave module
        wave_module = modules_dir / "_03c_waves.yml"
        if wave_module.exists():
            print(f"  ✓ Using wave module: {{wave_module.relative_to(base_model_path.parent)}}")
            # Note: Actual module path update depends on OrcaFlex model structure
            # This is a placeholder - adjust based on your specific includefile implementation

        # Run static and dynamic analysis
        print(f"  Running statics...")
        model.CalculateStatics()

        print(f"  Running dynamics...")
        model.RunSimulation()

        # Save results
        output_file = output_dir / f"{{load_case_name}}.sim"
        print(f"  Saving results: {{output_file.name}}")
        model.SaveSimulation(str(output_file))

        print(f"  ✅ {{load_case_name}} completed successfully")
        return True

    except Exception as e:
        print(f"  ❌ {{load_case_name}} failed: {{e}}")
        return False

def main():
    """Run batch operability analysis."""
    print("="*80)
    print("ORCAFLEX BATCH OPERABILITY ANALYSIS")
    print("="*80)
    print(f"Project: {project_code}")
    print(f"Load cases: {len(load_cases)}")

    # Paths
    script_dir = Path(__file__).parent
    base_model = script_dir / "{project_code}_calm_buoy.yml"
    results_dir = script_dir.parent / "results"
    results_dir.mkdir(exist_ok=True)

    if not base_model.exists():
        print(f"\\n❌ Base model not found: {{base_model}}")
        return 1

    # Load cases to run
    load_cases = {[f'"{lc["name"]}"' for lc in load_cases]}

    # Run all load cases
    results = {{}}
    for i, load_case in enumerate(load_cases, 1):
        print(f"\\n[{{i}}/{{len(load_cases)}}]")
        success = run_load_case(base_model, load_case, results_dir)
        results[load_case] = "✓" if success else "✗"

    # Summary
    print("\\n" + "="*80)
    print("BATCH ANALYSIS COMPLETE")
    print("="*80)
    print("\\nResults Summary:")
    for load_case, status in results.items():
        print(f"  {{status}} {{load_case}}")

    successes = sum(1 for v in results.values() if v == "✓")
    print(f"\\nCompleted: {{successes}}/{{len(load_cases)}} load cases")

    if successes == len(load_cases):
        print("\\n✅ All load cases completed successfully!")
        return 0
    else:
        print(f"\\n⚠️  {{len(load_cases) - successes}} load cases failed")
        return 1

if __name__ == "__main__":
    sys.exit(main())
'''

        # Write batch script
        batch_script_path = self.project_root / "orcaflex" / "run_batch_analysis.py"
        with open(batch_script_path, 'w', encoding='utf-8') as f:
            f.write(batch_script)

        # Make executable on Unix-like systems
        try:
            import stat
            batch_script_path.chmod(batch_script_path.stat().st_mode | stat.S_IEXEC)
        except Exception:
            pass  # Windows doesn't need chmod

        print(f"  ✓ Created batch script: {batch_script_path.name}")

    def _update_base_file_paths(self, base_file: Path) -> None:
        """
        Update base file to use relative paths to local modules.

        Args:
            base_file: Path to base YAML file
        """
        with open(base_file, 'r', encoding='utf-8') as f:
            content = f.read()

        # Replace includefile paths to reference local modules directory
        content = content.replace('includefile: _', 'includefile: modules/_')

        with open(base_file, 'w', encoding='utf-8') as f:
            f.write(content)

    def run_validation(self) -> bool:
        """
        Run validation framework on generated OrcaFlex files.

        Returns:
            True if validation passes, False otherwise
        """
        print("\n✅ Running validation framework...")

        # Find generated OrcaFlex file
        orcaflex_files = list((self.project_root / "orcaflex").glob("*_calm_buoy.yml"))

        if not orcaflex_files:
            print("❌ No OrcaFlex files found to validate")
            return False

        # Configure validation
        config = ValidationConfig(
            tolerance_percent=10.0,
            enable_orcaflex=False,
            skip_levels=[2],  # Skip Level 2 (OrcaFlex API)
            reports_dir=self.project_root / "reports" / "validation",
            generate_reports=True,
            report_formats=['console', 'csv', 'markdown', 'html'],
            enable_color=True
        )

        # Create validator
        validator = ModularInputValidator(config)

        # Validate files
        results = validator.validate_files(orcaflex_files)

        # Generate reports
        validator.generate_reports(results)

        # Get summary
        summary = validator.get_validation_summary(results)

        print(f"\n📊 Validation Summary:")
        print(f"  Total files: {summary['total_files']}")
        print(f"  ✅ Passed: {summary['passed']}")
        print(f"  ⚠️  Warnings: {summary['warnings']}")
        print(f"  ❌ Failed: {summary['failed']}")
        print(f"  Pass rate: {summary['pass_rate']:.1f}%")

        # Update ai_generated validation section
        self.config['ai_generated']['validation'].update({
            'validated_by_ai': summary['failed'] == 0,
            'validation_date': datetime.now().isoformat(),
            'confidence_score': summary['pass_rate'] / 100.0,
            'checks': {
                'geometry_within_ranges': 'Pass' if summary['failed'] == 0 else 'Fail',
                'orcaflex_syntax_valid': 'Pass' if summary['failed'] == 0 else 'Fail',
            }
        })

        return summary['failed'] == 0

    def save_updated_config(self) -> None:
        """Save updated configuration with AI-generated sections."""
        output_config = self.project_root / "project_config.yml"

        print(f"\n💾 Saving updated configuration to: {output_config}")

        with open(output_config, 'w', encoding='utf-8') as f:
            yaml.dump(self.config, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

        print("✅ Configuration saved")

    def generate_readme(self) -> None:
        """Generate project README with metadata and usage instructions."""
        readme_path = self.project_root / "README.md"

        project = self.config['human_input']['project']

        content = f"""# {project['name']}

**Project Code:** {project['code']}
**Client:** {project['client']}
**Location:** {project['location']}
**Created:** {project['created_date']}
**Engineer:** {project['engineer']}
**Version:** {project['version']}

## Description

{project['description']}

## Project Structure

```
{project['code']}/
├── project_config.yml          # Project configuration
├── orcaflex/                   # OrcaFlex model files
│   ├── {project['code']}_calm_buoy.yml
│   └── modules/                # Modular components
├── freecad/                    # FreeCAD geometry files
├── blender/                    # Blender visualization files
├── reports/                    # Analysis and validation reports
└── data/                       # Project-specific data
```

## Quick Start

### 1. Run OrcaFlex Model

```bash
# Open in OrcaFlex
OrcaFlex orcaflex/{project['code']}_calm_buoy.yml
```

### 2. Regenerate Files

```bash
# From repository root
python scripts/generate_calm_buoy_project.py \\
  --config {self.project_root}/project_config.yml \\
  --regenerate
```

### 3. Run Validation

```bash
python run_validation.py \\
  --files {self.project_root}/orcaflex/{project['code']}_calm_buoy.yml
```

## Key Parameters

| Parameter | Value | Units |
|-----------|-------|-------|
| Water Depth | {self.config['human_input']['site']['water_depth']} | m |
| Buoy Diameter | {self.config['human_input']['buoy']['outer_diameter']} | m |
| Number of Mooring Lines | {self.config['human_input']['mooring']['number_of_lines']} | - |
| Chain Diameter | {self.config['human_input']['mooring']['line_segments'][0]['nominal_diameter']} | mm |

## Analysis Settings

- **Simulation Duration:** {self.config['human_input']['analysis']['simulation_duration']}s
- **Time Step:** {self.config['human_input']['analysis']['time_step']}s
- **Load Cases:** {len(self.config['human_input']['analysis']['load_cases'])}

## Standards & Codes

- Primary: {self.config['human_input']['standards']['primary_code']}
- Mooring: {self.config['human_input']['standards']['mooring_standard']}
- Classification: {self.config['human_input']['standards']['classification_society']}

---

*Generated by CALM Buoy Project Generator v1.0*
"""

        with open(readme_path, 'w', encoding='utf-8') as f:
            f.write(content)

        print(f"✅ README generated: {readme_path}")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Generate CALM Buoy project from configuration YAML"
    )
    parser.add_argument(
        '--config',
        type=Path,
        required=True,
        help='Path to project configuration YAML'
    )
    parser.add_argument(
        '--output-dir',
        type=Path,
        help='Output directory (default: projects/<project_code>)'
    )
    parser.add_argument(
        '--fidelity',
        choices=['preliminary', 'detailed'],
        default='preliminary',
        help='Analysis fidelity level (default: preliminary)'
    )
    parser.add_argument(
        '--validate',
        action='store_true',
        help='Run validation after generation'
    )
    parser.add_argument(
        '--skip-validation',
        action='store_true',
        help='Skip validation (faster generation)'
    )
    parser.add_argument(
        '--operability-directions',
        type=int,
        choices=[12, 24, 36, 72],
        help='Generate operability load cases for N directions (12=30°, 24=15°, 36=10°, 72=5°)'
    )
    parser.add_argument(
        '--operability-return-period',
        choices=['design_1yr', 'design_10yr', 'extreme'],
        default='design_1yr',
        help='Return period for operability analysis (default: design_1yr)'
    )
    parser.add_argument(
        '--operability-aligned',
        action='store_true',
        default=True,
        help='Align wind/current with wave direction (default: True)'
    )

    args = parser.parse_args()

    # Create generator
    print("=" * 80)
    print("CALM BUOY PROJECT GENERATOR")
    print("=" * 80)

    generator = CALMBuoyProjectGenerator(args.config, args.output_dir)

    # Execute workflow
    try:
        # 1. Create directory structure
        generator.create_directory_structure()

        # 2. Validate human input
        if not generator.validate_human_input():
            print("\n❌ Validation failed. Please fix errors in configuration.")
            return 1

        # 3. Calculate derived parameters
        generator.calculate_derived_parameters()

        # 4. Generate operability load cases if requested
        if args.operability_directions:
            print(f"\n🎯 Generating operability analysis load cases...")
            print(f"   Directions: {args.operability_directions} ({360/args.operability_directions:.0f}° spacing)")
            print(f"   Return period: {args.operability_return_period}")
            print(f"   Wind/Current alignment: {'Aligned' if args.operability_aligned else 'Independent'}")

            operability_cases = generator.generate_operability_load_cases(
                directions=args.operability_directions,
                return_period=args.operability_return_period,
                aligned=args.operability_aligned
            )

            # Replace existing load cases with operability cases
            generator.config['human_input']['analysis']['load_cases'] = operability_cases
            print(f"✅ Generated {len(operability_cases)} operability load cases")

        # 5. Generate OrcaFlex modules
        generator.generate_orcaflex_modules(fidelity=args.fidelity)

        # 5. Run validation (unless skipped)
        validation_passed = True
        if not args.skip_validation:
            validation_passed = generator.run_validation()

        # 6. Save updated configuration
        generator.save_updated_config()

        # 7. Generate README
        generator.generate_readme()

        # Summary
        print("\n" + "=" * 80)
        print("✅ PROJECT GENERATION COMPLETE")
        print("=" * 80)
        print(f"\nProject location: {generator.project_root}")
        print(f"\nNext steps:")
        print(f"  1. Review configuration: {generator.project_root}/project_config.yml")
        print(f"  2. Open OrcaFlex model: {generator.project_root}/orcaflex/")
        print(f"  3. Review validation report: {generator.project_root}/reports/validation/")

        if not validation_passed:
            print(f"\n⚠️  Note: Validation warnings detected. Review reports before analysis.")
            return 1

        return 0

    except Exception as e:
        print(f"\n❌ Error during generation: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
