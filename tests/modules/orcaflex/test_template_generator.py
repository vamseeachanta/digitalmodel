"""
Test suite for OrcaFlex template generator CLI.

Tests the template discovery, generation, and validation functionality.
"""

import pytest
import yaml
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
import tempfile
import shutil


# Import after defining mocks since OrcFxAPI may not be available
@pytest.fixture(autouse=True)
def mock_orcfxapi():
    """Mock OrcFxAPI for testing without license."""
    mock_module = MagicMock()
    mock_model = MagicMock()
    mock_model.environment = MagicMock()
    mock_model.environment.WaterDepth = 100
    mock_module.Model.return_value = mock_model

    with patch.dict('sys.modules', {'OrcFxAPI': mock_module}):
        yield mock_module


# Base path for templates
TEMPLATES_DIR = Path(__file__).parent.parent.parent.parent / "docs" / "modules" / "orcaflex" / "templates"


@pytest.fixture
def temp_output_dir():
    """Create a temporary directory for output files."""
    temp_dir = tempfile.mkdtemp()
    yield Path(temp_dir)
    shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture
def sample_base_template(temp_output_dir):
    """Create a sample base template for testing."""
    base_dir = temp_output_dir / "templates" / "test_hybrid" / "base"
    base_dir.mkdir(parents=True)

    base_content = {
        "General": {
            "UnitsSystem": "SI",
            "StageDuration": [10, 100]
        },
        "Environment": {
            "WaterDepth": 100,
            "WaterSurfaceZ": 0
        },
        "Lines": [
            {
                "Name": "Line1",
                "Length": 500,
                "EndAConnection": "Vessel",
                "EndBConnection": "Anchored"
            }
        ]
    }

    base_file = base_dir / "test_base.yml"
    with open(base_file, 'w') as f:
        yaml.dump(base_content, f)

    return base_file


@pytest.fixture
def sample_variation(temp_output_dir):
    """Create a sample variation file for testing."""
    var_dir = temp_output_dir / "templates" / "test_hybrid" / "variations"
    var_dir.mkdir(parents=True)

    variation_content = {
        "Environment": {
            "WaterDepth": 200
        },
        "Lines": [
            {
                "Name": "Line1",
                "Length": 800,
                "EndAConnection": "Vessel",
                "EndBConnection": "Anchored"
            }
        ]
    }

    var_file = var_dir / "deep_water.yml"
    with open(var_file, 'w') as f:
        yaml.dump(variation_content, f)

    return var_file


@pytest.fixture
def sample_case_template(temp_output_dir, sample_base_template, sample_variation):
    """Create a sample case file combining base and variation."""
    cases_dir = temp_output_dir / "templates" / "test_hybrid" / "cases"
    cases_dir.mkdir(parents=True)

    # Create case file that references base and variation
    case_content = {
        "BaseFile": "../base/test_base.yml",
        "IncludeFile": "../variations/deep_water.yml"
    }

    case_file = cases_dir / "case_deep_water.yml"
    with open(case_file, 'w') as f:
        yaml.dump(case_content, f)

    # Also add README.md for template structure validation
    readme = temp_output_dir / "templates" / "test_hybrid" / "README.md"
    readme.write_text("# Test Hybrid Template\n")

    return case_file


class TestTemplateManager:
    """Tests for TemplateManager class."""

    def test_discover_templates_finds_hybrid_dirs(self):
        """Test that discover_templates finds directories ending in _hybrid."""
        from digitalmodel.modules.orcaflex.template_generator import TemplateManager

        if not TEMPLATES_DIR.exists():
            pytest.skip("Templates directory not found")

        manager = TemplateManager(TEMPLATES_DIR)
        templates = manager.discover_templates()

        # Should find at least some templates
        assert len(templates) > 0

        # All templates should have _hybrid suffix
        for template in templates:
            assert template['name'].endswith('_hybrid'), f"Template {template['name']} should end with _hybrid"

    def test_discover_templates_returns_correct_structure(self):
        """Test that discovered templates have required fields."""
        from digitalmodel.modules.orcaflex.template_generator import TemplateManager

        if not TEMPLATES_DIR.exists():
            pytest.skip("Templates directory not found")

        manager = TemplateManager(TEMPLATES_DIR)
        templates = manager.discover_templates()

        for template in templates:
            assert 'name' in template
            assert 'category' in template
            assert 'path' in template
            assert 'base_files' in template
            assert 'variations' in template

    def test_get_template_by_name(self):
        """Test getting a specific template by name."""
        from digitalmodel.modules.orcaflex.template_generator import TemplateManager

        if not TEMPLATES_DIR.exists():
            pytest.skip("Templates directory not found")

        manager = TemplateManager(TEMPLATES_DIR)

        # Try to get calm_buoy_hybrid template
        template = manager.get_template('calm_buoy_hybrid')

        if template is None:
            pytest.skip("calm_buoy_hybrid template not found")

        assert template['name'] == 'calm_buoy_hybrid'
        assert 'base_files' in template
        assert len(template['base_files']) > 0

    def test_list_variations_for_template(self):
        """Test listing available variations for a template."""
        from digitalmodel.modules.orcaflex.template_generator import TemplateManager

        if not TEMPLATES_DIR.exists():
            pytest.skip("Templates directory not found")

        manager = TemplateManager(TEMPLATES_DIR)
        template = manager.get_template('calm_buoy_hybrid')

        if template is None:
            pytest.skip("calm_buoy_hybrid template not found")

        variations = template['variations']
        assert isinstance(variations, list)
        # calm_buoy_hybrid should have at least one variation
        assert len(variations) >= 1


class TestTemplateGenerator:
    """Tests for template generation functionality."""

    def test_merge_yaml_files(self, sample_base_template, sample_variation):
        """Test that YAML files are merged correctly."""
        from digitalmodel.modules.orcaflex.template_generator import TemplateGenerator

        generator = TemplateGenerator()
        merged = generator.merge_yaml_files(sample_base_template, sample_variation)

        # Verify merge happened correctly
        assert merged['Environment']['WaterDepth'] == 200  # From variation
        assert merged['General']['UnitsSystem'] == 'SI'  # From base
        assert merged['Lines'][0]['Length'] == 800  # From variation

    def test_generate_case_file(self, sample_base_template, sample_variation, temp_output_dir):
        """Test generating a combined case file."""
        from digitalmodel.modules.orcaflex.template_generator import TemplateGenerator

        output_file = temp_output_dir / "generated_model.yml"

        generator = TemplateGenerator()
        result = generator.generate(
            base_file=sample_base_template,
            variation_file=sample_variation,
            output_file=output_file
        )

        assert result['success'] is True
        assert output_file.exists()

        # Load and verify the generated file
        with open(output_file) as f:
            generated = yaml.safe_load(f)

        assert generated['Environment']['WaterDepth'] == 200

    def test_generate_with_basefile_reference(self, temp_output_dir, sample_base_template, sample_variation):
        """Test generating with BaseFile + IncludeFile reference (OrcaFlex format)."""
        from digitalmodel.modules.orcaflex.template_generator import TemplateGenerator

        output_file = temp_output_dir / "case_output.yml"

        generator = TemplateGenerator()
        result = generator.generate(
            base_file=sample_base_template,
            variation_file=sample_variation,
            output_file=output_file,
            as_reference=True  # Generate as BaseFile + IncludeFile reference
        )

        assert result['success'] is True
        assert output_file.exists()

        # Load and verify the reference format
        with open(output_file) as f:
            content = yaml.safe_load(f)

        assert 'BaseFile' in content
        assert 'IncludeFile' in content


class TestModelValidator:
    """Tests for model validation functionality."""

    def test_validate_yaml_syntax(self, sample_base_template):
        """Test that valid YAML files pass syntax validation."""
        from digitalmodel.modules.orcaflex.template_generator import ModelValidator

        validator = ModelValidator()
        result = validator.validate_syntax(sample_base_template)

        assert result['valid'] is True
        assert result['errors'] == []

    def test_validate_yaml_syntax_invalid(self, temp_output_dir):
        """Test that invalid YAML fails syntax validation."""
        from digitalmodel.modules.orcaflex.template_generator import ModelValidator

        invalid_file = temp_output_dir / "invalid.yml"
        invalid_file.write_text("invalid: yaml: content: [bad")

        validator = ModelValidator()
        result = validator.validate_syntax(invalid_file)

        assert result['valid'] is False
        assert len(result['errors']) > 0

    def test_validate_required_sections(self, sample_base_template):
        """Test validation of required OrcaFlex sections."""
        from digitalmodel.modules.orcaflex.template_generator import ModelValidator

        validator = ModelValidator()
        result = validator.validate_structure(sample_base_template)

        # Should have required sections
        assert result['valid'] is True
        assert 'General' in result['sections_found']
        assert 'Environment' in result['sections_found']

    def test_validate_with_orcaflex_static(self, sample_base_template, mock_orcfxapi):
        """Test validation using OrcaFlex static analysis (mocked)."""
        from digitalmodel.modules.orcaflex.template_generator import ModelValidator

        validator = ModelValidator()
        result = validator.validate_with_orcaflex(sample_base_template)

        # With mock, should succeed
        assert result['valid'] is True


class TestCLI:
    """Tests for CLI interface."""

    def test_list_templates_command(self, capsys):
        """Test the list-templates command."""
        from digitalmodel.modules.orcaflex.template_generator import main

        if not TEMPLATES_DIR.exists():
            pytest.skip("Templates directory not found")

        # Run list-templates command
        exit_code = main(['list-templates', '--templates-dir', str(TEMPLATES_DIR)])

        captured = capsys.readouterr()

        assert exit_code == 0
        # Should output template names
        assert 'hybrid' in captured.out.lower() or 'template' in captured.out.lower()

    def test_generate_command(self, temp_output_dir, sample_base_template, sample_variation, capsys):
        """Test the generate command."""
        from digitalmodel.modules.orcaflex.template_generator import main

        output_file = temp_output_dir / "cli_output.yml"

        exit_code = main([
            'generate',
            '--base', str(sample_base_template),
            '--variation', str(sample_variation),
            '--output', str(output_file)
        ])

        assert exit_code == 0
        assert output_file.exists()

    def test_validate_command(self, sample_base_template, capsys):
        """Test the validate command."""
        from digitalmodel.modules.orcaflex.template_generator import main

        exit_code = main([
            'validate',
            str(sample_base_template)
        ])

        captured = capsys.readouterr()

        assert exit_code == 0
        assert 'valid' in captured.out.lower() or 'Valid' in captured.out

    def test_help_command(self, capsys):
        """Test that help displays usage information."""
        from digitalmodel.modules.orcaflex.template_generator import main

        with pytest.raises(SystemExit) as exc_info:
            main(['--help'])

        # argparse exits with 0 for help
        assert exc_info.value.code == 0

    def test_missing_required_args(self, capsys):
        """Test that missing required arguments show error."""
        from digitalmodel.modules.orcaflex.template_generator import main

        # Generate without output should fail
        exit_code = main(['generate', '--base', 'nonexistent.yml'])

        assert exit_code != 0


class TestIntegration:
    """Integration tests with actual template files."""

    @pytest.mark.skipif(not TEMPLATES_DIR.exists(), reason="Templates directory not found")
    def test_full_workflow_calm_buoy(self, temp_output_dir):
        """Test full workflow: discover -> generate -> validate."""
        from digitalmodel.modules.orcaflex.template_generator import (
            TemplateManager, TemplateGenerator, ModelValidator
        )

        # 1. Discover templates
        manager = TemplateManager(TEMPLATES_DIR)
        template = manager.get_template('calm_buoy_hybrid')

        if template is None:
            pytest.skip("calm_buoy_hybrid template not found")

        # 2. Get base and variation
        if not template['base_files'] or not template['variations']:
            pytest.skip("Template missing base or variations")

        base_file = template['base_files'][0]
        variation_file = template['variations'][0]

        # 3. Generate combined model
        generator = TemplateGenerator()
        output_file = temp_output_dir / "calm_buoy_deep.yml"

        result = generator.generate(
            base_file=base_file,
            variation_file=variation_file,
            output_file=output_file
        )

        assert result['success'] is True

        # 4. Validate generated model
        validator = ModelValidator()
        validation = validator.validate_syntax(output_file)

        assert validation['valid'] is True


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
