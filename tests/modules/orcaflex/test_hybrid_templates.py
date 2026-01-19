"""
Test suite for OrcaFlex hybrid templates.

Validates all hybrid templates can be loaded and pass static analysis.
"""

import pytest
from pathlib import Path

# Check if OrcFxAPI is available
try:
    import OrcFxAPI
    ORCFXAPI_AVAILABLE = True
except ImportError:
    ORCFXAPI_AVAILABLE = False


# Base path for templates
TEMPLATES_DIR = Path(__file__).parent.parent.parent.parent / "docs" / "modules" / "orcaflex" / "templates"


def get_all_template_files():
    """Get all base and case YAML files from templates directory."""
    templates = []

    # Find all base models
    for base_file in TEMPLATES_DIR.rglob("base/*.yml"):
        templates.append(("base", base_file))

    # Find all case files
    for case_file in TEMPLATES_DIR.rglob("cases/*.yml"):
        templates.append(("case", case_file))

    return templates


@pytest.fixture
def template_files():
    """Fixture providing all template files."""
    return get_all_template_files()


@pytest.mark.skipif(not ORCFXAPI_AVAILABLE, reason="OrcFxAPI not available")
class TestHybridTemplates:
    """Test suite for hybrid OrcaFlex templates."""

    # Riser templates
    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "risers" / "scr_hybrid" / "base" / "scr_base.yml",
        TEMPLATES_DIR / "risers" / "lazy_wave_hybrid" / "base" / "lwr_base.yml",
        TEMPLATES_DIR / "risers" / "pliant_wave_hybrid" / "base" / "pwr_base.yml",
    ])
    def test_riser_base_models_load(self, template_path):
        """Test that riser base models load successfully."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        assert model is not None
        assert model.environment.WaterDepth > 0

    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "risers" / "scr_hybrid" / "base" / "scr_base.yml",
        TEMPLATES_DIR / "risers" / "lazy_wave_hybrid" / "base" / "lwr_base.yml",
        TEMPLATES_DIR / "risers" / "pliant_wave_hybrid" / "base" / "pwr_base.yml",
    ])
    def test_riser_base_models_converge(self, template_path):
        """Test that riser base models pass static analysis."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        model.CalculateStatics()
        # If we get here without exception, statics converged

    # Pipeline templates
    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "pipelines" / "pipeline_hybrid" / "base" / "pipeline_base.yml",
    ])
    def test_pipeline_base_models_load(self, template_path):
        """Test that pipeline base models load successfully."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        assert model is not None
        assert model.environment.WaterDepth > 0

    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "pipelines" / "pipeline_hybrid" / "base" / "pipeline_base.yml",
    ])
    def test_pipeline_base_models_converge(self, template_path):
        """Test that pipeline base models pass static analysis."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        model.CalculateStatics()

    # Umbilical templates
    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "umbilicals" / "umbilical_hybrid" / "base" / "umbilical_base.yml",
    ])
    def test_umbilical_base_models_load(self, template_path):
        """Test that umbilical base models load successfully."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        assert model is not None
        assert model.environment.WaterDepth > 0

    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "umbilicals" / "umbilical_hybrid" / "base" / "umbilical_base.yml",
    ])
    def test_umbilical_base_models_converge(self, template_path):
        """Test that umbilical base models pass static analysis."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        model.CalculateStatics()

    # Mooring system templates
    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "mooring_systems" / "calm_buoy_hybrid" / "base" / "calm_buoy_base.yml",
        TEMPLATES_DIR / "mooring_systems" / "spread_mooring_hybrid" / "base" / "spread_mooring_base.yml",
        TEMPLATES_DIR / "mooring_systems" / "turret_mooring_hybrid" / "base" / "turret_mooring_base.yml",
    ])
    def test_mooring_base_models_load(self, template_path):
        """Test that mooring base models load successfully."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        assert model is not None
        assert model.environment.WaterDepth > 0

    @pytest.mark.parametrize("template_path", [
        TEMPLATES_DIR / "mooring_systems" / "calm_buoy_hybrid" / "base" / "calm_buoy_base.yml",
        TEMPLATES_DIR / "mooring_systems" / "spread_mooring_hybrid" / "base" / "spread_mooring_base.yml",
        TEMPLATES_DIR / "mooring_systems" / "turret_mooring_hybrid" / "base" / "turret_mooring_base.yml",
    ])
    def test_mooring_base_models_converge(self, template_path):
        """Test that mooring base models pass static analysis."""
        if not template_path.exists():
            pytest.skip(f"Template not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        model.CalculateStatics()

    # Case files (base + variation combinations)
    @pytest.mark.parametrize("template_path", [
        # Riser cases
        TEMPLATES_DIR / "risers" / "scr_hybrid" / "cases" / "case_deep_water.yml",
        TEMPLATES_DIR / "risers" / "scr_hybrid" / "cases" / "case_12inch.yml",
        TEMPLATES_DIR / "risers" / "lazy_wave_hybrid" / "cases" / "case_deep_water.yml",
        TEMPLATES_DIR / "risers" / "pliant_wave_hybrid" / "cases" / "case_deep_water.yml",
        # Pipeline cases
        TEMPLATES_DIR / "pipelines" / "pipeline_hybrid" / "cases" / "case_deep_water.yml",
        TEMPLATES_DIR / "pipelines" / "pipeline_hybrid" / "cases" / "case_20inch.yml",
        TEMPLATES_DIR / "pipelines" / "pipeline_hybrid" / "cases" / "case_12inch_flowline.yml",
        # Umbilical cases
        TEMPLATES_DIR / "umbilicals" / "umbilical_hybrid" / "cases" / "case_deep_water.yml",
        TEMPLATES_DIR / "umbilicals" / "umbilical_hybrid" / "cases" / "case_steel_tube.yml",
        # Mooring cases
        TEMPLATES_DIR / "mooring_systems" / "calm_buoy_hybrid" / "cases" / "case_deep_water.yml",
        TEMPLATES_DIR / "mooring_systems" / "spread_mooring_hybrid" / "cases" / "case_deep_water.yml",
        TEMPLATES_DIR / "mooring_systems" / "spread_mooring_hybrid" / "cases" / "case_twelve_leg.yml",
        TEMPLATES_DIR / "mooring_systems" / "turret_mooring_hybrid" / "cases" / "case_deep_water.yml",
        TEMPLATES_DIR / "mooring_systems" / "turret_mooring_hybrid" / "cases" / "case_external_turret.yml",
    ])
    def test_case_files_converge(self, template_path):
        """Test that case files (base + variation) pass static analysis."""
        if not template_path.exists():
            pytest.skip(f"Case file not found: {template_path}")

        model = OrcFxAPI.Model(str(template_path))
        model.CalculateStatics()


@pytest.mark.skipif(not ORCFXAPI_AVAILABLE, reason="OrcFxAPI not available")
class TestLibraryComponents:
    """Test that library components are valid."""

    LIBRARY_DIR = TEMPLATES_DIR.parent / "library"

    def test_line_types_exist(self):
        """Test that line type library files exist."""
        line_types_dir = self.LIBRARY_DIR / "line_types"
        if not line_types_dir.exists():
            pytest.skip("Library line_types directory not found")

        yml_files = list(line_types_dir.glob("*.yml"))
        assert len(yml_files) > 0, "No line type library files found"

    def test_buoy_types_exist(self):
        """Test that buoy type library files exist."""
        buoy_types_dir = self.LIBRARY_DIR / "buoy_types"
        if not buoy_types_dir.exists():
            pytest.skip("Library buoy_types directory not found")

        yml_files = list(buoy_types_dir.glob("*.yml"))
        assert len(yml_files) > 0, "No buoy type library files found"

    @pytest.mark.parametrize("line_type_file", list((TEMPLATES_DIR.parent / "library" / "line_types").glob("*.yml")) if (TEMPLATES_DIR.parent / "library" / "line_types").exists() else [])
    def test_line_type_files_are_valid_yaml(self, line_type_file):
        """Test that line type library files are valid YAML."""
        import yaml

        with open(line_type_file) as f:
            content = yaml.safe_load(f)

        assert content is not None, f"Empty or invalid YAML: {line_type_file}"


@pytest.mark.skipif(not ORCFXAPI_AVAILABLE, reason="OrcFxAPI not available")
class TestTemplateStructure:
    """Test template directory structure conventions."""

    def test_all_templates_have_readme(self):
        """Test that all template directories have README.md files."""
        missing_readmes = []

        # Check each template category
        for category_dir in TEMPLATES_DIR.iterdir():
            if category_dir.is_dir():
                for template_dir in category_dir.iterdir():
                    if template_dir.is_dir() and template_dir.name.endswith("_hybrid"):
                        readme = template_dir / "README.md"
                        if not readme.exists():
                            missing_readmes.append(str(template_dir))

        assert len(missing_readmes) == 0, f"Missing README.md in: {missing_readmes}"

    def test_all_templates_have_base_dir(self):
        """Test that all hybrid templates have base/ directory."""
        missing_base = []

        for category_dir in TEMPLATES_DIR.iterdir():
            if category_dir.is_dir():
                for template_dir in category_dir.iterdir():
                    if template_dir.is_dir() and template_dir.name.endswith("_hybrid"):
                        base_dir = template_dir / "base"
                        if not base_dir.exists():
                            missing_base.append(str(template_dir))

        assert len(missing_base) == 0, f"Missing base/ directory in: {missing_base}"

    def test_all_templates_have_variations_dir(self):
        """Test that all hybrid templates have variations/ directory."""
        missing_variations = []

        for category_dir in TEMPLATES_DIR.iterdir():
            if category_dir.is_dir():
                for template_dir in category_dir.iterdir():
                    if template_dir.is_dir() and template_dir.name.endswith("_hybrid"):
                        variations_dir = template_dir / "variations"
                        if not variations_dir.exists():
                            missing_variations.append(str(template_dir))

        assert len(missing_variations) == 0, f"Missing variations/ directory in: {missing_variations}"

    def test_all_templates_have_cases_dir(self):
        """Test that all hybrid templates have cases/ directory."""
        missing_cases = []

        for category_dir in TEMPLATES_DIR.iterdir():
            if category_dir.is_dir():
                for template_dir in category_dir.iterdir():
                    if template_dir.is_dir() and template_dir.name.endswith("_hybrid"):
                        cases_dir = template_dir / "cases"
                        if not cases_dir.exists():
                            missing_cases.append(str(template_dir))

        assert len(missing_cases) == 0, f"Missing cases/ directory in: {missing_cases}"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
