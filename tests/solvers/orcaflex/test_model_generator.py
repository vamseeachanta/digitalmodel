"""
Unit tests for OrcaFlex Model Generator (model_generator package).

Regression coverage:
- Default components/templates directories must point at the real in-repo
  location docs/domains/orcaflex/templates (not the stale docs/modules path).
- Category/template names containing "/" must be resolved portably (the old
  code did replace("/", "\\\\") which produced literal-backslash filenames on
  Linux and broke every lookup).

No OrcFxAPI / licensed runtime required.
"""

import pytest
import yaml
from pathlib import Path

from digitalmodel.solvers.orcaflex.model_generator import (
    OrcaFlexModelGenerator,
    ComponentNotFoundError,
)

# Real in-repo component/template library
REPO_ROOT = Path(__file__).parent.parent.parent.parent
TEMPLATES_DIR = REPO_ROOT / "docs" / "domains" / "orcaflex" / "templates"
COMPONENTS_DIR = TEMPLATES_DIR / "components"


class TestDefaultPaths:
    """Defaults must point at docs/domains/orcaflex/templates[/components]."""

    def test_default_dirs_exist(self):
        generator = OrcaFlexModelGenerator()

        assert generator.components_dir.exists(), (
            f"Default components_dir missing: {generator.components_dir}"
        )
        assert generator.templates_dir.exists(), (
            f"Default templates_dir missing: {generator.templates_dir}"
        )
        assert generator.components_dir.resolve() == COMPONENTS_DIR.resolve()
        assert generator.templates_dir.resolve() == TEMPLATES_DIR.resolve()

    def test_default_get_component_vessels(self):
        """get_component('vessels', 'FPSO_P50') must work with default dirs."""
        generator = OrcaFlexModelGenerator()

        vessel = generator.get_component("vessels", "FPSO_P50")

        assert vessel["LOA"] == 300.0
        assert vessel["Displacement"] == 200000.0

    def test_default_list_components_vessels(self):
        generator = OrcaFlexModelGenerator()

        vessels = generator.list_components("vessels")

        assert "FPSO_P50" in vessels


class TestSlashCategoryHandling:
    """Slash-separated categories must resolve on POSIX (no literal '\\')."""

    @pytest.fixture
    def nested_components(self, tmp_path):
        """Component tree with a two-level category directory (cat/sub)."""
        sub_dir = tmp_path / "components" / "cat" / "sub"
        sub_dir.mkdir(parents=True)
        (sub_dir / "items.csv").write_text(
            "ComponentID,Value\nITEM_A,1.0\nITEM_B,2.0\n"
        )
        return tmp_path / "components"

    def test_list_components_directory_category_with_slash(self, nested_components):
        """'cat/sub' has no cat/sub.csv, so it must fall back to the
        cat/sub/ DIRECTORY — which the old backslash code never found."""
        generator = OrcaFlexModelGenerator(components_dir=nested_components)

        components = generator.list_components("cat/sub")

        assert components == ["ITEM_A", "ITEM_B"]

    def test_get_component_directory_category_with_slash(self, nested_components):
        generator = OrcaFlexModelGenerator(components_dir=nested_components)

        item = generator.get_component("cat/sub", "ITEM_B")

        assert item["Value"] == 2.0

    def test_get_component_missing_raises(self, nested_components):
        generator = OrcaFlexModelGenerator(components_dir=nested_components)

        with pytest.raises(ComponentNotFoundError):
            generator.get_component("cat/sub", "NOPE")

    def test_add_component_directory_category_with_slash(self, nested_components):
        generator = OrcaFlexModelGenerator(components_dir=nested_components)

        generator.add_component(
            category="cat/sub",
            component_id="ITEM_C",
            properties={"ComponentID": "ITEM_C", "Value": 3.0},
        )

        item = generator.get_component("cat/sub", "ITEM_C")
        assert item["Value"] == 3.0

    def test_lines_risers_csv_category(self):
        """'lines/risers' resolves to the real lines/risers.csv lookup."""
        generator = OrcaFlexModelGenerator()

        risers = generator.list_components("lines/risers")

        assert "SCR_10inch_X65" in risers


class TestGenerateFromTemplate:
    """Template names with '/' must resolve against the default library."""

    def test_generate_from_template_slash_name(self, tmp_path):
        """generate_from_template('risers/scr_catenary', ...) must find
        docs/domains/orcaflex/templates/risers/scr_catenary/model_template.yml
        on Linux (regression: backslash path + stale docs/modules default)."""
        generator = OrcaFlexModelGenerator()

        output = tmp_path / "model.yml"
        model = generator.generate_from_template(
            template="risers/scr_catenary",
            config={},
            output=output,
        )

        # From the real template
        assert model["Environment"]["WaterDepth"] == 1200.0
        assert model["_metadata"]["template"] == "risers/scr_catenary"

        # Output written and loadable
        assert output.exists()
        saved = yaml.safe_load(output.read_text())
        assert saved["Environment"]["WaterDepth"] == 1200.0

    def test_generate_from_template_missing_raises(self):
        generator = OrcaFlexModelGenerator()

        with pytest.raises(FileNotFoundError):
            generator.generate_from_template(
                template="risers/does_not_exist", config={}
            )
