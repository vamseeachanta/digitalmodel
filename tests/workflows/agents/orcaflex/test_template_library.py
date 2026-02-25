"""
ABOUTME: Tests for the OrcaFlex template library — get_orcaflex_template, list_structure_types,
ABOUTME: get_all_templates, alias resolution, error handling, and render path.
"""

import pytest
from pathlib import Path

from digitalmodel.workflows.agents.orcaflex.template_library import (
    ALIASES,
    STRUCTURE_TYPES,
    get_all_templates,
    get_orcaflex_template,
    list_structure_types,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

REQUIRED_KEYS = {"structure_type", "template_file", "template_path", "content", "rendered"}


# ---------------------------------------------------------------------------
# get_orcaflex_template — return shape
# ---------------------------------------------------------------------------


class TestGetOrcaflexTemplateReturnShape:
    def test_general_returns_all_required_keys(self):
        result = get_orcaflex_template("general")
        assert REQUIRED_KEYS == set(result.keys())

    def test_environment_returns_all_required_keys(self):
        result = get_orcaflex_template("environment")
        assert REQUIRED_KEYS == set(result.keys())

    def test_vessel_returns_all_required_keys(self):
        result = get_orcaflex_template("vessel")
        assert REQUIRED_KEYS == set(result.keys())

    def test_vessel_instance_returns_all_required_keys(self):
        result = get_orcaflex_template("vessel_instance")
        assert REQUIRED_KEYS == set(result.keys())

    def test_line_types_returns_all_required_keys(self):
        result = get_orcaflex_template("line_types")
        assert REQUIRED_KEYS == set(result.keys())

    def test_lines_returns_all_required_keys(self):
        result = get_orcaflex_template("lines")
        assert REQUIRED_KEYS == set(result.keys())

    def test_buoys_returns_all_required_keys(self):
        result = get_orcaflex_template("buoys")
        assert REQUIRED_KEYS == set(result.keys())

    def test_groups_returns_all_required_keys(self):
        result = get_orcaflex_template("groups")
        assert REQUIRED_KEYS == set(result.keys())


# ---------------------------------------------------------------------------
# get_orcaflex_template — template_path is a real file
# ---------------------------------------------------------------------------


class TestGetOrcaflexTemplateFilePaths:
    def test_general_template_path_exists(self):
        result = get_orcaflex_template("general")
        assert Path(result["template_path"]).exists()

    def test_vessel_template_path_exists(self):
        result = get_orcaflex_template("vessel")
        assert Path(result["template_path"]).exists()

    def test_lines_template_path_exists(self):
        result = get_orcaflex_template("lines")
        assert Path(result["template_path"]).exists()

    def test_all_eight_base_types_template_path_exists(self):
        for stype in STRUCTURE_TYPES:
            result = get_orcaflex_template(stype)
            assert Path(result["template_path"]).exists(), (
                f"template_path missing for '{stype}': {result['template_path']}"
            )

    def test_template_path_is_absolute(self):
        result = get_orcaflex_template("general")
        assert Path(result["template_path"]).is_absolute()


# ---------------------------------------------------------------------------
# get_orcaflex_template — content is non-empty
# ---------------------------------------------------------------------------


class TestGetOrcaflexTemplateContent:
    def test_general_content_is_nonempty_string(self):
        result = get_orcaflex_template("general")
        assert isinstance(result["content"], str)
        assert len(result["content"]) > 0

    def test_all_eight_base_types_content_nonempty(self):
        for stype in STRUCTURE_TYPES:
            result = get_orcaflex_template(stype)
            assert isinstance(result["content"], str) and result["content"], (
                f"content is empty for '{stype}'"
            )

    def test_template_file_name_matches_structure_types_map(self):
        for stype, expected_file in STRUCTURE_TYPES.items():
            result = get_orcaflex_template(stype)
            assert result["template_file"] == expected_file


# ---------------------------------------------------------------------------
# Alias resolution
# ---------------------------------------------------------------------------


class TestAliasResolution:
    def test_riser_resolves_to_lines(self):
        result = get_orcaflex_template("riser")
        assert result["structure_type"] == "lines"

    def test_mooring_resolves_to_lines(self):
        result = get_orcaflex_template("mooring")
        assert result["structure_type"] == "lines"

    def test_hull_resolves_to_vessel(self):
        result = get_orcaflex_template("hull")
        assert result["structure_type"] == "vessel"

    def test_catenary_resolves_to_lines(self):
        result = get_orcaflex_template("catenary")
        assert result["structure_type"] == "lines"

    def test_tether_resolves_to_lines(self):
        result = get_orcaflex_template("tether")
        assert result["structure_type"] == "lines"

    def test_alias_returns_same_content_as_canonical(self):
        riser_result = get_orcaflex_template("riser")
        lines_result = get_orcaflex_template("lines")
        assert riser_result["content"] == lines_result["content"]

    def test_alias_template_file_matches_canonical(self):
        hull_result = get_orcaflex_template("hull")
        vessel_result = get_orcaflex_template("vessel")
        assert hull_result["template_file"] == vessel_result["template_file"]


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------


class TestGetOrcaflexTemplateErrors:
    def test_unknown_type_raises_value_error(self):
        with pytest.raises(ValueError, match="Unknown structure_type"):
            get_orcaflex_template("nonexistent_type")

    def test_empty_string_raises_value_error(self):
        with pytest.raises(ValueError):
            get_orcaflex_template("")

    def test_error_message_includes_structure_type(self):
        with pytest.raises(ValueError, match="bad_type"):
            get_orcaflex_template("bad_type")

    def test_error_message_lists_known_types(self):
        with pytest.raises(ValueError, match="general"):
            get_orcaflex_template("unknown_xyz")


# ---------------------------------------------------------------------------
# render=True behaviour
# ---------------------------------------------------------------------------


class TestRenderBehaviour:
    def test_render_false_returns_none_rendered(self):
        result = get_orcaflex_template("general", render=False)
        assert result["rendered"] is None

    def test_render_true_without_context_returns_none_rendered(self):
        result = get_orcaflex_template("general", render=True, context=None)
        assert result["rendered"] is None

    def test_render_true_with_context_returns_string(self):
        context = {
            "project_name": "test_project",
            "generation_date": "2026-01-01",
            "stage_duration_buildup": 10,
            "stage_duration_simulation": 100,
            "statics_min_damping": 5,
            "dynamics_solution_method": "Implicit time domain",
        }
        result = get_orcaflex_template("general", render=True, context=context)
        assert isinstance(result["rendered"], str)
        assert len(result["rendered"]) > 0

    def test_render_true_with_context_substitutes_values(self):
        context = {
            "project_name": "MY_TEST_PROJECT",
            "generation_date": "2026-01-01",
            "stage_duration_buildup": 15,
            "stage_duration_simulation": 200,
            "statics_min_damping": 3,
            "dynamics_solution_method": "Implicit time domain",
        }
        result = get_orcaflex_template("general", render=True, context=context)
        assert "MY_TEST_PROJECT" in result["rendered"]


# ---------------------------------------------------------------------------
# list_structure_types
# ---------------------------------------------------------------------------


class TestListStructureTypes:
    def test_returns_a_list(self):
        result = list_structure_types()
        assert isinstance(result, list)

    def test_includes_all_eight_base_types(self):
        result = list_structure_types()
        for stype in STRUCTURE_TYPES:
            assert stype in result, f"Base type '{stype}' missing from list"

    def test_includes_aliases(self):
        result = list_structure_types()
        for alias in ALIASES:
            assert alias in result, f"Alias '{alias}' missing from list"

    def test_total_count_matches_base_plus_aliases(self):
        result = list_structure_types()
        assert len(result) == len(STRUCTURE_TYPES) + len(ALIASES)

    def test_riser_alias_in_list(self):
        assert "riser" in list_structure_types()

    def test_mooring_alias_in_list(self):
        assert "mooring" in list_structure_types()

    def test_hull_alias_in_list(self):
        assert "hull" in list_structure_types()


# ---------------------------------------------------------------------------
# get_all_templates
# ---------------------------------------------------------------------------


class TestGetAllTemplates:
    def test_returns_a_list(self):
        result = get_all_templates()
        assert isinstance(result, list)

    def test_returns_eight_entries(self):
        result = get_all_templates()
        assert len(result) == 8

    def test_each_entry_has_required_metadata_keys(self):
        metadata_keys = {"structure_type", "template_file", "template_path"}
        for entry in get_all_templates():
            assert metadata_keys <= set(entry.keys()), (
                f"Entry missing keys: {entry}"
            )

    def test_no_content_key_in_entries(self):
        for entry in get_all_templates():
            assert "content" not in entry

    def test_all_template_paths_exist(self):
        for entry in get_all_templates():
            assert Path(entry["template_path"]).exists(), (
                f"template_path missing: {entry['template_path']}"
            )

    def test_structure_types_match_canonical_map(self):
        entries = get_all_templates()
        returned_types = {e["structure_type"] for e in entries}
        assert returned_types == set(STRUCTURE_TYPES.keys())
