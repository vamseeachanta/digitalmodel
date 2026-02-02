# ABOUTME: Tests for skill versioning and dependency resolution
# Tests skill loading, version compatibility, dependency resolution, and recommendations

import json
import pytest
from pathlib import Path

from digitalmodel.skills.skill_resolver import (
    SkillResolver,
    SkillMetadata,
    SkillRecommendation
)


@pytest.fixture
def resolver():
    """Create a skill resolver instance."""
    return SkillResolver()


class TestSkillLoading:
    """Test skill metadata loading."""

    def test_loads_all_skills(self, resolver):
        """Test that all skills are loaded."""
        assert len(resolver.skills) >= 17  # We have 17 skills

    def test_skill_has_metadata(self, resolver):
        """Test that skills have required metadata fields."""
        skill = resolver.get_skill("aqwa-analysis")
        assert skill is not None
        assert skill.name == "aqwa-analysis"
        assert skill.version
        assert skill.description
        assert skill.category

    def test_version_format(self, resolver):
        """Test that versions follow semantic versioning."""
        for name, skill in resolver.skills.items():
            # Should be in format X.Y.Z
            parts = skill.version.split('.')
            assert len(parts) == 3, f"{name} has invalid version: {skill.version}"
            assert all(p.isdigit() for p in parts)

    def test_list_all_skills(self, resolver):
        """Test listing all skill names."""
        skills = resolver.list_all_skills()
        assert isinstance(skills, list)
        assert len(skills) >= 17
        assert "aqwa-analysis" in skills
        assert "orcaflex-modeling" in skills

    def test_catalog_exists(self, resolver):
        """Test that catalog file is created."""
        assert resolver.catalog_path.exists()

        with open(resolver.catalog_path, 'r') as f:
            catalog = json.load(f)

        assert "skills" in catalog
        assert "version" in catalog
        assert len(catalog["skills"]) >= 17


class TestVersionCompatibility:
    """Test version compatibility checking."""

    def test_exact_version_match(self, resolver):
        """Test exact version matching."""
        assert resolver._version_satisfies_constraint("1.0.0", "==1.0.0")
        assert not resolver._version_satisfies_constraint("1.0.1", "==1.0.0")

    def test_greater_than_or_equal(self, resolver):
        """Test >= constraint."""
        assert resolver._version_satisfies_constraint("1.2.0", ">=1.0.0")
        assert resolver._version_satisfies_constraint("2.0.0", ">=1.0.0")
        assert not resolver._version_satisfies_constraint("0.9.0", ">=1.0.0")

    def test_less_than(self, resolver):
        """Test < constraint."""
        assert resolver._version_satisfies_constraint("1.9.9", "<2.0.0")
        assert not resolver._version_satisfies_constraint("2.0.0", "<2.0.0")
        assert not resolver._version_satisfies_constraint("2.1.0", "<2.0.0")

    def test_combined_constraints(self, resolver):
        """Test multiple constraints."""
        # Version should be >= 1.0.0 AND < 2.0.0
        assert resolver._version_satisfies_constraint("1.5.0", ">=1.0.0,<2.0.0")
        assert not resolver._version_satisfies_constraint("0.9.0", ">=1.0.0,<2.0.0")
        assert not resolver._version_satisfies_constraint("2.0.0", ">=1.0.0,<2.0.0")

    def test_check_version_compatibility(self, resolver):
        """Test skill version compatibility checking."""
        # aqwa-analysis should be compatible with its current version
        skill = resolver.get_skill("aqwa-analysis")
        assert resolver.check_version_compatibility(
            "aqwa-analysis",
            f">={skill.version}"
        )


class TestDependencyResolution:
    """Test dependency resolution."""

    def test_no_dependencies(self, resolver):
        """Test skill with no dependencies."""
        deps = resolver.resolve_dependencies("signal-analysis")
        assert deps == []

    def test_single_dependency(self, resolver):
        """Test skill with one dependency."""
        deps = resolver.resolve_dependencies("aqwa-analysis")
        assert len(deps) >= 1
        # Should include hydrodynamics
        dep_names = [d[0] for d in deps]
        assert "hydrodynamics" in dep_names

    def test_multi_level_dependencies(self, resolver):
        """Test recursive dependency resolution."""
        # fatigue-analysis depends on signal-analysis and structural-analysis
        deps = resolver.resolve_dependencies("fatigue-analysis")
        dep_names = [d[0] for d in deps]
        assert "signal-analysis" in dep_names
        assert "structural-analysis" in dep_names

    def test_circular_dependency_prevention(self, resolver):
        """Test that circular dependencies don't cause infinite loops."""
        # Should handle gracefully even if circular deps exist
        for skill_name in resolver.list_all_skills():
            deps = resolver.resolve_dependencies(skill_name)
            # Should return a finite list
            assert isinstance(deps, list)

    def test_validate_dependencies(self, resolver):
        """Test dependency validation."""
        validation = resolver.validate_dependencies("aqwa-analysis")
        assert isinstance(validation, dict)
        # hydrodynamics should be valid
        if "hydrodynamics" in validation:
            assert validation["hydrodynamics"] is True


class TestBreakingChanges:
    """Test breaking change detection."""

    def test_major_version_breaking(self, resolver):
        """Test that major version changes are detected as breaking."""
        assert resolver.check_breaking_changes("aqwa-analysis", "1.0.0")
        assert resolver.check_breaking_changes("aqwa-analysis", "2.9.9")

    def test_minor_version_not_breaking(self, resolver):
        """Test that minor version changes are not breaking."""
        skill = resolver.get_skill("aqwa-analysis")
        current_major = skill.major_version
        assert not resolver.check_breaking_changes(
            "aqwa-analysis",
            f"{current_major}.0.0"
        )

    def test_patch_version_not_breaking(self, resolver):
        """Test that patch version changes are not breaking."""
        skill = resolver.get_skill("aqwa-analysis")
        from_version = f"{skill.major_version}.{skill.minor_version}.0"
        assert not resolver.check_breaking_changes("aqwa-analysis", from_version)


class TestSkillRecommendation:
    """Test NLP-based skill recommendation."""

    def test_recommend_by_keyword(self, resolver):
        """Test recommendation based on keywords."""
        recommendations = resolver.recommend_skills("AQWA RAO extraction")
        assert len(recommendations) > 0
        # aqwa-analysis should be top recommendation
        assert recommendations[0].skill.name == "aqwa-analysis"

    def test_recommend_by_description(self, resolver):
        """Test recommendation based on description words."""
        recommendations = resolver.recommend_skills(
            "I need to analyze mooring systems"
        )
        assert len(recommendations) > 0
        # mooring-design should be in recommendations
        skill_names = [r.skill.name for r in recommendations]
        assert "mooring-design" in skill_names

    def test_recommendation_scoring(self, resolver):
        """Test that recommendations are scored and sorted."""
        recommendations = resolver.recommend_skills(
            "OrcaFlex hydrodynamic simulation"
        )
        assert len(recommendations) > 0
        # Scores should be decreasing
        for i in range(len(recommendations) - 1):
            assert recommendations[i].relevance_score >= recommendations[i + 1].relevance_score

    def test_max_results(self, resolver):
        """Test max_results parameter."""
        recommendations = resolver.recommend_skills(
            "offshore engineering analysis",
            max_results=3
        )
        assert len(recommendations) <= 3

    def test_python_version_filter(self, resolver):
        """Test filtering by Python version."""
        # All skills should be compatible with Python 3.13.5
        recommendations = resolver.recommend_skills(
            "analysis",
            python_version="3.13.5"
        )
        assert len(recommendations) > 0

        # Try incompatible Python version (should filter out)
        recommendations_old = resolver.recommend_skills(
            "analysis",
            python_version="3.9.0"
        )
        # Should have fewer or same recommendations
        assert len(recommendations_old) <= len(recommendations)

    def test_matched_keywords_included(self, resolver):
        """Test that matched keywords are included in recommendations."""
        recommendations = resolver.recommend_skills("AQWA diffraction")
        if recommendations:
            rec = recommendations[0]
            assert isinstance(rec.matched_keywords, list)

    def test_dependencies_included(self, resolver):
        """Test that dependencies are included in recommendations."""
        recommendations = resolver.recommend_skills("fatigue analysis")
        if recommendations:
            rec = next(
                (r for r in recommendations if r.skill.name == "fatigue-analysis"),
                None
            )
            if rec:
                # Should include dependency names
                assert isinstance(rec.dependencies_included, list)


class TestOrcaFlexCompatibility:
    """Test OrcaFlex version compatibility."""

    def test_orcaflex_version_tracking(self, resolver):
        """Test that OrcaFlex versions are tracked."""
        skill = resolver.get_skill("orcaflex-modeling")
        assert skill.orcaflex_version is not None
        assert ">=" in skill.orcaflex_version

    def test_non_orcaflex_skills(self, resolver):
        """Test that non-OrcaFlex skills have no version requirement."""
        skill = resolver.get_skill("cad-engineering")
        assert skill.orcaflex_version is None


class TestSkillMetadata:
    """Test SkillMetadata dataclass."""

    def test_version_properties(self):
        """Test version property extraction."""
        metadata = SkillMetadata(
            name="test-skill",
            version="2.5.3",
            description="Test",
            category="test"
        )
        assert metadata.major_version == 2
        assert metadata.minor_version == 5
        assert metadata.patch_version == 3

    def test_metadata_defaults(self):
        """Test default values for optional fields."""
        metadata = SkillMetadata(
            name="test",
            version="1.0.0",
            description="Test",
            category="test"
        )
        assert metadata.python_min_version is None
        assert metadata.orcaflex_version is None
        assert metadata.dependencies == {}
        assert metadata.keywords == []


class TestIntegration:
    """Integration tests for complete workflows."""

    def test_find_skill_and_resolve_deps(self, resolver):
        """Test complete workflow: find skill -> resolve dependencies."""
        # Find skill
        recommendations = resolver.recommend_skills("mooring design")
        assert len(recommendations) > 0

        skill_name = recommendations[0].skill.name

        # Resolve dependencies
        deps = resolver.resolve_dependencies(skill_name)

        # Validate all dependencies
        validation = resolver.validate_dependencies(skill_name)
        for dep_name, is_valid in validation.items():
            assert is_valid, f"Dependency {dep_name} is not valid"

    def test_check_compatibility_chain(self, resolver):
        """Test checking compatibility across dependency chain."""
        # Get a skill with dependencies
        skill = resolver.get_skill("mooring-design")

        # Check each dependency
        for dep_name, version_constraint in skill.dependencies.items():
            is_compatible = resolver.check_version_compatibility(
                dep_name,
                version_constraint
            )
            assert is_compatible, f"{dep_name} {version_constraint} not compatible"

    def test_export_and_reload_catalog(self, resolver):
        """Test exporting catalog and reloading it."""
        # Export
        resolver.export_catalog()

        # Read catalog
        with open(resolver.catalog_path, 'r') as f:
            catalog = json.load(f)

        # Verify structure
        assert "version" in catalog
        assert "skills" in catalog
        assert len(catalog["skills"]) == len(resolver.skills)

        # Verify a specific skill
        aqwa = catalog["skills"]["aqwa-analysis"]
        assert aqwa["version"]
        assert aqwa["description"]
        assert "dependencies" in aqwa
