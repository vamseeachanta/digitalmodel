#!/usr/bin/env python
# ABOUTME: Demo script showing skill resolver capabilities
# Demonstrates skill recommendation, dependency resolution, and version checking

"""
Demo skill resolver functionality.

Usage:
    python scripts/demo_skill_resolver.py
"""

from digitalmodel.skills.skill_resolver import SkillResolver


def print_section(title: str):
    """Print a formatted section header."""
    print(f"\n{'='*70}")
    print(f"{title:^70}")
    print(f"{'='*70}\n")


def demo_list_skills(resolver: SkillResolver):
    """Demo: List all available skills."""
    print_section("Available Skills")

    skills = resolver.list_all_skills()
    print(f"Total skills: {len(skills)}\n")

    for skill_name in skills:
        skill = resolver.get_skill(skill_name)
        print(f"  {skill.name:30s} v{skill.version:8s} [{skill.category}]")


def demo_skill_details(resolver: SkillResolver):
    """Demo: Show details for a specific skill."""
    print_section("Skill Details: aqwa-analysis")

    skill = resolver.get_skill("aqwa-analysis")

    print(f"Name: {skill.name}")
    print(f"Version: {skill.version}")
    print(f"Category: {skill.category}")
    print(f"Description: {skill.description[:100]}...")
    print(f"\nPython requirement: {skill.python_min_version}")
    print(f"OrcaFlex requirement: {skill.orcaflex_version}")

    print(f"\nKeywords ({len(skill.keywords)}):")
    for kw in skill.keywords[:5]:
        print(f"  - {kw}")
    if len(skill.keywords) > 5:
        print(f"  ... and {len(skill.keywords) - 5} more")

    print(f"\nDependencies:")
    if skill.dependencies:
        for dep_name, constraint in skill.dependencies.items():
            print(f"  - {dep_name}: {constraint}")
    else:
        print("  (none)")


def demo_dependency_resolution(resolver: SkillResolver):
    """Demo: Resolve skill dependencies."""
    print_section("Dependency Resolution")

    test_skills = ["fatigue-analysis", "mooring-design", "orcaflex-post-processing"]

    for skill_name in test_skills:
        print(f"\nResolving dependencies for: {skill_name}")
        deps = resolver.resolve_dependencies(skill_name)

        if deps:
            print(f"  Found {len(deps)} dependencies:")
            for dep_name, constraint in deps:
                dep_skill = resolver.get_skill(dep_name)
                if dep_skill:
                    status = "[OK]" if resolver.check_version_compatibility(
                        dep_name, constraint
                    ) else "[FAIL]"
                    print(f"    {status} {dep_name} {constraint} (current: v{dep_skill.version})")
                else:
                    print(f"    [MISSING] {dep_name} {constraint}")
        else:
            print("  No dependencies")


def demo_skill_recommendation(resolver: SkillResolver):
    """Demo: Recommend skills based on task description."""
    print_section("Skill Recommendation")

    test_tasks = [
        "I need to extract RAOs from AQWA results",
        "Design a mooring system for an FPSO",
        "Convert OrcaFlex .dat files to YAML format",
        "Perform fatigue analysis on riser stress data",
        "Generate mesh for hydrodynamic analysis"
    ]

    for task in test_tasks:
        print(f"\nTask: '{task}'")
        recommendations = resolver.recommend_skills(task, max_results=3)

        if recommendations:
            print(f"  Top {len(recommendations)} recommendations:")
            for i, rec in enumerate(recommendations, 1):
                print(f"\n  {i}. {rec.skill.name} (score: {rec.relevance_score:.1f})")
                print(f"     {rec.skill.description[:80]}...")
                if rec.matched_keywords:
                    print(f"     Matched: {', '.join(rec.matched_keywords[:3])}")
                if rec.dependencies_included:
                    print(f"     Requires: {', '.join(rec.dependencies_included[:2])}")
        else:
            print("  No recommendations found")


def demo_version_compatibility(resolver: SkillResolver):
    """Demo: Check version compatibility."""
    print_section("Version Compatibility Checking")

    test_cases = [
        ("1.5.0", ">=1.0.0,<2.0.0", True),
        ("2.0.0", ">=1.0.0,<2.0.0", False),
        ("1.0.0", ">=1.0.0", True),
        ("0.9.0", ">=1.0.0", False),
    ]

    print("Testing version constraint matching:\n")
    for version, constraint, expected in test_cases:
        result = resolver._version_satisfies_constraint(version, constraint)
        status = "[PASS]" if result == expected else "[FAIL]"
        print(f"  {status} {version} satisfies '{constraint}': {result}")


def demo_breaking_changes(resolver: SkillResolver):
    """Demo: Detect breaking changes."""
    print_section("Breaking Change Detection")

    skill_name = "aqwa-analysis"
    skill = resolver.get_skill(skill_name)

    print(f"Current version of {skill_name}: {skill.version}\n")

    test_versions = [
        "1.0.0",
        "2.9.9",
        f"{skill.major_version}.0.0",
        f"{skill.major_version}.{skill.minor_version}.0"
    ]

    print("Testing for breaking changes from:")
    for from_version in test_versions:
        is_breaking = resolver.check_breaking_changes(skill_name, from_version)
        status = "[BREAKING]" if is_breaking else "[SAFE]"
        print(f"  {status} {from_version} -> {skill.version}")


def demo_validate_dependencies(resolver: SkillResolver):
    """Demo: Validate all dependencies."""
    print_section("Dependency Validation")

    skills_with_deps = [
        name for name, skill in resolver.skills.items()
        if skill.dependencies
    ]

    print(f"Validating {len(skills_with_deps)} skills with dependencies:\n")

    for skill_name in skills_with_deps[:5]:  # Show first 5
        validation = resolver.validate_dependencies(skill_name)
        all_valid = all(validation.values())
        status = "[OK]" if all_valid else "[WARN]"

        print(f"{status} {skill_name}:")
        for dep_name, is_valid in validation.items():
            dep_status = "  [OK]" if is_valid else "  [FAIL]"
            print(f"    {dep_status} {dep_name}")


def demo_python_compatibility(resolver: SkillResolver):
    """Demo: Filter by Python version."""
    print_section("Python Version Compatibility")

    python_versions = ["3.9.0", "3.10.0", "3.13.5"]

    for py_version in python_versions:
        recommendations = resolver.recommend_skills(
            "offshore analysis",
            python_version=py_version
        )
        print(f"Python {py_version}: {len(recommendations)} compatible skills")


def main():
    """Run all demos."""
    print("\n" + "="*70)
    print("SKILL RESOLVER DEMO".center(70))
    print("="*70)

    # Initialize resolver
    print("\nInitializing skill resolver...")
    resolver = SkillResolver()
    print(f"Loaded {len(resolver.skills)} skills from {resolver.skills_directory}")

    # Run demos
    demo_list_skills(resolver)
    demo_skill_details(resolver)
    demo_dependency_resolution(resolver)
    demo_skill_recommendation(resolver)
    demo_version_compatibility(resolver)
    demo_breaking_changes(resolver)
    demo_validate_dependencies(resolver)
    demo_python_compatibility(resolver)

    # Final summary
    print_section("Demo Complete")
    print("Skill resolver features demonstrated:")
    print("  [OK] Skill loading and metadata parsing")
    print("  [OK] Version compatibility checking")
    print("  [OK] Dependency resolution")
    print("  [OK] NLP-based skill recommendation")
    print("  [OK] Breaking change detection")
    print("  [OK] Python version filtering")
    print("\nFor more information, see:")
    print("  - src/digitalmodel/modules/skills/skill_resolver.py")
    print("  - tests/test_skill_resolver.py")
    print("  - .claude/skills/skills-catalog.json")
    print()


if __name__ == "__main__":
    main()
