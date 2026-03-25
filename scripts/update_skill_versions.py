#!/usr/bin/env python
# ABOUTME: Interactive script to update skill versions with changelog management
# Prompts for version bump type (major/minor/patch) and changelog entry for each skill

"""
Update skill versions interactively.

Usage:
    python scripts/update_skill_versions.py
    python scripts/update_skill_versions.py --skill aqwa-analysis
    python scripts/update_skill_versions.py --bump patch --all
"""

import argparse
import re
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import yaml


def parse_version(version: str) -> Tuple[int, int, int]:
    """Parse semantic version string."""
    parts = version.split('.')
    return int(parts[0]), int(parts[1]), int(parts[2])


def bump_version(version: str, bump_type: str) -> str:
    """
    Bump version number.

    Args:
        version: Current version (e.g., "1.2.3")
        bump_type: One of 'major', 'minor', 'patch'

    Returns:
        New version string
    """
    major, minor, patch = parse_version(version)

    if bump_type == 'major':
        return f"{major + 1}.0.0"
    elif bump_type == 'minor':
        return f"{major}.{minor + 1}.0"
    elif bump_type == 'patch':
        return f"{major}.{minor}.{patch + 1}"
    else:
        raise ValueError(f"Invalid bump type: {bump_type}")


def extract_frontmatter(content: str) -> Tuple[Dict, str]:
    """Extract YAML frontmatter from SKILL.md content."""
    match = re.match(r'^---\s*\n(.*?)\n---\s*\n(.*)', content, re.DOTALL)
    if not match:
        raise ValueError("No YAML frontmatter found")

    frontmatter = yaml.safe_load(match.group(1))
    body = match.group(2)

    return frontmatter, body


def extract_version_metadata(content: str) -> Optional[Dict]:
    """Extract version metadata section if it exists."""
    match = re.search(
        r'## Version Metadata\s*\n\s*```yaml\s*\n(.*?)\n\s*```',
        content,
        re.DOTALL
    )

    if match:
        try:
            return yaml.safe_load(match.group(1))
        except yaml.YAMLError:
            return None

    return None


def extract_changelog(content: str) -> Optional[str]:
    """Extract existing changelog section."""
    match = re.search(
        r'## Changelog\s*\n(.*?)(?=\n## |$)',
        content,
        re.DOTALL
    )

    if match:
        return match.group(1).strip()

    return None


def create_version_metadata(
    version: str,
    python_min: str = "3.10",
    dependencies: Optional[Dict[str, str]] = None,
    orcaflex_version: Optional[str] = None
) -> str:
    """Create version metadata YAML block."""
    metadata = {
        "version": version,
        "python_min_version": python_min,
    }

    if orcaflex_version:
        metadata["orcaflex_version"] = orcaflex_version

    if dependencies:
        metadata["dependencies"] = dependencies

    metadata["compatibility"] = {
        "tested_python": ["3.10", "3.11", "3.12", "3.13"],
        "os": ["Windows", "Linux", "macOS"]
    }

    yaml_str = yaml.dump(metadata, default_flow_style=False, sort_keys=False)

    return f"## Version Metadata\n\n```yaml\n{yaml_str}```\n"


def create_changelog_entry(version: str, changes: Dict[str, List[str]]) -> str:
    """
    Create changelog entry for a version.

    Args:
        version: Version number
        changes: Dictionary with categories (Added, Changed, Fixed, Removed)

    Returns:
        Formatted changelog entry
    """
    date = datetime.now().strftime("%Y-%m-%d")
    lines = [f"### [{version}] - {date}\n"]

    for category in ["Added", "Changed", "Fixed", "Removed"]:
        if category in changes and changes[category]:
            lines.append(f"\n**{category}:**")
            for item in changes[category]:
                lines.append(f"- {item}")

    return "\n".join(lines)


def update_skill_file(
    skill_path: Path,
    new_version: str,
    changelog_changes: Dict[str, List[str]],
    dependencies: Optional[Dict[str, str]] = None,
    orcaflex_version: Optional[str] = None
) -> None:
    """
    Update a skill file with new version and changelog.

    Args:
        skill_path: Path to SKILL.md file
        new_version: New version number
        changelog_changes: Changes for this version
        dependencies: Optional skill dependencies
        orcaflex_version: Optional OrcaFlex version requirement
    """
    with open(skill_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Extract frontmatter and body
    frontmatter, body = extract_frontmatter(content)

    # Update version in frontmatter
    frontmatter['version'] = new_version
    frontmatter['updated'] = datetime.now().strftime("%Y-%m-%d")

    # Create new frontmatter
    new_frontmatter = yaml.dump(frontmatter, default_flow_style=False, sort_keys=False)

    # Check if version metadata exists
    existing_metadata = extract_version_metadata(content)
    if existing_metadata:
        # Update existing metadata
        existing_metadata['version'] = new_version
        if dependencies:
            existing_metadata['dependencies'] = dependencies
        if orcaflex_version:
            existing_metadata['orcaflex_version'] = orcaflex_version

        metadata_yaml = yaml.dump(existing_metadata, default_flow_style=False, sort_keys=False)
        new_metadata = f"## Version Metadata\n\n```yaml\n{metadata_yaml}```\n"

        # Replace in body
        body = re.sub(
            r'## Version Metadata\s*\n\s*```yaml\s*\n.*?\n\s*```',
            new_metadata.rstrip(),
            body,
            flags=re.DOTALL
        )
    else:
        # Insert new metadata section after first heading
        new_metadata = create_version_metadata(
            new_version,
            dependencies=dependencies,
            orcaflex_version=orcaflex_version
        )
        # Find first ## heading
        match = re.search(r'\n## ', body)
        if match:
            insert_pos = match.start()
            body = body[:insert_pos] + f"\n{new_metadata}\n" + body[insert_pos:]

    # Update or create changelog
    existing_changelog = extract_changelog(body)
    new_entry = create_changelog_entry(new_version, changelog_changes)

    if existing_changelog:
        # Prepend new entry to existing changelog
        new_changelog = f"## Changelog\n\n{new_entry}\n\n{existing_changelog}"
        body = re.sub(
            r'## Changelog\s*\n.*?(?=\n## |$)',
            new_changelog,
            body,
            flags=re.DOTALL
        )
    else:
        # Add new changelog section before Version History or at end
        match = re.search(r'\n## Version History', body)
        if match:
            insert_pos = match.start()
            body = body[:insert_pos] + f"\n## Changelog\n\n{new_entry}\n" + body[insert_pos:]
        else:
            body += f"\n\n## Changelog\n\n{new_entry}\n"

    # Write updated file
    new_content = f"---\n{new_frontmatter}---\n{body}"

    with open(skill_path, 'w', encoding='utf-8') as f:
        f.write(new_content)


def interactive_update(skill_path: Path) -> None:
    """Interactive update for a single skill."""
    skill_name = skill_path.parent.name

    with open(skill_path, 'r', encoding='utf-8') as f:
        content = f.read()

    frontmatter, _ = extract_frontmatter(content)
    current_version = frontmatter.get('version', '1.0.0')

    print(f"\n{'='*60}")
    print(f"Skill: {skill_name}")
    print(f"Current version: {current_version}")
    print(f"{'='*60}")

    # Ask for bump type
    print("\nVersion bump type:")
    print("  1. Major (breaking changes)")
    print("  2. Minor (new features, backwards compatible)")
    print("  3. Patch (bug fixes)")
    print("  4. Skip")

    choice = input("\nSelect (1-4): ").strip()

    if choice == '4':
        print("Skipped.")
        return

    bump_type = {'1': 'major', '2': 'minor', '3': 'patch'}.get(choice)
    if not bump_type:
        print("Invalid choice. Skipped.")
        return

    new_version = bump_version(current_version, bump_type)
    print(f"New version: {new_version}")

    # Collect changelog entries
    changes = {}
    for category in ["Added", "Changed", "Fixed", "Removed"]:
        print(f"\n{category} (enter items, empty line to finish):")
        items = []
        while True:
            item = input(f"  - ").strip()
            if not item:
                break
            items.append(item)
        if items:
            changes[category] = items

    if not changes:
        print("No changelog entries. Skipped.")
        return

    # Ask about dependencies
    deps_input = input("\nDependencies (format: skill-name>=1.0.0,<2.0.0; leave empty for none): ").strip()
    dependencies = None
    if deps_input:
        dependencies = {}
        for dep in deps_input.split(';'):
            dep = dep.strip()
            if ':' in dep:
                name, constraint = dep.split(':', 1)
                dependencies[name.strip()] = constraint.strip()

    # Ask about OrcaFlex version
    orcaflex_input = input("OrcaFlex version requirement (e.g., >=11.0; leave empty for none): ").strip()
    orcaflex_version = orcaflex_input if orcaflex_input else None

    # Confirm
    print("\nSummary:")
    print(f"  Version: {current_version} -> {new_version}")
    print(f"  Changes: {sum(len(v) for v in changes.values())} items")
    if dependencies:
        print(f"  Dependencies: {list(dependencies.keys())}")
    if orcaflex_version:
        print(f"  OrcaFlex: {orcaflex_version}")

    confirm = input("\nProceed? (y/n): ").strip().lower()
    if confirm != 'y':
        print("Cancelled.")
        return

    # Update file
    update_skill_file(skill_path, new_version, changes, dependencies, orcaflex_version)
    print(f"✅ Updated {skill_name} to {new_version}")


def batch_update(skills_dir: Path, bump_type: str) -> None:
    """Batch update all skills with same bump type."""
    for skill_dir in sorted(skills_dir.iterdir()):
        if skill_dir.is_dir():
            skill_md = skill_dir / "SKILL.md"
            if skill_md.exists():
                try:
                    with open(skill_md, 'r', encoding='utf-8') as f:
                        content = f.read()

                    frontmatter, _ = extract_frontmatter(content)
                    current_version = frontmatter.get('version', '1.0.0')
                    new_version = bump_version(current_version, bump_type)

                    # Use generic changelog
                    changes = {
                        "Changed": [f"Version bump to {new_version}"]
                    }

                    update_skill_file(skill_md, new_version, changes)
                    print(f"✅ Updated {skill_dir.name}: {current_version} -> {new_version}")

                except Exception as e:
                    print(f"❌ Failed to update {skill_dir.name}: {e}")


def main():
    parser = argparse.ArgumentParser(description="Update skill versions")
    parser.add_argument("--skill", help="Update specific skill only")
    parser.add_argument("--bump", choices=['major', 'minor', 'patch'], help="Bump type for batch update")
    parser.add_argument("--all", action="store_true", help="Update all skills (requires --bump)")
    args = parser.parse_args()

    # Find skills directory
    script_dir = Path(__file__).parent
    project_root = script_dir.parent
    skills_dir = project_root / ".claude" / "skills"

    if not skills_dir.exists():
        print(f"Error: Skills directory not found: {skills_dir}")
        sys.exit(1)

    if args.all:
        if not args.bump:
            print("Error: --all requires --bump")
            sys.exit(1)
        batch_update(skills_dir, args.bump)
    elif args.skill:
        skill_path = skills_dir / args.skill / "SKILL.md"
        if not skill_path.exists():
            print(f"Error: Skill not found: {args.skill}")
            sys.exit(1)
        interactive_update(skill_path)
    else:
        # Interactive mode for all skills
        for skill_dir in sorted(skills_dir.iterdir()):
            if skill_dir.is_dir():
                skill_md = skill_dir / "SKILL.md"
                if skill_md.exists():
                    try:
                        interactive_update(skill_md)
                    except KeyboardInterrupt:
                        print("\n\nInterrupted by user.")
                        sys.exit(0)
                    except Exception as e:
                        print(f"❌ Error updating {skill_dir.name}: {e}")

    print("\n✅ Version update complete!")


if __name__ == "__main__":
    main()
