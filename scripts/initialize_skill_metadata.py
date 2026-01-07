#!/usr/bin/env python
# ABOUTME: Initialize version metadata and changelog for all skills
# One-time script to add version metadata blocks to existing SKILL.md files

"""
Initialize skill version metadata and changelog sections.

Usage:
    python scripts/initialize_skill_metadata.py
"""

import re
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

import yaml


# Skill dependency mapping (based on domain knowledge)
SKILL_DEPENDENCIES = {
    "aqwa-analysis": {
        "hydrodynamics": ">=1.0.0,<2.0.0"
    },
    "catenary-riser": {
        "orcaflex-modeling": ">=2.0.0,<3.0.0"
    },
    "fatigue-analysis": {
        "signal-analysis": ">=1.0.0,<2.0.0",
        "structural-analysis": ">=1.0.0,<2.0.0"
    },
    "freecad-automation": {
        "cad-engineering": ">=1.0.0,<2.0.0",
        "gmsh-meshing": ">=1.0.0,<2.0.0"
    },
    "mooring-design": {
        "orcaflex-modeling": ">=2.0.0,<3.0.0",
        "hydrodynamics": ">=1.0.0,<2.0.0"
    },
    "orcaflex-model-generator": {
        "orcaflex-file-conversion": ">=1.0.0,<2.0.0"
    },
    "orcaflex-modeling": {
        "orcaflex-file-conversion": ">=1.0.0,<2.0.0"
    },
    "orcaflex-post-processing": {
        "orcaflex-modeling": ">=2.0.0,<3.0.0",
        "signal-analysis": ">=1.0.0,<2.0.0"
    },
    "orcawave-analysis": {
        "hydrodynamics": ">=1.0.0,<2.0.0"
    },
    "signal-analysis": {},
    "structural-analysis": {},
    "viv-analysis": {
        "structural-analysis": ">=1.0.0,<2.0.0"
    },
    "cad-engineering": {},
    "cathodic-protection": {},
    "gmsh-meshing": {},
    "hydrodynamics": {},
    "orcaflex-file-conversion": {}
}

# OrcaFlex version requirements
ORCAFLEX_SKILLS = {
    "orcaflex-modeling": ">=11.0",
    "orcaflex-file-conversion": ">=11.0",
    "orcaflex-model-generator": ">=11.0",
    "orcaflex-post-processing": ">=11.0",
    "orcawave-analysis": ">=11.0",
    "catenary-riser": ">=11.0",
    "mooring-design": ">=11.0",
    "viv-analysis": ">=11.0"
}


def create_version_metadata_block(
    version: str,
    skill_name: str,
    python_min: str = "3.10"
) -> str:
    """Create version metadata YAML block."""
    metadata = {
        "version": version,
        "python_min_version": python_min,
    }

    # Add dependencies if any
    if skill_name in SKILL_DEPENDENCIES and SKILL_DEPENDENCIES[skill_name]:
        metadata["dependencies"] = SKILL_DEPENDENCIES[skill_name]

    # Add OrcaFlex requirement if applicable
    if skill_name in ORCAFLEX_SKILLS:
        metadata["orcaflex_version"] = ORCAFLEX_SKILLS[skill_name]

    metadata["compatibility"] = {
        "tested_python": ["3.10", "3.11", "3.12", "3.13"],
        "os": ["Windows", "Linux", "macOS"]
    }

    yaml_str = yaml.dump(metadata, default_flow_style=False, sort_keys=False)
    return f"## Version Metadata\n\n```yaml\n{yaml_str}```\n"


def create_initial_changelog(version: str) -> str:
    """Create initial changelog section."""
    date = datetime.now().strftime("%Y-%m-%d")
    return f"""## Changelog

### [{version}] - {date}

**Added:**
- Initial version metadata and dependency management
- Semantic versioning support
- Compatibility information for Python 3.10-3.13

**Changed:**
- Enhanced skill documentation structure
"""


def initialize_skill_metadata(skill_path: Path) -> None:
    """Add version metadata and changelog to a skill file."""
    skill_name = skill_path.parent.name

    with open(skill_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Check if metadata already exists
    if "## Version Metadata" in content:
        print(f"[SKIP] {skill_name}: Already has version metadata, skipping")
        return

    # Extract frontmatter
    frontmatter_match = re.match(r'^---\s*\n(.*?)\n---\s*\n(.*)', content, re.DOTALL)
    if not frontmatter_match:
        print(f"[ERROR] {skill_name}: No YAML frontmatter found")
        return

    frontmatter_yaml = frontmatter_match.group(1)
    body = frontmatter_match.group(2)

    frontmatter = yaml.safe_load(frontmatter_yaml)
    current_version = frontmatter.get('version', '1.0.0')

    # Update updated date if missing
    if 'updated' not in frontmatter:
        frontmatter['updated'] = datetime.now().strftime("%Y-%m-%d")

    # Create new sections
    metadata_block = create_version_metadata_block(current_version, skill_name)
    changelog_block = create_initial_changelog(current_version)

    # Find insertion point (after first heading)
    first_heading_match = re.search(r'\n## ', body)
    if first_heading_match:
        insert_pos = first_heading_match.start()
        body = body[:insert_pos] + f"\n{metadata_block}\n{changelog_block}\n" + body[insert_pos:]
    else:
        # Insert at beginning of body
        body = f"\n{metadata_block}\n{changelog_block}\n{body}"

    # Reconstruct file
    new_frontmatter = yaml.dump(frontmatter, default_flow_style=False, sort_keys=False)
    new_content = f"---\n{new_frontmatter}---\n{body}"

    # Write back
    with open(skill_path, 'w', encoding='utf-8') as f:
        f.write(new_content)

    # Report dependencies
    deps_info = ""
    if skill_name in SKILL_DEPENDENCIES and SKILL_DEPENDENCIES[skill_name]:
        deps = list(SKILL_DEPENDENCIES[skill_name].keys())
        deps_info = f" (deps: {', '.join(deps)})"

    orcaflex_info = ""
    if skill_name in ORCAFLEX_SKILLS:
        orcaflex_info = f" [OrcaFlex {ORCAFLEX_SKILLS[skill_name]}]"

    print(f"[OK] {skill_name}: Initialized v{current_version}{deps_info}{orcaflex_info}")


def main():
    # Find skills directory
    script_dir = Path(__file__).parent
    project_root = script_dir.parent
    skills_dir = project_root / ".claude" / "skills"

    if not skills_dir.exists():
        print(f"Error: Skills directory not found: {skills_dir}")
        return

    print("Initializing skill metadata...\n")

    for skill_dir in sorted(skills_dir.iterdir()):
        if skill_dir.is_dir():
            skill_md = skill_dir / "SKILL.md"
            if skill_md.exists():
                try:
                    initialize_skill_metadata(skill_md)
                except Exception as e:
                    print(f"[ERROR] {skill_dir.name}: Error - {e}")

    print("\n[OK] Initialization complete!")
    print("\nNext steps:")
    print("  1. Review changes: git diff .claude/skills")
    print("  2. Update versions: python scripts/update_skill_versions.py")
    print("  3. Generate catalog: python -c 'from digitalmodel.modules.skills.skill_resolver import SkillResolver; SkillResolver().export_catalog()'")


if __name__ == "__main__":
    main()
