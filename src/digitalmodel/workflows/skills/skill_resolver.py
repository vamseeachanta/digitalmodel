# ABOUTME: Skill resolver for version management, dependency checking, and NLP-based recommendations
# Loads skill metadata, resolves dependencies recursively, checks version compatibility

import json
import re
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple
from dataclasses import dataclass, field
import yaml


@dataclass
class SkillMetadata:
    """Metadata for a skill including version and dependencies."""

    name: str
    version: str
    description: str
    category: str
    python_min_version: Optional[str] = None
    orcaflex_version: Optional[str] = None
    dependencies: Dict[str, str] = field(default_factory=dict)
    keywords: List[str] = field(default_factory=list)
    file_path: Optional[Path] = None

    @property
    def major_version(self) -> int:
        """Extract major version number."""
        return int(self.version.split('.')[0])

    @property
    def minor_version(self) -> int:
        """Extract minor version number."""
        return int(self.version.split('.')[1])

    @property
    def patch_version(self) -> int:
        """Extract patch version number."""
        return int(self.version.split('.')[2])


@dataclass
class SkillRecommendation:
    """Skill recommendation with relevance score."""

    skill: SkillMetadata
    relevance_score: float
    matched_keywords: List[str]
    dependencies_included: List[str] = field(default_factory=list)


class SkillResolver:
    """
    Resolve skill dependencies and recommend skills based on task descriptions.

    Features:
    - Load skill metadata from SKILL.md files
    - Resolve dependencies recursively
    - Check version compatibility
    - NLP-based skill recommendation using keyword matching
    - Filter by Python version compatibility
    """

    def __init__(self, skills_directory: Optional[Path] = None):
        """
        Initialize the skill resolver.

        Args:
            skills_directory: Path to .claude/skills directory. If None, auto-detect.
        """
        if skills_directory is None:
            # Auto-detect from project root
            current_file = Path(__file__)
            project_root = current_file.parents[4]  # Go up to project root
            skills_directory = project_root / ".claude" / "skills"

        self.skills_directory = Path(skills_directory)
        self.skills: Dict[str, SkillMetadata] = {}
        self.catalog_path = self.skills_directory / "skills-catalog.json"

        self._load_all_skills()

    def _load_all_skills(self) -> None:
        """Load all skills from the skills directory."""
        if not self.skills_directory.exists():
            raise FileNotFoundError(f"Skills directory not found: {self.skills_directory}")

        for skill_dir in self.skills_directory.iterdir():
            if skill_dir.is_dir():
                skill_md = skill_dir / "SKILL.md"
                if skill_md.exists():
                    try:
                        metadata = self._parse_skill_file(skill_md)
                        self.skills[metadata.name] = metadata
                    except Exception as e:
                        print(f"Warning: Failed to load skill {skill_dir.name}: {e}")

    def _parse_skill_file(self, skill_path: Path) -> SkillMetadata:
        """
        Parse a SKILL.md file and extract metadata.

        Args:
            skill_path: Path to SKILL.md file

        Returns:
            SkillMetadata object
        """
        with open(skill_path, 'r', encoding='utf-8') as f:
            content = f.read()

        # Extract YAML frontmatter
        frontmatter_match = re.match(r'^---\s*\n(.*?)\n---\s*\n', content, re.DOTALL)
        if not frontmatter_match:
            raise ValueError(f"No YAML frontmatter found in {skill_path}")

        frontmatter = yaml.safe_load(frontmatter_match.group(1))

        # Extract version metadata section
        version_section = self._extract_version_metadata(content)

        # Build metadata object
        metadata = SkillMetadata(
            name=frontmatter.get('name', skill_path.parent.name),
            version=version_section.get('version', frontmatter.get('version', '1.0.0')),
            description=frontmatter.get('description', ''),
            category=frontmatter.get('category', 'general'),
            python_min_version=version_section.get('python_min_version'),
            orcaflex_version=version_section.get('orcaflex_version'),
            dependencies=version_section.get('dependencies', {}),
            keywords=frontmatter.get('triggers', []),
            file_path=skill_path
        )

        return metadata

    def _extract_version_metadata(self, content: str) -> Dict:
        """
        Extract version metadata section from skill file.

        Args:
            content: Full content of SKILL.md file

        Returns:
            Dictionary with version metadata
        """
        # Look for ## Version Metadata section
        metadata_match = re.search(
            r'## Version Metadata\s*\n\s*```yaml\s*\n(.*?)\n\s*```',
            content,
            re.DOTALL
        )

        if metadata_match:
            try:
                return yaml.safe_load(metadata_match.group(1))
            except yaml.YAMLError:
                pass

        return {}

    def resolve_dependencies(
        self,
        skill_name: str,
        visited: Optional[Set[str]] = None
    ) -> List[Tuple[str, str]]:
        """
        Recursively resolve all dependencies for a skill.

        Args:
            skill_name: Name of the skill
            visited: Set of already visited skills (for cycle detection)

        Returns:
            List of (skill_name, version_constraint) tuples
        """
        if visited is None:
            visited = set()

        if skill_name in visited:
            # Circular dependency detected
            return []

        if skill_name not in self.skills:
            raise ValueError(f"Skill not found: {skill_name}")

        visited.add(skill_name)
        skill = self.skills[skill_name]

        dependencies = []

        for dep_name, version_constraint in skill.dependencies.items():
            dependencies.append((dep_name, version_constraint))

            # Recursively resolve dependencies
            if dep_name in self.skills:
                sub_deps = self.resolve_dependencies(dep_name, visited.copy())
                dependencies.extend(sub_deps)

        return dependencies

    def check_version_compatibility(
        self,
        skill_name: str,
        version_constraint: str
    ) -> bool:
        """
        Check if a skill's version satisfies a constraint.

        Args:
            skill_name: Name of the skill
            version_constraint: Version constraint (e.g., ">=1.0.0,<2.0.0")

        Returns:
            True if compatible, False otherwise
        """
        if skill_name not in self.skills:
            return False

        skill = self.skills[skill_name]
        return self._version_satisfies_constraint(skill.version, version_constraint)

    def _version_satisfies_constraint(self, version: str, constraint: str) -> bool:
        """
        Check if a version satisfies a constraint.

        Args:
            version: Version string (e.g., "1.2.3")
            constraint: Constraint string (e.g., ">=1.0.0,<2.0.0")

        Returns:
            True if version satisfies constraint
        """
        # Parse version
        v_parts = [int(x) for x in version.split('.')]

        # Handle multiple constraints separated by comma
        constraints = [c.strip() for c in constraint.split(',')]

        for c in constraints:
            if not self._check_single_constraint(v_parts, c):
                return False

        return True

    def _check_single_constraint(self, version_parts: List[int], constraint: str) -> bool:
        """Check a single version constraint."""
        # Parse constraint operator and version
        match = re.match(r'([><=]+)([\d.]+)', constraint)
        if not match:
            return True  # No constraint

        operator, const_version = match.groups()
        const_parts = [int(x) for x in const_version.split('.')]

        # Pad to same length
        max_len = max(len(version_parts), len(const_parts))
        v = version_parts + [0] * (max_len - len(version_parts))
        c = const_parts + [0] * (max_len - len(const_parts))

        # Compare
        if operator == '>=':
            return v >= c
        elif operator == '>':
            return v > c
        elif operator == '<=':
            return v <= c
        elif operator == '<':
            return v < c
        elif operator == '==':
            return v == c
        else:
            return True

    def recommend_skills(
        self,
        task_description: str,
        python_version: Optional[str] = None,
        max_results: int = 5
    ) -> List[SkillRecommendation]:
        """
        Recommend skills based on task description using keyword matching.

        Args:
            task_description: Description of the task
            python_version: Python version to filter by (e.g., "3.13.5")
            max_results: Maximum number of recommendations

        Returns:
            List of SkillRecommendation objects sorted by relevance
        """
        # Normalize task description
        task_lower = task_description.lower()
        task_words = set(re.findall(r'\b\w+\b', task_lower))

        recommendations = []

        for skill in self.skills.values():
            # Check Python version compatibility
            if python_version and skill.python_min_version:
                if not self._version_satisfies_constraint(
                    python_version,
                    f">={skill.python_min_version}"
                ):
                    continue

            # Calculate relevance score
            matched_keywords = []
            for keyword in skill.keywords:
                keyword_lower = keyword.lower()
                if keyword_lower in task_lower:
                    matched_keywords.append(keyword)

            # Also check description words
            description_words = set(re.findall(r'\b\w+\b', skill.description.lower()))
            common_words = task_words & description_words

            # Calculate score
            score = len(matched_keywords) * 2.0 + len(common_words) * 0.5

            if score > 0:
                # Include dependencies
                try:
                    deps = self.resolve_dependencies(skill.name)
                    dep_names = [d[0] for d in deps]
                except ValueError:
                    dep_names = []

                recommendations.append(SkillRecommendation(
                    skill=skill,
                    relevance_score=score,
                    matched_keywords=matched_keywords,
                    dependencies_included=dep_names
                ))

        # Sort by relevance score
        recommendations.sort(key=lambda x: x.relevance_score, reverse=True)

        return recommendations[:max_results]

    def check_breaking_changes(self, skill_name: str, from_version: str) -> bool:
        """
        Check if there are breaking changes between versions.

        Args:
            skill_name: Name of the skill
            from_version: Previous version

        Returns:
            True if there are breaking changes (major version change)
        """
        if skill_name not in self.skills:
            return False

        skill = self.skills[skill_name]
        from_major = int(from_version.split('.')[0])

        return skill.major_version > from_major

    def export_catalog(self) -> None:
        """Export skills catalog to JSON file."""
        catalog = {
            "version": "1.0.0",
            "generated": str(Path.cwd()),
            "skills": {}
        }

        for name, skill in self.skills.items():
            catalog["skills"][name] = {
                "version": skill.version,
                "description": skill.description,
                "category": skill.category,
                "python_min_version": skill.python_min_version,
                "orcaflex_version": skill.orcaflex_version,
                "dependencies": skill.dependencies,
                "keywords": skill.keywords
            }

        with open(self.catalog_path, 'w', encoding='utf-8') as f:
            json.dump(catalog, f, indent=2)

    def get_skill(self, skill_name: str) -> Optional[SkillMetadata]:
        """Get metadata for a specific skill."""
        return self.skills.get(skill_name)

    def list_all_skills(self) -> List[str]:
        """Get list of all available skill names."""
        return sorted(self.skills.keys())

    def validate_dependencies(self, skill_name: str) -> Dict[str, bool]:
        """
        Validate all dependencies for a skill.

        Args:
            skill_name: Name of the skill

        Returns:
            Dictionary mapping dependency names to validation status
        """
        if skill_name not in self.skills:
            raise ValueError(f"Skill not found: {skill_name}")

        skill = self.skills[skill_name]
        validation = {}

        for dep_name, version_constraint in skill.dependencies.items():
            if dep_name not in self.skills:
                validation[dep_name] = False
            else:
                validation[dep_name] = self.check_version_compatibility(
                    dep_name,
                    version_constraint
                )

        return validation
