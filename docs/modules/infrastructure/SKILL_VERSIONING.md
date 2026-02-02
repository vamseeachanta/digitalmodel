# Skill Versioning and Dependency Management

Complete system for managing skill versions, dependencies, and recommendations.

## Overview

This system provides:
- **Semantic versioning** (MAJOR.MINOR.PATCH) for all skills
- **Dependency resolution** with version constraints
- **NLP-based skill recommendation** using keyword matching
- **Breaking change detection** for major version updates
- **Python version compatibility** filtering
- **Automated catalog generation**

## Components

### 1. Skill Resolver (`src/digitalmodel/modules/skills/skill_resolver.py`)

Core module for skill management:

```python
from digitalmodel.skills.skill_resolver import SkillResolver

# Initialize resolver
resolver = SkillResolver()

# Get skill metadata
skill = resolver.get_skill("aqwa-analysis")
print(f"{skill.name} v{skill.version}")

# Resolve dependencies
deps = resolver.resolve_dependencies("fatigue-analysis")
for dep_name, constraint in deps:
    print(f"Requires: {dep_name} {constraint}")

# Recommend skills
recommendations = resolver.recommend_skills(
    "I need to analyze mooring systems",
    max_results=5
)
for rec in recommendations:
    print(f"{rec.skill.name}: {rec.relevance_score}")
```

### 2. Update Script (`scripts/update_skill_versions.py`)

Interactive tool for version updates:

```bash
# Update specific skill
python scripts/update_skill_versions.py --skill aqwa-analysis

# Batch update all skills (patch version)
python scripts/update_skill_versions.py --bump patch --all

# Interactive mode (all skills)
python scripts/update_skill_versions.py
```

### 3. Skills Catalog (`.claude/skills/skills-catalog.json`)

Consolidated registry of all skills with metadata:

```json
{
  "version": "1.0.0",
  "skills": {
    "aqwa-analysis": {
      "version": "3.0.0",
      "dependencies": {
        "hydrodynamics": ">=1.0.0,<2.0.0"
      },
      "python_min_version": "3.10",
      "orcaflex_version": null
    }
  }
}
```

## Skill Metadata Format

Each skill's `SKILL.md` file includes:

```markdown
---
name: skill-name
description: Brief description
version: 1.0.0
updated: 2026-01-07
category: offshore-engineering
triggers:
  - keyword 1
  - keyword 2
---

## Version Metadata

```yaml
version: 1.0.0
python_min_version: "3.10"
dependencies:
  other-skill: ">=1.0.0,<2.0.0"
orcaflex_version: ">=11.0"  # Optional
compatibility:
  tested_python: ["3.10", "3.11", "3.12", "3.13"]
  os: ["Windows", "Linux", "macOS"]
```

## Changelog

### [1.0.0] - 2026-01-07

**Added:**
- New feature

**Changed:**
- Updated feature

**Fixed:**
- Bug fix

**Removed:**
- Deprecated feature
```

## Semantic Versioning

We follow [SemVer 2.0.0](https://semver.org/):

- **MAJOR** (X.0.0): Breaking changes - incompatible API changes
- **MINOR** (0.X.0): New features - backwards compatible
- **PATCH** (0.0.X): Bug fixes - backwards compatible

### Version Constraints

Dependency version constraints use standard syntax:

```yaml
dependencies:
  skill-name: ">=1.0.0,<2.0.0"  # 1.x.x only
  other-skill: ">=2.5.0"        # 2.5.0 or higher
  another: "==1.2.3"            # Exact version
```

## Dependency Resolution

The resolver recursively resolves dependencies:

```python
# Example: fatigue-analysis depends on:
#   - signal-analysis >=1.0.0,<2.0.0
#   - structural-analysis >=1.0.0,<2.0.0

deps = resolver.resolve_dependencies("fatigue-analysis")
# Returns: [
#   ("signal-analysis", ">=1.0.0,<2.0.0"),
#   ("structural-analysis", ">=1.0.0,<2.0.0")
# ]

# Validate all dependencies
validation = resolver.validate_dependencies("fatigue-analysis")
# Returns: {
#   "signal-analysis": True,
#   "structural-analysis": True
# }
```

## Skill Recommendation Engine

NLP-based recommendation using keyword matching:

```python
# Recommend by task description
recommendations = resolver.recommend_skills(
    "Extract RAOs from AQWA and convert to OrcaFlex format",
    python_version="3.13.5",  # Filter by Python version
    max_results=5
)

for rec in recommendations:
    print(f"{rec.skill.name}: score={rec.relevance_score}")
    print(f"  Matched keywords: {rec.matched_keywords}")
    print(f"  Dependencies: {rec.dependencies_included}")
```

### Scoring Algorithm

Relevance score calculation:
- Keyword match in triggers: **+2.0 points** per match
- Word match in description: **+0.5 points** per word
- Scores sorted descending

## Breaking Change Detection

Major version changes indicate breaking changes:

```python
# Check if upgrade has breaking changes
is_breaking = resolver.check_breaking_changes(
    "aqwa-analysis",
    from_version="2.5.0"  # Upgrading from 2.5.0
)

if is_breaking:
    print("WARNING: Major version change detected!")
    print("Review migration guide before upgrading.")
```

## Python Version Compatibility

All skills specify minimum Python version:

```python
# Filter recommendations by Python version
recommendations = resolver.recommend_skills(
    "fatigue analysis",
    python_version="3.13.5"
)

# Only returns skills compatible with Python 3.13.5
```

## Current Skill Dependencies

| Skill | Dependencies |
|-------|-------------|
| aqwa-analysis | hydrodynamics >=1.0.0,<2.0.0 |
| catenary-riser | orcaflex-modeling >=2.0.0,<3.0.0 |
| fatigue-analysis | signal-analysis >=1.0.0,<2.0.0<br>structural-analysis >=1.0.0,<2.0.0 |
| freecad-automation | cad-engineering >=1.0.0,<2.0.0<br>gmsh-meshing >=1.0.0,<2.0.0 |
| mooring-design | orcaflex-modeling >=2.0.0,<3.0.0<br>hydrodynamics >=1.0.0,<2.0.0 |
| orcaflex-model-generator | orcaflex-file-conversion >=1.0.0,<2.0.0 |
| orcaflex-modeling | orcaflex-file-conversion >=1.0.0,<2.0.0 |
| orcaflex-post-processing | orcaflex-modeling >=2.0.0,<3.0.0<br>signal-analysis >=1.0.0,<2.0.0 |
| orcawave-analysis | hydrodynamics >=1.0.0,<2.0.0 |
| viv-analysis | structural-analysis >=1.0.0,<2.0.0 |

## Workflow Examples

### Adding a New Skill

1. Create skill directory: `.claude/skills/new-skill/`
2. Create `SKILL.md` with frontmatter and metadata
3. Run initialization (if needed):
   ```bash
   python scripts/initialize_skill_metadata.py
   ```
4. Regenerate catalog:
   ```bash
   python -c "from digitalmodel.skills.skill_resolver import SkillResolver; SkillResolver().export_catalog()"
   ```

### Updating Skill Version

Interactive mode:
```bash
python scripts/update_skill_versions.py --skill aqwa-analysis
```

Follow prompts:
1. Select version bump type (major/minor/patch)
2. Enter changelog entries
3. Specify dependencies (if changed)
4. Confirm update

### Batch Version Update

Bump patch version for all skills:
```bash
python scripts/update_skill_versions.py --bump patch --all
```

## Testing

Comprehensive test suite in `tests/test_skill_resolver.py`:

```bash
# Run all tests
pytest tests/test_skill_resolver.py -v

# Run specific test class
pytest tests/test_skill_resolver.py::TestSkillRecommendation -v

# Run with coverage
pytest tests/test_skill_resolver.py --cov=digitalmodel.skills
```

## Demo Script

See complete functionality demo:

```bash
python scripts/demo_skill_resolver.py
```

Demonstrates:
- Skill loading and listing
- Dependency resolution
- Version compatibility
- Skill recommendation
- Breaking change detection
- Python version filtering

## API Reference

### SkillResolver

Main class for skill management.

#### Methods

```python
__init__(skills_directory: Optional[Path] = None)
    Initialize resolver with skills directory.

get_skill(skill_name: str) -> Optional[SkillMetadata]
    Get metadata for a specific skill.

list_all_skills() -> List[str]
    Get list of all skill names.

resolve_dependencies(skill_name: str) -> List[Tuple[str, str]]
    Recursively resolve all dependencies.

check_version_compatibility(skill_name: str, version_constraint: str) -> bool
    Check if skill version satisfies constraint.

recommend_skills(task_description: str, python_version: Optional[str] = None, max_results: int = 5) -> List[SkillRecommendation]
    Recommend skills based on task description.

check_breaking_changes(skill_name: str, from_version: str) -> bool
    Check if upgrade has breaking changes.

validate_dependencies(skill_name: str) -> Dict[str, bool]
    Validate all dependencies.

export_catalog() -> None
    Export skills catalog to JSON.
```

### SkillMetadata

Dataclass for skill metadata.

#### Properties

```python
name: str
version: str
description: str
category: str
python_min_version: Optional[str]
orcaflex_version: Optional[str]
dependencies: Dict[str, str]
keywords: List[str]
file_path: Optional[Path]

# Computed properties
major_version: int
minor_version: int
patch_version: int
```

### SkillRecommendation

Dataclass for skill recommendations.

#### Properties

```python
skill: SkillMetadata
relevance_score: float
matched_keywords: List[str]
dependencies_included: List[str]
```

## Files Created

```
.claude/skills/
├── skills-catalog.json          # Consolidated skill registry
├── aqwa-analysis/SKILL.md       # Updated with version metadata
├── cad-engineering/SKILL.md     # Updated with version metadata
├── catenary-riser/SKILL.md      # Updated with version metadata
├── [... 14 more skills ...]     # All updated with metadata

src/digitalmodel/modules/skills/
├── __init__.py                  # Module initialization
└── skill_resolver.py            # Core resolver implementation

scripts/
├── initialize_skill_metadata.py # One-time initialization script
├── update_skill_versions.py     # Interactive version updater
└── demo_skill_resolver.py       # Feature demonstration

tests/
└── test_skill_resolver.py       # Comprehensive test suite (32 tests)

docs/
└── SKILL_VERSIONING.md          # This documentation
```

## Best Practices

1. **Semantic Versioning**: Always follow SemVer strictly
2. **Dependency Constraints**: Use ranges (>=1.0.0,<2.0.0) not exact versions
3. **Changelog**: Document ALL changes in categorized format
4. **Testing**: Update tests when adding dependencies
5. **Catalog**: Regenerate after any metadata changes
6. **Breaking Changes**: Warn users and provide migration guides

## Future Enhancements

Potential improvements:
- [ ] Skill compatibility matrix visualization
- [ ] Automatic dependency graph generation
- [ ] Migration path suggestions for breaking changes
- [ ] Skill usage analytics
- [ ] Version lock files for reproducibility
- [ ] CLI integration for skill selection

## Related Documentation

- [Skill Guidelines](../.claude/skills/README.md)
- [Project CLAUDE.md](../CLAUDE.md)
- [Testing Framework](./CLI_TESTING_FRAMEWORK_COMPLETE.md)

---

**Last Updated**: 2026-01-07
**Version**: 1.0.0
