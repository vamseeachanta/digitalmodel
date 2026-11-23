# Skill Versioning Quick Reference

## Quick Start

```python
# Import and initialize
from digitalmodel.workflows.skills.skill_resolver import SkillResolver
resolver = SkillResolver()

# Get skill info
skill = resolver.get_skill("aqwa-analysis")
print(f"{skill.name} v{skill.version}")

# Recommend skills
recs = resolver.recommend_skills("Extract RAOs from AQWA")
print(f"Top: {recs[0].skill.name}")

# Check dependencies
deps = resolver.resolve_dependencies("fatigue-analysis")
for name, constraint in deps:
    print(f"Needs: {name} {constraint}")
```

## Command Line

```bash
# Update specific skill
python scripts/update_skill_versions.py --skill aqwa-analysis

# Batch update (patch version)
python scripts/update_skill_versions.py --bump patch --all

# Run demo
python scripts/demo_skill_resolver.py

# Run tests
pytest tests/test_skill_resolver.py -v

# Regenerate catalog
python -c "from digitalmodel.workflows.skills.skill_resolver import SkillResolver; SkillResolver().export_catalog()"
```

## Version Format

```
MAJOR.MINOR.PATCH

1.2.3
│ │ └─ Bug fixes (backwards compatible)
│ └─── New features (backwards compatible)
└───── Breaking changes (incompatible)
```

## Dependency Syntax

```yaml
dependencies:
  skill-name: ">=1.0.0,<2.0.0"  # 1.x only
  other-skill: ">=2.5.0"        # 2.5.0+
  another: "==1.2.3"            # Exact
```

## Skill Metadata Block

```yaml
## Version Metadata

version: 1.0.0
python_min_version: "3.10"
dependencies:
  other-skill: ">=1.0.0,<2.0.0"
orcaflex_version: ">=11.0"  # Optional
compatibility:
  tested_python: ["3.10", "3.11", "3.12", "3.13"]
  os: ["Windows", "Linux", "macOS"]
```

## Changelog Format

```markdown
## Changelog

### [1.2.0] - 2026-01-07

**Added:**
- New feature description

**Changed:**
- Modified feature description

**Fixed:**
- Bug fix description

**Removed:**
- Deprecated feature
```

## API Cheat Sheet

```python
# List all skills
skills = resolver.list_all_skills()

# Get skill metadata
skill = resolver.get_skill("skill-name")

# Resolve dependencies (recursive)
deps = resolver.resolve_dependencies("skill-name")

# Check version compatibility
is_ok = resolver.check_version_compatibility("skill-name", ">=1.0.0,<2.0.0")

# Recommend skills
recs = resolver.recommend_skills(
    "task description",
    python_version="3.13.5",
    max_results=5
)

# Check breaking changes
breaking = resolver.check_breaking_changes("skill-name", "1.5.0")

# Validate dependencies
valid = resolver.validate_dependencies("skill-name")

# Export catalog
resolver.export_catalog()
```

## Common Tasks

### Add New Skill
1. Create `.claude/skills/new-skill/SKILL.md`
2. Add frontmatter with version
3. Run: `python scripts/initialize_skill_metadata.py`
4. Regenerate catalog

### Update Version
1. Run: `python scripts/update_skill_versions.py --skill NAME`
2. Select bump type (major/minor/patch)
3. Enter changelog items
4. Confirm update
5. Catalog auto-updated

### Find Compatible Skills
```python
recs = resolver.recommend_skills(
    "your task",
    python_version="3.13.5"
)
```

## Current Skills (17)

**Offshore Engineering** (5):
- aqwa-analysis v3.0.0
- cathodic-protection v1.0.0
- orcaflex-file-conversion v1.0.0
- orcaflex-modeling v2.0.0
- orcawave-analysis v1.0.0

**CAD Engineering** (3):
- cad-engineering v1.0.0
- freecad-automation v1.0.0
- gmsh-meshing v1.0.0

**General** (9):
- catenary-riser v1.0.0
- fatigue-analysis v1.0.0
- hydrodynamics v1.0.0
- mooring-design v1.0.0
- orcaflex-model-generator v1.0.0
- orcaflex-post-processing v1.0.0
- signal-analysis v1.0.0
- structural-analysis v1.0.0
- viv-analysis v1.0.0

## Files

```
src/digitalmodel/modules/skills/
└── skill_resolver.py           # Core module

scripts/
├── initialize_skill_metadata.py
├── update_skill_versions.py
└── demo_skill_resolver.py

tests/
└── test_skill_resolver.py      # 32 tests

.claude/skills/
└── skills-catalog.json         # Auto-generated

docs/
├── SKILL_VERSIONING.md         # Full guide
└── SKILL_VERSIONING_QUICK_REF.md # This file
```

## Help

```bash
# Script help
python scripts/update_skill_versions.py --help

# Module docs
python -c "from digitalmodel.workflows.skills.skill_resolver import SkillResolver; help(SkillResolver)"

# Run demo
python scripts/demo_skill_resolver.py
```

---
**Quick Ref Version**: 1.0.0 | **Last Updated**: 2026-01-07
