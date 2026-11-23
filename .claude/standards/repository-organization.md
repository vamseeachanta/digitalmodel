# Repository Organization Standard

## CRITICAL MANDATORY PATTERN

**THIS REPOSITORY USES MODULE-BASED ORGANIZATION**

### Universal Directory Pattern

ALL directory groups MUST follow this structure:
```
<group_name>/modules/<module_name>/
```

### Pattern Examples

**✅ CORRECT PATTERNS:**
```
specs/modules/agent-os/
specs/modules/marine-engineering/
specs/modules/test-suite-automation/
src/modules/hydrodynamics/
src/modules/orcaflex-integration/
docs/domains/installation/
docs/domains/user-guides/
tests/domains/unit-tests/
tests/domains/integration-tests/
configs/modules/development/
configs/modules/production/
```

**❌ INCORRECT PATTERNS:**
```
specs/agent-os/              # Missing modules/ level
marine-specs/                # Not following pattern
src/hydrodynamics/          # Missing modules/ level
agent-os-docs/              # Not following pattern
```

### Enforcement Rules

1. **NEVER create directories that violate this pattern**
2. **ALWAYS relocate non-compliant structures to proper pattern**
3. **ENFORCE across ALL sessions, users, and systems**
4. **NO EXCEPTIONS - this is a fundamental architectural decision**

### Directory Group Guidelines

| Group | Purpose | Pattern | Example |
|-------|---------|---------|---------|
| `specs/modules/<module>/` | All specifications | Module-specific specs | `specs/modules/agent-os/foundation/spec.md` |
| `src/modules/<module>/` | All source code | Module-specific implementation | `src/modules/marine-engineering/hydrodynamics.py` |
| `docs/domains/<module>/` | All documentation | Module-specific docs | `docs/domains/installation/setup-guide.md` |
| `tests/domains/<module>/` | All test files | Module-specific tests | `tests/domains/agent-os/test_framework.py` |
| `configs/modules/<module>/` | All configurations | Module-specific configs | `configs/modules/development/settings.yaml` |

### Module Naming Conventions

- Use lowercase with hyphens: `marine-engineering`, `test-suite-automation`
- Be descriptive and specific: `agent-os` not `aos`
- Avoid abbreviations unless industry standard: `api` is acceptable, `mng` is not
- Group related functionality logically

### Migration Guidelines

When encountering non-compliant structures:

1. **Identify** the proper module classification
2. **Create** the correct `<group>/modules/<module>/` structure
3. **Move** all content to new location
4. **Update** all references and links
5. **Remove** old non-compliant directories
6. **Document** the migration in commit message

### Cross-Module Dependencies

When modules need to reference each other:
```yaml
# In specs/modules/marine-engineering/spec.md
dependencies:
  - module: agent-os
    location: specs/modules/agent-os/
    reason: "Framework foundation"
  
  - module: test-suite-automation
    location: specs/modules/test-suite-automation/
    reason: "Validation framework"
```

### Exception Policy

**NO EXCEPTIONS** to this pattern are permitted. This is a foundational architectural decision that enables:

- Consistent navigation across all repository areas
- Automated tooling and discovery
- Clear module boundaries and dependencies
- Scalable organization as project grows
- AI-friendly structure recognition

### Validation Commands

```bash
# Check for pattern compliance
find . -type d -name "*" | grep -v "/modules/" | grep -E "(specs|src|docs|tests|configs)/"

# List properly organized modules
find . -path "*/modules/*" -type d -depth 2
```

---

**REMEMBER: This pattern is MANDATORY and must be enforced in every session, by every user, across all systems working with this repository.**