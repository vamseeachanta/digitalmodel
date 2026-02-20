# Module-Based Refactor Skill

> **Full documentation**: [meta/module-based-refactor/SKILL.md](meta/module-based-refactor/SKILL.md)

## Quick Reference

Reorganize a repository from flat structure to module-based 5-layer architecture.

### Target Structure

```
src/<package>/modules/<module_name>/
tests/modules/<module_name>/
specs/modules/<module_name>/
docs/domains/<module_name>/
examples/domains/<module_name>/
```

### Quick Commands

```bash
# Move source module (preserves git history)
git mv src/<package>/<module> src/<package>/modules/<module>

# Move tests
git mv tests/<module> tests/modules/<module>

# Search for old imports
grep -r "from <module>" --include="*.py" .

# Update imports
find . -name "*.py" -exec sed -i 's/from package.module/from package.modules.module/g' {} \;
```

### Infrastructure Folders (Keep at root)

- `tests/test_data/`, `tests/fixtures/`, `tests/conftest.py`
- `specs/templates/`, `specs/features/`
- `config/`, `scripts/`

### Checklist

- [ ] All modules moved to modules/ subdirectory
- [ ] `__init__.py` files created with exports
- [ ] Import references updated
- [ ] pyproject.toml updated if needed
- [ ] Tests pass after refactor

See [full skill documentation](meta/module-based-refactor/SKILL.md) for detailed process and examples.
