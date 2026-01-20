# Development Process

## Specification-Driven Development

### 1. Start with Specification
- All features begin with a specification in `specs/` directory
- Use specification templates from `specs/templates/`
- Include acceptance criteria and technical requirements
- Review and approve specifications before implementation

### 2. Design Phase
- Review existing similar modules for patterns
- Identify required configuration parameters
- Plan YAML configuration structure
- Design module interface and API

### 3. Implementation Phase
- Create/update YAML configuration templates
- Implement core analysis logic
- Add comprehensive error handling
- Follow architecture patterns in `.ai/code-guidance/`

### 4. Testing Phase
- Write unit tests with mocks for external dependencies
- Create integration tests with sample configurations
- Test error conditions and edge cases
- Validate against specification acceptance criteria

### 5. Documentation Phase
- Update module documentation
- Add usage examples
- Update CLAUDE.md if needed
- Document any new configuration options

## Code Review Process

### Before Creating PR
```bash
# Format and lint
black src/ tests/
isort src/ tests/
ruff check src/ tests/

# Type check
mypy src/

# Run tests
pytest tests/

# Check for security issues
# Review changes against specification
```

### PR Requirements
- Link to relevant specification
- Include test coverage
- Pass all automated checks
- Update documentation as needed
- Follow commit message conventions

## Release Process

### Version Management
- Use `bumpver` for version updates
- Follow semantic versioning
- Update changelog for significant changes

### Deployment
```bash
# Build package
python -m build

# Test in staging environment
# Deploy to PyPI (when ready)
twine upload dist/*
```

## Quality Gates

### Automated Checks
- Code formatting (black, isort)
- Linting (ruff)
- Type checking (mypy)
- Test coverage (pytest)
- Security scanning

### Manual Review
- Specification compliance
- Code review by domain expert
- Integration testing
- Documentation review

## GitHub Issue Implementation Workflow

### Issue-Driven Development
1. **Issue Analysis**: Use `.ai/workflows/implementation/issue-planning.md`
2. **Implementation Tracking**: Follow `.ai/workflows/implementation/implementation-tracking.md`
3. **Change Documentation**: Apply `.ai/workflows/implementation/change-documentation.md`
4. **Implementation Summary**: Create using `.ai/workflows/implementation/implementation-summary.md`

### Integration with Specifications
- Link GitHub issues to relevant specifications
- Create new specifications for significant features
- Update existing specifications based on implementation learnings
- Maintain traceability from issue → spec → implementation

## Continuous Integration

### Pre-commit Hooks
- Format code automatically
- Run basic linting
- Check for secrets/sensitive data

### CI Pipeline
- Run full test suite
- Check code quality metrics
- Build and test package
- Generate documentation