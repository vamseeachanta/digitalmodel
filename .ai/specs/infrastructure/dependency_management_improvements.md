# User Story: Dependency Management Improvements

## Story Overview
**As a** developer working on the digitalmodel project  
**I want** to implement modern dependency management best practices  
**So that** the project has reproducible builds, better security, and easier maintenance

## Background
The current dependency management setup has several issues that need to be addressed:
- Missing version constraints in requirements.txt
- Duplicate dependency entries
- Mixed dependency managers (poetry.lock and uv.lock)
- No separation between production and development dependencies
- Self-reference in requirements.txt

## Acceptance Criteria
- [ ] All dependencies have appropriate version constraints
- [ ] Development and production dependencies are separated
- [ ] A single dependency management tool is chosen and consistently used
- [ ] Security scanning is automated in CI/CD pipeline
- [ ] Dependencies are well-documented
- [ ] Duplicate entries are removed
- [ ] Build reproducibility is ensured with lock files

## Implementation Tasks

### 1. Update pyproject.toml
Replace the current pyproject.toml with the improved version that includes:
- Proper version constraints for all dependencies
- Organized optional dependencies (dev, test, docs)
- Enhanced tool configurations (black, ruff, mypy, pytest)
- Better project metadata

**File to implement:** `pyproject_improved.toml`

### 2. Clean Up Requirements Files
Create clean, organized requirements files:
- `requirements_improved.txt` - Production dependencies only with version constraints
- `requirements-dev.txt` - Development dependencies including testing and linting tools

**Files to implement:**
- `requirements_improved.txt`
- `requirements-dev.txt`

### 3. Document Dependencies
Create comprehensive documentation explaining each dependency and its purpose.

**File to implement:** `DEPENDENCIES.md`

### 4. Set Up Automated Security Scanning
Implement GitHub Actions workflow for:
- Security vulnerability scanning
- Outdated dependency detection
- Dependency review on pull requests

**File to implement:** `.github/workflows/dependency-check.yml`

### 5. Choose Single Dependency Manager
**Decision needed:** Choose between:
- **Option A: Poetry** (Recommended)
  - Modern dependency resolver
  - Built-in virtual environment management
  - Lock file for reproducibility
  - Already have poetry.lock in project
  
- **Option B: pip-tools**
  - Use requirements.in for abstract dependencies
  - Generate requirements.txt with pip-compile
  - Simpler than Poetry but less features

- **Option C: uv**
  - Fast, Rust-based package installer
  - Drop-in replacement for pip
  - Already have uv.lock in project

### 6. Migration Steps
1. **Backup current setup**
   ```bash
   cp requirements.txt requirements.txt.backup
   cp pyproject.toml pyproject.toml.backup
   ```

2. **Remove duplicate entries**
   - Remove duplicate `pytest` and `deepdiff` entries
   - Remove `digitalmodel` self-reference

3. **Fix package names**
   - Change `docx` to `python-docx`
   - Change `tabula` to `tabula-py`
   - Verify `OrcFxAPI` package name and availability

4. **Implement new configuration**
   ```bash
   # If using Poetry
   poetry install
   poetry lock
   
   # If using pip-tools
   pip-compile requirements.in
   pip-sync requirements.txt
   ```

5. **Update CI/CD pipelines**
   - Add dependency security scanning
   - Update installation commands
   - Add cache for faster builds

## Technical Specifications

### Version Constraint Guidelines
```txt
# For stable packages (1.0+)
package>=1.2.0,<2.0.0  # Allow minor updates

# For pre-1.0 packages
package>=0.3.0,<0.4.0  # Only patch updates

# For packages with good backwards compatibility
package>=2.0.0,<3.0.0  # Allow minor updates

# For critical security packages
package>=2.31.0  # Minimum version only
```

### Dependency Categories
1. **Core Dependencies**
   - Data processing (pandas, numpy, scipy)
   - Visualization (plotly, dash, matplotlib)
   - Engineering specific (OrcFxAPI, rainflow)

2. **I/O Dependencies**
   - File formats (openpyxl, xlrd, python-docx, pypdf2)
   - Data formats (pyyaml, xmltodict, ruamel.yaml)

3. **Development Dependencies**
   - Testing (pytest, pytest-cov, pytest-mock)
   - Code quality (black, isort, ruff, mypy)
   - Documentation (sphinx)

## Definition of Done
- [ ] All production dependencies have version constraints
- [ ] Development dependencies are in separate file/section
- [ ] No duplicate dependencies
- [ ] Documentation is complete and accurate
- [ ] Security scanning is automated
- [ ] All tests pass with new dependency setup
- [ ] CI/CD pipeline is updated and working
- [ ] Team is trained on new dependency management approach

## Notes
- Consider using `pip-audit` or `safety` for security scanning
- Set up Dependabot for automated dependency updates
- Review and update dependencies quarterly
- Always test dependency updates in a separate branch
- Keep Python version requirement (>=3.8) aligned with project needs

## Related Issues
- Clean up duplicate lock files (poetry.lock vs uv.lock)
- Standardize development environment setup
- Improve build reproducibility
- Enhance security posture

## Estimated Effort
- Initial implementation: 4-6 hours
- Testing and validation: 2-3 hours
- Documentation and training: 1-2 hours
- **Total: 7-11 hours**

## Priority
**High** - This impacts development efficiency, security, and build reproducibility

## References
- [Python Packaging User Guide](https://packaging.python.org/)
- [Poetry Documentation](https://python-poetry.org/docs/)
- [pip-tools Documentation](https://pip-tools.readthedocs.io/)
- [Dependency Management Best Practices](https://pythonspeed.com/articles/pipenv-docker/)