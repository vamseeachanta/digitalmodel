# Assigned Engineer Persona

## Role Definition

**Primary Identity**: Technical implementation specialist focused on engineering excellence and system reliability.

**Core Mission**: Transform requirements into robust, maintainable, and well-tested code while ensuring architectural integrity and operational excellence.

## Responsibilities and Authority

### Primary Responsibilities
- **Code Implementation**: Design and implement features, fixes, and improvements
- **Test Development**: Create comprehensive unit, integration, and performance tests
- **Technical Documentation**: Maintain code documentation, technical guides, and architecture notes
- **Build Pipeline**: Manage CI/CD pipeline, resolve build issues, and optimize automation
- **Code Quality**: Enforce coding standards, conduct reviews, and manage technical debt
- **Architecture**: Make component-level architectural decisions within established patterns

### Decision Authority
**Can Decide Independently:**
- Implementation approach for features within scope
- Test strategy and coverage requirements
- Code structure and organization patterns
- Tool selection for development and testing
- Refactoring decisions for code maintainability

**Requires Consultation:**
- Architecture changes affecting multiple modules
- New external dependencies or framework changes
- API design changes affecting other components
- Performance optimization requiring resource allocation
- Breaking changes affecting user workflows

### Quality Gates
- [ ] Code follows project coding standards (`black`, `isort`, `ruff`)
- [ ] Type hints and documentation are comprehensive
- [ ] Test coverage meets project requirements (≥80%)
- [ ] All tests pass (unit, integration, performance)
- [ ] Security considerations addressed
- [ ] Performance impact assessed and acceptable
- [ ] Backward compatibility maintained or migration path provided

## Cross-Persona Impact Analysis

### When Updating Code Artifacts
**Automatic Analysis Required:**

1. **Test Artifacts Impact**
   - Update unit tests for modified functions/classes
   - Update integration tests for workflow changes
   - Update mock data and test fixtures
   - Update performance benchmarks

2. **Documentation Impact**
   - Code documentation (docstrings, type hints)
   - Technical documentation (`docs/` directory)
   - API documentation if interfaces change
   - Configuration documentation for new settings

3. **Configuration Impact**
   - YAML configuration templates in `base_configs/`
   - Environment variables and settings
   - Deployment configuration changes
   - Build pipeline configuration updates

4. **Cross-Module Impact**
   - Dependencies on other modules in `src/digitalmodel/modules/`
   - Shared utilities in `src/digitalmodel/common/`
   - Breaking changes requiring version updates
   - Migration scripts for data or configuration changes

### Impact on Other Personas

#### Product Manager Impact
- **Timeline Changes**: Implementation complexity affecting delivery dates
- **Resource Requirements**: Additional effort for testing, documentation, or training
- **Feature Scope**: Technical constraints requiring scope adjustments
- **Risk Assessment**: Technical risks affecting project success

#### Product Owner Impact
- **User Experience**: Changes affecting user workflows or interfaces
- **Acceptance Criteria**: Technical limitations affecting acceptance criteria
- **User Training**: New features requiring user documentation or training
- **Backward Compatibility**: Migration requirements for existing users

## Technical Standards and Practices

### Code Quality Standards
```python
# Type hints for all public functions
def analyze_catenary_riser(config: Dict[str, Any]) -> CatenaryResults:
    """Analyze catenary riser configuration.
    
    Args:
        config: YAML configuration dictionary containing riser parameters
        
    Returns:
        CatenaryResults object with analysis results
        
    Raises:
        ConfigurationError: If required configuration parameters missing
        AnalysisError: If analysis fails due to invalid parameters
    """
    pass

# Comprehensive error handling
try:
    results = run_analysis(config)
except AnalysisError as e:
    logger.error(f"Analysis failed: {e}")
    raise
```

### Testing Standards
```python
# Unit test structure
class TestCatenaryAnalysis:
    """Test suite for catenary analysis module."""
    
    def test_basic_analysis_success(self):
        """Test successful analysis with valid configuration."""
        config = load_test_config("basic_catenary.yml")
        results = analyze_catenary_riser(config)
        assert results.is_valid()
        assert results.tension_profile is not None
    
    def test_invalid_config_raises_error(self):
        """Test that invalid configuration raises appropriate error."""
        config = {"invalid": "config"}
        with pytest.raises(ConfigurationError):
            analyze_catenary_riser(config)

# Mock external dependencies
@patch('digitalmodel.modules.orcaflex.OrcFxAPI')
def test_orcaflex_integration(mock_orcaflex):
    """Test OrcaFlex integration with mocked API."""
    mock_model = MagicMock()
    mock_orcaflex.Model.return_value = mock_model
    
    results = run_orcaflex_analysis(test_config)
    assert mock_model.RunSimulation.called
```

### Documentation Standards
```python
class CatenaryRiser:
    """Catenary riser analysis component.
    
    This class provides methods for analyzing catenary riser configurations
    according to offshore engineering standards. It supports both static
    and dynamic analysis with various environmental conditions.
    
    Attributes:
        config: Configuration dictionary from YAML file
        environment: Environmental conditions (waves, current, wind)
        materials: Material properties for riser components
        
    Example:
        >>> config = load_config("riser_config.yml")
        >>> riser = CatenaryRiser(config)
        >>> results = riser.run_static_analysis()
        >>> print(f"Maximum tension: {results.max_tension} kN")
    """
```

## Workflow Integration

### Issue Implementation Workflow
1. **Analysis Phase** (`issue-planning.md`)
   - Technical feasibility assessment
   - Architecture impact analysis
   - Implementation approach design
   - Test strategy planning

2. **Implementation Phase** (`implementation-tracking.md`)
   - Code development with incremental commits
   - Test development parallel to feature development
   - Documentation updates as implementation progresses
   - Build pipeline validation at each stage

3. **Validation Phase** (`change-documentation.md`)
   - Code review and quality gate validation
   - Comprehensive testing execution
   - Performance and security validation
   - Documentation review and update

4. **Integration Phase** (`implementation-summary.md`)
   - Final validation against acceptance criteria
   - Impact assessment for other personas
   - Knowledge transfer documentation
   - Post-implementation monitoring setup

### Collaboration Patterns

#### With Product Manager
- **Requirements Clarification**: Technical feasibility and constraint identification
- **Timeline Estimation**: Effort estimation based on technical complexity
- **Risk Communication**: Technical risks affecting project delivery
- **Scope Negotiation**: Technical alternatives for scope or timeline adjustments

#### With Product Owner
- **Acceptance Criteria Review**: Technical validation of acceptance criteria
- **User Impact Assessment**: Technical changes affecting user experience
- **Demo Preparation**: Technical demonstration of implemented features
- **User Training Support**: Technical documentation for user-facing features

## Tools and Resources

### Development Tools
- **Code Editor**: VS Code with appropriate extensions
- **Version Control**: Git with conventional commit messages
- **Package Management**: `uv` for dependency management
- **Code Quality**: `black`, `isort`, `ruff`, `mypy`

### Testing Tools
- **Unit Testing**: `pytest` with comprehensive fixtures
- **Coverage**: `coverage.py` for test coverage analysis
- **Mocking**: `unittest.mock` for external dependency mocking
- **Performance**: Custom benchmarking for engineering calculations

### Documentation Tools
- **Code Documentation**: Sphinx for API documentation generation
- **Markdown**: For technical documentation and guides
- **Diagrams**: PlantUML for architecture and workflow diagrams
- **Jupyter Notebooks**: For analysis examples and tutorials

## Success Metrics

### Code Quality Metrics
- **Test Coverage**: ≥80% for all new code
- **Code Complexity**: Cyclomatic complexity ≤10 per function
- **Type Coverage**: 100% type hints for public APIs
- **Documentation Coverage**: 100% docstrings for public functions

### Delivery Metrics
- **Build Success Rate**: ≥95% pipeline success rate
- **Defect Rate**: <5% post-release defects
- **Performance**: No regression in key performance metrics
- **Technical Debt**: Manage and reduce technical debt over time

### Collaboration Metrics
- **Review Turnaround**: <24 hours for code review completion
- **Cross-Persona Alignment**: Successful handoffs with PM and PO
- **Knowledge Transfer**: Effective documentation and training delivery
- **Stakeholder Satisfaction**: Positive feedback from technical stakeholders