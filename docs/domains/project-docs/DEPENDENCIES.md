# Dependencies Documentation

## Core Dependencies

### Data Processing
- **pandas**: DataFrame operations and data analysis
- **numpy**: Numerical computing (implicit dependency of pandas)
- **scipy**: Scientific computing and technical computing

### Visualization
- **plotly**: Interactive visualizations
- **dash**: Web applications for data visualization
- **matplotlib**: Static plotting library
- **kaleido**: Static image export for plotly

### Engineering Specific
- **OrcFxAPI**: OrcaFlex integration for offshore engineering analysis
- **rainflow**: Fatigue analysis using rainflow counting
- **assetutilities**: Asset management utilities

### File I/O
- **openpyxl**: Excel file operations (.xlsx)
- **xlrd**: Legacy Excel file reading (.xls)
- **xlsxwriter**: Excel file writing with advanced features
- **python-docx**: Word document manipulation
- **pypdf2**: PDF file operations
- **tabula-py**: Extract tables from PDFs

### Data Formats
- **pyyaml**: YAML file parsing and writing
- **ruamel.yaml**: YAML with comment preservation
- **xmltodict**: XML to dictionary conversion

### Web & Scraping
- **beautifulsoup4**: HTML/XML parsing
- **requests**: HTTP library
- **scrapy**: Web scraping framework

### Utilities
- **deepdiff**: Deep difference of objects
- **pydantic**: Data validation using Python type annotations
- **loguru**: Logging made simple
- **sympy**: Symbolic mathematics
- **webcolors**: Color name/value conversions
- **imgkit**: HTML to image conversion
- **tabulate**: Pretty-print tabular data

## Development Dependencies

### Testing
- **pytest**: Testing framework
- **pytest-cov**: Coverage plugin for pytest
- **pytest-mock**: Mock/patch helpers for pytest
- **pytest-xdist**: Distributed testing

### Code Quality
- **black**: Code formatter
- **isort**: Import statement organizer
- **ruff**: Fast Python linter
- **mypy**: Static type checker

### Documentation
- **sphinx**: Documentation generator
- **sphinx-rtd-theme**: Read the Docs theme

## Version Policy

We follow these versioning practices:
- **Major version cap**: Prevent breaking changes (e.g., `pandas>=2.0.0,<3.0.0`)
- **Security updates**: Regular dependency updates for security patches
- **Compatibility testing**: All updates tested in CI/CD before merge

## Adding New Dependencies

1. Evaluate the need and alternatives
2. Check license compatibility (must be MIT-compatible)
3. Review security advisories
4. Add with appropriate version constraints
5. Update this documentation
6. Test thoroughly before committing