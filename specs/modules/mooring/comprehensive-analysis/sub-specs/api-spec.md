# API Specification

This is the API specification for the spec detailed in @specs/modules/mooring/comprehensive-analysis/spec.md

> Created: 2024-12-20
> Version: 1.0.0

## Python Module API

### Main Entry Point

```python
from digitalmodel.orcaflex.mooring_analysis.comprehensive_analysis import ComprehensiveMooringAnalyzer

analyzer = ComprehensiveMooringAnalyzer(config)
results = analyzer.analyze_directory(input_dir)
```

## CLI Interface

### Primary Command

```bash
python -m digitalmodel.orcaflex.mooring_analysis.comprehensive_analysis \
    --input-directory /path/to/csv \
    --output-directory /path/to/output \
    --config config.yml
```

### CLI Arguments

| Argument | Short | Type | Default | Description |
|----------|-------|------|---------|-------------|
| --input-directory | -d | Path | . | Directory containing CSV files |
| --output-directory | -o | Path | ./output | Output directory for reports |
| --pattern | -p | String | *.csv | File pattern for discovery |
| --config | -c | Path | None | Configuration YAML file |
| --parallel | | Flag | True | Enable parallel processing |
| --workers | -w | Int | CPU count | Number of parallel workers |
| --recursive | -r | Flag | True | Recursive directory search |
| --verbose | -v | Flag | False | Verbose output |
| --dry-run | | Flag | False | Preview without processing |
| --format | -f | String | markdown | Report format (markdown/html/excel) |

## Core API Classes

### ComprehensiveMooringAnalyzer

```python
class ComprehensiveMooringAnalyzer:
    def __init__(self, config: AnalysisConfig):
        """Initialize analyzer with configuration"""
        
    def analyze_directory(self, path: Path) -> ComprehensiveResults:
        """Analyze all CSV files in directory"""
        
    def analyze_files(self, files: List[Path]) -> ComprehensiveResults:
        """Analyze specific list of files"""
        
    def compare_groups(self, results: Dict[str, AnalysisResults]) -> GroupComparison:
        """Compare results across groups"""
```

### PretensionAnalyzer

```python
class PretensionAnalyzer:
    def parse_pretension_csv(self, file: Path) -> PretensionData:
        """Parse pretension CSV file"""
        
    def calculate_metrics(self, data: PretensionData) -> PretensionMetrics:
        """Calculate pretension metrics"""
        
    def assess_convergence(self, metrics: PretensionMetrics) -> ConvergenceStatus:
        """Assess convergence status"""
        
    def plot_convergence(self, metrics: PretensionMetrics) -> Figure:
        """Generate convergence plot"""
```

### StiffnessAnalyzer

```python
class StiffnessAnalyzer:
    def parse_stiffness_csv(self, file: Path) -> StiffnessData:
        """Parse stiffness CSV file"""
        
    def compute_stiffness_matrix(self, data: StiffnessData) -> np.ndarray:
        """Compute 6-DOF stiffness matrix"""
        
    def analyze_characteristics(self, matrix: np.ndarray) -> StiffnessMetrics:
        """Analyze stiffness characteristics"""
        
    def estimate_natural_periods(self, matrix: np.ndarray, mass: float) -> Dict:
        """Estimate natural periods"""
        
    def plot_stiffness_matrix(self, matrix: np.ndarray) -> Figure:
        """Generate stiffness matrix visualization"""
```

### FenderForcesAnalyzer

```python
class FenderForcesAnalyzer:
    def parse_fender_csv(self, file: Path) -> FenderForceData:
        """Parse fender forces CSV file"""
        
    def calculate_utilization(self, data: FenderForceData) -> FenderMetrics:
        """Calculate fender utilization metrics"""
        
    def assess_capacity(self, metrics: FenderMetrics, design: FenderDesign) -> CapacityAssessment:
        """Assess against design capacity"""
        
    def plot_force_distribution(self, metrics: FenderMetrics) -> Figure:
        """Generate force distribution plot"""
```

### GroupComparator

```python
class GroupComparator:
    def identify_groups(self, files: List[Path]) -> Dict[str, List[Path]]:
        """Identify groups from filename patterns"""
        
    def compare_metrics(self, group_results: Dict[str, AnalysisResults]) -> Comparison:
        """Compare metrics across groups"""
        
    def rank_configurations(self, comparison: Comparison) -> Rankings:
        """Rank configurations by performance"""
        
    def identify_trends(self, comparison: Comparison) -> Trends:
        """Identify trends across groups"""
```

### ContextExtractor

```python
class ContextExtractor:
    def extract_from_filename(self, filename: str) -> ContextInfo:
        """Extract context from filename using LLM"""
        
    def extract_from_content(self, content: pd.DataFrame) -> ContextInfo:
        """Extract context from file content"""
        
    def apply_standards(self, context: ContextInfo) -> Standards:
        """Apply relevant industry standards"""
```

### ComprehensiveSummarizer

```python
class ComprehensiveSummarizer:
    def summarize_individual(self, results: AnalysisResults) -> IndividualSummary:
        """Generate individual run summary"""
        
    def summarize_group(self, group: str, results: List[AnalysisResults]) -> GroupSummary:
        """Generate group summary"""
        
    def summarize_overall(self, all_results: Dict[str, List[AnalysisResults]]) -> OverallSummary:
        """Generate overall summary"""
        
    def generate_recommendations(self, summary: OverallSummary) -> Recommendations:
        """Generate actionable recommendations"""
```

## Data Models

### Input Models

```python
@dataclass
class AnalysisConfig:
    input_directory: Path
    output_directory: Path
    convergence: ConvergenceCriteria
    stiffness: StiffnessConfig
    fender: FenderConfig
    grouping: GroupingConfig
    report: ReportConfig
```

### Output Models

```python
@dataclass
class ComprehensiveResults:
    config: AnalysisConfig
    individual_results: Dict[str, AnalysisResults]
    group_comparisons: GroupComparison
    overall_summary: OverallSummary
    processing_stats: Dict[str, Any]
    errors: List[str]
    warnings: List[str]
```

## Error Responses

### Exception Hierarchy

```python
MooringAnalysisError (base)
├── ConfigurationError
├── DataParsingError
├── ConvergenceError
├── StiffnessCalculationError
├── FenderAnalysisError
├── GroupComparisonError
├── ContextExtractionError
├── ReportGenerationError
└── ProcessingError
```

### Error Handling

```python
try:
    results = analyzer.analyze_directory(path)
except DataParsingError as e:
    print(f"Failed to parse {e.filename}: {e.message}")
except ProcessingError as e:
    print(f"Processing failed: {e.message}")
    print(f"Failed files: {e.failed_files}")
```

## Usage Examples

### Basic Analysis

```python
from digitalmodel.orcaflex.mooring_analysis.comprehensive_analysis import (
    ComprehensiveMooringAnalyzer,
    AnalysisConfig
)

# Create configuration
config = AnalysisConfig(
    input_directory=Path("./csv_files"),
    output_directory=Path("./reports")
)

# Run analysis
analyzer = ComprehensiveMooringAnalyzer(config)
results = analyzer.analyze_directory(config.input_directory)

# Generate report
report = results.generate_markdown_report()
```

### Advanced Configuration

```python
# Load configuration from YAML
config = AnalysisConfig.from_yaml("analysis_config.yml")

# Customize specific settings
config.processing.max_workers = 16
config.report.formats = ['markdown', 'html', 'excel']

# Run with custom configuration
analyzer = ComprehensiveMooringAnalyzer(config)
results = analyzer.analyze_files(specific_files)
```

### CLI Batch Processing

```bash
# Process all CSV files in directory tree
python -m digitalmodel.orcaflex.mooring_analysis.comprehensive_analysis \
    --input-directory /project/orcaflex/output \
    --recursive \
    --parallel \
    --workers 16 \
    --format markdown \
    --output-directory /project/reports

# Dry run to preview what will be processed
python -m digitalmodel.orcaflex.mooring_analysis.comprehensive_analysis \
    --input-directory /project/orcaflex/output \
    --dry-run \
    --verbose
```