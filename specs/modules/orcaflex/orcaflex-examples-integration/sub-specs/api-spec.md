# API Specification

This is the API specification for the spec detailed in @specs/modules/orcaflex/orcaflex-examples-integration/spec.md

> Created: 2024-12-19
> Version: 1.0.0

## Internal APIs

### ExampleDownloader API

```python
class ExampleDownloader:
    """Downloads OrcaFlex examples from Orcina portal"""
    
    def download_all(self, output_dir: str) -> DownloadReport:
        """Download all available examples"""
        pass
    
    def download_category(self, category: str, output_dir: str) -> List[str]:
        """Download examples from specific category"""
        pass
    
    def get_available_categories(self) -> List[str]:
        """Get list of available example categories"""
        pass
    
    def get_example_metadata(self, example_url: str) -> ExampleMetadata:
        """Get metadata for specific example"""
        pass
```

### FormatConverter API

```python
class FormatConverter:
    """Converts OrcaFlex files between formats"""
    
    def convert_to_yaml(self, input_path: str, output_path: str) -> ConversionResult:
        """Convert single file to YAML format"""
        pass
    
    def batch_convert(self, input_dir: str, output_dir: str, 
                     parallel: bool = True) -> BatchConversionReport:
        """Convert multiple files in parallel"""
        pass
    
    def validate_yaml(self, yaml_path: str) -> ValidationResult:
        """Validate converted YAML file"""
        pass
```

### FeatureAnalyzer API

```python
class FeatureAnalyzer:
    """Analyzes OrcaFlex models for key features"""
    
    def analyze_model(self, model_path: str) -> ModelFeatures:
        """Extract features from single model"""
        pass
    
    def analyze_directory(self, directory: str) -> AnalysisReport:
        """Analyze all models in directory"""
        pass
    
    def compare_models(self, model1: str, model2: str) -> ComparisonResult:
        """Compare features between two models"""
        pass
    
    def generate_summary(self, features: ModelFeatures) -> str:
        """Generate human-readable feature summary"""
        pass
```

### KnowledgeIntegrator API

```python
class KnowledgeIntegrator:
    """Integrates analyzed data into module agent"""
    
    def update_agent_knowledge(self, analysis_report: AnalysisReport) -> bool:
        """Update agent's knowledge base with new examples"""
        pass
    
    def build_search_index(self, examples_dir: str) -> SearchIndex:
        """Build searchable index of examples"""
        pass
    
    def generate_catalog(self, analysis_report: AnalysisReport) -> str:
        """Generate markdown catalog of examples"""
        pass
```

## Data Models

### DownloadReport

```python
@dataclass
class DownloadReport:
    total_files: int
    successful_downloads: int
    failed_downloads: int
    categories: Dict[str, int]
    errors: List[DownloadError]
    duration: float
    manifest: Dict[str, str]  # filename -> checksum
```

### ModelFeatures

```python
@dataclass
class ModelFeatures:
    # Metadata
    name: str
    category: str
    file_size: int
    orcaflex_version: str
    
    # Components
    vessels: List[VesselInfo]
    lines: List[LineInfo]
    buoys: List[BuoyInfo]
    constraints: List[ConstraintInfo]
    winches: List[WinchInfo]
    links: List[LinkInfo]
    
    # Analysis
    analysis_types: List[str]
    simulation_duration: float
    time_step: float
    
    # Environment
    wave_data: WaveData
    current_data: CurrentData
    wind_data: WindData
    
    # Results
    output_variables: List[str]
    range_graphs: List[str]
    time_histories: List[str]
```

### ConversionResult

```python
@dataclass
class ConversionResult:
    success: bool
    input_file: str
    output_file: str
    file_size_before: int
    file_size_after: int
    conversion_time: float
    validation_passed: bool
    errors: List[str]
    warnings: List[str]
```

## CLI Interface

### Command-Line Usage

```bash
# Download all examples
python -m orcaflex_examples download --all --output ./examples

# Convert files to YAML
python -m orcaflex_examples convert --input ./examples/raw --output ./examples/yaml

# Analyze features
python -m orcaflex_examples analyze --input ./examples/yaml --report features.json

# Update agent knowledge
python -m orcaflex_examples integrate --analysis features.json --agent-dir ./agents/orcaflex

# Full pipeline
python -m orcaflex_examples pipeline --output ./docs/modules/orcaflex/examples
```

## REST API Endpoints (Future Enhancement)

### Example Management

```yaml
GET /api/examples
  description: List all downloaded examples
  response: List[ExampleSummary]

GET /api/examples/{id}
  description: Get specific example details
  response: ExampleDetails

POST /api/examples/download
  description: Trigger download of new examples
  body: {category: string, force: boolean}
  response: DownloadReport

POST /api/examples/convert/{id}
  description: Convert specific example to YAML
  response: ConversionResult
```

### Feature Analysis

```yaml
GET /api/analysis/{example_id}
  description: Get feature analysis for example
  response: ModelFeatures

POST /api/analysis/compare
  description: Compare features between examples
  body: {example1_id: string, example2_id: string}
  response: ComparisonResult

GET /api/analysis/search
  description: Search examples by features
  query: {component: string, analysis_type: string}
  response: List[ExampleSummary]
```

## Error Codes

| Code | Description | Resolution |
|------|-------------|------------|
| E001 | Network connection failed | Check internet connection |
| E002 | Portal structure changed | Update scraper logic |
| E003 | OrcaFlex license not found | Ensure valid license |
| E004 | Conversion failed | Check file compatibility |
| E005 | Analysis timeout | Increase timeout or simplify model |
| E006 | Storage full | Free up disk space |
| E007 | Invalid YAML output | Review conversion settings |
| E008 | Agent update failed | Check file permissions |