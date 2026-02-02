# OrcaFlex Time Trace Signal Processing Architecture

## Overview
Architecture design for seamlessly integrating OrcaFlex simulation outputs with the signal analysis module for automated fatigue and spectral analysis.

## Architecture Pattern

### 1. Layered Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                         │
│  (Fatigue Reports, Dashboards, Automated Assessments)       │
└─────────────────────────────────────────────────────────────┘
                              ▲
                              │
┌─────────────────────────────────────────────────────────────┐
│                    Orchestration Layer                       │
│         (OrcaFlexTimeTraceProcessor, Pipeline)              │
└─────────────────────────────────────────────────────────────┘
                              ▲
                              │
        ┌─────────────────────┴─────────────────────┐
        ▼                                           ▼
┌──────────────────┐                    ┌──────────────────────┐
│  Data Access     │                    │  Signal Analysis     │
│     Layer        │                    │      Layer           │
│                  │                    │                      │
│ • OrcaFlex API   │                    │ • RainflowCounter    │
│ • File Readers   │                    │ • SpectralAnalyzer   │
│ • Cache Manager  │                    │ • FatigueDamage      │
└──────────────────┘                    └──────────────────────┘
        ▲                                           ▲
        │                                           │
┌─────────────────────────────────────────────────────────────┐
│                      Data Layer                             │
│         (.sim, .dat, .yml, .csv, .h5 files)                │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. OrcaFlexTimeTraceProcessor (Main Integration Class)

```python
from dataclasses import dataclass
from typing import Dict, List, Optional, Union
import pandas as pd
import numpy as np
from pathlib import Path

@dataclass
class TimeTraceConfig:
    """Configuration for time trace processing"""
    # Source configuration
    source_type: str = 'orcaflex'  # 'orcaflex', 'csv', 'hdf5'
    file_path: Optional[Path] = None
    model: Optional[object] = None  # OrcaFlex model object
    
    # Time trace selection
    objects: List[str] = None  # ['Line1', 'Line2', 'Vessel1']
    variables: List[str] = None  # ['Tension', 'BendMoment', 'X', 'Y', 'Z']
    time_range: Optional[tuple] = None  # (start_time, end_time)
    
    # Processing options
    preprocessing: Dict = None  # {'detrend': True, 'filter': {...}}
    fatigue_analysis: Dict = None  # {'sn_curve': 'DNV-F', 'scf': 1.0}
    spectral_analysis: Dict = None  # {'method': 'welch', 'window': 'hann'}
    
    # Output configuration
    output_dir: Path = Path('./results')
    save_intermediate: bool = False
    generate_report: bool = True


class OrcaFlexTimeTraceProcessor:
    """
    Main processor for OrcaFlex time trace analysis
    Bridges OrcaFlex outputs with signal analysis module
    """
    
    def __init__(self, config: TimeTraceConfig):
        self.config = config
        self.signal_analyzer = self._initialize_analyzers()
        self.results = {}
        
    def process(self) -> Dict:
        """Main processing pipeline"""
        # Step 1: Load time traces
        time_traces = self._load_time_traces()
        
        # Step 2: Preprocess signals
        if self.config.preprocessing:
            time_traces = self._preprocess_signals(time_traces)
        
        # Step 3: Perform analyses
        results = {}
        
        if self.config.fatigue_analysis:
            results['fatigue'] = self._perform_fatigue_analysis(time_traces)
            
        if self.config.spectral_analysis:
            results['spectral'] = self._perform_spectral_analysis(time_traces)
        
        # Step 4: Generate reports
        if self.config.generate_report:
            self._generate_reports(results)
        
        return results
    
    def _load_time_traces(self) -> pd.DataFrame:
        """Load time traces from OrcaFlex or files"""
        if self.config.source_type == 'orcaflex':
            return self._load_from_orcaflex()
        elif self.config.source_type == 'csv':
            return self._load_from_csv()
        elif self.config.source_type == 'hdf5':
            return self._load_from_hdf5()
    
    def _perform_fatigue_analysis(self, traces: pd.DataFrame) -> Dict:
        """Perform fatigue analysis on time traces"""
        from digitalmodel.signal_analysis import (
            RainflowCounter, 
            FatigueDamageCalculator,
            SNCurve
        )
        
        results = {}
        for column in traces.columns:
            if column == 'Time':
                continue
                
            # Rainflow counting
            counter = RainflowCounter()
            cycles = counter.count_cycles(traces[column].values)
            
            # Fatigue damage
            sn_params = self._get_sn_curve_params()
            damage_calc = FatigueDamageCalculator()
            damage = damage_calc.calculate_damage(cycles, sn_params)
            
            results[column] = {
                'cycles': cycles,
                'damage': damage,
                'statistics': counter.get_statistics(cycles)
            }
        
        return results
```

### 2. Pipeline Architecture

```python
class OrcaFlexAnalysisPipeline:
    """
    Configurable pipeline for batch processing multiple simulations
    """
    
    def __init__(self):
        self.stages = []
        self.results_aggregator = ResultsAggregator()
    
    def add_stage(self, stage: 'PipelineStage'):
        """Add processing stage to pipeline"""
        self.stages.append(stage)
        return self
    
    def process_batch(self, simulations: List[Path]) -> pd.DataFrame:
        """Process multiple simulations"""
        all_results = []
        
        for sim_path in simulations:
            result = self.process_single(sim_path)
            all_results.append(result)
        
        return self.results_aggregator.aggregate(all_results)
    
    def process_single(self, sim_path: Path) -> Dict:
        """Process single simulation through pipeline"""
        data = {'simulation': sim_path}
        
        for stage in self.stages:
            data = stage.process(data)
        
        return data


class PipelineStage:
    """Base class for pipeline stages"""
    
    def process(self, data: Dict) -> Dict:
        raise NotImplementedError


class LoadOrcaFlexStage(PipelineStage):
    """Load OrcaFlex simulation results"""
    
    def process(self, data: Dict) -> Dict:
        import OrcFxAPI
        model = OrcFxAPI.Model(str(data['simulation']))
        data['model'] = model
        data['time_traces'] = self._extract_time_traces(model)
        return data


class SignalAnalysisStage(PipelineStage):
    """Apply signal analysis to time traces"""
    
    def __init__(self, analysis_config: Dict):
        self.config = analysis_config
    
    def process(self, data: Dict) -> Dict:
        processor = OrcaFlexTimeTraceProcessor(
            TimeTraceConfig(**self.config)
        )
        data['analysis_results'] = processor.process()
        return data


class QualityCheckStage(PipelineStage):
    """Validate results against criteria"""
    
    def __init__(self, criteria: Dict):
        self.criteria = criteria
    
    def process(self, data: Dict) -> Dict:
        data['qa_results'] = self._check_criteria(
            data['analysis_results'],
            self.criteria
        )
        return data
```

### 3. Efficient Batch Processing

```python
class ParallelBatchProcessor:
    """
    Parallel processing for multiple simulations/load cases
    """
    
    def __init__(self, n_workers: int = 4):
        self.n_workers = n_workers
        
    def process_load_cases(self, 
                          base_model: Path,
                          load_cases: List[Dict],
                          analysis_config: TimeTraceConfig) -> pd.DataFrame:
        """
        Process multiple load cases in parallel
        """
        from concurrent.futures import ProcessPoolExecutor
        from functools import partial
        
        # Create processing function
        process_fn = partial(
            self._process_single_case,
            base_model=base_model,
            analysis_config=analysis_config
        )
        
        # Process in parallel
        with ProcessPoolExecutor(max_workers=self.n_workers) as executor:
            results = list(executor.map(process_fn, load_cases))
        
        # Aggregate results
        return self._aggregate_results(results)
    
    def _process_single_case(self, 
                            load_case: Dict,
                            base_model: Path,
                            analysis_config: TimeTraceConfig) -> Dict:
        """Process single load case"""
        # Apply load case to model
        model = self._apply_load_case(base_model, load_case)
        
        # Run simulation if needed
        if not model.is_simulated():
            model.simulate()
        
        # Extract and analyze time traces
        processor = OrcaFlexTimeTraceProcessor(analysis_config)
        return processor.process()
```

### 4. Result Aggregation and Reporting

```python
class ResultsAggregator:
    """
    Aggregate results from multiple analyses
    """
    
    def aggregate(self, results: List[Dict]) -> pd.DataFrame:
        """Combine results into summary DataFrame"""
        summary_data = []
        
        for result in results:
            summary = self._extract_summary(result)
            summary_data.append(summary)
        
        df = pd.DataFrame(summary_data)
        
        # Add statistical summaries
        df['damage_total'] = df[[c for c in df.columns if 'damage' in c]].sum(axis=1)
        df['critical_component'] = df[[c for c in df.columns if 'damage' in c]].idxmax(axis=1)
        
        return df
    
    def generate_report(self, 
                       summary_df: pd.DataFrame,
                       output_path: Path):
        """Generate comprehensive report"""
        from digitalmodel.reporting import ReportGenerator
        
        report = ReportGenerator()
        report.add_summary_table(summary_df)
        report.add_fatigue_charts(summary_df)
        report.add_compliance_check(summary_df)
        report.save(output_path)
```

## Recommended Implementation Pattern

### 1. Simple Single Analysis
```python
# For single simulation analysis
config = TimeTraceConfig(
    source_type='orcaflex',
    file_path=Path('simulation.sim'),
    objects=['Line1', 'Line2'],
    variables=['Tension'],
    fatigue_analysis={
        'sn_curve': 'DNV-F',
        'scf': 1.15,
        'mean_stress_correction': 'goodman'
    },
    spectral_analysis={
        'method': 'welch',
        'nperseg': 256
    }
)

processor = OrcaFlexTimeTraceProcessor(config)
results = processor.process()
```

### 2. Batch Pipeline Processing
```python
# For multiple simulations/load cases
pipeline = OrcaFlexAnalysisPipeline()
pipeline.add_stage(LoadOrcaFlexStage())
pipeline.add_stage(SignalAnalysisStage(analysis_config))
pipeline.add_stage(QualityCheckStage(design_criteria))

# Process all simulations
simulations = Path('./simulations').glob('*.sim')
results = pipeline.process_batch(simulations)
```

### 3. Parallel Load Case Analysis
```python
# For parametric studies
processor = ParallelBatchProcessor(n_workers=8)
results = processor.process_load_cases(
    base_model=Path('base_model.dat'),
    load_cases=load_case_matrix,
    analysis_config=config
)
```

## Key Design Principles

### 1. **Separation of Concerns**
- OrcaFlex I/O separate from signal processing
- Clear interfaces between layers
- Modular, testable components

### 2. **Configuration-Driven**
- YAML/JSON configurations for reproducibility
- Parameterized processing options
- Version-controlled analysis settings

### 3. **Performance Optimization**
- Lazy loading of large datasets
- Parallel processing for batch jobs
- Caching of intermediate results
- Memory-efficient streaming for long time series

### 4. **Extensibility**
- Plugin architecture for custom stages
- Support for multiple data sources
- Configurable output formats

### 5. **Traceability**
- Full audit trail of processing steps
- Metadata preservation
- Reproducible results

## Integration Points

### Input Sources
- OrcaFlex .sim/.dat files
- CSV time history exports
- HDF5 databases
- Direct API connection to running simulations

### Output Formats
- Structured HDF5 databases
- Excel reports with charts
- JSON for web dashboards
- PDF technical reports

### External Systems
- Design databases
- Asset integrity systems
- Digital twin platforms
- Cloud storage (Azure, S3)

## Example Workflow Configuration

```yaml
# orcaflex_analysis_config.yml
analysis:
  name: "Mooring System Fatigue Assessment"
  version: "1.0"
  
data_source:
  type: "orcaflex"
  path: "./simulations/"
  pattern: "*.sim"
  
objects:
  - name: "Mooring Lines"
    items: ["Line1", "Line2", "Line3", "Line4"]
    variables: ["Tension", "BendMoment"]
    
preprocessing:
  detrend: true
  filter:
    type: "highpass"
    cutoff: 0.01  # Hz
    
fatigue:
  sn_curve:
    standard: "DNV"
    class: "F"
  safety_factors:
    scf: 1.15
    design_life: 25  # years
  mean_stress_correction: "goodman"
  
spectral:
  method: "welch"
  window: "hann"
  nperseg: 512
  
reporting:
  generate_pdf: true
  generate_excel: true
  include_plots: true
  
quality_checks:
  max_damage: 1.0
  min_safety_factor: 10.0
  max_tension_utilization: 0.6
```

## Benefits of This Architecture

1. **Scalability**: Handle single analyses to thousands of load cases
2. **Maintainability**: Clear separation between OrcaFlex and analysis logic
3. **Reusability**: Components can be used independently
4. **Performance**: Parallel processing and efficient data handling
5. **Flexibility**: Easy to add new analysis types or data sources
6. **Reliability**: Error handling and validation at each stage
7. **Reproducibility**: Configuration-driven for consistent results

## Next Steps

1. Implement `OrcaFlexTimeTraceProcessor` core class
2. Create standard pipeline configurations
3. Build parallel processing capabilities
4. Develop reporting templates
5. Add validation and QA stages