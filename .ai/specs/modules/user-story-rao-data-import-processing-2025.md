# User Story Specification: RAO Data Import and Processing

**Story ID**: US-2025-201  
**Feature**: `.ai/specs/modules/feature-6dof-motion-analysis-2025.md`  
**Epic**: `.ai/specs/modules/epic-marine-analysis-ship-design-dynamics-2025.md`  
**Implementation Date**: TBD  
**Effort**: 5 days (Estimated)  
**Status**: Draft

## User Story Definition

### Story Statement
**As a** Marine Engineer working on vessel motion analysis  
**I want** to import and process displacement RAO (Response Amplitude Operator) data from various sources  
**So that** I can perform accurate 6-DOF motion analysis and seakeeping assessment for marine vessels and floating structures

### User Context
- **Primary Persona**: Marine Engineer (Senior level with hydrodynamics experience)
- **Secondary Personas**: Naval Architect, FPSO Designer, Offshore Engineer
- **User Journey**: Motion analysis workflow from RAO data acquisition to vessel response calculation
- **Frequency of Use**: Daily for active marine design projects, weekly for design validation

### Business Value
- **User Value**: Streamlined RAO data processing eliminates manual data manipulation and reduces analysis time by 70%
- **Business Value**: Enables accurate motion analysis for marine vessel design and FPSO systems worth $100M+ projects
- **Priority Rationale**: Foundation requirement for all 6-DOF motion analysis capabilities
- **Success Metrics**: Import 99% of industry-standard RAO data formats with <5% accuracy loss

## Acceptance Criteria

### Primary Acceptance Criteria
- [ ] AC-201.1.1: Import RAO data from ANSYS AQWA .lis output files with complete 6-DOF data extraction
- [ ] AC-201.1.2: Process experimental RAO data from CSV and Excel formats with flexible column mapping
- [ ] AC-201.1.3: Validate RAO data quality including frequency range, heading coverage, and physical reasonableness
- [ ] AC-201.1.4: Interpolate RAO data to user-defined frequency and heading grids with cubic spline interpolation
- [ ] AC-201.1.5: Handle missing data points and provide quality warnings for incomplete datasets
- [ ] AC-201.1.6: Return RAO data as pandas DataFrames containing:
  - [ ] All headings as columns
  - [ ] 6 DOF (surge, sway, heave, roll, pitch, yaw) amplitude data
  - [ ] 6 DOF phase data (in degrees)
  - [ ] Frequency/period as index

### Technical Acceptance Criteria
- [ ] TAC-201.1.1: Support ANSYS AQWA versions 18.0+ with backward compatibility to version 15.0
- [ ] TAC-201.1.2: Process RAO datasets up to 1000 frequency points × 37 headings × 6 DOF within 30 seconds
- [ ] TAC-201.1.3: Maintain numerical precision with <0.1% interpolation error for smooth RAO functions
- [ ] TAC-201.1.4: Provide comprehensive error handling with specific feedback for data format issues
- [ ] TAC-201.1.5: Export processed RAO data to standardized HDF5 format for efficient storage and retrieval

### Definition of Done
- [ ] Functionality implemented with comprehensive RAO data support
- [ ] Unit tests covering all RAO import formats and edge cases
- [ ] Integration tests with sample AQWA and experimental datasets
- [ ] Performance tests validating processing time requirements
- [ ] Documentation complete with data format specifications
- [ ] Validation against known benchmark RAO datasets

## GitHub Integration

### Related Issues and PRs
- **Primary Issue**: Marine Analysis Implementation - 6-DOF Motion Analysis Feature
- **Related Issues**: Hydrodynamic modeling integration, vessel configuration system
- **Dependencies**: Environmental loading module, marine vessel database

### Code Changes Summary
```
Files Added: 8
Files Modified: 3
Files Deleted: 0
Lines Added: ~2,500
Test Coverage: ~800 lines
```

### Key File Changes
- **Added**: `src/digitalmodel/modules/marine_analysis/rao_processor.py` - Core RAO data processing
- **Added**: `src/digitalmodel/modules/marine_analysis/aqwa_reader.py` - ANSYS AQWA file parser
- **Added**: `src/digitalmodel/modules/marine_analysis/experimental_reader.py` - Experimental data import
- **Added**: `src/digitalmodel/modules/marine_analysis/rao_interpolator.py` - Data interpolation and validation
- **Added**: `src/digitalmodel/base_configs/modules/rao_analysis.yml` - Configuration template
- **Added**: `tests/modules/marine_analysis/test_rao_processor.py` - Comprehensive test suite
- **Modified**: `src/digitalmodel/modules/marine_analysis/__init__.py` - Module integration
- **Modified**: `src/digitalmodel/engine.py` - Analysis workflow integration

## Technical Implementation

### Implementation Approach
Develop modular RAO data processing system with support for multiple data sources, robust validation, and efficient storage:

1. **Multi-format Data Import**: Support ANSYS AQWA, experimental, and generic formats
2. **Data Validation Pipeline**: Comprehensive quality checking and error reporting
3. **Interpolation Framework**: Advanced interpolation with quality preservation
4. **Standardized Storage**: Efficient HDF5-based storage with metadata
5. **Integration Interface**: Clean API for motion analysis workflows

### Key Code Implementation
```python
# RAO Data Processing Core Implementation
class RAODataProcessor:
    """Process Response Amplitude Operator data from various sources."""
    
    def __init__(self, config: Dict[str, Any]):
        """Initialize RAO processor with configuration."""
        self.config = config
        self.validators = RAODataValidators()
        self.interpolator = RAOInterpolator()
        self.storage = RAODataStorage()
    
    def import_aqwa_lis_file(self, file_path: str) -> RAOData:
        """Import RAO data from ANSYS AQWA .lis file.
        
        Args:
            file_path: Path to ANSYS AQWA .lis output file
            
        Returns:
            RAOData object with 6-DOF displacement RAOs
            
        Raises:
            AQWAFileError: If file format is invalid or unsupported
            DataQualityError: If RAO data fails quality validation
        """
        # Parse AQWA output format
        raw_data = self._parse_aqwa_file(file_path)
        
        # Extract 6-DOF RAO data vs frequency and heading
        rao_data = self._extract_displacement_raos(raw_data)
        
        # Validate data completeness and physical reasonableness
        validation_report = self.validators.validate_rao_data(rao_data)
        if not validation_report.is_valid:
            raise DataQualityError(f"RAO validation failed: {validation_report.errors}")
        
        return rao_data
    
    def get_rao_dataframes(self, rao_data: RAOData) -> Dict[str, pd.DataFrame]:
        """Convert RAO data to pandas DataFrames.
        
        Returns:
            Dictionary with 'amplitude' and 'phase' DataFrames:
            - Each DataFrame has frequency/period as index
            - Columns are multi-level: (DOF, Heading)
            - DOF: surge, sway, heave, roll, pitch, yaw
            - Heading: all available headings (0-360 degrees)
            - Phase values in degrees
        """
        import pandas as pd
        
        # Create amplitude DataFrame
        amplitude_data = {}
        phase_data = {}
        
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        for dof in dof_names:
            for heading in rao_data.headings:
                # Create column name as (DOF, Heading)
                col_name = (dof, f"{heading}deg")
                
                # Extract amplitude and phase for this DOF and heading
                amplitude_data[col_name] = rao_data.raos[dof]['amplitude'][:, heading_idx]
                phase_data[col_name] = rao_data.raos[dof]['phase'][:, heading_idx]  # in degrees
        
        # Create DataFrames with frequency/period as index
        freq_index = pd.Index(rao_data.frequencies, name='Frequency (rad/s)')
        
        amplitude_df = pd.DataFrame(amplitude_data, index=freq_index)
        amplitude_df.columns = pd.MultiIndex.from_tuples(amplitude_df.columns, names=['DOF', 'Heading'])
        
        phase_df = pd.DataFrame(phase_data, index=freq_index)
        phase_df.columns = pd.MultiIndex.from_tuples(phase_df.columns, names=['DOF', 'Heading'])
        
        return {
            'amplitude': amplitude_df,
            'phase': phase_df
        }
    
    def import_experimental_data(self, file_path: str, format_config: Dict) -> RAOData:
        """Import experimental RAO data from CSV/Excel formats.
        
        Args:
            file_path: Path to experimental data file
            format_config: Configuration for column mapping and units
            
        Returns:
            Standardized RAOData object
        """
        # Support CSV, Excel, custom formats
        if file_path.endswith('.csv'):
            raw_data = self._read_csv_data(file_path, format_config)
        elif file_path.endswith(('.xlsx', '.xls')):
            raw_data = self._read_excel_data(file_path, format_config)
        else:
            raise UnsupportedFormatError(f"Unsupported file format: {file_path}")
        
        # Standardize data structure and units
        standardized_data = self._standardize_experimental_data(raw_data, format_config)
        
        # Quality check and interpolation
        validated_data = self.validators.validate_experimental_data(standardized_data)
        return validated_data
    
    def interpolate_rao_data(self, rao_data: RAOData, 
                           target_frequencies: np.ndarray,
                           target_headings: np.ndarray) -> RAOData:
        """Interpolate RAO data to target frequency and heading grids.
        
        Args:
            rao_data: Source RAO data
            target_frequencies: Target frequency array (rad/s)
            target_headings: Target heading array (degrees)
            
        Returns:
            Interpolated RAOData on target grid
        """
        return self.interpolator.interpolate_2d(
            rao_data, target_frequencies, target_headings
        )

# ANSYS AQWA File Parser
class AQWAReader:
    """Parser for ANSYS AQWA .lis output files."""
    
    def parse_lis_file(self, file_path: str) -> Dict[str, Any]:
        """Parse AQWA .lis file and extract RAO data.
        
        Returns:
            Dictionary with frequency, heading, and RAO arrays
        """
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Extract displacement RAO section
        rao_section = self._extract_rao_section(content)
        
        # Parse frequency and heading arrays
        frequencies = self._parse_frequencies(rao_section)
        headings = self._parse_headings(rao_section)
        
        # Parse 6-DOF RAO data
        surge_raos = self._parse_dof_raos(rao_section, dof=1)
        sway_raos = self._parse_dof_raos(rao_section, dof=2)
        heave_raos = self._parse_dof_raos(rao_section, dof=3)
        roll_raos = self._parse_dof_raos(rao_section, dof=4)
        pitch_raos = self._parse_dof_raos(rao_section, dof=5)
        yaw_raos = self._parse_dof_raos(rao_section, dof=6)
        
        return {
            'frequencies': frequencies,
            'headings': headings,
            'raos': {
                'surge': surge_raos,
                'sway': sway_raos,
                'heave': heave_raos,
                'roll': roll_raos,
                'pitch': pitch_raos,
                'yaw': yaw_raos
            }
        }
    
    def _extract_rao_amplitudes_phases(self, rao_section: str) -> Tuple[Dict, Dict]:
        """Extract amplitude and phase data from RAO section.
        
        Returns:
            Tuple of (amplitude_dict, phase_dict) where each dict contains:
            - Keys: DOF names ('surge', 'sway', etc.)
            - Values: 2D arrays [frequency x heading] with amplitude/phase values
        """
        # Implementation to parse amplitude and phase values
        # Phase values should be in degrees
        pass

# RAO Data Validation
class RAODataValidators:
    """Validation suite for RAO data quality and physical reasonableness."""
    
    def validate_rao_data(self, rao_data: RAOData) -> ValidationReport:
        """Comprehensive RAO data validation."""
        report = ValidationReport()
        
        # Check frequency range and resolution
        self._validate_frequency_range(rao_data.frequencies, report)
        
        # Check heading coverage
        self._validate_heading_coverage(rao_data.headings, report)
        
        # Check physical reasonableness
        self._validate_rao_magnitudes(rao_data, report)
        
        # Check data completeness
        self._validate_data_completeness(rao_data, report)
        
        return report
    
    def _validate_frequency_range(self, frequencies: np.ndarray, 
                                 report: ValidationReport) -> None:
        """Validate frequency range covers typical wave conditions."""
        min_freq, max_freq = frequencies.min(), frequencies.max()
        
        if min_freq > 0.2:  # rad/s
            report.add_warning("Frequency range may miss low-frequency waves")
        
        if max_freq < 1.5:  # rad/s
            report.add_warning("Frequency range may miss high-frequency waves")
        
        # Check for reasonable resolution
        freq_resolution = np.mean(np.diff(frequencies))
        if freq_resolution > 0.1:
            report.add_warning("Frequency resolution may be too coarse")
```

### Configuration Schema
```yaml
# RAO Data Processing Configuration
rao_data_processing:
  import_settings:
    aqwa:
      file_type: "lis"  # lis, out
      version_compatibility: ["15.0", "18.0", "19.0", "20.0"]
      encoding: "utf-8"
      
    experimental:
      supported_formats: ["csv", "xlsx", "xls"]
      column_mapping:
        frequency: "Freq_rad_s"  # or custom column name
        heading: "Head_deg"
        surge_amplitude: "Surge_m_m"
        surge_phase: "Surge_phase_deg"
        # ... similar for other DOFs
      
  validation:
    frequency_range:
      min_required: 0.1    # rad/s
      max_required: 2.0    # rad/s
      resolution_warning: 0.1  # rad/s
      
    heading_coverage:
      min_required: 0      # degrees
      max_required: 360    # degrees
      increment_warning: 30   # degrees
      
    physical_limits:
      max_surge_rao: 5.0      # m/m
      max_heave_rao: 2.0      # m/m
      max_pitch_rao: 10.0     # deg/m
      
  interpolation:
    method: "cubic_spline"   # linear, cubic_spline, pchip
    extrapolation: "constant" # constant, linear, zero
    quality_threshold: 0.95   # R² for interpolation quality
    
  storage:
    format: "hdf5"           # hdf5, pickle, json
    compression: "gzip"      # gzip, lzf, szip
    metadata_required: ["vessel_name", "analysis_date", "source"]
    
  output:
    formats: ["hdf5", "csv", "excel", "matlab", "dataframe"]
    include_metadata: true
    include_validation_report: true
    dataframe_structure:
      amplitude_df: "Multi-level columns: (DOF, Heading)"
      phase_df: "Multi-level columns: (DOF, Heading)"
      index: "Frequency (rad/s) or Period (s)"
      phase_units: "degrees"
```

## User Experience Implementation

### User Interface Changes
- **RAO Import Wizard**: Step-by-step data import with format detection
- **Data Quality Dashboard**: Visual validation results and quality metrics
- **Interactive RAO Viewer**: 3D plots of RAO magnitude and phase vs frequency/heading
- **Data Management Interface**: RAO dataset library with search and filtering

### Interaction Design
- **Drag-and-Drop Import**: Simple file drop interface with automatic format detection
- **Quality Feedback**: Real-time validation with color-coded quality indicators
- **Interactive Plots**: Zoomable, rotatable RAO surface plots with tooltips
- **Export Options**: One-click export to multiple formats for downstream analysis

### Error Handling and User Feedback
```python
# User-Friendly Error Messages
class RAOImportError(Exception):
    """User-friendly RAO import error with suggested solutions."""
    
    def __init__(self, message: str, suggestions: List[str] = None):
        self.message = message
        self.suggestions = suggestions or []
        super().__init__(self.message)
    
    def user_message(self) -> str:
        """Format error message for user interface."""
        msg = f"RAO Import Error: {self.message}\n"
        if self.suggestions:
            msg += "\nSuggested solutions:\n"
            for i, suggestion in enumerate(self.suggestions, 1):
                msg += f"{i}. {suggestion}\n"
        return msg

# Example error handling
try:
    rao_data = processor.import_aqwa_lis_file("vessel_rao.lis")
except AQWAFileError as e:
    suggestions = [
        "Verify the file is a valid ANSYS AQWA .lis output file",
        "Check that displacement RAO analysis was performed",
        "Ensure file is not corrupted and completely written"
    ]
    raise RAOImportError(
        "Could not parse ANSYS AQWA file", 
        suggestions
    ) from e
```

## Testing and Validation

### Test Strategy
- **Unit Testing**: Individual component testing with mock data
- **Integration Testing**: End-to-end RAO processing workflows
- **Validation Testing**: Comparison with known benchmark datasets
- **Performance Testing**: Large dataset processing and memory usage
- **User Acceptance Testing**: Marine engineer workflow validation

### Comprehensive Test Suite
```python
class TestRAODataProcessing:
    """Comprehensive test suite for RAO data processing."""
    
    @pytest.fixture
    def sample_aqwa_file(self):
        """Create sample ANSYS AQWA .lis file for testing."""
        return create_sample_aqwa_file()
    
    @pytest.fixture
    def experimental_rao_data(self):
        """Create sample experimental RAO data."""
        return create_experimental_rao_dataset()
    
    def test_aqwa_lis_import_success(self, sample_aqwa_file):
        """Test successful AQWA .lis file import."""
        processor = RAODataProcessor()
        rao_data = processor.import_aqwa_lis_file(sample_aqwa_file)
        
        # Validate data structure
        assert isinstance(rao_data, RAOData)
        assert len(rao_data.frequencies) > 0
        assert len(rao_data.headings) > 0
        assert all(dof in rao_data.raos for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw'])
        
        # Validate data ranges
        assert 0.1 <= rao_data.frequencies.min() <= 0.3
        assert 1.5 <= rao_data.frequencies.max() <= 3.0
        assert rao_data.headings.min() == 0
        assert rao_data.headings.max() == 360
        
        # Test DataFrame output
        df_dict = processor.get_rao_dataframes(rao_data)
        assert 'amplitude' in df_dict
        assert 'phase' in df_dict
        
        # Validate DataFrame structure
        amp_df = df_dict['amplitude']
        phase_df = df_dict['phase']
        
        assert isinstance(amp_df.columns, pd.MultiIndex)
        assert amp_df.columns.names == ['DOF', 'Heading']
        assert phase_df.columns.names == ['DOF', 'Heading']
        assert amp_df.index.name == 'Frequency (rad/s)'
        
        # Validate all DOFs and headings present
        dofs_in_df = amp_df.columns.get_level_values('DOF').unique()
        assert all(dof in dofs_in_df for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw'])
    
    def test_experimental_data_import(self, experimental_rao_data):
        """Test experimental RAO data import from CSV."""
        processor = RAODataProcessor()
        format_config = {
            'frequency_column': 'freq_rad_s',
            'heading_column': 'heading_deg',
            'units': {'frequency': 'rad/s', 'heading': 'deg'}
        }
        
        rao_data = processor.import_experimental_data(
            experimental_rao_data, format_config
        )
        
        # Validate successful import and standardization
        assert isinstance(rao_data, RAOData)
        assert rao_data.units['frequency'] == 'rad/s'
        assert rao_data.units['heading'] == 'deg'
    
    def test_rao_interpolation_accuracy(self):
        """Test RAO interpolation accuracy and quality."""
        # Create known analytical RAO function
        analytical_rao = create_analytical_rao_function()
        
        # Subsample and interpolate
        subsampled = subsample_rao_data(analytical_rao, factor=3)
        interpolated = processor.interpolate_rao_data(
            subsampled, analytical_rao.frequencies, analytical_rao.headings
        )
        
        # Validate interpolation accuracy
        error = calculate_interpolation_error(analytical_rao, interpolated)
        assert error < 0.001  # <0.1% error for smooth functions
    
    def test_data_validation_quality_checks(self):
        """Test comprehensive data validation."""
        # Test with various data quality issues
        corrupted_data = create_corrupted_rao_data()
        
        validator = RAODataValidators()
        report = validator.validate_rao_data(corrupted_data)
        
        # Should detect quality issues
        assert not report.is_valid
        assert len(report.errors) > 0
        assert any("frequency range" in error.lower() for error in report.errors)
    
    def test_performance_large_dataset(self):
        """Test performance with large RAO datasets."""
        large_dataset = create_large_rao_dataset(
            n_frequencies=500, n_headings=73
        )
        
        start_time = time.time()
        processor = RAODataProcessor()
        processed_data = processor.process_rao_data(large_dataset)
        processing_time = time.time() - start_time
        
        # Should process within time limit
        assert processing_time < 30.0  # seconds
        assert processed_data.is_valid()

    def test_benchmark_validation(self):
        """Validate against known benchmark RAO datasets."""
        benchmark_cases = load_benchmark_rao_cases()
        
        for case in benchmark_cases:
            processor = RAODataProcessor()
            rao_data = processor.import_aqwa_lis_file(case.aqwa_file)
            
            # Compare with reference results
            errors = calculate_benchmark_errors(rao_data, case.reference)
            assert all(error < 0.05 for error in errors)  # <5% error
```

### Validation Datasets
- **FPSO Reference**: Standard FPSO RAO data from industry benchmarks
- **Semi-submersible**: Typical semi-submersible platform RAO dataset
- **Ship Hull Forms**: Various ship types (tanker, container, bulk carrier)
- **Experimental Data**: Model test results for validation
- **ANSYS AQWA Cases**: Verified AQWA analysis results

## Implementation Timeline

### Development Sessions
**Week 1** (5 days)
- **Day 1-2**: ANSYS AQWA .lis file parser implementation
- **Day 3**: Experimental data import (CSV/Excel) functionality
- **Day 4**: Data validation and quality checking framework
- **Day 5**: RAO interpolation and grid processing

**Integration and Testing** (Ongoing)
- Unit test development parallel to implementation
- Integration testing with sample datasets
- Performance optimization and memory efficiency
- User interface integration and error handling

### Key Milestones
- **Day 2**: AQWA parser functional with basic RAO extraction
- **Day 4**: Multi-format import operational with validation
- **Day 5**: Complete RAO processing pipeline functional
- **Week 2**: User acceptance testing with marine engineers
- **Week 3**: Integration with 6-DOF motion analysis module

## Success Measurement

### Technical Success Metrics
- **Data Import Success Rate**: >99% for standard AQWA, OrcaFlex and experimental formats
- **Processing Performance**: <30 seconds for typical RAO datasets (200 freq × 37 headings)
- **Interpolation Accuracy**: <0.1% error for smooth RAO functions
- **Memory Efficiency**: <500MB memory usage for largest expected datasets
- **DataFrame Generation**: <5 seconds to generate amplitude and phase DataFrames

### User Success Metrics
- **User Adoption**: 100% of marine engineers successfully import RAO data
- **Workflow Efficiency**: 70% reduction in RAO data preparation time
- **Error Reduction**: 90% reduction in data import errors vs manual methods
- **User Satisfaction**: >4.5/5 rating for RAO data processing workflow

### Business Impact
- **Project Acceleration**: Enable 6-DOF motion analysis for $100M+ marine projects
- **Quality Improvement**: Eliminate manual data transcription errors
- **Capability Enhancement**: Support industry-standard RAO data workflows
- **Competitive Advantage**: Seamless integration with major marine analysis tools

## Future Considerations

### Enhancement Opportunities
- **Real-time RAO Processing**: Live data import from ANSYS AQWA simulations
- **Machine Learning Validation**: AI-powered RAO data quality assessment
- **Cloud Integration**: Remote RAO data processing and storage
- **Advanced Visualization**: VR/AR visualization of RAO data and vessel motions

### Integration Expansion
- **CAD Integration**: Direct RAO import from CAD-integrated hydrodynamic analysis
- **Database Integration**: RAO data library with vessel family databases
- **API Development**: RESTful API for external tool integration
- **Standards Compliance**: Enhanced support for emerging industry data standards

### Performance Optimization
- **Parallel Processing**: Multi-threaded RAO processing for large datasets
- **Compressed Storage**: Advanced compression for RAO data archives
- **Streaming Processing**: Memory-efficient processing of very large datasets
- **GPU Acceleration**: GPU-accelerated interpolation for real-time applications

## Lessons Learned Integration

### Best Practices from Implementation
- **Error Handling**: Provide specific, actionable error messages for data format issues
- **Validation Feedback**: Visual quality indicators help users understand data reliability
- **Flexible Configuration**: Support for various data formats increases adoption
- **Performance Monitoring**: Real-time processing feedback improves user experience

### Marine Engineering Domain Insights
- **Industry Standards**: Different organizations use varying RAO data formats
- **Quality Expectations**: Marine engineers require high confidence in data accuracy
- **Workflow Integration**: RAO processing must integrate seamlessly with analysis workflows
- **Documentation Needs**: Comprehensive validation reports essential for project documentation