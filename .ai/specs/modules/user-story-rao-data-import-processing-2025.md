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
- [ ] AC-201.1.1: Read Displacement RAO data from 2 categories: OrcaFlex and AQWA
  - [ ] AC-201.1.1.1: OrcaFlex datafile support
    - [ ] Read YAML format: `tests\modules\rao_analysis\SS_Off_0_8P.yml`
    - [ ] Parse YAML structure: VesselTypes > Name: Vessel Type1 > Draughts > Name: Draught1 > DisplacementRAOs: RAOs
    - [ ] Extract headings from file content
  - [ ] AC-201.1.1.2: AQWA datafile support  
    - [ ] Read ASCII format: `tests\modules\rao_analysis\NO_DAMP_FST1_L015.LIS`
    - [ ] Search for "R.A.O.S-VARIATION WITH WAVE DIRECTION" pattern
    - [ ] If multiple sets exist, take the last set
    - [ ] Extract headings from file content
    - [ ] Use Fortran fixed character delimiter parsing similar to body_item_str format in aqwa_dat_files.py
    - [ ] For every block of repeating data the pattern is:
      - First line: period, freq, direction, and data
      - Following lines: just direction, and data (no period/freq repeats)
      - Last line: either empty or 1
    - [ ] If data is missing, use previous row data
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
- [ ] Manual verification of DataFrame outputs completed:
  - [ ] Visual inspection of amplitude values for physical reasonableness
  - [ ] Phase continuity check across frequencies
  - [ ] Spot check against source file values
  - [ ] Symmetry verification for appropriate headings

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
Core implementation available in:
- **RAO Data Processor**: `src/digitalmodel/modules/marine_analysis/rao_processor.py`
- **AQWA File Parser**: `src/digitalmodel/modules/marine_analysis/aqwa_reader.py` and `aqwa_enhanced_parser.py`
- **Data Validators**: `src/digitalmodel/modules/marine_analysis/rao_validators.py`
- **Interpolation**: `src/digitalmodel/modules/marine_analysis/rao_interpolator.py`

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
- **Manual Verification Interface**: 
  - Side-by-side comparison of source data and imported values
  - Visual highlighting of suspicious values
  - Checkbox system for verification steps
  - Engineer sign-off capability

### Interaction Design
- **Drag-and-Drop Import**: Simple file drop interface with automatic format detection
- **Quality Feedback**: Real-time validation with color-coded quality indicators
- **Interactive Plots**: Zoomable, rotatable RAO surface plots with tooltips
- **Export Options**: One-click export to multiple formats for downstream analysis

### Error Handling and User Feedback
User-friendly error handling implementation available in `src/digitalmodel/modules/marine_analysis/rao_processor.py` (RAOImportError class).

## Testing and Validation

### Test Strategy
- **Unit Testing**: Individual component testing with mock data
- **Integration Testing**: End-to-end RAO processing workflows
- **Validation Testing**: Comparison with known benchmark datasets
- **Performance Testing**: Large dataset processing and memory usage
- **User Acceptance Testing**: Marine engineer workflow validation

### Comprehensive Test Suite
Complete test implementations available in:
- **RAO Processor Tests**: `tests/modules/marine_analysis/test_rao_processor.py`
- **Data Interpretation Tests**: `tests/modules/marine_analysis/test_data_interpretation.py`
- **Additional Test Files**: See `tests/modules/marine_analysis/` directory for specific test cases

### Validation Datasets
- **FPSO Reference**: Standard FPSO RAO data from industry benchmarks
- **Semi-submersible**: Typical semi-submersible platform RAO dataset
- **Ship Hull Forms**: Various ship types (tanker, container, bulk carrier)
- **Experimental Data**: Model test results for validation
- **ANSYS AQWA Cases**: Verified AQWA analysis results
- **Manual Verification Set**: Curated dataset with known values for spot checking

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
- **Engineering Confidence**: Manual verification ensures data integrity for critical analyses

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

## AI Implementation Journey

### Implementation Summary
RAO data import and processing feature implemented with file reorganization and enhanced AQWA parser supporting abbreviated data interpretation.

### Key Implementation Details

**File Organization**: Moved scattered RAO files to proper structure:
- `src/digitalmodel/base_configs/modules/marine_analysis/` (config)
- `src/digitalmodel/modules/marine_analysis/` (code)  
- `tests/modules/marine_analysis/` (tests)

**Enhanced AQWA Parser**: Created `AQWAEnhancedParser` class to handle abbreviated AQWA format. See data transformation examples in `tests/modules/marine_analysis/test_data_interpretation.py`.

**Integration**: Added optional enhanced parsing to existing API with backward compatibility via `use_enhanced_parser` parameter.

### Lessons Learned

**For Users**:
- AQWA files use FORTRAN fixed-width format requiring careful column position handling
- Marine engineering data often uses abbreviated formats requiring domain knowledge
- File reorganization benefits from systematic discovery and batch operations

**For AI Assistants**:
- Build on existing working parsers rather than replacing them
- Use optional feature flags for safe backward-compatible enhancements  
- Create realistic mock data for testing complex parsing logic
- Systematic todo list management ensures complete requirement coverage

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