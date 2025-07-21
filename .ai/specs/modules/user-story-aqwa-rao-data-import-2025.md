# User Story Specification: AQWA RAO Data Import and Processing

**Story ID**: US-2025-201-A  
**Feature**: `.ai/specs/modules/feature-6dof-motion-analysis-2025.md`  
**Epic**: `.ai/specs/modules/epic-marine-analysis-ship-design-dynamics-2025.md`  
**Implementation Date**: TBD  
**Effort**: 3 days (Estimated)  
**Status**: Draft

## User Story Definition

### Story Statement
**As a** Marine Engineer working with ANSYS AQWA hydrodynamic analysis  
**I want** to import and process displacement RAO (Response Amplitude Operator) data from AQWA .lis files  
**So that** I can perform accurate 6-DOF motion analysis using AQWA-generated hydrodynamic data

### User Context
- **Primary Persona**: Marine Engineer (Senior level with AQWA experience)
- **Secondary Personas**: Naval Architect using AQWA, Hydrodynamics Analyst
- **User Journey**: AQWA analysis results → RAO data import → motion analysis workflow
- **Frequency of Use**: Daily for AQWA-based marine design projects

### Business Value
- **User Value**: Direct AQWA integration eliminates manual data extraction and reduces analysis time by 80%
- **Business Value**: Enables seamless AQWA workflow for marine vessel design worth $50M+ projects
- **Priority Rationale**: AQWA is primary hydrodynamic analysis tool in marine industry
- **Success Metrics**: Import 99% of AQWA RAO data formats with <1% accuracy loss

## Acceptance Criteria

### Primary Acceptance Criteria
- [ ] AC-201-A.1.1: Read Displacement RAO data from ANSYS AQWA .lis files
  - [ ] AC-201-A.1.1.1: AQWA .lis file support  
    - [ ] Read ASCII format: `tests\modules\rao_analysis\NO_DAMP_FST1_L015.LIS`
    - [ ] Search for "R.A.O.S-VARIATION WITH WAVE DIRECTION" pattern
      - [ ] Each pattern will contain multiple sets of frequency blocks
      - [ ] Each frequency block should be parsed as below:
        - First line: period, freq, direction, and data
        - Following lines: just direction, and data (no period/freq repeats)
        - Last line: either empty or 1
        - Suggested step 1: Example block as csv: tests/modules/rao_analysis/NO_DAMP_FST1_L015_frequency1_block_step1.csv
        - Suggested step 2: Example block as csv: tests/modules/rao_analysis/NO_DAMP_FST1_L015_frequency1_block_step2.csv
    - [ ] If multiple data sets exist (due to analysis file inherent iterations), take the last set
    - [ ] Extract headings from file content
    - [ ] Use Fortran fixed character delimiter parsing similar to body_item_str format in aqwa_dat_files.py
    - [ ] If data is missing, use previous row data
- [ ] AC-201-A.1.2: Validate AQWA RAO data quality including frequency range, heading coverage, and physical reasonableness
- [ ] AC-201-A.1.3: Handle missing data points and provide quality warnings for incomplete AQWA datasets
- [ ] AC-201-A.1.4: Return AQWA RAO data as pandas DataFrames containing:
  - [ ] All headings as columns
  - [ ] 6 DOF (surge, sway, heave, roll, pitch, yaw) amplitude data
  - [ ] 6 DOF phase data (in degrees)
  - [ ] Frequency/period as index

### Technical Acceptance Criteria
- [ ] TAC-201-A.1.1: Support ANSYS AQWA versions 18.0+ with backward compatibility to version 15.0
- [ ] TAC-201-A.1.2: Process AQWA RAO datasets up to 1000 frequency points × 37 headings × 6 DOF within 15 seconds
- [ ] TAC-201-A.1.3: Maintain AQWA data precision with exact value preservation from source file
- [ ] TAC-201-A.1.4: Provide comprehensive error handling with specific feedback for AQWA format issues

### Definition of Done
- [ ] AQWA RAO import functionality implemented
- [ ] Unit tests covering AQWA .lis format parsing and edge cases
- [ ] Integration tests with sample AQWA datasets
- [ ] Performance tests validating AQWA processing time requirements
- [ ] Documentation complete with AQWA data format specifications
- [ ] Validation against known benchmark AQWA datasets
- [ ] Manual verification of AQWA DataFrame outputs completed:
  - [ ] Visual inspection of amplitude values for physical reasonableness
  - [ ] Phase continuity check across frequencies
  - [ ] Spot check against AQWA source file values
  - [ ] Symmetry verification for appropriate headings

## GitHub Integration

### Related Issues and PRs
- **Primary Issue**: AQWA RAO Import Implementation
- **Related Issues**: AQWA file format enhancement, FORTRAN parser optimization
- **Dependencies**: Marine analysis base framework

### Code Changes Summary
```
Files Added: 3
Files Modified: 2
Files Deleted: 0
Lines Added: ~800
Test Coverage: ~300 lines
```

### Key File Changes
- **Enhanced**: `src/digitalmodel/modules/marine_analysis/aqwa_enhanced_parser.py` - AQWA-specific parsing
- **Enhanced**: `src/digitalmodel/modules/marine_analysis/aqwa_reader.py` - AQWA file reader
- **Added**: `tests/modules/marine_analysis/test_aqwa_rao.py` - AQWA-specific test suite
- **Modified**: `src/digitalmodel/modules/marine_analysis/rao_processor.py` - AQWA integration
- **Modified**: `src/digitalmodel/modules/marine_analysis/__init__.py` - Module exports

## Technical Implementation

### Implementation Approach
Develop specialized AQWA RAO data processing system:

1. **AQWA Format Parser**: Handle FORTRAN fixed-width format and abbreviations
2. **Data Interpretation**: Convert AQWA engineering units and coordinate systems
3. **Quality Validation**: AQWA-specific data quality checks
4. **Integration Interface**: Clean API for AQWA workflow integration

### Key Code Implementation
Core AQWA implementation available in:
- **AQWA Enhanced Parser**: `src/digitalmodel/modules/marine_analysis/aqwa_enhanced_parser.py`
- **AQWA Reader**: `src/digitalmodel/modules/marine_analysis/aqwa_reader.py`
- **AQWA Tests**: `tests/modules/marine_analysis/test_aqwa_rao.py`

### Configuration Schema
```yaml
# AQWA RAO Data Processing Configuration
aqwa_rao_processing:
  import_settings:
    file_type: "lis"  # lis, out
    version_compatibility: ["15.0", "18.0", "19.0", "20.0", "21.0"]
    encoding: "utf-8"
    fortran_format: true
    
  parsing:
    search_pattern: "R.A.O.S-VARIATION WITH WAVE DIRECTION"
    block_delimiter: "empty_line_or_1"
    fixed_width_columns: true
    abbreviation_handling: true
    
  validation:
    frequency_range:
      min_required: 0.05   # rad/s (typical AQWA range)
      max_required: 3.0    # rad/s
      
    aqwa_specific:
      coordinate_system: "aqwa_standard"  # body-fixed coordinate system
      phase_convention: "aqwa_degrees"    # AQWA phase convention
      
  output:
    preserve_aqwa_precision: true
    include_aqwa_metadata: true
    period_source: "original"  # Use exact period from AQWA, not calculated
```

## User Experience Implementation

### AQWA-Specific Interface
- **AQWA File Browser**: Specialized .lis file selection with preview
- **AQWA Data Validator**: Real-time validation with AQWA format feedback
- **AQWA Coordinate Display**: Visual coordinate system orientation for verification

### AQWA Integration Features
- **Version Detection**: Automatic AQWA version identification
- **Format Validation**: AQWA-specific format compliance checking
- **Engineering Units**: Proper handling of AQWA unit conventions

## Testing and Validation

### AQWA Test Strategy
- **AQWA Version Testing**: Test across multiple AQWA versions (15.0-21.0)
- **Format Variation Testing**: Handle various AQWA output format variations
- **Large Dataset Testing**: Performance with typical AQWA analysis sizes

### AQWA Validation Datasets
- **AQWA Benchmark**: Standard AQWA verification cases
- **Industry Cases**: Real AQWA analysis results from marine projects
- **Version Compatibility**: Sample files from different AQWA versions

## Implementation Timeline

### Development Sessions
**Days 1-2**: AQWA Parser Enhancement
- Enhanced AQWA .lis file parser implementation
- FORTRAN format handling and abbreviation interpretation
- Integration with existing RAO processor framework

**Day 3**: Testing and Validation
- Comprehensive AQWA test suite development
- Performance optimization for AQWA data processing
- Documentation and user guide completion

## Success Measurement

### AQWA-Specific Success Metrics
- **AQWA Import Success Rate**: >99.5% for standard AQWA .lis files
- **AQWA Processing Performance**: <15 seconds for typical AQWA datasets
- **AQWA Data Fidelity**: Exact preservation of AQWA source values
- **Version Compatibility**: Support for AQWA versions 15.0-21.0

### User Success Metrics
- **AQWA User Adoption**: 100% of AQWA users successfully import RAO data
- **AQWA Workflow Efficiency**: 80% reduction in AQWA data preparation time
- **AQWA Integration**: Seamless workflow from AQWA analysis to motion analysis

## Future AQWA Considerations

### AQWA Enhancement Opportunities
- **Real-time AQWA Integration**: Direct API connection to running AQWA simulations
- **AQWA Batch Processing**: Automated processing of multiple AQWA analysis files
- **AQWA Metadata Extraction**: Enhanced extraction of AQWA model information
- **AQWA Result Comparison**: Side-by-side comparison of multiple AQWA runs