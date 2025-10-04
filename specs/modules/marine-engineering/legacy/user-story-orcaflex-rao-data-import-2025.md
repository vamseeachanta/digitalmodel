# User Story Specification: OrcaFlex RAO Data Import and Processing

**Story ID**: US-2025-201-B  
**Feature**: `.ai/specs/modules/feature-6dof-motion-analysis-2025.md`  
**Epic**: `.ai/specs/modules/epic-marine-analysis-ship-design-dynamics-2025.md`  
**Implementation Date**: TBD  
**Effort**: 2 days (Estimated)  
**Status**: Draft

## User Story Definition

### Story Statement
**As a** Marine Engineer working with Orcina OrcaFlex dynamic analysis  
**I want** to import and process displacement RAO (Response Amplitude Operator) data from OrcaFlex YAML files  
**So that** I can perform accurate 6-DOF motion analysis using OrcaFlex-generated hydrodynamic data

### User Context
- **Primary Persona**: Marine Engineer (Senior level with OrcaFlex experience)
- **Secondary Personas**: Offshore Engineer using OrcaFlex, Floating Systems Analyst
- **User Journey**: OrcaFlex analysis results → RAO data import → motion analysis workflow
- **Frequency of Use**: Weekly for OrcaFlex-based offshore design projects

### Business Value
- **User Value**: Direct OrcaFlex integration eliminates manual data extraction and reduces analysis time by 75%
- **Business Value**: Enables seamless OrcaFlex workflow for offshore systems design worth $50M+ projects
- **Priority Rationale**: OrcaFlex is leading offshore dynamic analysis tool
- **Success Metrics**: Import 99% of OrcaFlex RAO data formats with <1% accuracy loss

## Acceptance Criteria

### Primary Acceptance Criteria
- [ ] AC-201-B.1.1: Read Displacement RAO data from OrcaFlex YAML files
  - [ ] AC-201-B.1.1.1: OrcaFlex YAML file support
    - [ ] Read YAML format: `tests\modules\rao_analysis\SS_Off_0_8P.yml`
    - [ ] Parse YAML structure: VesselTypes > Name: Vessel Type1 > Draughts > Name: Draught1 > DisplacementRAOs: RAOs
    - [ ] Extract headings from file content
    - [ ] Handle nested YAML structure for vessel types and draughts
    - [ ] Support multiple vessel configurations in single file
- [ ] AC-201-B.1.2: Validate OrcaFlex RAO data quality including frequency range, heading coverage, and physical reasonableness
- [ ] AC-201-B.1.3: Handle missing data points and provide quality warnings for incomplete OrcaFlex datasets
- [ ] AC-201-B.1.4: Return OrcaFlex RAO data as pandas DataFrames containing:
  - [ ] All headings as columns
  - [ ] 6 DOF (surge, sway, heave, roll, pitch, yaw) amplitude data
  - [ ] 6 DOF phase data (in degrees)
  - [ ] Frequency/period as index
  - [ ] Vessel name and draught information

### Technical Acceptance Criteria
- [ ] TAC-201-B.1.1: Support OrcaFlex versions 10.0+ with forward compatibility
- [ ] TAC-201-B.1.2: Process OrcaFlex RAO datasets up to 1000 frequency points × 37 headings × 6 DOF within 10 seconds
- [ ] TAC-201-B.1.3: Maintain OrcaFlex data precision with exact value preservation from YAML
- [ ] TAC-201-B.1.4: Provide comprehensive error handling with specific feedback for YAML format issues

### Definition of Done
- [ ] OrcaFlex RAO import functionality implemented
- [ ] Unit tests covering OrcaFlex YAML format parsing and edge cases
- [ ] Integration tests with sample OrcaFlex datasets
- [ ] Performance tests validating OrcaFlex processing time requirements
- [ ] Documentation complete with OrcaFlex YAML format specifications
- [ ] Validation against known benchmark OrcaFlex datasets
- [ ] Manual verification of OrcaFlex DataFrame outputs completed:
  - [ ] Visual inspection of amplitude values for physical reasonableness
  - [ ] Phase continuity check across frequencies
  - [ ] Spot check against OrcaFlex source file values
  - [ ] Vessel configuration validation

## GitHub Integration

### Related Issues and PRs
- **Primary Issue**: OrcaFlex RAO Import Implementation
- **Related Issues**: YAML parsing optimization, vessel configuration handling
- **Dependencies**: Marine analysis base framework

### Code Changes Summary
```
Files Added: 2
Files Modified: 2
Files Deleted: 0
Lines Added: ~500
Test Coverage: ~200 lines
```

### Key File Changes
- **Enhanced**: `src/digitalmodel/modules/marine_analysis/orcaflex_reader.py` - OrcaFlex YAML reader
- **Added**: `tests/modules/marine_analysis/test_orcaflex_rao.py` - OrcaFlex-specific test suite
- **Modified**: `src/digitalmodel/modules/marine_analysis/rao_processor.py` - OrcaFlex integration
- **Modified**: `src/digitalmodel/modules/marine_analysis/__init__.py` - Module exports

## Technical Implementation

### Implementation Approach
Develop specialized OrcaFlex RAO data processing system:

1. **YAML Parser**: Handle nested OrcaFlex YAML structure and vessel configurations
2. **Data Extraction**: Navigate complex vessel/draught hierarchy for RAO data
3. **Quality Validation**: OrcaFlex-specific data quality checks
4. **Integration Interface**: Clean API for OrcaFlex workflow integration

### Key Code Implementation
Core OrcaFlex implementation available in:
- **OrcaFlex Reader**: `src/digitalmodel/modules/marine_analysis/orcaflex_reader.py`
- **OrcaFlex Tests**: `tests/modules/marine_analysis/test_orcaflex_rao.py`

### Configuration Schema
```yaml
# OrcaFlex RAO Data Processing Configuration
orcaflex_rao_processing:
  import_settings:
    file_type: "yml"  # yml, yaml
    version_compatibility: ["10.0", "11.0", "12.0", "13.0"]
    encoding: "utf-8"
    
  parsing:
    yaml_structure:
      vessel_types: "VesselTypes"
      draughts: "Draughts"
      displacement_raos: "DisplacementRAOs"
    vessel_selection: "first"  # first, all, specific_name
    draught_selection: "first"  # first, all, specific_name
    
  validation:
    frequency_range:
      min_required: 0.1    # rad/s (typical OrcaFlex range)
      max_required: 2.5    # rad/s
      
    orcaflex_specific:
      coordinate_system: "orcaflex_standard"  # OrcaFlex coordinate conventions
      phase_convention: "orcaflex_degrees"    # OrcaFlex phase convention
      vessel_validation: true  # Validate vessel name extraction
      
  output:
    preserve_orcaflex_precision: true
    include_vessel_metadata: true
    include_draught_info: true
```

## User Experience Implementation

### OrcaFlex-Specific Interface
- **YAML File Browser**: Specialized .yml/.yaml file selection with structure preview
- **Vessel Selector**: Interface to choose specific vessel type and draught from file
- **OrcaFlex Data Validator**: Real-time validation with OrcaFlex format feedback

### OrcaFlex Integration Features
- **Vessel Configuration**: Clear display of available vessels and draughts
- **YAML Structure Navigation**: Visual tree view of OrcaFlex file structure
- **Multi-Configuration Support**: Handle multiple vessel configurations

## Testing and Validation

### OrcaFlex Test Strategy
- **YAML Structure Testing**: Test various OrcaFlex YAML file structures
- **Multi-Vessel Testing**: Handle files with multiple vessel configurations
- **Version Compatibility**: Test across different OrcaFlex output formats

### OrcaFlex Validation Datasets
- **OrcaFlex Samples**: Standard OrcaFlex verification cases
- **Offshore Cases**: Real OrcaFlex analysis results from offshore projects
- **Multi-Vessel Files**: Sample files with multiple vessel configurations

## Implementation Timeline

### Development Sessions
**Day 1**: OrcaFlex Parser Enhancement
- Enhanced OrcaFlex YAML parser implementation
- Vessel and draught hierarchy navigation
- Integration with existing RAO processor framework

**Day 2**: Testing and Optimization
- Comprehensive OrcaFlex test suite development
- Performance optimization for YAML processing
- Documentation and integration completion

## Success Measurement

### OrcaFlex-Specific Success Metrics
- **OrcaFlex Import Success Rate**: >99.5% for standard OrcaFlex YAML files
- **OrcaFlex Processing Performance**: <10 seconds for typical OrcaFlex datasets
- **OrcaFlex Data Fidelity**: Exact preservation of YAML source values
- **Vessel Extraction**: 100% accurate vessel name and configuration extraction

### User Success Metrics
- **OrcaFlex User Adoption**: 100% of OrcaFlex users successfully import RAO data
- **OrcaFlex Workflow Efficiency**: 75% reduction in OrcaFlex data preparation time
- **OrcaFlex Integration**: Seamless workflow from OrcaFlex analysis to motion analysis

## Future OrcaFlex Considerations

### OrcaFlex Enhancement Opportunities
- **OrcaFlex API Integration**: Direct connection to OrcaFlex software via API
- **Batch YAML Processing**: Automated processing of multiple OrcaFlex files
- **Advanced Vessel Filtering**: Smart vessel selection based on project requirements
- **OrcaFlex Result Comparison**: Side-by-side comparison of different OrcaFlex configurations