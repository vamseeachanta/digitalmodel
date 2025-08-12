# OrcaFlex Dashboard Enhanced Specification

> Module: visualization/orcaflex-dashboard  
> Version: 2.0.0  
> Status: Active Development  
> Last Updated: 2025-08-11  
> Original: spec-original.md  

## Overview

This enhanced specification builds upon the original OrcaFlex Results Visualization Dashboard to include comprehensive support for multiple vessel types, Excel-driven configuration, and real-time CSV data browsing capabilities.

## Enhanced Features (Beyond Original Spec)

### 1. Multi-Vessel Type Support
Building on the original component classification (fst1, fst2, strut, jacket, lngc), the enhanced system supports:

- **FST (Floating Storage Tank)** configurations with dual tank support
- **LNGC (LNG Carrier)** with multiple vessel capacities (125,000 m³ / 180,000 m³)
- **Custom** vessel types defined in Excel collation files
- Dynamic UI adaptation based on vessel selection

### 2. Excel-Driven Configuration System
Extends the original data processing engine with:

- **Master Configuration Files**: `wlng_dm_fsts*.xlsx` in parent directory
- **Worksheet Integration**: 
  - `inputs` tab: Parameter definitions and UI options
  - `r_inputs` tab: File paths and naming patterns
- **Dynamic UI Generation**: Build filter options from Excel parameters
- **Pattern Learning**: Extract and apply naming conventions from Excel

### 3. Enhanced File Discovery
Improves upon the original file organization pattern:

```
D:\1522\ctr7\orcaflex\rev_a08\          # Parent directory with .dat files
├── wlng_dm_fsts*.xlsx                  # Excel collation files
└── output\csv\                         # Original CSV output directory
    ├── 02c_005yr\                      # Existing structure maintained
    ├── 03c_100yr\
    └── [dynamic folders from Excel]
```

### 4. Real-Time Data Browsing
Enhances the original visualization components:

- **Live File System Access**: No simulation or hardcoded data
- **Dynamic Filter Population**: All options from actual files
- **Full Duration Support**: Complete 3600s datasets (vs. samples)
- **No Data Extrapolation**: Display only actual CSV contents

## Excel Integration Architecture

### Excel Collation Reader
```python
class ExcelCollationReader:
    """Enhanced data processor for Excel-driven configuration"""
    
    def __init__(self, excel_path: Path):
        self.excel_path = excel_path
        self.config = self.parse_excel_config()
        
    def parse_excel_config(self) -> Dict:
        """Extract comprehensive configuration from Excel"""
        config = {
            'file_patterns': [],      # From r_inputs
            'parameters': {},          # From inputs
            'vessel_configs': {},      # Vessel-specific settings
            'ui_options': {},          # Dynamic UI elements
            'case_matrix': []          # Analysis case combinations
        }
        
        # Parse inputs worksheet for UI configuration
        inputs_df = pd.read_excel(self.excel_path, sheet_name='inputs')
        config['parameters'] = self.extract_parameters(inputs_df)
        config['ui_options'] = self.build_ui_options(inputs_df)
        
        # Parse r_inputs for file patterns
        r_inputs_df = pd.read_excel(self.excel_path, sheet_name='r_inputs')
        config['file_patterns'] = self.extract_file_patterns(r_inputs_df)
        
        return config
```

## Enhanced User Interface Components

### Vessel Type Selector (New)
**Radio Button Group**:
- FST Mode → Shows FST-specific controls
- LNGC Mode → Shows LNGC-specific controls  
- Custom Mode → Shows generic controls from Excel

### FST-Specific Controls (Enhanced)
When FST mode is selected:
- **FST1 Configuration**: 
  - 15% LNG (Empty) - E designation
  - 95% LNG (Flooded) - F designation
- **FST2 Configuration**: Independent selection
- **Mooring Status**: Intact / 1-line damaged / 2-lines damaged
- **Pattern**: `FST1_[E/F]_FST2_[E/F]_[tide]_[env]_[heading].csv`

### LNGC-Specific Controls (New)
When LNGC mode is selected:
- **Vessel Capacity**: 125,000 m³ / 180,000 m³ radio buttons
- **Berthing Side**: Port / Starboard toggle
- **Loading Condition**: 
  - Ballast (10% loaded)
  - Partial (50% loaded)
  - Laden (95% loaded)
  - Custom percentage input
- **Pattern**: `LNGC_[size]_[berthing]_[loading]_[tide]_[env]_[heading].csv`

### Environmental Controls (Enhanced)
Common to all vessel types:
- **Tide Level**: HWL / MWL / LWL (maintains original)
- **Environment Type**: Colinear / Non-colinear (new)
- **Wave Heading**: 0-315° in 45° increments (enhanced from 15°)
- **Current Heading**: Independent selection (new)
- **Wind Heading**: Independent selection (new)
- **Return Period**: 5yr / 10yr / 100yr / 1000yr (new)

## Implementation Requirements

### Excel Integration Requirements
1. **File Discovery**:
   - Scan parent directory for `wlng_dm_fsts*.xlsx` files
   - Cache Excel configuration per session
   - Watch for Excel file changes
   - Fallback to manual configuration if Excel unavailable

2. **Dynamic UI Generation**:
   - Parse Excel to determine available options
   - Build cascading filters from Excel parameters
   - Validate selections against actual CSV files
   - Show/hide controls based on data availability

3. **Pattern Matching**:
   - Use Excel-defined patterns for file discovery
   - Support regex and wildcard patterns
   - Handle multiple naming conventions per vessel
   - Provide intelligent fallback for partial matches

### Data Processing Enhancements
1. **No Simulation**:
   - Read actual CSV files only
   - No mathematical extrapolation
   - Display exact data points
   - Preserve original units and precision

2. **Full Duration**:
   - Load complete 3600s datasets
   - Handle large files efficiently
   - Progressive loading for performance
   - Memory management for multiple files

3. **Dynamic Population**:
   - Scan directories in real-time
   - Build options from actual files
   - Update on refresh button click
   - No hardcoded dropdown values

## Backend Architecture Enhancement

### Enhanced Python Flask Server
```python
class OrcaFlexDataServer:
    """Enhanced server with Excel integration"""
    
    def __init__(self):
        self.parent_dir = r"D:\1522\ctr7\orcaflex\rev_a08"
        self.csv_dir = os.path.join(self.parent_dir, "output", "csv")
        self.excel_config = None
        self.load_excel_configuration()
        
    def load_excel_configuration(self):
        """Load all wlng_dm_fsts*.xlsx files"""
        excel_files = glob.glob(os.path.join(self.parent_dir, "wlng_dm_fsts*.xlsx"))
        for excel_file in excel_files:
            reader = ExcelCollationReader(excel_file)
            self.excel_config = reader.config
            
    @app.route('/api/vessel_configs')
    def get_vessel_configs(self):
        """Return vessel-specific configurations from Excel"""
        return jsonify(self.excel_config['vessel_configs'])
        
    @app.route('/api/file_patterns')
    def get_file_patterns(self):
        """Return file naming patterns from Excel"""
        return jsonify(self.excel_config['file_patterns'])
```

## Migration Path from Original Spec

### Phase 1: Maintain Compatibility
- Keep existing polar plot functionality
- Preserve original file structure support
- Add Excel reading alongside existing processing

### Phase 2: Enhance with New Features
- Add vessel type selector UI
- Implement Excel-driven configuration
- Enable real-time file browsing
- Remove simulated data

### Phase 3: Full Integration
- Complete Excel-based UI generation
- Dynamic pattern matching
- Multi-vessel comparison views
- Advanced filtering with Excel parameters

## Testing Strategy

### Unit Tests (Enhanced)
- Excel parsing with various formats
- Pattern matching for all vessel types
- File discovery with complex structures
- UI control visibility logic

### Integration Tests (Enhanced)
- Excel to UI pipeline
- Multi-vessel switching
- Real-time data loading
- Refresh functionality

### Performance Tests (Enhanced)
- Large Excel file parsing
- Multiple vessel type switching
- 3600s dataset loading
- Concurrent user access with different configs

## Success Metrics

### Original Metrics (Maintained)
- Load time < 3 seconds
- Response time < 500ms
- Support 10+ concurrent users
- 99.9% uptime

### Enhanced Metrics (New)
- Excel parsing < 5 seconds
- Dynamic UI generation < 1 second
- Support 3 vessel types minimum
- Zero hardcoded values in production
- 100% real data (no simulation)

## Documentation Updates

### User Guide Additions
- Vessel type selection guide
- Excel template documentation
- Pattern matching explanation
- Refresh functionality usage

### Technical Documentation
- Excel integration architecture
- Pattern matching algorithms
- Dynamic UI generation logic
- Real-time data access methods

## Backwards Compatibility

The enhanced system maintains full compatibility with the original specification while adding:
- Optional Excel configuration (falls back to manual)
- Additional vessel types (FST remains primary)
- Enhanced environmental parameters (original subset works)
- Real data focus (can still process original structure)

## References

- Original Specification: spec-original.md
- Excel Collation Format: wlng_dm_fsts_format.xlsx
- OrcaFlex User Manual v11.x
- Marine Engineering Standards (API, DNV, ABS)