# OrcaFlex Dashboard Enhanced Implementation Tasks

> Created: 2025-08-11  
> Module: visualization/orcaflex-dashboard  
> Status: Active Implementation  
> Based on: spec-enhanced.md  
> Total Effort: 120 hours (15 weeks)  

## Priority 1: Excel Integration Tasks (24 hours)

### Task E1.1: Excel Collation Reader Implementation
**Effort**: 8 hours  
**Priority**: CRITICAL  
**Dependencies**: None  
**Status**: ✅ Completed

**Subtasks**:
- [x] Create ExcelCollationReader class
- [x] Implement worksheet parsing (inputs, r_inputs)
- [x] Extract file patterns from Excel
- [x] Build parameter dictionary from Excel data
- [x] Cache Excel configuration
- [x] Handle missing/corrupt Excel files gracefully

**Implementation**:
```python
# src/modules/orcaflex-browser/excel_reader.py
class ExcelCollationReader:
    def __init__(self, excel_path):
        self.excel_path = excel_path
        self.config = self.parse_excel_config()
    
    def parse_inputs_worksheet(self):
        # Extract UI parameters
        pass
    
    def parse_r_inputs_worksheet(self):
        # Extract file patterns
        pass
```

**Acceptance Criteria**:
- ✅ Successfully reads wlng_dm_fsts*.xlsx files
- ✅ Extracts all parameters from inputs worksheet
- ✅ Parses file paths from r_inputs worksheet
- ✅ Handles missing worksheets with fallback

### Task E1.2: Pattern Matching Engine
**Effort**: 8 hours  
**Priority**: CRITICAL  
**Dependencies**: E1.1  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Implement pattern matching for FST files
- [ ] Implement pattern matching for LNGC files
- [ ] Support custom patterns from Excel
- [ ] Create fallback for partial matches
- [ ] Build pattern validation system

**Acceptance Criteria**:
- ✅ Matches FST patterns: `FST1_[E/F]_FST2_[E/F]_*`
- ✅ Matches LNGC patterns: `LNGC_[size]_[berthing]_*`
- ✅ Handles variations in naming conventions
- ✅ Provides match confidence scores

### Task E1.3: Dynamic Configuration API
**Effort**: 8 hours  
**Priority**: HIGH  
**Dependencies**: E1.1, E1.2  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Create /api/excel_config endpoint
- [ ] Implement /api/vessel_types endpoint
- [ ] Add /api/file_patterns endpoint
- [ ] Build configuration caching
- [ ] Add configuration refresh capability

**Implementation**:
```python
@app.route('/api/excel_config')
def get_excel_config():
    """Return complete Excel configuration"""
    return jsonify(excel_reader.config)

@app.route('/api/vessel_types')
def get_vessel_types():
    """Return available vessel configurations"""
    return jsonify(excel_reader.get_vessel_types())
```

## Priority 2: Multi-Vessel UI Tasks (32 hours)

### Task E2.1: Vessel Type Selector Implementation
**Effort**: 8 hours  
**Priority**: CRITICAL  
**Dependencies**: E1.3  
**Status**: ✅ Completed

**Subtasks**:
- [x] Create vessel type radio button group
- [x] Implement FST/LNGC/Custom selection
- [x] Add dynamic UI switching logic
- [x] Hide/show controls based on vessel type
- [x] Store vessel selection in state

**Frontend Implementation**:
```javascript
// VesselTypeSelector.jsx
const VesselTypeSelector = () => {
    const [vesselType, setVesselType] = useState('FST');
    
    return (
        <div className="vessel-selector">
            <label>
                <input type="radio" value="FST" 
                       checked={vesselType === 'FST'}
                       onChange={(e) => setVesselType(e.target.value)} />
                FST (Floating Storage Tank)
            </label>
            <label>
                <input type="radio" value="LNGC" 
                       checked={vesselType === 'LNGC'}
                       onChange={(e) => setVesselType(e.target.value)} />
                LNGC (LNG Carrier)
            </label>
            <label>
                <input type="radio" value="Custom" 
                       checked={vesselType === 'Custom'}
                       onChange={(e) => setVesselType(e.target.value)} />
                Custom
            </label>
        </div>
    );
};
```

### Task E2.2: FST-Specific Controls
**Effort**: 8 hours  
**Priority**: HIGH  
**Dependencies**: E2.1  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Create FST1 configuration selector (15%/95% LNG)
- [ ] Create FST2 configuration selector (15%/95% LNG)
- [ ] Add mooring status dropdown (Intact/Damaged)
- [ ] Implement FST pattern validation
- [ ] Add FST-specific help tooltips

**Acceptance Criteria**:
- ✅ Independent FST1 and FST2 selection
- ✅ Shows only when FST vessel type selected
- ✅ Updates file patterns based on selection
- ✅ Validates against available CSV files

### Task E2.3: LNGC-Specific Controls
**Effort**: 8 hours  
**Priority**: HIGH  
**Dependencies**: E2.1  
**Status**: ✅ Completed

**Subtasks**:
- [x] Create vessel capacity selector (125,000/180,000 m³)
- [x] Add berthing side toggle (Port/Starboard)
- [x] Implement loading condition selector
- [x] Add custom loading percentage input
- [x] Create LNGC pattern validation

**Frontend Implementation**:
```javascript
// LNGCControls.jsx
const LNGCControls = ({ visible }) => {
    if (!visible) return null;
    
    return (
        <div className="lngc-controls">
            <div className="capacity-selector">
                <label>Vessel Capacity:</label>
                <input type="radio" name="capacity" value="125000" /> 125,000 m³
                <input type="radio" name="capacity" value="180000" /> 180,000 m³
            </div>
            <div className="berthing-selector">
                <label>Berthing:</label>
                <button className="toggle-port">Port</button>
                <button className="toggle-starboard">Starboard</button>
            </div>
            <div className="loading-selector">
                <label>Loading Condition:</label>
                <select>
                    <option value="ballast">Ballast (10%)</option>
                    <option value="partial">Partial (50%)</option>
                    <option value="laden">Laden (95%)</option>
                    <option value="custom">Custom</option>
                </select>
                <input type="number" min="0" max="100" placeholder="Custom %" />
            </div>
        </div>
    );
};
```

### Task E2.4: Environmental Controls Enhancement
**Effort**: 8 hours  
**Priority**: MEDIUM  
**Dependencies**: E2.1  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Add environment type selector (Colinear/Non-colinear)
- [ ] Separate wave/current/wind heading controls
- [ ] Add return period selector (5yr/10yr/100yr/1000yr)
- [ ] Implement cascading filter logic
- [ ] Add environmental preset configurations

## Priority 3: Real-Time Data Integration (24 hours)

### Task E3.1: File System Scanner Enhancement
**Effort**: 8 hours  
**Priority**: CRITICAL  
**Dependencies**: E1.2  
**Status**: 🟡 In Progress

**Subtasks**:
- [x] Scan parent directory for .dat files
- [ ] Match CSV files with parent .dat files
- [ ] Build file catalog with metadata
- [ ] Implement file change detection
- [ ] Add file validation checks

### Task E3.2: Dynamic Filter Population
**Effort**: 8 hours  
**Priority**: CRITICAL  
**Dependencies**: E3.1  
**Status**: 🟡 In Progress

**Subtasks**:
- [x] Populate dropdowns from actual files
- [x] Remove hardcoded values
- [ ] Update filters based on vessel type
- [ ] Implement cascading filter updates
- [ ] Add filter validation against files

### Task E3.3: Full Duration Data Loading
**Effort**: 8 hours  
**Priority**: HIGH  
**Dependencies**: E3.1  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Load complete 3600s datasets
- [ ] Implement progressive loading
- [ ] Add memory management for large files
- [ ] Create data pagination system
- [ ] Optimize rendering for full datasets

## Priority 4: User Experience Enhancements (16 hours)

### Task E4.1: Refresh Functionality
**Effort**: 4 hours  
**Priority**: HIGH  
**Dependencies**: E3.1  
**Status**: 🟢 Completed

**Subtasks**:
- [x] Add refresh button to UI
- [x] Clear cached data on refresh
- [x] Rescan file system
- [x] Update all dropdowns
- [x] Provide refresh feedback

### Task E4.2: Loading States and Feedback
**Effort**: 4 hours  
**Priority**: MEDIUM  
**Dependencies**: None  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Add loading spinners for data operations
- [ ] Implement progress bars for large files
- [ ] Create status messages for operations
- [ ] Add error notifications
- [ ] Implement success confirmations

### Task E4.3: Help and Documentation
**Effort**: 4 hours  
**Priority**: LOW  
**Dependencies**: All UI tasks  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Add contextual help tooltips
- [ ] Create vessel type selection guide
- [ ] Document naming conventions
- [ ] Add troubleshooting section
- [ ] Create quick start tutorial

### Task E4.4: Performance Monitoring
**Effort**: 4 hours  
**Priority**: MEDIUM  
**Dependencies**: E3.3  
**Status**: 🔴 Not Started

**Subtasks**:
- [ ] Add performance metrics collection
- [ ] Monitor file loading times
- [ ] Track memory usage
- [ ] Create performance dashboard
- [ ] Add performance alerts

## Priority 5: Testing and Validation (24 hours)

### Task E5.1: Excel Integration Testing
**Effort**: 6 hours  
**Priority**: HIGH  
**Dependencies**: E1.1, E1.2, E1.3  
**Status**: 🔴 Not Started

**Test Cases**:
- [ ] Test with valid wlng_dm_fsts*.xlsx files
- [ ] Test with missing worksheets
- [ ] Test with corrupt Excel files
- [ ] Test pattern matching accuracy
- [ ] Test configuration caching

### Task E5.2: Multi-Vessel UI Testing
**Effort**: 6 hours  
**Priority**: HIGH  
**Dependencies**: E2.1, E2.2, E2.3  
**Status**: 🔴 Not Started

**Test Cases**:
- [ ] Test vessel type switching
- [ ] Test control visibility logic
- [ ] Test FST configuration combinations
- [ ] Test LNGC parameter selections
- [ ] Test custom vessel mode

### Task E5.3: Data Loading Testing
**Effort**: 6 hours  
**Priority**: HIGH  
**Dependencies**: E3.1, E3.2, E3.3  
**Status**: 🔴 Not Started

**Test Cases**:
- [ ] Test with 3600s datasets
- [ ] Test with multiple CSV files
- [ ] Test memory management
- [ ] Test refresh functionality
- [ ] Test error handling

### Task E5.4: Integration Testing
**Effort**: 6 hours  
**Priority**: CRITICAL  
**Dependencies**: All tasks  
**Status**: 🔴 Not Started

**Test Scenarios**:
- [ ] End-to-end FST workflow
- [ ] End-to-end LNGC workflow
- [ ] Excel to UI pipeline
- [ ] File discovery to visualization
- [ ] Multi-user concurrent access

## Implementation Schedule

### Week 1-2: Excel Foundation
- E1.1: Excel Collation Reader
- E1.2: Pattern Matching Engine
- E1.3: Dynamic Configuration API

### Week 3-4: Multi-Vessel UI
- E2.1: Vessel Type Selector
- E2.2: FST-Specific Controls
- E2.3: LNGC-Specific Controls

### Week 5-6: Data Integration
- E3.1: File System Scanner (Complete)
- E3.2: Dynamic Filter Population (Complete)
- E3.3: Full Duration Data Loading

### Week 7: UX Polish
- E4.1: Refresh Functionality (Done)
- E4.2: Loading States
- E4.3: Help Documentation
- E4.4: Performance Monitoring

### Week 8: Testing & Deployment
- E5.1-E5.4: All testing tasks
- Bug fixes and optimization
- Production deployment

## Success Metrics

### Technical Metrics
- Excel parsing time < 5 seconds
- UI generation time < 1 second
- Support for 3+ vessel types
- Zero hardcoded dropdown values
- 100% real data display

### User Experience Metrics
- Vessel type switching < 500ms
- File discovery < 2 seconds
- Data loading < 5 seconds for 3600s
- Refresh operation < 3 seconds

### Quality Metrics
- Test coverage > 80%
- Zero critical bugs in production
- All vessel types fully functional
- Excel fallback working correctly

## Current Status Summary

### Completed ✅
- Basic file system access
- Dynamic filter population
- Refresh functionality
- Backend server structure

### In Progress 🟡
- File system scanner enhancement
- Dynamic filter population

### Not Started 🔴
- Excel integration (CRITICAL)
- Vessel type selector (CRITICAL)
- LNGC controls
- Full duration data loading
- All testing tasks

## Next Actions

1. **IMMEDIATE**: Implement Excel Collation Reader (E1.1)
2. **URGENT**: Create Vessel Type Selector (E2.1)
3. **HIGH**: Add LNGC-specific controls (E2.3)
4. **IMPORTANT**: Complete file system scanner (E3.1)
5. **REQUIRED**: Comprehensive testing (E5.1-E5.4)

---

*Enhanced tasks based on spec-enhanced.md requirements. Original tasks preserved in tasks.md.*