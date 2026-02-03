# Signal Analysis Module Migration Plan

## Overview
Systematic migration from scattered signal processing implementations to the consolidated signal_analysis module.

## Migration Status

### Phase 1: Core Module Implementation ✅ COMPLETE
- Created unified signal_analysis module
- Implemented ASTM E1049-85 rainflow counting
- Added comprehensive spectral analysis
- Integrated fatigue damage calculations
- Built backward compatibility adapters

### Phase 2: Integration Architecture ✅ COMPLETE  
- Designed OrcaFlexTimeTraceProcessor
- Created pipeline architecture
- Implemented batch processing capabilities
- Added parallel processing support

### Phase 3: Code Migration 🚧 IN PROGRESS

#### Files Requiring Migration (30 total)

##### High Priority (Core functionality)
- [ ] `src/digitalmodel/modules/orcaflex/opp_time_series.py` - Main OrcaFlex postprocessing
- [ ] `src/digitalmodel/modules/time_series/time_series_components.py` - Legacy implementation
- [ ] `src/digitalmodel/common/fatigue_analysis.py` - Old fatigue module
- [ ] `src/digitalmodel/modules/orcaflex/orcaflex_fatigue_analysis.py` - OrcaFlex-specific fatigue

##### Medium Priority (Analysis modules)
- [ ] `src/digitalmodel/modules/viv_analysis/viv_analysis_legacy.py` - VIV analysis
- [ ] `src/digitalmodel/common/ship_fatigue_analysis.py` - Ship-specific fatigue
- [ ] `src/digitalmodel/common/viv_fatigue_analysis_components.py` - VIV fatigue
- [ ] `src/digitalmodel/time_series.py` - General time series

##### Low Priority (Tests and utilities)
- [ ] Test files in `tests/in_progress/`
- [ ] Test files in `tests/no_license/`
- [ ] Example files (except new ones)

## Migration Strategy

### Step 1: Add Deprecation Warnings (Week 1)
Add warnings to all old modules to alert users:

```python
import warnings
warnings.warn(
    "This module is deprecated. Use digitalmodel.signal_processing.signal_analysis instead",
    DeprecationWarning,
    stacklevel=2
)
```

### Step 2: Update High-Priority Files (Week 2)
Focus on core functionality files that are actively used:

1. **OrcaFlex Integration**
   - Update `opp_time_series.py` to use `OrcaFlexTimeTraceProcessor`
   - Replace direct TimeSeriesComponents calls
   - Maintain configuration compatibility

2. **Legacy Module Updates**
   - Keep old modules but redirect to new implementation
   - Use adapters for backward compatibility
   - Update documentation

### Step 3: Migrate Analysis Modules (Week 3)
Update specialized analysis modules:

1. **VIV Analysis**
   - Integrate spectral analysis from new module
   - Use fatigue damage calculator
   - Maintain VIV-specific calculations

2. **Ship Fatigue**
   - Update to use new S-N curves
   - Integrate rainflow counting
   - Preserve ship-specific methods

### Step 4: Update Tests (Week 4)
- Update test imports
- Verify backward compatibility
- Add new integration tests
- Document behavior changes

## Migration Patterns

### Pattern 1: Direct Migration
```python
# Old
from digitalmodel.signal_processing.time_series.time_series_components import TimeSeriesComponents
tsc = TimeSeriesComponents(cfg)
cycles = tsc.get_rainflow_count_from_time_series(signal)

# New
from digitalmodel.signal_processing.signal_analysis import RainflowCounter
counter = RainflowCounter()
cycles = counter.count_cycles(signal)
```

### Pattern 2: Using Adapter
```python
# Temporary compatibility
from digitalmodel.signal_processing.signal_analysis.adapters import TimeSeriesComponentsAdapter
adapter = TimeSeriesComponentsAdapter(cfg)
cycles = adapter.get_rainflow_count_from_time_series(signal)  # Works with deprecation warning
```

### Pattern 3: OrcaFlex Integration
```python
# Old
tsc = TimeSeriesComponents(cfg)
for trace in time_traces:
    cycles = tsc.get_rainflow_count_from_time_series(trace)
    
# New
from digitalmodel.orcaflex.time_trace_processor import OrcaFlexTimeTraceProcessor
processor = OrcaFlexTimeTraceProcessor(config)
results = processor.process()  # Handles all traces automatically
```

## Benefits After Migration

1. **Performance**: 5-10x faster processing with parallel support
2. **Maintainability**: Single source of truth for signal processing
3. **Standards Compliance**: ASTM, DNV, API, BS standards
4. **Extensibility**: Easy to add new analysis types
5. **Documentation**: Comprehensive API documentation
6. **Testing**: Full test coverage

## Validation Checklist

- [ ] All tests passing after migration
- [ ] Performance benchmarks improved
- [ ] Documentation updated
- [ ] Examples working
- [ ] Backward compatibility verified
- [ ] Deprecation warnings in place

## Rollback Plan

If issues arise:
1. Adapters provide immediate compatibility
2. Original files backed up as `.bak`
3. Git history preserves all changes
4. Can run parallel implementations during transition

## Success Metrics

- **Code Reduction**: Expect 30-40% reduction in duplicate code
- **Performance**: 5x improvement in batch processing
- **Test Coverage**: Maintain >90% coverage
- **Zero Breaking Changes**: All existing workflows continue to work

## Timeline

- **Week 1**: Deprecation warnings and documentation
- **Week 2**: Core module migration
- **Week 3**: Analysis module updates  
- **Week 4**: Test updates and validation
- **Week 5**: Performance testing and optimization
- **Week 6**: Final cleanup and documentation

## Next Steps

1. Run migration script with `--apply` flag on development branch
2. Test each migrated module thoroughly
3. Update configuration files
4. Document any behavior changes
5. Communicate changes to team