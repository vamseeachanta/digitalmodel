# Repository Specification Organization Learnings

> Document: Repository Learnings  
> Created: 2025-08-11  
> Purpose: Capture organizational insights and best practices  

## Key Learnings from Specification Organization

### 1. Module-Based Organization is Critical

**Learning**: Specifications should be organized by functional modules, not by implementation approach.

**What Happened**: 
- The OrcaFlex dashboard specification was initially placed in `specs/modules/agent-os/mixed-documentation-agent/`
- This was incorrect as it's a visualization feature, not an agent creation feature
- The specification belonged in `specs/modules/visualization/orcaflex-dashboard/`

**Best Practice**:
```
specs/modules/
├── visualization/           # All UI/dashboard specs
│   └── orcaflex-dashboard/ # Specific dashboard specs
├── agent-os/               # Agent creation specs
├── marine-engineering/     # Domain-specific engineering specs
└── infrastructure/         # System infrastructure specs
```

### 2. Avoid Specification Sprawl

**Learning**: Don't mix unrelated features in a single specification document.

**What Happened**:
- The mixed-documentation-agent spec contained both agent creation logic AND dashboard visualization
- This made the spec unnecessarily complex and hard to maintain
- Different teams couldn't work independently on their parts

**Best Practice**:
- One spec file = One cohesive feature set
- Use cross-references for related specifications
- Keep specifications focused and single-purpose

### 3. Excel-Driven Configuration Requires Dedicated Architecture

**Learning**: When external files (like Excel) drive system behavior, they need first-class architectural support.

**Discovery**:
- The `wlng_dm_fsts*.xlsx` files contain critical configuration data
- These files define UI options, file patterns, and naming conventions
- The system must gracefully handle missing or corrupt Excel files

**Best Practice**:
```python
# Always provide fallback mechanisms
if excel_exists:
    config = load_from_excel()
else:
    config = load_default_config()
    log.warning("Using default configuration")
```

### 4. Real Data vs Simulated Data

**Learning**: Be explicit about data sources in specifications.

**Issue Identified**:
- Initial implementations used simulated/generated data
- Users expected real CSV file data
- This created confusion and rework

**Best Practice**:
- Clearly specify: "Display only actual data from CSV files"
- Avoid phrases like "generate visualization" when you mean "display data"
- Include data source validation in requirements

### 5. Versioning Specifications

**Learning**: Keep both original and enhanced versions when making major changes.

**Approach Used**:
- `spec-original.md` - Preserved original specification
- `spec-enhanced.md` - Enhanced version with new requirements
- `spec.md` - Current active specification

**Benefits**:
- Maintains history of requirements evolution
- Allows rollback if needed
- Shows clear progression of features

### 6. Directory Structure Patterns

**Learning**: Establish and enforce consistent directory patterns early.

**Current Pattern**:
```
<group>/modules/<module>/
```

**Examples**:
- ✅ `specs/modules/visualization/orcaflex-dashboard/`
- ✅ `src/modules/orcaflex-browser/`
- ❌ `specs/orcaflex-dashboard/` (missing modules level)

### 7. Cross-Module Dependencies

**Learning**: Document and manage dependencies between modules explicitly.

**Discovered Dependencies**:
- OrcaFlex dashboard depends on:
  - File system access (infrastructure)
  - Excel parsing (data processing)
  - Marine engineering domain knowledge
  - Visualization libraries

**Best Practice**:
```yaml
# In each spec, clearly list dependencies
dependencies:
  - infrastructure/file-system
  - data-processing/excel-reader
  - visualization/plotly
  - marine-engineering/orcaflex
```

### 8. UI Adaptation Based on Context

**Learning**: Dynamic UIs that adapt based on user selection reduce complexity.

**Implementation**:
- Vessel type selection (FST/LNGC/Custom) changes available controls
- This prevents overwhelming users with irrelevant options
- Reduces error potential by hiding invalid combinations

**Pattern**:
```javascript
if (vesselType === 'FST') {
  showFSTControls();
  hideLNGCControls();
} else if (vesselType === 'LNGC') {
  showLNGCControls();
  hideFSTControls();
}
```

### 9. Refresh Functionality is Essential

**Learning**: Users need explicit control over when data updates.

**Why It Matters**:
- File systems change during analysis
- New simulations complete while dashboard is open
- Users want to control when updates occur (not automatic)

**Implementation**:
- Add explicit "Refresh" button
- Clear all cached data on refresh
- Regenerate options from file system
- Provide feedback during refresh

### 10. Pattern Matching Over Hardcoding

**Learning**: Use pattern matching for file discovery instead of hardcoded lists.

**Anti-pattern** (Avoid):
```javascript
const files = ['file1.csv', 'file2.csv', 'file3.csv'];
```

**Best Practice**:
```javascript
const patterns = getFromExcel() || getDefaultPatterns();
const files = findFilesMatching(patterns);
```

## Action Items for Future Development

1. **Create Module Template**: Standard structure for new module specifications
2. **Implement Specification Linter**: Check for organizational compliance
3. **Build Dependency Graph Tool**: Visualize module relationships
4. **Establish Review Process**: Ensure specs go in correct modules
5. **Document Naming Conventions**: Clear guidelines for file/folder names

## Repository Best Practices Summary

### DO:
- ✅ Use module-based organization consistently
- ✅ Keep specifications focused and cohesive
- ✅ Provide fallback mechanisms for external dependencies
- ✅ Version specifications when making major changes
- ✅ Document cross-module dependencies explicitly
- ✅ Use pattern matching for dynamic discovery
- ✅ Provide user control over data refresh

### DON'T:
- ❌ Mix unrelated features in single specifications
- ❌ Hardcode values that should be dynamic
- ❌ Assume external files always exist
- ❌ Generate/simulate when real data is expected
- ❌ Create flat specification structures
- ❌ Hide refresh functionality from users

## Conclusion

The main lesson learned is that **organization matters as much as content**. A well-organized specification repository:
- Enables parallel development
- Reduces confusion and rework
- Facilitates maintenance and updates
- Improves discoverability
- Supports scalability

By following these learnings, future specifications will be better organized, more maintainable, and easier to implement.