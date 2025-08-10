# OrcaFlex Results Dashboard - Comprehensive Filtering Interface

A sophisticated filtering system for the OrcaFlex Results Dashboard with cascading filters, real-time updates, and advanced preset management.

## Features

### Core Filtering Capabilities
- **Cascading Filter UI** - Analysis case → loading condition → heading dependency chain
- **Multi-select Functionality** - Select multiple items with search capabilities
- **Real-time Chart Updates** - Sub-500ms response time for filter changes
- **Filter State Persistence** - Automatic URL parameter synchronization
- **Invalid Filter Handling** - Clear error messages and validation

### Filter Types

#### 1. Analysis Case Filter (`AnalysisCaseFilter.tsx`)
- **Hierarchical Organization** - Cases grouped by category with expansion controls
- **Advanced Search** - Search by name, description, and tags
- **Bulk Operations** - Select all, clear all, category-based selection
- **Visual Indicators** - Active/inactive status, selection counters

#### 2. Loading Condition Filter (`LoadingConditionFilter.tsx`)
- **Dual Mode Interface** - Simple component-based and advanced direct selection
- **Component Selection** - Individual toggles for water level, volume, and side
- **Condition Validation** - Only valid combinations based on analysis cases
- **Visual Preview** - Real-time display of selected combinations

#### 3. Environmental Heading Filter (`HeadingFilter.tsx`)
- **Multiple Input Modes** - Range slider, individual selection, visual compass
- **Compass Visualization** - Interactive 360° compass with cardinal directions
- **Custom Heading Input** - Manual entry with validation (0°-345°, 15° increments)
- **Quick Selection** - Common headings, cardinal directions shortcuts

### Filter Management

#### 4. Filter Presets (`FilterPresets.tsx`)
- **Preset Management** - Save, edit, delete, and organize filter configurations
- **Import/Export** - JSON-based preset sharing and backup
- **Preset Preview** - Visual summary of filter combinations
- **Auto-matching** - Detect when current filters match saved presets

#### 5. Main Filter Panel (`FilterPanel.tsx`)
- **Accordion Interface** - Collapsible sections for organized filter management
- **Real-time Updates** - Debounced filter application with progress indicators
- **Auto-apply Toggle** - Manual vs automatic filter application
- **Performance Monitoring** - Update timestamps and processing indicators

## Architecture

### Context Management (`FilterContext.tsx`)
```typescript
// React Context for filter state management
const { state, actions, presets } = useFilterContext();

// Available actions
actions.setAnalysisCase(['case1', 'case2']);
actions.setLoadingCondition([{ waterLevel: 'hwl', volume: '125km3', side: 'pb' }]);
actions.setHeading([0, 45, 90]);
actions.resetFilters();
actions.savePreset('My Preset', 'Description');
```

### State Structure
```typescript
interface FilterState {
  analysisCase: string[];           // Selected analysis case IDs
  loadingCondition: LoadingCondition[];  // Selected loading combinations
  heading: number[];                // Selected headings (0°-345°)
  isLoading: boolean;              // Loading state
  error: string | null;            // Error messages
  lastUpdated: Date | null;        // Last update timestamp
}
```

### Loading Condition Structure
```typescript
interface LoadingCondition {
  waterLevel: 'hwl' | 'lwl';       // High/Low Water Level
  volume: '125km3' | '180km3';     // Tank volume
  side: 'pb' | 'sb';               // Port/Starboard side
}
```

## Usage Examples

### Basic Implementation
```tsx
import { FilterProvider, FilterPanel } from './filters';

function Dashboard() {
  const handleFilterChange = () => {
    // Update charts and data displays
    console.log('Filters changed, updating charts...');
  };

  return (
    <FilterProvider>
      <FilterPanel
        onFilterChange={handleFilterChange}
        autoApplyFilters={true}
        showPresets={true}
        compactMode={false}
      />
      {/* Your charts and data components */}
    </FilterProvider>
  );
}
```

### Individual Filter Components
```tsx
import { 
  FilterProvider,
  AnalysisCaseFilter, 
  LoadingConditionFilter, 
  HeadingFilter,
  useFilterContext 
} from './filters';

function CustomFilterPanel() {
  const { state } = useFilterContext();
  
  return (
    <Grid container spacing={2}>
      <Grid item xs={4}>
        <AnalysisCaseFilter />
      </Grid>
      <Grid item xs={4}>
        <LoadingConditionFilter disabled={state.analysisCase.length === 0} />
      </Grid>
      <Grid item xs={4}>
        <HeadingFilter disabled={state.analysisCase.length === 0} />
      </Grid>
    </Grid>
  );
}
```

### Filter State Hook
```tsx
import { useFilterState } from './filters';

function ChartComponent() {
  const {
    filters,
    hasActiveFilters,
    totalFilterCount,
    isValidCascade,
    resetFilters
  } = useFilterState();

  const filteredData = useMemo(() => {
    if (!hasActiveFilters) return allData;
    
    return allData.filter(item => {
      // Apply filtering logic
      return filters.analysisCase.includes(item.analysisCase) &&
             (filters.loadingCondition.length === 0 || 
              filters.loadingCondition.some(lc => matchesCondition(item, lc))) &&
             (filters.heading.length === 0 || 
              filters.heading.includes(item.heading));
    });
  }, [filters, allData, hasActiveFilters]);

  return (
    <Box>
      {hasActiveFilters && (
        <Alert>
          Showing {filteredData.length} results with {totalFilterCount} active filters
          <Button onClick={resetFilters}>Clear All</Button>
        </Alert>
      )}
      <YourChartComponent data={filteredData} />
    </Box>
  );
}
```

## Integration with Redux

### Redux State Structure
```typescript
interface DashboardState {
  data: {
    analysisCases: AnalysisCase[];
    loadingConditions: Record<string, LoadingCondition[]>;
    headings: Record<string, Record<string, number[]>>;
    results: any[];
  };
  loading: boolean;
  error: string | null;
}
```

### Connecting Filters to Redux
```tsx
import { useSelector } from 'react-redux';
import { RootState } from '../../store';

function FilteredDashboard() {
  const dashboardData = useSelector((state: RootState) => state.dashboard);
  const { filters } = useFilterState();
  
  // Filter logic here
  const filteredResults = useMemo(() => {
    return applyFilters(dashboardData.data.results, filters);
  }, [dashboardData.data.results, filters]);
  
  return <ChartsDisplay data={filteredResults} />;
}
```

## Performance Optimizations

### Debounced Updates
- 500ms debounce for filter changes
- Prevents excessive API calls and chart re-renders
- Configurable via `FILTER_CONFIG.UPDATE_DEBOUNCE_MS`

### Memoized Calculations
- Filter validation results cached
- Available options computed once per dependency change
- Preset matching optimized with JSON serialization

### Virtual Scrolling
- Large analysis case lists use virtual scrolling
- Efficient rendering for 1000+ items
- Search and filtering maintain performance

## Configuration

### Default Configuration
```typescript
export const FILTER_CONFIG = {
  HEADING: {
    MIN: 0,
    MAX: 345,
    STEP: 15,
    COMMON: [0, 45, 90, 135, 180, 225, 270, 315],
  },
  WATER_LEVELS: ['hwl', 'lwl'],
  VOLUMES: ['125km3', '180km3'],
  SIDES: ['pb', 'sb'],
  UPDATE_DEBOUNCE_MS: 500,
};
```

## Error Handling

### Validation Errors
- Invalid heading ranges (not 0°-345° or not 15° increments)
- Invalid loading condition combinations
- Missing required analysis cases
- Network errors during data loading

### Error Display
- Inline error messages in each filter component
- Global error banner in main panel
- Toast notifications for preset operations
- Clear error recovery instructions

## Accessibility

### Keyboard Navigation
- Full keyboard support for all filter components
- Tab order follows logical filter hierarchy
- Escape key closes dialogs and menus

### Screen Reader Support
- ARIA labels for all interactive elements
- Live regions for dynamic content updates
- Descriptive text for visual indicators

### Visual Indicators
- High contrast color schemes
- Clear focus indicators
- Loading states with progress indicators
- Error states with appropriate colors and icons

## File Structure

```
src/components/filters/
├── FilterContext.tsx       # React Context and state management
├── FilterPanel.tsx         # Main filter panel with accordion layout
├── AnalysisCaseFilter.tsx  # Analysis case selection with search
├── LoadingConditionFilter.tsx  # Loading condition selection
├── HeadingFilter.tsx       # Environmental heading selection
├── FilterPresets.tsx       # Preset management interface
├── types.ts               # TypeScript type definitions
├── index.ts               # Exports and utilities
└── README.md              # This documentation file
```

## Dependencies

### Required Packages
```json
{
  "@mui/material": "^5.14.0",
  "@mui/icons-material": "^5.14.0",
  "react": "^18.2.0",
  "react-dom": "^18.2.0",
  "react-router-dom": "^6.8.0",
  "react-redux": "^8.1.0",
  "lodash": "^4.17.21"
}
```

### Type Definitions
```json
{
  "@types/react": "^18.2.0",
  "@types/react-dom": "^18.2.0",
  "@types/lodash": "^4.14.0"
}
```

## Testing

### Unit Tests
- Filter validation logic tests
- Context state management tests
- Component rendering tests
- Preset serialization/deserialization tests

### Integration Tests
- Filter cascade behavior
- Redux integration tests
- URL parameter synchronization
- Performance benchmarks

### End-to-End Tests
- Complete filter workflow tests
- Preset import/export functionality
- Chart integration tests
- Accessibility compliance tests

## Browser Support

- **Modern Browsers**: Chrome 90+, Firefox 88+, Safari 14+, Edge 90+
- **Mobile Support**: iOS Safari 14+, Chrome Mobile 90+
- **Accessibility**: WCAG 2.1 AA compliant
- **Performance**: Optimized for 60fps updates

## License

This filtering interface is part of the OrcaFlex Results Dashboard and follows the project's licensing terms.