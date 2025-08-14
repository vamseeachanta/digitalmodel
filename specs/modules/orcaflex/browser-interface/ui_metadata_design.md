# UI Metadata Design - OrcaFlex Browser Interface

## Core Requirement
**Capture ALL metadata for UI context and browsing capabilities**

## Comprehensive Metadata Schema

### 1. File-Level Metadata
```javascript
{
  // Primary Identifiers
  "csv_filename": "dm_fsts_03c_0100yr_l015_hwl_strut_dyn.csv",
  "csv_path": "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr\\",
  "file_size": "9.5 KB",
  "last_modified": "2024-08-13T10:30:00",
  
  // Parsed from CSV Filename
  "project_code": "fsts_03c",
  "storm_condition": "0100yr",
  "lng_loading_code": "l015",
  "lng_loading_percent": "15%",
  "tide_code": "hwl",
  "tide_level": "High Water Level",
  
  // Associated FE Files (from CSV content)
  "fe_simulations": [
    {
      "filename": "fsts_l015_hwl_ncl_240deg.sim",
      "direction": "240°",
      "environment": "Non-colinear",
      "max_tension": 8265.55,
      "critical_strut": "Strut7"
    },
    // ... more FE files
  ]
}
```

### 2. Analysis Metadata
```javascript
{
  // Strut Analysis Summary
  "strut_summary": {
    "total_struts": 8,
    "critical_strut": "Strut7",
    "max_tension_overall": 8265.55,
    "min_tension_overall": -1764.76,
    "strut_details": [
      {
        "strut_id": "Strut7",
        "max_tension": 8265.55,
        "min_tension": -637.54,
        "range": 8903.09,
        "critical_direction": "240°"
      }
      // ... all struts
    ]
  },
  
  // Environmental Conditions
  "conditions": {
    "lng_loading": {
      "value": "15%",
      "code": "l015",
      "display": "15% LNG Loading"
    },
    "tide_level": {
      "value": "HWL",
      "code": "hwl", 
      "display": "High Water Level (HHWL)"
    },
    "storm": {
      "value": "100-year",
      "code": "0100yr",
      "display": "100-Year Storm Event"
    },
    "environment_type": {
      "value": "Non-colinear",
      "code": "ncl",
      "display": "Non-colinear Environmental Conditions"
    },
    "wave_direction": {
      "value": "240",
      "unit": "degrees",
      "display": "240° Wave Direction"
    }
  }
}
```

### 3. Browsable Metadata Categories

#### For UI Filtering/Navigation:
```javascript
{
  "browsable_categories": {
    // LNG Loading Options
    "lng_loading": [
      { "code": "l015", "display": "15% LNG", "count": 2 },
      { "code": "l095", "display": "95% LNG", "count": 2 }
    ],
    
    // Tide Levels
    "tide_levels": [
      { "code": "hwl", "display": "High Water Level", "count": 2 },
      { "code": "lwl", "display": "Low Water Level", "count": 2 },
      { "code": "mwl", "display": "Mean Water Level", "count": 0 }
    ],
    
    // Storm Conditions
    "storm_conditions": [
      { "code": "0001yr", "display": "1-Year", "count": 0 },
      { "code": "0010yr", "display": "10-Year", "count": 0 },
      { "code": "0100yr", "display": "100-Year", "count": 4 }
    ],
    
    // Wave Directions
    "wave_directions": [
      { "value": "000", "display": "0°", "count": 12 },
      { "value": "045", "display": "45°", "count": 8 },
      { "value": "090", "display": "90°", "count": 10 },
      // ... all directions
      { "value": "240", "display": "240°", "count": 15 },
      { "value": "315", "display": "315°", "count": 9 }
    ],
    
    // Environment Types
    "environment_types": [
      { "code": "cl", "display": "Colinear", "count": 45 },
      { "code": "ncl", "display": "Non-colinear", "count": 67 }
    ],
    
    // Struts
    "struts": [
      { "id": "Strut1", "display": "Strut 1", "max": 6007.98 },
      { "id": "Strut2", "display": "Strut 2", "max": 6413.55 },
      // ... all struts
      { "id": "Strut7", "display": "Strut 7", "max": 8265.55 },
      { "id": "Strut8", "display": "Strut 8", "max": 7654.77 }
    ]
  }
}
```

## UI Components Design

### 1. Metadata Display Panel
```typescript
interface MetadataPanel {
  // Current Selection Context
  currentFile: string;
  loadingCondition: string;
  lngPercent: string;
  tideLevel: string;
  stormCondition: string;
  
  // Critical Values
  maxTension: number;
  criticalStrut: string;
  criticalDirection: string;
  
  // Quick Stats
  totalFiles: number;
  totalStruts: number;
  dateRange: DateRange;
}
```

### 2. Filter/Browse Panel
```typescript
interface FilterPanel {
  // Multi-select filters
  lngLoadings: FilterOption[];
  tideLevels: FilterOption[];
  stormConditions: FilterOption[];
  waveDirections: FilterOption[];
  environmentTypes: FilterOption[];
  struts: FilterOption[];
  
  // Range filters
  tensionRange: RangeFilter;
  dateRange: DateRangeFilter;
  
  // Search
  searchQuery: string;
  searchIn: ['filename', 'fe_files', 'all'];
}
```

### 3. Breadcrumb Navigation
```typescript
interface BreadcrumbNav {
  path: [
    { label: "03c_100yr", value: "storm_condition" },
    { label: "15% LNG", value: "lng_loading" },
    { label: "HWL", value: "tide_level" },
    { label: "Strut7", value: "strut_id" }
  ];
}
```

## UI Features Using Metadata

### 1. Smart Filtering
- **Cascading Filters**: Selection of LNG loading updates available tide levels
- **Result Counts**: Show number of files/results for each filter option
- **Quick Presets**: "Show Critical Cases", "15% LNG Only", "High Tensions"

### 2. Contextual Information
- **Hover Details**: Show metadata on hover over data points
- **Comparison Mode**: Compare metadata between different conditions
- **History Trail**: Track browsing path through metadata selections

### 3. Advanced Browse Modes

#### Browse by Condition
```
LNG Loading → Tide Level → Direction → View Results
```

#### Browse by Criticality
```
Sort by Max Tension → Filter by Strut → View Time Series
```

#### Browse by Similarity
```
"Show similar conditions to current selection"
- Same LNG, different tide
- Same direction, different LNG
- Adjacent directions
```

### 4. Metadata-Driven Visualizations
```javascript
{
  "visualization_contexts": {
    "heatmap": {
      "x_axis": "wave_direction",
      "y_axis": "strut_id",
      "value": "max_tension",
      "filter": "lng_loading=15%"
    },
    "comparison_chart": {
      "series": ["15% LNG", "95% LNG"],
      "x_axis": "tide_level",
      "y_axis": "max_tension"
    },
    "trend_analysis": {
      "group_by": "storm_condition",
      "metric": "max_tension",
      "breakdown": "strut_id"
    }
  }
}
```

## Implementation Notes

### Backend Requirements
1. **Metadata Extraction Service**: Parse and cache all metadata on file load
2. **Metadata Index**: Build searchable index of all metadata fields
3. **Aggregation Service**: Calculate counts, ranges, and statistics
4. **Relationship Mapping**: Link related files and conditions

### Frontend Requirements
1. **Metadata Store**: Vuex/Redux store for metadata state
2. **Filter Engine**: Dynamic filter application with counts
3. **Context Provider**: Pass metadata context to all components
4. **Cache Manager**: Cache metadata for performance

### API Endpoints
```typescript
// Get all available metadata categories and options
GET /api/metadata/categories

// Get metadata for specific file
GET /api/metadata/file/{filename}

// Get filtered results based on metadata
POST /api/browse
{
  "filters": {
    "lng_loading": ["l015"],
    "tide_level": ["hwl"],
    "min_tension": 5000
  }
}

// Get metadata statistics
GET /api/metadata/stats
{
  "groupBy": "strut_id",
  "metric": "max_tension"
}
```

## User Stories

1. **"Show me all high tension cases"**
   - Filter: max_tension > 5000
   - Display: Sorted by tension, showing all metadata

2. **"Compare 15% vs 95% LNG loading"**
   - Filter: Group by LNG loading
   - Display: Side-by-side comparison with metadata

3. **"Find similar conditions to the critical case"**
   - Context: Current critical case metadata
   - Display: Similar cases with variation in one parameter

4. **"Browse by wave direction"**
   - Navigation: Direction selector (0° - 360°)
   - Display: Results for selected direction with all metadata

## Benefits of Comprehensive Metadata Capture

1. **Rich Context**: Users always know exactly what data they're viewing
2. **Efficient Navigation**: Quick filtering and browsing through large datasets
3. **Comparison Capability**: Easy to compare different conditions
4. **Traceability**: Full audit trail of data sources and conditions
5. **Discoverability**: Users can discover patterns and relationships
6. **Export Ready**: Metadata included in all exports for documentation