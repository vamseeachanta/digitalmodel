# OrcaFlex Browser Interface Specification

## Overview
A web-based interface for browsing, analyzing, and visualizing OrcaFlex CSV output files with focus on structural analysis results, particularly effective tension in strut components.

## Core Functionality

### 1. File Navigation and Discovery
- **Primary Directory**: Navigate to OrcaFlex output directories (e.g., `D:\1522\ctr7\orcaflex\rev_a08\output\csv\03c_100yr`)
- **Pattern-Based Search**: Search for files matching specific patterns (e.g., `dm_*_strut_dyn.csv`)
- **Bulk Data Loading**: Read multiple CSV files into dataframes for analysis

### 2. Data Analysis Engine

#### 2.1 Effective Tension Analysis
- **Multi-File Processing**: Process all strut dynamic CSV files simultaneously
- **Min/Max Identification**: Extract minimum and maximum `eff_tension` values from each strut
- **Numerical Organization**: Organize results by strut number for easy comparison
- **Absolute Maximum Detection**: 
  - Identify the absolute maximum tension across all struts
  - Extract associated row data including `fe_filename` and `fe_filename_stem`

#### 2.2 Metadata Extraction
Parse filename components to extract critical metadata:

**Loading Condition Information**:
- Folder path indicates loading condition (e.g., `output/csv/03c_100yr/`)
- Full filename preserves complete loading condition context

**Filename Stem Patterns**:
- **LNG Loading States**:
  - `fsts_l015`: FSTs with 15% LNG loading
  - `fsts_l095`: FSTs with 95% LNG loading
- **Tide Levels**:
  - `hwl`: Highest High Water Level (HHWL)
  - `lwl`: Lowest Low Water Level (LLWL)  
  - `mwl`: Mean Water Level (MWL)
- **Environment Types**:
  - `ncl`: Non-colinear environmental conditions
  - `cl`: Colinear environmental conditions
- **Direction Information**:
  - `000deg`: 0 degrees
  - `045deg`: 45 degrees
  - Pattern continues for all directions

### 3. User Interface Components

#### 3.1 File Browser Panel
- Directory tree navigation
- Pattern-based file filtering
- Multi-select capability for batch operations
- Real-time file preview

#### 3.2 Data Analysis Dashboard
- Summary statistics display
- Min/Max tension visualization
- Critical case identification
- Metadata display panel

#### 3.3 Results Visualization
- Tabular view of analysis results
- Sortable/filterable data grid
- Export capabilities (CSV, Excel)
- Visual highlighting of critical values

## Technical Requirements

### Data Processing
- **File Format**: CSV files from OrcaFlex simulations
- **Key Columns**: 
  - `eff_tension`: Effective tension values
  - `fe_filename`: Full filename reference
  - `fe_filename_stem`: Filename stem for metadata parsing
- **Performance**: Handle directories with 100+ CSV files efficiently

### Frontend Technologies
- **Framework**: React/TypeScript for component-based UI
- **Data Grid**: AG-Grid or similar for tabular data display
- **Visualization**: Chart.js or D3.js for data visualization
- **State Management**: Redux or Context API for application state

### Backend Services
- **File System Access**: Python FastAPI for file operations
- **Data Processing**: Pandas for CSV processing and analysis
- **Caching**: Redis or in-memory caching for processed results
- **API Design**: RESTful endpoints for data operations

## Implementation Phases

### Phase 1: Core File Operations
- Directory navigation
- File search and filtering
- CSV reading and basic display

### Phase 2: Analysis Engine
- Effective tension calculations
- Min/max identification
- Metadata extraction from filenames

### Phase 3: User Interface
- Dashboard components
- Interactive data grid
- Visualization components

### Phase 4: Advanced Features
- Batch processing capabilities
- Export functionality
- Report generation
- Performance optimization

## Success Criteria
1. Successfully navigate and load OrcaFlex CSV files
2. Accurately identify min/max effective tension values
3. Correctly parse and display filename metadata
4. Provide intuitive user interface for analysis workflow
5. Handle large datasets (100+ files) efficiently

## Future Enhancements
- Automated report generation
- Comparison across multiple loading conditions
- Time-series analysis capabilities
- Integration with OrcaFlex directly
- Advanced filtering and search options