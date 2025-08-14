# OrcaFlex Browser Interface - Prompt Documentation

## Initial Context
User requested to start over with the browser interface specification after backing up existing files.

## Manual Steps Provided by User

The user described their manual workflow for analyzing OrcaFlex output files:

1. **Navigate to folder**: `D:\1522\ctr7\orcaflex\rev_a08\output\csv\03c_100yr`

2. **Search for files**: Look for `dm_*_strut_dyn.csv` files and read them into dataframes

3. **Identify min/max tensions**: Find minimum and maximum effective tension (`eff_tension`) from all struts numbered numerically

4. **Find absolute maximum**: Identify the absolute max value and associated row to extract `fe_filename` and `fe_filename_stem`

5. **Parse metadata from filename**:
   - Loading condition folder: `output/csv/{folder}/`
   - Filename stem patterns:
     - `fsts_l015` = FSTs with 15% LNG
     - `fsts_l095` = FSTs with 95% LNG
   - Tide information:
     - `hwl` = HHWL (Highest High Water Level)
     - `lwl` = LLWL (Lowest Low Water Level)
     - `mwl` = MWL (Mean Water Level)
   - Environment type:
     - `ncl` = non-colinear
     - `cl` = colinear
   - Direction: `000deg` = 0 degrees, etc.

## Key Requirements Identified

1. **File Management**: Need to handle large directories of CSV files with pattern matching
2. **Data Analysis**: Process effective tension data across multiple strut files
3. **Metadata Extraction**: Parse complex filename patterns for contextual information
4. **User Interface**: Provide intuitive browser interface for the analysis workflow

## Specification Approach

Created a comprehensive specification that:
- Documents the manual workflow as core functionality
- Defines technical architecture (Python backend, React frontend)
- Breaks down implementation into manageable phases
- Includes specific filename parsing patterns
- Plans for scalability with large datasets

## Implementation Strategy

1. **Backend First**: Build robust file processing and analysis engine
2. **API Layer**: Create RESTful endpoints for frontend consumption
3. **Progressive UI**: Start with basic functionality, enhance iteratively
4. **Testing Throughout**: Include testing at each phase

## Next Steps for User

User mentioned "there are many more steps" beyond what was initially provided. The specification is designed to be extensible to accommodate additional workflow steps as they are defined.

## Reusable Prompt for Future Sessions

```
I need to create a web-based interface for browsing and analyzing OrcaFlex CSV output files. The system should:

1. Navigate to OrcaFlex output directories and search for specific file patterns (e.g., dm_*_strut_dyn.csv)
2. Process CSV files to find min/max effective tension values across all struts
3. Identify the absolute maximum tension and extract associated metadata
4. Parse filenames to extract loading conditions, LNG percentages, tide levels, environment types, and directions
5. Provide an intuitive web interface for this analysis workflow

The filename patterns include:
- fsts_l015/l095 for LNG loading percentages
- hwl/lwl/mwl for tide levels
- ncl/cl for environment types
- Direction in degrees (000deg, 045deg, etc.)

Please help implement this system with a Python/FastAPI backend and React/TypeScript frontend.
```