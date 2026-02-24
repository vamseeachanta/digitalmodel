# OrcaFlex Browser Backend Implementation

## Overview
This is the backend implementation for the OrcaFlex Browser Interface, designed to process CSV output files from OrcaFlex simulations and extract critical tension data and metadata.

## Features

### Core Capabilities
- **Parallel File Processing**: Read multiple CSV files simultaneously using thread pools
- **Tension Analysis**: Identify min/max effective tensions across all struts
- **Metadata Extraction**: Parse complex filename patterns for context
- **Verification System**: Timestamped JSON logs for AI learning and user feedback
- **Interactive CLI Testing**: Rich terminal interface for testing and validation

## Components

### 1. Backend Module (`backend/file_processor.py`)
- `FileProcessor`: Handles file discovery and parallel CSV reading
- `TensionAnalyzer`: Analyzes effective tension data across struts
- `MetadataExtractor`: Parses filename patterns for metadata
- `VerificationLogger`: Logs results with timestamps for AI learning

### 2. CLI Testing Interface (`cli/test_interface.py`)
- Interactive testing with rich terminal UI
- Step-by-step validation of each component
- User feedback collection
- Results verification and logging

### 3. Quick Test Script (`run_test.py`)
- Simplified testing for quick validation
- Automatic workflow through all components
- Basic feedback collection

## Installation

```bash
# Install required packages
pip install -r requirements.txt
```

Required packages:
- pandas >= 2.0.0
- numpy >= 1.24.0
- rich >= 13.0.0

## Usage

### Method 1: Batch File (Windows)
```bash
# Run the test batch file
test_backend.bat
```

### Method 2: Direct Python
```bash
# Quick test
python run_test.py

# Interactive CLI test
python cli/test_interface.py

# View latest verification
python cli/test_interface.py --verify-latest
```

### Method 3: Import and Use
```python
from backend.file_processor import FileProcessor, TensionAnalyzer

# Initialize
processor = FileProcessor("D:/path/to/csv/files")
analyzer = TensionAnalyzer()

# Search files
files = processor.search_files("dm_*_strut_dyn.csv")

# Read in parallel
dataframes = processor.read_multiple_csvs_parallel(files)

# Analyze
results = analyzer.analyze_tensions(dataframes)
```

## File Pattern Recognition

The system recognizes these filename patterns:

### LNG Loading States
- `fsts_l015`: FSTs with 15% LNG
- `fsts_l095`: FSTs with 95% LNG

### Tide Levels
- `hwl`: Highest High Water Level (HHWL)
- `lwl`: Lowest Low Water Level (LLWL)
- `mwl`: Mean Water Level (MWL)

### Environment Types
- `ncl`: Non-colinear
- `cl`: Colinear

### Direction
- `000deg`, `045deg`, `090deg`, etc.

## Verification System

All test results are saved to timestamped JSON files in the `verification/` folder:
- `verification_YYYYMMDD_HHMMSS.json`

Each verification file contains:
- Analysis results
- User feedback
- Timestamps
- Metadata for AI learning

## Example Output

```
ABSOLUTE MAXIMUM TENSION:
Value: 12345.67
Strut: strut_1
FE Filename: fsts_03c_l095_hwl_ncl_045deg.csv
FE Filename Stem: fsts_03c_l095_hwl_ncl_045deg

Metadata:
Loading Condition: 03c_100yr
LNG Loading: 95% LNG
Tide Level: HHWL
Environment Type: Non-colinear
Direction: 045 degrees
```

## Testing Workflow

1. **File Discovery**: Search for CSV files matching pattern
2. **Parallel Reading**: Load multiple files simultaneously
3. **Tension Analysis**: Calculate min/max for each strut
4. **Absolute Maximum**: Find critical tension value
5. **Metadata Extraction**: Parse filename for context
6. **User Feedback**: Collect validation and corrections
7. **Verification Logging**: Save timestamped JSON for AI learning

## Performance

- Handles 100+ CSV files efficiently
- Parallel processing with configurable workers
- In-memory caching for repeated operations
- Optimized for large datasets

## Future Enhancements

- [ ] Add FastAPI REST endpoints
- [ ] Implement Redis caching
- [ ] Add data visualization
- [ ] Create web dashboard
- [ ] Add batch processing queue
- [ ] Implement real-time monitoring