# User Story: Parallel Processing for OrcaFlex Post-Processing

## Story
As a user processing multiple OrcaFlex simulation files, I want the post-processing to run in parallel so that I can significantly reduce the total processing time when working with large datasets.

## Background
The current implementation in `src/digitalmodel/modules/orcaflex/opp.py` processes simulation files sequentially in a for loop (lines 91-149). When dealing with hundreds of simulation files, this becomes a bottleneck as each file must wait for the previous one to complete.

## Acceptance Criteria
1. Multiple simulation files can be processed concurrently
2. Results from parallel processing match exactly with sequential processing
3. System gracefully falls back to sequential processing when:
   - Only one file needs processing
   - Parallel processing is disabled in configuration
   - Errors occur during parallel initialization
4. Progress tracking shows which files are being processed
5. Errors in one file don't stop processing of other files
6. Memory usage remains reasonable even with many large files

## Technical Implementation

### Step 1: Import Required Modules
```python
from concurrent.futures import ProcessPoolExecutor, as_completed
from multiprocessing import cpu_count, Manager
import traceback
from typing import Dict, List, Tuple, Optional
```

### Step 2: Extract Single File Processing Logic
Create `process_single_file()` function that encapsulates the processing logic for one simulation file:
- Extract lines 92-148 from current implementation
- Return results as a dictionary
- Handle exceptions within the function

### Step 3: Create Result Aggregation Functions
Implement `aggregate_results()` to combine results from all processed files:
- Maintain original file order
- Aggregate RangeGraph data
- Combine summaries and linked statistics
- Collect load matrix updates

### Step 4: Implement Parallel Processing
Modify `post_process()` method to use ProcessPoolExecutor:
- Configure number of workers
- Submit all files for processing
- Track progress with as_completed()
- Aggregate results after completion

### Step 5: Error Handling
- Wrap individual file processing in try-except blocks
- Log failures without stopping other files
- Optional error report generation
- Return error status in result dictionary

### Step 6: Configuration
Add parallel processing configuration:
```yaml
parallel_processing:
  enabled: true
  max_workers: 4  # or "auto" for cpu_count()
  timeout_per_file: 3600  # seconds
  save_error_reports: true
  progress_reporting: true
```

### Step 7: Sequential Processing Fallback
Preserve original sequential logic as `_post_process_sequential()` for:
- Single file processing
- When parallel processing is disabled
- Debugging and comparison purposes

### Step 8: Testing Strategy
1. Compare sequential vs parallel results for data integrity
2. Performance benchmarking to measure speedup
3. Error handling tests with simulated failures
4. Resource monitoring for memory/CPU usage
5. Edge cases: empty files, corrupt files, very large files

## Benefits
- **Performance**: Process N files in approximately 1/N time (depending on CPU cores)
- **Reliability**: Isolated failures don't affect other files
- **Scalability**: Handles large batches of files efficiently
- **Monitoring**: Real-time progress tracking
- **Flexibility**: Configurable worker count and timeouts

## Risks and Mitigation
1. **Memory Usage**: Large models may consume significant memory
   - Mitigation: Limit max_workers based on available RAM
   
2. **Pickling Issues**: Some OrcaFlex objects may not be serializable
   - Mitigation: Pass file paths instead of objects, load models in workers
   
3. **Resource Contention**: License limitations for OrcaFlex
   - Mitigation: Configure max_workers to respect license limits

## Dependencies
- Python's `concurrent.futures` module (standard library)
- No additional third-party dependencies required

## Estimated Effort
- Implementation: 2-3 days
- Testing: 1-2 days
- Documentation: 0.5 days

## Success Metrics
- Processing time reduced by at least 50% for batches of 10+ files
- Zero data discrepancies between sequential and parallel processing
- Error rate remains unchanged
- Memory usage increases by less than 2x despite parallel processing