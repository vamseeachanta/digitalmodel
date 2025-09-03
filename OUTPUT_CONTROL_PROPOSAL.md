# Output Control Proposal for OrcaFlex Post-Processing

## Current Noise Issues

Based on analysis of the post-processing command, the following sources generate excessive output:

### 1. **Per-File Processing Messages**
- `Post-processing file: [filename]` - Printed for EVERY file
- `Completed: [filename]` - Printed after EVERY successful file
- `Failed: [filename] - [error]` - Printed for failures

**Impact**: With hundreds of files, this creates hundreds of lines of output

### 2. **Optimization Messages**
- File size analysis details
- Worker count optimization reasoning
- Performance tips and suggestions

### 3. **Progress Tracking**
- Individual file completion status
- No aggregated progress indicator
- No time estimates

## Proposed Solutions

### Solution 1: Verbosity Levels (RECOMMENDED)

Add verbosity control with three levels:

```bash
# Quiet mode - minimal output
python -m digitalmodel config.yml --quiet
# or
python -m digitalmodel config.yml -q

# Normal mode (default) - summary only
python -m digitalmodel config.yml

# Verbose mode - detailed output
python -m digitalmodel config.yml --verbose
# or
python -m digitalmodel config.yml -v
```

#### Output by Level:

**Quiet Mode (-q)**:
```
Processing 245 files with 6 workers...
[========================================] 100% (245/245) 
Completed: 243 | Failed: 2 | Time: 12m 34s
```

**Normal Mode (default)**:
```
[OPP Auto-Optimization] Files: 245, Median size: 1200.0MB (very large), Optimal threads: 6
Processing 245 files in parallel with 6 workers
[========================================] 100% (245/245) ETA: 00:00
Summary:
  Completed: 243 files
  Failed: 2 files (see error_log.txt)
  Total time: 12m 34s
  Avg per file: 3.08s
```

**Verbose Mode (-v)**:
```
[Current behavior - all messages shown]
Post-processing file: fsts_001.sim
Completed: fsts_001.sim
Post-processing file: fsts_002.sim
...
```

### Solution 2: Progress Bar with Summary

Replace individual file messages with a progress bar:

```python
# Instead of printing each file, use a progress bar
from tqdm import tqdm

# In parallel processing
with tqdm(total=len(sim_files), desc="Processing", unit="file") as pbar:
    for future in as_completed(future_to_file):
        # Process file
        pbar.update(1)
        pbar.set_postfix(completed=completed, failed=failed)
```

Output would look like:
```
Processing: 100%|████████████| 245/245 [12:34<00:00, 3.08s/file, completed=243, failed=2]
```

### Solution 3: Logging Configuration

Add logging configuration in YAML:

```yaml
logging:
  level: INFO  # DEBUG, INFO, WARNING, ERROR, CRITICAL
  console: true
  file: "processing.log"
  format: "simple"  # simple, detailed, json
  
parallel_processing:
  max_workers: 30
  show_progress: true  # Enable/disable progress bar
  quiet_mode: false    # Override from command line
```

### Solution 4: Summary-Only Mode

Add a flag to show only final summary:

```bash
python -m digitalmodel config.yml --summary-only
```

Output:
```
═══════════════════════════════════════════════════════════
                 POST-PROCESSING COMPLETE
═══════════════════════════════════════════════════════════
Files Processed:    245
Successful:         243 (99.2%)
Failed:             2 (0.8%)
Total Time:         12m 34s
Average Time:       3.08s per file
Workers Used:       6
═══════════════════════════════════════════════════════════

Failed Files:
  - fsts_123.sim: License checkout failed
  - fsts_234.sim: File corrupted

Full log saved to: processing_20250903_152234.log
═══════════════════════════════════════════════════════════
```

## Implementation Priority

### Phase 1 (Quick Win) - Add Quiet Flag
```python
# Add to argument parsing
parser.add_argument('--quiet', '-q', action='store_true', 
                   help='Minimal output - show only summary')

# In processing code
if not quiet_mode:
    print(f"Post-processing file: {file_name}")
```

### Phase 2 - Progress Bar
- Add `tqdm` dependency
- Replace file-by-file output with progress bar
- Keep failed files in a list for summary

### Phase 3 - Full Verbosity Control
- Implement three-level verbosity
- Add logging configuration
- Support both CLI flags and config file settings

## Recommended Implementation

### 1. Update opp.py to support verbosity:

```python
def post_process_sim_files(self, cfg):
    """Process multiple simulation files with smart parallel processing."""
    
    # Get verbosity settings
    verbose = cfg.get('verbose', False)
    quiet = cfg.get('quiet', False)
    show_progress = cfg.get('show_progress', True) and not verbose
    
    # ... existing code ...
    
    if not quiet:
        print(f"[OPP Auto-Optimization] {optimization_reason}")
    
    # Use progress bar for normal mode
    if show_progress:
        from tqdm import tqdm
        pbar = tqdm(total=len(sim_files), desc="Processing", unit="file")
    
    # Process files
    failed_files = []
    for future in as_completed(future_to_file):
        file_name = future_to_file[future]
        try:
            result = future.result()
            if result.get('error'):
                failed_files.append((file_name, result['error']))
                if verbose:
                    print(f"Failed: {file_name} - {result['error']}")
            elif verbose:
                print(f"Completed: {file_name}")
                
            if show_progress:
                pbar.update(1)
                pbar.set_postfix(failed=len(failed_files))
        except Exception as e:
            failed_files.append((file_name, str(e)))
    
    if show_progress:
        pbar.close()
    
    # Show summary
    if not quiet or failed_files:
        self._print_summary(len(sim_files), failed_files, start_time)
```

### 2. Add command-line argument handling:

```python
# In engine.py or ApplicationManager
if '--quiet' in sys.argv or '-q' in sys.argv:
    cfg['quiet'] = True
    cfg['show_progress'] = True
elif '--verbose' in sys.argv or '-v' in sys.argv:
    cfg['verbose'] = True
    cfg['show_progress'] = False
```

## Benefits

1. **Reduced Noise**: 245 lines reduced to 3-4 lines in quiet mode
2. **Better UX**: Progress bar shows actual progress and ETA
3. **Flexibility**: Users can choose their preferred output level
4. **Debugging**: Verbose mode still available when needed
5. **Log Files**: Detailed output saved for post-analysis

## Backward Compatibility

- Default behavior shows moderate output (not breaking existing scripts)
- Verbose mode preserves current behavior exactly
- No changes to configuration files required (optional additions only)

## Testing Command Examples

```bash
# Quiet mode - for production runs
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel \
    tests/modules/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts/config/dm_ofx_post_fsts_lngc.yml \
    --max_workers 30 \
    --quiet

# Normal mode - balanced output
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel \
    config/dm_ofx_post_fsts_lngc.yml \
    --max_workers 30

# Verbose mode - for debugging
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel \
    config/dm_ofx_post_fsts_lngc.yml \
    --max_workers 30 \
    --verbose
```