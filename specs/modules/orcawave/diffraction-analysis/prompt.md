# OrcaWave Diffraction Analysis Workflow - Prompt Documentation

## Original User Request

The user requested a complete workflow specification for OrcaWave diffraction analysis with the following specific steps:

### Step 1 (User): Geometry Preparation
- **Action:** Manual step where user uses GMsh program
- **Reference:** `agents\gmsh` documentation and agent capabilities
- **Output:** Mesh file for analysis

### Step 2 (AI): Create OrcaWave Input Files
- **Input:** GMsh .msh file located at:
  - `specs\modules\orcawave\diffraction-analysis\inputs\geometry\Sea Cypress_0.25 Mesh_Ascii.msh`
- **Reference Files:** Go-by OrcaWave files at:
  - `specs\modules\orcawave\diffraction-analysis\inputs\orcawave\go-by`
- **Output Location:** Create spec OrcaWave files in:
  - `specs\modules\orcawave\diffraction-analysis\inputs\orcawave`

### Step 3 (AI/User): Analysis Execution
- **AI Responsibility:**
  - Provide Python script or Windows batch script based on module documentation
  - Check if solution works using parallel thread before proposing to user
- **User Responsibility:**
  - Run using batch file or GUI

### Step 4 (AI/User): Post-Processing
- **AI Responsibility:**
  - Create post-processing script to extract key output data
  - Check if solution works using parallel thread before proposing to user
- **User Responsibility:**
  - Run using batch file or GUI

## Context and Background

### Initial Situation
The user had a `specs/modules/orcawave/diffraction-analysis` directory with many geometry iteration files from previous work. These needed to be cleaned up and organized before implementing the new automated workflow.

### Actions Taken
1. **Cleanup:** Moved all geometry iteration files to `revision-1/` subfolder
2. **Organization:** Created clear directory structure for inputs, outputs, scripts
3. **Documentation:** Created README.md for directory overview

### Existing Resources Discovered
- **GMsh Agent:** Fully functional agent at `agents/gmsh/` with mesh generation capabilities
- **OrcaWave Agent:** Specialized agent at `agents/orcawave/` for diffraction analysis
- **Go-by Files:** Example OrcaWave configurations in YAML format
- **Mesh Files:** Multiple formats including STL, OBJ, and MSH

## Clarification Questions and Assumptions

### Questions Asked (Implicitly Addressed)
1. **Q:** What format should the OrcaWave input files be in?
   - **A:** YAML format based on go-by examples found

2. **Q:** Should the scripts validate before execution?
   - **A:** Yes, user specifically requested parallel testing before proposing

3. **Q:** What outputs are considered "key" for extraction?
   - **A:** RAOs, added mass, damping, excitation forces, mean drift, QTF

### Assumptions Made
1. **OrcaWave Version:** Assumed version 11.5+ based on go-by files
2. **Parallel Testing:** Interpreted as validation tests running concurrently
3. **UV Environment:** All scripts should use repository UV environment
4. **Agent Delegation:** GMsh and OrcaWave agents should be utilized where applicable

## Implementation Approach

### Technical Decisions
1. **Language:** Python for all scripts (better for parallel processing)
2. **Configuration:** YAML format for OrcaWave (matches go-by examples)
3. **Validation:** 4-point parallel validation before execution
4. **Output Formats:** Both YAML and JSON for OrcaFlex compatibility

### Parallel Processing Strategy
- **Validation:** Run 4 tests concurrently using ThreadPoolExecutor
- **Extraction:** Process multiple result files in parallel
- **Visualization:** Generate plots concurrently
- **Testing:** Background validation threads before user interaction

### Error Handling
- Comprehensive try-catch blocks in all scripts
- Dry-run mode for testing without execution
- Validation reports saved for review
- Clear error messages and recovery suggestions

## Scripts Created

### 1. `generate_orcawave_input.py`
- Converts GMsh .msh to OrcaWave YAML
- Uses vessel properties from reference files
- Generates GDF file references
- Configurable parameters via command line

### 2. `execute_orcawave_parallel.py`
- Parallel validation framework
- Auto-detects OrcaWave installation
- Creates batch files for execution
- Generates validation reports

### 3. `postprocess_orcawave_parallel.py`
- Extracts all hydrodynamic data
- Parallel file processing
- Creates visualizations
- Generates OrcaFlex-ready formats

### 4. Batch Files
- `run_complete_workflow.bat` - End-to-end execution
- `run_with_parallel_test.bat` - Testing mode with UV

## Curated Reuse Prompt

For future similar specifications, use this template:

```
Create an automated workflow specification for [ANALYSIS_TYPE] with these steps:

Step 1 (User): [MANUAL_PREPARATION]
- Tool: [SOFTWARE_NAME]
- Reference: [DOCUMENTATION_PATH]
- Output: [FILE_TYPE]

Step 2 (AI): Generate Input Files
- Input: [SOURCE_FILE_PATH]
- Reference: [EXAMPLE_FILES_PATH]
- Output: [DESTINATION_PATH]
- Requirements: Convert from [FORMAT_A] to [FORMAT_B]

Step 3 (AI/User): Execute Analysis
- AI: Provide script with parallel validation
- User: Run via batch/GUI
- Validation: Test in parallel before execution

Step 4 (AI/User): Post-Process Results
- AI: Create extraction script with parallel processing
- User: Execute and review results
- Output: [TARGET_FORMAT] for [INTEGRATION_SOFTWARE]

Additional Requirements:
- Use UV environment for all Python execution
- Implement parallel validation before user actions
- Create batch files for easy execution
- Generate comprehensive documentation
- Utilize existing agents: [AGENT_LIST]
```

## Lessons Learned

### What Worked Well
1. **Parallel Validation:** Catches issues before expensive operations
2. **Agent Integration:** Leveraging existing agents saved development time
3. **Batch Files:** Simple interface for non-technical users
4. **Multiple Output Formats:** Flexibility for different use cases

### Challenges Addressed
1. **Mesh Format Conversion:** Handled by referencing existing GDF files
2. **OrcaWave Detection:** Auto-detection with fallback to validation-only
3. **Large File Processing:** Parallel extraction handles efficiently
4. **User Confirmation:** Clear prompts at critical decision points

## Next Steps

### Immediate Actions
1. User reviews generated scripts
2. Test with `run_with_parallel_test.bat`
3. Execute full workflow if validation passes
4. Import results into OrcaFlex

### Future Enhancements
1. GUI interface for workflow management
2. Cloud execution capabilities
3. Real-time progress monitoring
4. Automated report generation in PDF

## References

- GMsh Documentation: `agents/gmsh/README.md`
- OrcaWave Agent: `agents/orcawave/capabilities.yml`
- Go-by Examples: `inputs/orcawave/go-by/*.yml`
- OrcaFlex Integration: `specs/modules/orcaflex/orcawave-results-integration/`