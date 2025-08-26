# Prepare Diffraction Analysis Inputs - Prompt Documentation

## Original Request
Create a specification for preparing OrcaWave diffraction analysis inputs with the following workflow:

**User-Defined Steps:**
1. **Geometry** (User): Manual step using GMsh program. Reference: `agents\gmsh`
2. **Data Extraction** (AI): Extract required CSV data from Excel files
3. **Input Generation** (AI): Create OrcaWave input files using:
   - Geometry: `specs\modules\orcawave\diffraction-analysis\inputs\geometry\Sea Cypress_0.25 Mesh_Ascii.msh`
   - Go-by templates: `specs\modules\orcawave\diffraction-analysis\inputs\orcawave\go-by\`
   - Output location: `specs\modules\orcawave\diffraction-analysis\inputs\orcawave\`
4. **Execution** (AI/User): 
   - AI provides Python/batch scripts based on module documentation
   - Scripts tested in parallel threads before user execution
   - User runs via batch or GUI
5. **Post-Processing** (AI/User):
   - AI creates extraction scripts for key output data
   - Scripts tested in parallel before user execution
   - User runs for final results

## Key Requirements Identified
1. **Automation Focus**: Minimize manual steps, automate data flow
2. **Parallel Processing**: Use parallel threads for validation and testing
3. **Agent Delegation**: Utilize specialized agents (OrcaWave, GMsh, Testing)
4. **Validation First**: Test all scripts before proposing to user
5. **Multiple Formats**: Support Excel input, YAML config, GDF geometry, Excel/CSV output

## Design Decisions
1. **Modular Architecture**: Separate modules for each workflow stage
2. **Template-Based**: Use go-by files as templates with variable substitution
3. **Error Recovery**: Implement fallbacks for geometry and data issues
4. **Performance Target**: 3x speedup through parallelization
5. **Mock Mode**: Enable testing without OrcaWave license

## Context Gathered
- Repository has comprehensive OrcaWave module at `src/modules/orcawave/diffraction/`
- Existing scripts in `specs/modules/orcawave/diffraction-analysis/scripts/`
- OrcaWave agent available with COM API capabilities
- GMsh agent supports mesh operations and conversions
- Go-by templates use YAML format with vessel parameters

## Implementation Approach
1. **Leverage Existing**: Build on current OrcaWave module capabilities
2. **Agent Coordination**: GMsh handles geometry, OrcaWave handles analysis
3. **Parallel Validation**: Testing agent runs all validations concurrently
4. **Progressive Enhancement**: Start with core functionality, add features
5. **User-Friendly**: Provide both batch and Python execution options

## Curated Reuse Prompt
```
Create an automated workflow for OrcaWave diffraction analysis preparation that:
1. Extracts vessel data from Excel files
2. Converts GMsh .msh geometry to OrcaWave GDF format
3. Generates OrcaWave YAML input files from templates
4. Provides execution scripts (batch and Python)
5. Extracts results to Excel/CSV formats

Use these specific files:
- Geometry: specs\modules\orcawave\diffraction-analysis\inputs\geometry\Sea Cypress_0.25 Mesh_Ascii.msh
- Templates: specs\modules\orcawave\diffraction-analysis\inputs\orcawave\go-by\*.yml
- Output to: specs\modules\orcawave\diffraction-analysis\inputs\orcawave\

Requirements:
- Test all scripts in parallel before user execution
- Delegate to specialized agents (OrcaWave, GMsh, Testing)
- Target 3x performance through parallelization
- Include comprehensive validation at each step
- Support both GUI and command-line execution
```

## Lessons Learned
1. **Agent Specialization**: Each agent brings domain expertise
2. **Parallel First**: Design for parallelization from the start
3. **Validation Critical**: Geometry validation prevents downstream errors
4. **Template Power**: Go-by files enable rapid configuration generation
5. **User Options**: Provide multiple execution paths for flexibility