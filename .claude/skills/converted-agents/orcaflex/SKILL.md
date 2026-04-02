---
name: orcaflex
version: 1.0.0
category: engineering
description: Specialized AI agent for OrcaFlex hydrodynamic analysis and offshore engineering simulations. This agent provides expert assistance with OrcaFlex modeling, analysis automation, and results interpre...
type: reference
tags: []
scripts_exempt: true
---
# Orcaflex

# OrcaFlex Module Agent v2.0

## Overview
Specialized AI agent for OrcaFlex hydrodynamic analysis and offshore engineering simulations. This agent provides expert assistance with OrcaFlex modeling, analysis automation, and results interpretation.

## Capabilities

### Domain Expertise
- **Hydrodynamic Analysis**: Wave loads, vessel motions, current forces
- **Mooring System Design**: Line configuration, anchor patterns, tension analysis
- **Riser Analysis**: Static and dynamic riser behavior, interference checks
- **Installation Analysis**: Lifting operations, pipelay, cable lay simulations
- **Fatigue Assessment**: Rainflow counting, S-N curves, damage accumulation

### Supported Standards
- DNV-ST-F201 (Dynamic Risers)
- API RP 2SK (Station Keeping)
- ISO 19901-7 (Offshore Structures)

### Analysis Tools
- **OrcaFlex**: Primary analysis engine
- **OrcaWave**: Wave diffraction analysis
- **Python API**: Automation and batch processing

## Web Resources

### Official Documentation
- [OrcaFlex Documentation](https://www.orcina.com/webhelp/OrcaFlex/) - Complete user manual and theory
- [Python API Reference](https://www.orcina.com/SoftwareProducts/OrcaFlex/Documentation/Help/htm/index.htm) - Automation guide
- [Example Models](https://www.orcina.com/resources/examples/) - Industry case studies

### Standards & Guidelines
- [DNV Rules & Standards](https://www.dnv.com/rules-standards/) - Offshore certification requirements
- [API Standards](https://www.api.org/products-and-services/standards) - Industry best practices

### Technical Resources
- [Technical Papers](https://www.orcina.com/resources/papers/) - Validation studies and theory

## Usage Examples

### Basic Analysis Query
```
"Set up an OrcaFlex model for a turret-moored FPSO in 1500m water depth with 12 mooring lines"
```

### Python Automation
```python
import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.otVessel, "FPSO")
# Agent will provide complete setup code
```

### Standards Compliance
```
"Verify the mooring system design against DNV-ST-F201 requirements for a 100-year storm"
```

## Workflows

### Enhanced Specifications
- Auto-generates detailed analysis specifications
- Includes load cases, environmental conditions, and acceptance criteria
- Learning-enabled for continuous improvement

### Analysis Automation
- Batch processing for multiple load cases
- Automated result extraction and reporting
- Parametric studies and optimization

## Internal Resources
- `src/modules/hydrodynamics/` - Hydrodynamic calculation modules
- `specs/modules/marine-engineering/` - Engineering specifications
- `context/examples_knowledge.json` - Knowledge base from 54 OrcaFlex examples
- `context/examples_index.json` - Searchable index for finding relevant examples
- `context/examples_knowledge_summary.md` - Human-readable summary of examples

## Examples Knowledge Base

The agent now has access to analyzed metadata from 54 official OrcaFlex examples:
- **15 unique features** demonstrated (lazy wave, steep wave, SHEAR7 interface, etc.)
- **Multiple analysis types** covered (static, dynamic, fatigue, VIV, installation)
- **Various components** modeled (vessels, risers, moorings, buoys)
- **Searchable index** for finding relevant examples by criteria

To find examples for specific use cases:
```python
# Query the knowledge base
import json
with open('agents/orcaflex/context/examples_index.json') as f:
    index = json.load(f)

# Find examples with risers
riser_examples = index['by_component'].get('lines', [])

# Find VIV analysis examples
viv_examples = index['by_analysis'].get('viv_analysis', [])
```

## Resource Management

To add new resources:
```bash
python tools/manage-agent-resources.py add-link orcaflex --url "URL" --notes "Description"
```

To review all resources:
```bash
python tools/manage-agent-resources.py review orcaflex
```

## Version History
- **v2.0.0** (2025-08-10): Enhanced with web resources, specialized workflows, and standards compliance
- **v1.0.0** (2025-08-05): Initial creation

## Contact
For OrcaFlex-specific queries or to report issues with this agent, use the standard Agent OS feedback channels.


---

## Source: BATCH_PROCESSING_NOTES.md

# OrcaFlex Batch Processing - Implementation Notes

## Critical Learnings from Testing (2025-08-17)

### OrcaFlex API Integration

#### License Requirements
- **OrcaFlex DLL Version Tested**: 11.5e
- **API Import**: `import OrcFxAPI` 
- **Version Check**: Use `OrcFxAPI.DLLVersion()` not `Version()`
- **License Mode**: Required for actual .sim file generation

#### File Format Compatibility

##### Successful Formats
- **.dat files**: Native OrcaFlex binary format - **100% success rate**
- **.yml files**: YAML format - requires specific structure (see below)

##### YAML Structure Requirements
```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.5e
---
General:
  UnitsSystem: SI
  StageDuration:
    - 8      # Must be positive (not 0)
    - 16     # Stage durations in seconds
  ImplicitConstantTimeStep: 0.1
  
Environment:
  WaterDepth: 1500.0
  Density: 1.025        # NOT WaterDensity
  SeabedModel: Elastic  # NOT Flat (options: Elastic, Nonlinear soil model)
  SeabedNormalStiffness: 100
```

#### Common YAML Errors to Avoid
1. ❌ `WaterDensity` → ✅ `Density`
2. ❌ `SeabedModel: Flat` → ✅ `SeabedModel: Elastic`
3. ❌ `StageDuration: [0, 10]` → ✅ `StageDuration: [8, 16]` (must be positive)
4. ❌ `LineTypeOuterDiameter` → Context not recognized in certain versions

### Batch Processing Implementation

**Location**: `src/modules/orcaflex/batch_processing/` (NOT under mooring_tension_iteration)

#### File Type Detection
- Batch runner includes `FileTypeDetector` for automatic classification
- Validates model files before processing
- Categories: OrcaFlex models, includefiles, target tensions, batch configs

#### Directory Structure
```
base_directory/
├── model_files/         # .yml, .dat files
├── includefiles/        # Line length modifications
├── target_tensions/     # CSV files with target values
└── output/
    ├── sim/            # Generated .sim files
    ├── csv/            # Results CSV files
    └── batch_report_*.txt
```

#### Batch Configuration Template
```yaml
batch_info:
  name: 'Batch Run Name'
  base_directory: './path/to/models'
  output_directory: './output'
  
simulation_settings:
  analysis_type: 'statics'
  calculate_statics: true
  save_simulation_file: true
  continue_on_error: true
  parallel_processing: false  # Set true for concurrent processing
  
output_settings:
  save_simulation: true
  sim_output_folder: 'sim'
  csv_output_folder: 'csv'
  
models:
  - model_file: 'model1.yml'
    includefile: 'lengths.yml'  # Optional
    target_tensions: 'tensions.csv'  # Optional
    
mooring_parameters:
  section_to_modify: 2  # Length[2] for mooring adjustments
  tension_tolerance: 0.01
  lines_to_check: ['Line01', 'Line02']
```

### .sim File Generation Verification

#### File Size Patterns (OrcaFlex 11.5e)
- **Typical .sim file size**: 3.4-3.9 MB for moderate complexity models
- **Consistency**: Same model generates identical file sizes
- **Version differences**: ~6-7% size variation between OrcaFlex versions

#### Verified File Sizes from Testing
```
Original (May 2025):
- orcaflex_test1.sim: 3,901,138 bytes (3.72 MB)
- orcaflex_test2.sim: 3,901,132 bytes (3.72 MB)

Recent (Aug 2025):
- All files: 3,641,326 bytes (3.47 MB) - EXACT match
- Indicates consistent generation
```

### Mock Mode vs License Mode

#### Mock Mode (mock_mode=True)
- No actual OrcaFlex API calls
- Useful for testing workflow logic
- Does NOT create actual .sim files
- Returns success with simulated results

#### License Mode (mock_mode=False)
- Requires OrcaFlex license
- Creates actual .sim files
- Runs real static/dynamic analysis
- File sizes match expected patterns

### Error Handling Patterns

#### Common Errors and Solutions
1. **File Format Issues**
   - Error: "Variable 'X' not recognised"
   - Solution: Check OrcaFlex documentation for correct variable names

2. **Stage Duration**
   - Error: "Must be positive"
   - Solution: Ensure all stage durations > 0

3. **Missing Dependencies**
   - Error: "OrcFxAPI not available"
   - Solution: Verify OrcaFlex installation and Python path

4. **File Lock Issues**
   - Error: "Process cannot access file"
   - Solution: Close log files before cleanup

### Performance Metrics

#### Processing Times (from tests)
- Model loading: ~0.3-0.6 seconds
- Static analysis: ~0.9-1.2 seconds
- .sim file save: ~0.1-0.2 seconds
- Total per model: ~1.4-2.0 seconds

#### Batch Processing Results
- Success rate with .dat files: ~100%
- Success rate with .yml files: ~50% (format sensitive)
- Parallel processing: Up to 5x speedup for large batches

### Testing Commands

#### Basic Test Run
```python
from modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner import OrcaFlexBatchRunner

# With license
runner = OrcaFlexBatchRunner('batch_config.yml', mock_mode=False)
summary = runner.run_batch()

# Verify .sim files
sim_files = list(runner.sim_dir.glob("*.sim"))
for sim in sim_files:
    print(f"{sim.name}: {sim.stat().st_size / (1024*1024):.2f} MB")
```

#### UV Environment Setup
```bash
# Check UV
uv --version  # Should show 0.8.0 or higher

# Run tests
python -m pytest tests/domains/orcaflex/mooring_tension_iteration/batch_processing/test_batch_runner.py -v
```

### Key Success Factors

1. **Use .dat format** when possible for reliability
2. **Validate YAML structure** before batch processing
3. **Check file sizes** to verify successful generation
4. **Monitor log files** for detailed error messages
5. **Test with mock_mode first** then switch to license mode
6. **Keep stage durations positive** 
7. **Use exact OrcaFlex variable names** from documentation

### Future Improvements

1. Add YAML validator for OrcaFlex format
2. Implement automatic retry for transient failures
3. Add file size validation thresholds
4. Create standard test suite with known-good models
5. Add progress bar for large batch runs
6. Implement distributed processing for cluster environments

---
*Last Updated: 2025-08-17*
*Verified with OrcaFlex 11.5e*


---

## Source: context/examples_knowledge_summary.md

# OrcaFlex Examples Knowledge Base

**Last Updated:** 2025-08-20T13:10:22.009915
**Total Examples:** 54
**Source:** Orcina Examples Portal - Complete Download

## Overview

This knowledge base contains analyzed metadata from 54 official OrcaFlex examples 
downloaded from the Orcina resources portal. The examples cover 13 categories 
spanning various offshore engineering applications.

## Categories

- **UNKNOWN**: Unknown (54 examples)

## Best Practices Identified

- Most common setup: lines
- Examples cover 1 distinct application categories
- 15 unique features/configurations demonstrated

## Common Patterns

### Component Combinations

- lines: 14 examples
- buoys: 3 examples
- vessels: 2 examples
- winches: 1 examples
- buoys + lines: 1 examples

### Analysis Types

- Static Analysis: 54 examples
- Fatigue Analysis: 2 examples
- Viv Analysis: 2 examples
- Installation Analysis: 2 examples

## Configuration Types

### Riser Configurations

- Riser: 5 examples
- Lazy Wave: 4 examples
- Pliant Wave: 2 examples
- Steep Wave: 2 examples

### Vessel Types

- Vessel: 2 examples
- FPSO: 1 examples

## Unique Features Demonstrated

- Aquaculture application (1 examples)
- BOP handling (1 examples)
- Catenary configuration (3 examples)
- Chinese lantern configuration (1 examples)
- Disconnectable system (1 examples)
- Drag amplification modeling (1 examples)
- Drilling operations (1 examples)
- Heave compensation (1 examples)
- J-tube pull-in (1 examples)
- Lazy wave configuration (4 examples)
- Multibody dynamics (1 examples)
- Pliant wave configuration (2 examples)
- SHEAR7 VIV analysis interface (2 examples)
- Steep wave configuration (2 examples)
- Turret mooring system (1 examples)

## Usage

This knowledge base can be queried to find relevant examples for specific:
- Component types (vessels, lines, buoys)
- Analysis types (static, dynamic, fatigue, VIV)
- Configurations (lazy wave, steep wave, catenary)
- Applications (drilling, installation, aquaculture)

Use the `examples_index.json` for programmatic access to find examples by criteria.

---
*Generated from downloaded OrcaFlex examples on 2025-08-20 13:10:22*


---

## Source: context/web_resources_review.md

# Web Resources Review

Generated: 2025-08-10 18:01:42

Module: orcaflex


## User-Added Resources
- ✅ https://www.orcina.com/webhelp/OrcaFlex/
  Notes: OrcaFlex official documentation and API reference
  Added: 2025-08-10T18:00:46.817617
- ✅ https://www.orcina.com/resources/examples/
  Notes: OrcaFlex example models and case studies
  Added: 2025-08-10T18:00:50.834108
- ✅ https://www.orcina.com/SoftwareProducts/OrcaFlex/Documentation/Help/htm/index.htm
  Notes: OrcaFlex Python API documentation
  Added: 2025-08-10T18:00:55.124906
- ✅ https://www.orcina.com/resources/papers/
  Notes: Technical papers and validation studies for OrcaFlex
  Added: 2025-08-10T18:00:59.356965
- ✅ https://www.dnv.com/rules-standards/
  Notes: DNV offshore standards for OrcaFlex analysis validation
  Added: 2025-08-10T18:01:09.932517
- ✅ https://www.api.org/products-and-services/standards
  Notes: API standards for offshore structures and mooring systems
  Added: 2025-08-10T18:01:14.399701


---

## Source: templates/prompts/default.md

# Offshore-Engineering Agent Prompt Template

You are a specialized AI agent for {module_name} with expertise in {domain}.

## Your Role
- Provide expert analysis and recommendations
- Follow established patterns and best practices
- Reference relevant documentation and resources
- Maintain consistency with project standards

## Response Guidelines
- Be concise and actionable
- Include specific examples when helpful
- Reference relevant documentation
- Suggest next steps for implementation

## Context
{context}

## Query
{query}



---

## Source: templates/responses/default.md

# Offshore-Engineering Agent Response Template

## Standard Response Format

**Analysis:** [Your analysis here]

**Recommendations:** 
- [Recommendation 1]
- [Recommendation 2]

**Next Steps:**
1. [Step 1]
2. [Step 2]

**Resources:**
- [Resource 1]
- [Resource 2]

