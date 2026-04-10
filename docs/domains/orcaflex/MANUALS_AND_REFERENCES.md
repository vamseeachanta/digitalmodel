# OrcaFlex & OrcaWave Manuals and References

Last Updated: 2026-04-10

## Official Online Documentation

### OrcaFlex (Dynamic Analysis)
| Resource | URL |
|----------|-----|
| OrcaFlex Help (online) | https://www.orcina.com/webhelp/OrcaFlex/Default.htm |
| OrcaFlex Help (download) | https://www.orcina.com/wp-content/uploads/releases/114/OrcaFlexHelp.zip |
| Resources & Examples | https://www.orcina.com/resources/ |
| Documentation Landing | https://www.orcina.com/resources/documentation/ |
| Technical Papers | https://www.orcina.com/resources/papers/ |
| Release Notes | https://www.orcina.com/releases/ |

### OrcaWave (Diffraction/Radiation)
| Resource | URL |
|----------|-----|
| OrcaWave Help (online) | https://www.orcina.com/webhelp/OrcaWave/Default.htm |
| OrcaWave Help (download) | https://www.orcina.com/wp-content/uploads/releases/114/OrcaWaveHelp.zip |

### OrcFxAPI (Programming Interface)
| Resource | URL |
|----------|-----|
| OrcFxAPI Help (online) | https://www.orcina.com/webhelp/OrcFxAPI/Default.htm |
| OrcFxAPI Help (download) | https://www.orcina.com/wp-content/uploads/orcaflex/help/OrcFxAPIHelp.zip |
| Python Interface Training | https://www.orcina.com/wp-content/uploads/training/An%20introduction%20to%20the%20Python%20interface%20to%20OrcaFlex.zip |

### Key Topic Pages (Direct Links)
| Topic | OrcaFlex URL |
|-------|-------------|
| Text Data Files (YAML) | https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm |
| YAML Examples | https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles,Examplesofsettingdata.htm |
| Variation Models | https://www.orcina.com/webhelp/OrcaFlex/Content/html/Variationmodels.htm |
| Data Files Overview | https://www.orcina.com/webhelp/OrcaFlex/Content/html/Datafiles.htm |
| OrcaWave Text Files | https://www.orcina.com/webhelp/OrcaWave/Content/html/Userinterface,Textdatafiles.htm |
| OrcaWave Data: Environment | https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Environment.htm |
| OrcaWave Data: Calculation | https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Calculationandoutput.htm |
| OrcaWave Results: Load RAOs | https://www.orcina.com/webhelp/OrcaWave/Content/html/Results,LoadRAOs.htm |

## Current Software Versions

- OrcaFlex 11.6c (released 2026-02-23)
- OrcaFlex 11.6 (released 2025-11-13)
- DLL Version tested in repo: 11.5e

## YAML Format Reference (OrcaFlex & OrcaWave)

### File Structure
```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.6c
---
General:
  UnitsSystem: SI
  ...
Environment:
  WaterDepth: 1500.0
  ...
...
```

### Critical Rules
1. UTF-8 encoding mandatory
2. Indentation with SPACE characters only (convention: 2 spaces), never TAB
3. Name: value syntax (colon + space before value)
4. Referenced objects must precede references (line types before lines, vessels before connections)
5. Only active data can appear in YAML; set configuration flags before dependent parameters
6. Comments (`# text`) are not preserved when OrcaFlex re-saves the file
7. IncludeFile directive for modular composition
8. Expression evaluator with `=` prefix (e.g., `=4+3`, supports `pi`, trig functions, `if()`)
9. Variables section for reusable named values

### File Types
| Extension | Format | Notes |
|-----------|--------|-------|
| `.dat` | Binary | Stronger version compatibility, stores hidden/inactive data |
| `.yml` | YAML text | Human-readable, active data only, ideal for QA/archival/automation |
| `.sim` | Simulation | Generated analysis results (3-4 MB typical) |

### Variation Models (Parametric Studies)
```yaml
BaseFile: parent.dat
Environment:
  WaveTrains:
    Wave1:
      WaveHeight: 2.2
```
Variation files store only differences from a parent model. Changes to parent auto-propagate.

### Common YAML Pitfalls (from repo experience)
| Wrong | Correct | Notes |
|-------|---------|-------|
| `WaterDensity` | `Density` | Under Environment section |
| `SeabedModel: Flat` | `SeabedModel: Elastic` | Valid options: Elastic, Nonlinear soil model |
| `StageDuration: [0, 10]` | `StageDuration: [8, 16]` | All stage durations must be positive |
| Hybrid YAML structure | Flat or include-based only | See CRITICAL_YAML_FORMAT_FINDINGS.md |

## OrcaWave-Specific Reference

### Capabilities
- Diffraction/radiation analysis via potential flow theory
- Added mass and damping matrices
- Load RAOs and displacement RAOs
- QTF (Quadratic Transfer Function) calculations
- Multi-body interaction analysis
- Results export to OrcaFlex (.owr files)

### OrcaWave File Types
| Extension | Format |
|-----------|--------|
| `.owd` | Binary model data |
| `.yml` | YAML text model |
| `.owr` | Results file (importable by OrcaFlex) |
| `.gdf` | Panel mesh geometry |

### OrcaWave YAML Ordering Constraints
- Referenced objects must appear before references
- Solve type must be set before QTF calculation method data
- Active data must precede dependent settings

## OrcFxAPI Reference

### Supported Languages
- Python (recommended for automation/scripting)
- MATLAB (recommended for automation/scripting)
- C++ (recommended for software integration)
- Delphi (recommended for software integration)

### Python Quick Start
```python
import OrcFxAPI

# Version check
print(OrcFxAPI.DLLVersion())  # Not Version()

# Create and manipulate model
model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.otVessel, "FPSO")
model.CalculateStatics()
model.SaveData("output.dat")
model.SaveSimulation("output.sim")
```

## In-Repo Documentation Map

### Skills (agent capabilities)
| Skill | Path |
|-------|------|
| OrcaFlex core agent | `.claude/skills/engineering/orcaflex-agents/orcaflex/` |
| OrcaWave core agent | `.claude/skills/engineering/orcawave-agents/orcawave/` |
| Line wizard | `.claude/skills/orcaflex-line-wizard/` |
| Batch manager | `.claude/skills/orcaflex-batch-manager/` |
| File conversion | `.claude/skills/orcaflex-file-conversion/` |
| Post-processing | `.claude/skills/orcaflex-post-processing/` |
| Static debug | `.claude/skills/orcaflex-static-debug/` |
| Vessel setup | `.claude/skills/orcaflex-vessel-setup/` |
| Modal analysis | `.claude/skills/orcaflex-modal-analysis/` |
| Installation analysis | `.claude/skills/orcaflex-installation-analysis/` |
| Mooring iteration | `.claude/skills/orcaflex-mooring-iteration/` |
| Extreme analysis | `.claude/skills/orcaflex-extreme-analysis/` |
| Operability | `.claude/skills/orcaflex-operability/` |
| RAO import | `.claude/skills/orcaflex-rao-import/` |
| Results comparison | `.claude/skills/orcaflex-results-comparison/` |
| Visualization | `.claude/skills/orcaflex-visualization/` |
| Code check | `.claude/skills/orcaflex-code-check/` |
| OrcaWave mesh gen | `.claude/skills/orcawave-mesh-generation/` |
| OrcaWave damping sweep | `.claude/skills/orcawave-damping-sweep/` |
| OrcaWave multi-body | `.claude/skills/orcawave-multi-body/` |
| OrcaWave QTF analysis | `.claude/skills/orcawave-qtf-analysis/` |
| OrcaWave to OrcaFlex | `.claude/skills/orcawave-to-orcaflex/` |

### Domain Docs
| Doc | Path | Purpose |
|-----|------|---------|
| YAML format findings | `docs/domains/orcaflex/CRITICAL_YAML_FORMAT_FINDINGS.md` | Known YAML pitfalls |
| File requirements | `docs/domains/orcaflex/FILE_REQUIREMENTS.md` | Format specs |
| Human-friendly YAML | `docs/domains/orcaflex/HUMAN_FRIENDLY_YAML_FORMAT.md` | Readable format guide |
| Batch YAML | `docs/domains/orcaflex/orcaflex-batch-yml.md` | Batch config format |
| Examples catalog | `docs/domains/orcaflex/examples/catalog/` | 54 analyzed examples |
| OrcaWave examples | `docs/domains/orcawave/examples/` | L01-L04 worked examples |
| Batch processing notes | SKILL.md (orcaflex agent) | API integration learnings |

### Examples Knowledge Base
- 54 official OrcaFlex examples analyzed
- Index: `.claude/skills/engineering/orcaflex-agents/orcaflex/context/examples_index.json`
- Knowledge: `.claude/skills/engineering/orcaflex-agents/orcaflex/context/examples_knowledge.json`
- Summary: `.claude/skills/engineering/orcaflex-agents/orcaflex/context/examples_knowledge_summary.md`

## LLM Ingestion Notes

### Current State
No dedicated "llm-wiki" skill exists in the ecosystem. The closest pipeline is the
document-index-pipeline at `.claude/skills/data/document-index-pipeline/` which handles
the 1M+ document corpus (Phase A-G pipeline for O&G standards). That pipeline is designed
for PDF extraction and classification, not for web-scraped HTML documentation.

### Recommended Ingestion Approach
1. **Download offline help**: Use the OrcaFlexHelp.zip and OrcaWaveHelp.zip download links
   above to get the complete HTML documentation locally
2. **Convert to markdown**: Use markitdown (already in the repo venv) to convert HTML help
   topics to markdown for LLM consumption
3. **Store on ace drive**: Per project convention (#1540), generated/extracted data goes to
   the ace drive, not git: `/mnt/ace/digitalmodel/docs/orcaflex-help/` and
   `/mnt/ace/digitalmodel/docs/orcawave-help/`
4. **Index**: Add entries to the document-index pipeline (Phase A) pointing to the
   converted markdown files
5. **Skill reference**: Update the orcaflex and orcawave agent SKILL.md files to point to
   the local help paths

### What an llm-wiki Skill Would Need
- Web scraper targeting `www.orcina.com/webhelp/` HTML structure
- HTML-to-markdown converter (markitdown available)
- Chunking strategy respecting topic boundaries (one .htm page = one chunk)
- Metadata extraction (topic title, section hierarchy, cross-references)
- Storage convention: `/mnt/ace/digitalmodel/llm-wiki/<product>/`
- Search/retrieval index for agent queries
