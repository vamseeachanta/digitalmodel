---
title: Convert OrcaFlex .dat to .yml - Energy Pipeline Installation MP
description: Batch convert 189 OrcaFlex binary .dat files to YAML (.yml) format
version: "1.0"
module: orcaflex
session:
  id: parallel-nibbling-aho
  agent: orchestrator
review: pending
---

# Convert OrcaFlex .dat to .yml

## Scope

Convert all 189 OrcaFlex binary `.dat` input files to `.yml` in-place (alongside originals) in:
`D:\workspace-hub\client_projects\energy_pipeline_installation_mp`

## Approach

Use the existing `OrcaFlexConverterEnhanced` batch CLI - no new code needed.

**Key file:** `src/digitalmodel/orcaflex/orcaflex_converter_enhanced.py:535` (`main()` CLI)

### Why this tool
- Recursive file discovery via glob (`**/*.dat`)
- When `input_dir == output_dir`, places `.yml` alongside each `.dat` (line 169-171)
- Retry with exponential backoff (line 194-230)
- Skip-if-exists for safe re-runs (line 183-186)
- Progress bar via tqdm
- Generates `conversion_report.md` + `conversion_report.json`

## Execution Steps

### Step 1: Verify OrcFxAPI

```bash
cd D:\workspace-hub\digitalmodel
uv run python -c "import OrcFxAPI; print(OrcFxAPI.Version())"
```

If this fails, OrcFxAPI is not installed or not licensed - conversion cannot proceed.

### Step 2: Test single file

```bash
uv run python -m digitalmodel.orcaflex.orcaflex_converter_enhanced ^
  "D:\workspace-hub\client_projects\energy_pipeline_installation_mp\12. WERL-PRPP-MS-RPT-026\Rev 0\Analysis\SB-SA_NL\Max_WD.dat" ^
  --verbose
```

Confirm `Max_WD.yml` appears in the same directory and contains valid OrcaFlex YAML (sections like `General`, `Environment`, etc.).

### Step 3: Batch convert all 189 files

```bash
uv run python -m digitalmodel.orcaflex.orcaflex_converter_enhanced ^
  --input-dir "D:\workspace-hub\client_projects\energy_pipeline_installation_mp" ^
  --output-dir "D:\workspace-hub\client_projects\energy_pipeline_installation_mp" ^
  --pattern "**/*.dat" ^
  --output-format yml ^
  --max-retries 3 ^
  --verbose
```

**Behavior:**
- Finds all `.dat` files recursively
- Converts each to `.yml` in the same directory
- Skips any already-converted files
- Retries failures up to 3 times with backoff
- Prints progress bar and per-file status

### Step 4: Verify results

1. Check the generated report:
   `D:\workspace-hub\client_projects\energy_pipeline_installation_mp\conversion_report.md`

2. Count output files (expect 189 `.yml` files matching the 189 `.dat` files):
   ```bash
   find "D:/workspace-hub/client_projects/energy_pipeline_installation_mp" -name "*.yml" | wc -l
   ```

3. Spot-check a converted file for valid content (should have OrcaFlex sections like `General`, `Environment`, `Line`, `Vessel`, etc.)

## Notes

- **No parallel mode** - OrcFxAPI licensing may not support concurrent instances; sequential is safe and sufficient for 189 files (~260KB each)
- **No new code required** - existing tooling handles this completely
- **Idempotent** - safe to re-run if interrupted; already-converted files are skipped
- **Reports** - `conversion_report.md` and `conversion_report.json` are written to the project root directory

## Files Involved

| File | Role |
|------|------|
| `src/digitalmodel/orcaflex/orcaflex_converter_enhanced.py` | Primary converter + CLI |
| `src/digitalmodel/orcaflex/convert_cli.py` | Alternative CLI wrapper (same functionality) |
| `src/digitalmodel/orcaflex/orcaflex_yml_converter.py` | Simple fallback if enhanced has import issues |

## Verification

- [ ] OrcFxAPI imports and reports version
- [ ] Single file test produces valid `.yml`
- [ ] Batch conversion completes with 189 successful, 0 failed
- [ ] `conversion_report.md` shows 100% success rate
- [ ] Spot-check 2-3 `.yml` files from different subdirectories for valid OrcaFlex content
