---
name: orcaflex-model-sanitization
description: Sanitize and normalize OrcaFlex model files by fixing structural issues, inconsistent settings, and schema mismatches. Use when preparing models for reliable batch execution and downstream tooling.
---

# OrcaFlex Model Sanitization Skill

## Description
Sanitize OrcaFlex models from client projects by stripping identifiable references (project names, vessel names, operator codes, user/machine metadata), converting binary `.dat` to YAML `.yml`, deduplicating, and organizing into the reference model library.

## When to Use
- Importing OrcaFlex models from external/client sources
- Converting `.dat` (binary) to `.yml` (YAML) format
- Stripping client-identifiable references before committing
- Organizing models into the `docs/modules/orcaflex/` library structure
- Deduplicating model collections (hash-based)

## Sanitization Pipeline

### Overview
```
Source .dat/.yml → Dedup → Convert → Sanitize Text → Strip Metadata → Organize → Legal Scan → Extract Spec
```

### Step 1: Inventory & Dedup
- Walk source directory for `.dat` and `.yml` files
- Compute SHA-256 hash for each file
- Skip files with identical content (keep first occurrence)
- Exclude: oversized files (>50MB), non-model files (CAD, reports, scripts)

### Step 2: Format Conversion (.dat → .yml)
```python
import OrcFxAPI
model = OrcFxAPI.Model(str(dat_path))
model.SaveData(str(output_yml_path))
```

**Gotchas:**
- `SaveData()` embeds `User:`, `Machine:`, `File:` metadata in YAML header
- `SaveData()` exports ALL properties including dormant ones
- Binary `.dat` files may reference external DLLs or data files
- Large models (>50MB) may timeout — skip and log

### Step 3: Text-Based Sanitization
Apply regex replacements using a mapping table:

```python
SANITIZATION_MAP = {
    "ClientProject": "generic_project_a",
    "VesselName": "installation_vessel_01",
    # ... sorted by key length descending to avoid partial matches
}

# Sort keys longest-first
sorted_keys = sorted(SANITIZATION_MAP.keys(), key=len, reverse=True)

text = Path(yml_file).read_text(encoding="utf-8", errors="replace")
for key in sorted_keys:
    replacement = SANITIZATION_MAP[key]
    text = text.replace(key, replacement)
```

**Critical: Sort by length descending** — prevents "Seven Arctic" from being partially matched by "Seven".

### Step 4: Strip OrcaFlex Metadata
Remove embedded user/machine/file path lines:
```python
import re
# Remove User/Machine/File header lines
text = re.sub(r"^User:.*$", "", text, flags=re.MULTILINE)
text = re.sub(r"^Machine:.*$", "", text, flags=re.MULTILINE)
text = re.sub(r"^File:.*$", "", text, flags=re.MULTILINE)
# Clean up resulting blank lines
text = re.sub(r"\n{3,}", "\n\n", text)
```

### Step 5: Organize into Library
Target structure:
```
docs/modules/orcaflex/<category>/
├── <subcategory>/
│   ├── monolithic/        # Sanitized original YAML
│   │   ├── model_SZ.yml
│   │   └── model_DZ.yml
│   └── spec.yml           # Extracted spec for modular builder
```

Categories: `jumper/`, `installation/`, `mooring/`, `training/`, `regional/`, `vessel_raos/`

### Step 6: Legal Scan Gate
```bash
bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel
# Must exit 0 before proceeding to spec extraction
```

### Step 7: Extract Spec
Only after legal scan passes:
```python
from digitalmodel.solvers.orcaflex.modular_generator.extractor import MonolithicExtractor
extractor = MonolithicExtractor(Path("monolithic/model.yml"))
spec_dict = extractor.extract()
```

## Sanitization Mapping Best Practices

### Pattern Categories
| Category | Examples | Replacement Convention |
|----------|----------|----------------------|
| Project names | Field names, codenames | `deepwater_field_a`, `shallow_field_b` |
| Vessel names | Installation vessels, rigs | `installation_vessel_01`, `fpso_01` |
| Operator names | Oil companies, contractors | `operator_a`, `contractor_b` |
| Location names | Pipeline routes, regions | `pipeline_route_a`, `region_b` |
| User/machine IDs | Hostnames, usernames | Remove entirely (empty string) |

### Rules
1. **Longest match first** — sort replacement keys by length descending
2. **Case variants** — include both original case and common variants (CamelCase, snake_case, lowercase)
3. **Empty replacements** — for user/machine IDs, replace with empty string and clean up artifacts
4. **Audit trail** — log every transformation to `sanitization_audit.json`

## Deny List Integration

The sanitization mapping should be mirrored in `.legal-deny-list.yaml`:
- Every key in `SANITIZATION_MAP` → pattern in deny list with `severity: block`
- The sanitization script itself is excluded from scanning

## Commands

### Run sanitization
```bash
uv run python scripts/sanitize_s7_models.py \
  --s7-root D:/workspace-hub/rock-oil-field/s7 \
  --output-root docs/modules/orcaflex \
  --dry-run  # Preview first
```

### Run without OrcFxAPI (.yml only)
```bash
uv run python scripts/sanitize_s7_models.py \
  --s7-root D:/workspace-hub/rock-oil-field/s7 \
  --output-root docs/modules/orcaflex \
  --skip-dat
```

### Verify sanitization
```bash
# Check no client references remain
grep -ri "ClientProject\|VesselName" docs/modules/orcaflex/

# Run legal scan
bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel
```

## Audit Output

The sanitization script generates `sanitization_audit.json`:
```json
{
  "timestamp": "2026-02-11T10:00:00",
  "source_root": "D:/workspace-hub/rock-oil-field/s7",
  "files_processed": 180,
  "files_skipped_dedup": 45,
  "files_skipped_excluded": 12,
  "files_failed": 3,
  "categories": {
    "jumper": 15,
    "installation": 80,
    "mooring": 10
  },
  "transformations": [
    {
      "source": "s7/ballymore/Jumper_Manifold to PLET/SZ.yml",
      "target": "docs/modules/orcaflex/jumper/manifold_to_plet/monolithic/SZ.yml",
      "hash": "sha256:abc123...",
      "replacements": ["Ballymore→deepwater_field_a", "Candies→installation_vessel_01"],
      "size_bytes": 245000
    }
  ]
}
```

## Related Skills
- `/legal-sanity-scan` — Legal compliance scanning
- `/orcaflex-file-conversion` — Format conversion (.dat ↔ .yml)
- `/orcaflex-monolithic-to-modular` — Monolithic → modular conversion
- `/orcaflex-jumper-analysis` — Jumper-specific modelling concepts
- `/orcaflex-model-generator` — Modular generation from spec.yml
