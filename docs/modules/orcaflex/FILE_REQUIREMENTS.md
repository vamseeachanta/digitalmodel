# OrcaFlex File Format Requirements & Critical Guidelines

## ⚠️ CRITICAL: Read This Before Creating Any OrcaFlex Files

**OrcaFlex is EXTREMELY picky about file encoding and format.** Even minor deviations will cause cryptic errors. This document captures all known requirements and common pitfalls.

---

## 1. File Encoding Requirements

### ✅ REQUIRED: UTF-8 with BOM (Byte Order Mark)

All OrcaFlex YAML configuration files MUST:
- Use **UTF-8 encoding**
- Include **exactly ONE** BOM at the file start
- BOM bytes: `EF BB BF` (hex)

### Verification Commands

```bash
# Check for BOM (should show: ef bb bf followed by content)
od -An -tx1 -N10 filename.yml

# Check encoding (should show: charset=utf-8)
file -i filename.yml

# Compare with working reference file
od -An -tx1 -N10 reference_file.yml
```

---

## 2. Common Errors & Causes

### Error: "Did not find expected <document start> at Line 2, Column 1"

**Root Cause:** Double BOM in file

**How it happens:**
```python
# ❌ WRONG - Creates double BOM
content = f"\ufeffBaseFile: ../base_files/config.yml\n"
with open(filepath, 'w', encoding='utf-8-sig') as f:
    f.write(content)
# Results in: ef bb bf ef bb bf 42 61... (DOUBLE BOM!)

# ✅ CORRECT - Single BOM
content = f"BaseFile: ../base_files/config.yml\n"
with open(filepath, 'w', encoding='utf-8-sig') as f:
    f.write(content)
# Results in: ef bb bf 42 61... (SINGLE BOM)
```

**Binary Analysis:**
- ❌ Double BOM: `ef bb bf ef bb bf 42 61 73 65...`
- ✅ Single BOM: `ef bb bf 42 61 73 65 46 69 6c...`
  - `42 61 73 65` = "Base"
  - `46 69 6c` = "Fil"

---

## 3. File Creation Best Practices

### Method 1: Python with utf-8-sig (Recommended)

```python
# DO NOT include \ufeff in content string
content = f"BaseFile: ../base_files/config.yml\nincludefile: ../base_files/env/fc001.yml\n\n"

with open(filepath, 'w', encoding='utf-8-sig') as f:
    f.write(content)
```

### Method 2: Binary Write with Explicit BOM

```python
# Manually add BOM as bytes
bom = b'\xef\xbb\xbf'
content = "BaseFile: ../base_files/config.yml\n"

with open(filepath, 'wb') as f:
    f.write(bom)
    f.write(content.encode('utf-8'))
```

### Method 3: Bash/Shell Script

```bash
# Add BOM to existing file
printf '\xef\xbb\xbf' > temp.yml
cat original.yml >> temp.yml
mv temp.yml original.yml
```

---

## 4. Fixing Double BOM Issues

### Python Script to Fix Double BOM

```python
import os

def fix_double_bom(filepath):
    """Remove double BOM from file, keeping only one."""
    with open(filepath, 'rb') as f:
        content = f.read()

    # Check for double BOM
    if content.startswith(b'\xef\xbb\xbf\xef\xbb\xbf'):
        # Remove first BOM, keep second
        content = content[3:]

        with open(filepath, 'wb') as f:
            f.write(content)
        return True
    return False

# Fix all files in directory
for filename in os.listdir(directory):
    if filename.endswith('.yml'):
        filepath = os.path.join(directory, filename)
        if fix_double_bom(filepath):
            print(f"Fixed: {filename}")
```

---

## 5. File Structure Requirements

### YAML File Structure
- Must start **immediately after BOM** with YAML content
- No extra whitespace or characters before document start
- No blank lines before first key
- Proper YAML indentation (2 spaces)

### Example Correct File

```yaml
BaseFile: ../base_files/fsts_l015_mwl_fatigue.yml
includefile: ../base_files/env/fc001.yml

```

**Note:** File ends with blank line

---

## 6. Project-Specific File Patterns

### Environment Files
- **Location:** `D:\1522\ctr9\fatigue_full\rev_a08\base_files\env\`
- **Naming:** `fc001.yml` through `fc081.yml`
- **Encoding:** UTF-8 with single BOM
- **Structure:**
  ```yaml
  Environment:
    WaveTrains:
      Wave1:
        WaveDirection: 150.00
        WaveGamma: 3.00
        WaveHs: 0.15
        WaveTp: 2.50
    RefCurrentSpeed: 0.00
    RefCurrentDirection: 150.00
    WindSpeed: 5.00
    WindDirection: 180.00
  ```

### Run Files
- **Location:** `D:\1522\ctr9\fatigue_full\rev_a08\07c_fatigue\`
- **Naming Pattern:** `{config_name}_fc{###}.yml`
- **Encoding:** UTF-8 with single BOM
- **Structure:**
  ```yaml
  BaseFile: ../base_files/{configuration}.yml
  includefile: ../base_files/env/fc{###}.yml

  ```

### Configuration Types
1. `fsts_l015_mwl` - Base: `fsts_l015_mwl_fatigue.yml`
2. `fsts_l095_mwl` - Base: `fsts_l095_mwl_fatigue.yml`
3. `fsts_l015_125km3_l100_pb_mwl` - Base: `fsts_l015_mwl_125km3_l100_pb_pretension_initialpos.yml`
4. `fsts_l095_125km3_l000_pb_mwl` - Base: `fsts_l095_mwl_125km3_l000_pb_pretension_initialpos.yml`

---

## 7. Testing & Validation Checklist

Before using files in OrcaFlex:

- [ ] Verify single BOM: `od -An -tx1 -N10 file.yml`
- [ ] Check encoding: `file -i file.yml`
- [ ] Compare with reference file byte-for-byte
- [ ] Test with one file in OrcaFlex before batch processing
- [ ] Validate YAML structure (no tabs, proper indentation)
- [ ] Ensure file ends with blank line

---

## 8. Reference Files (Known Good)

Always compare new files against these verified working files:
- `D:\1522\ctr9\fatigue_full\rev_a08\base_files\env\fat_wave01.yml`
- `D:\1522\ctr9\fatigue_full\rev_a08\07c_fatigue\go_by\fsts_l015_mwl_wave01.yml`

---

## 9. Troubleshooting Guide

| Error Message | Likely Cause | Solution |
|--------------|--------------|----------|
| "Did not find expected <document start> at Line 2, Column 1" | Double BOM | Use fix_double_bom script |
| File won't load | Missing BOM | Add UTF-8 BOM to file |
| Encoding error | Wrong charset | Convert to UTF-8 with BOM |
| Parse error at line 1 | Invalid characters before YAML | Check for extra whitespace/chars |

---

## 10. Historical Issues & Resolutions

### Issue 1: Double BOM in Generated Files (2025-10-08)
- **Problem:** 324 run files had double BOM causing OrcaFlex errors
- **Cause:** Python script used `encoding='utf-8-sig'` AND content had `\ufeff` prefix
- **Resolution:** Removed first BOM from all files
- **Prevention:** Never use `\ufeff` with `utf-8-sig` encoding

### Issue 2: Missing BOM in fc*.yml Files
- **Problem:** Environment files created without BOM (US-ASCII)
- **Cause:** Python Write tool didn't specify encoding
- **Resolution:** Added BOM to all 81 files
- **Prevention:** Always use `encoding='utf-8-sig'` or explicit BOM

---

## 11. Quick Reference Commands

```bash
# Count files in directory
ls -1 *.yml | wc -l

# Check all files for proper BOM
for file in *.yml; do echo "$file:"; od -An -tx1 -N3 "$file"; done

# Find files without BOM
for file in *.yml; do
  bom=$(od -An -tx1 -N3 "$file" | xargs)
  if [ "$bom" != "ef bb bf" ]; then
    echo "Missing/incorrect BOM: $file"
  fi
done

# Find files with double BOM
for file in *.yml; do
  bom=$(od -An -tx1 -N6 "$file" | xargs)
  if [ "$bom" = "ef bb bf ef bb bf" ]; then
    echo "Double BOM: $file"
  fi
done
```

---

## 12. File Generation Template (Python)

```python
#!/usr/bin/env python3
"""
Template for generating OrcaFlex-compatible YAML files.
Ensures proper UTF-8 BOM encoding.
"""

import os

def create_orcaflex_file(filepath, content):
    """
    Create OrcaFlex YAML file with proper encoding.

    Args:
        filepath: Full path to file
        content: String content (NO \\ufeff prefix)
    """
    # Ensure content doesn't have BOM character
    if content.startswith('\ufeff'):
        raise ValueError("Content should not include \\ufeff - encoding handles BOM")

    # Write with utf-8-sig to add BOM automatically
    with open(filepath, 'w', encoding='utf-8-sig') as f:
        f.write(content)

    # Verify single BOM
    with open(filepath, 'rb') as f:
        first_bytes = f.read(6)

    if first_bytes.startswith(b'\xef\xbb\xbf\xef\xbb\xbf'):
        raise RuntimeError(f"Double BOM detected in {filepath}")
    elif not first_bytes.startswith(b'\xef\xbb\xbf'):
        raise RuntimeError(f"Missing BOM in {filepath}")

    return True

# Example usage
if __name__ == "__main__":
    # Environment file
    env_content = """Environment:
  WaveTrains:
    Wave1:
      WaveDirection: 150.00
      WaveGamma: 3.00
      WaveHs: 0.15
      WaveTp: 2.50
  RefCurrentSpeed: 0.00
  RefCurrentDirection: 150.00
  WindSpeed: 5.00
  WindDirection: 180.00
"""

    # Run file
    run_content = """BaseFile: ../base_files/fsts_l015_mwl_fatigue.yml
includefile: ../base_files/env/fc001.yml

"""

    create_orcaflex_file("test_env.yml", env_content)
    create_orcaflex_file("test_run.yml", run_content)
```

---

## 13. Summary: Golden Rules for OrcaFlex Files

1. ✅ **ALWAYS** use UTF-8 encoding with BOM
2. ✅ **ALWAYS** verify single BOM (not double)
3. ✅ **NEVER** mix `\ufeff` character with `utf-8-sig` encoding
4. ✅ **ALWAYS** test against reference files
5. ✅ **ALWAYS** validate with hex dump before using in OrcaFlex
6. ✅ **ALWAYS** use proper YAML structure (no tabs, 2-space indent)
7. ✅ **ALWAYS** end files with blank line

---

**Last Updated:** 2025-10-08
**Validated Against:** OrcaFlex (version used in D:\1522\ctr9\fatigue_full\rev_a08 project)
**Files Successfully Created:** 405 total (81 environment + 324 run files)
