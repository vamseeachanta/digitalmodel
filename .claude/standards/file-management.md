# File Management Standards

## 🚨 CRITICAL: User Input File Management Protocol

### MANDATORY Requirements

All AI agents MUST follow these file management protocols without exception.

## User-Provided Input Files

### Naming Convention
**Required Format**: `user_<descriptor>_YYYYMMDD_<optional_version>.<ext>`

#### Examples
```
✅ CORRECT:
user_test_data_20250121.csv
user_mooring_config_20250121_v2.yml
user_requirements_spec_20250121.docx
user_orcaflex_model_20250121.dat

❌ INCORRECT:
data.csv                  # No user identification
test.xlsx                 # Ambiguous name
input.txt                 # No context or date
my_file.pdf              # Personal naming
temp.json                # No meaningful descriptor
```

### Permission Protocol

**MANDATORY**: Before saving ANY user-provided file:

```markdown
📁 FILE SAVE PERMISSION REQUEST:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━
User provided: [original_filename.ext]
Proposed name: user_<descriptor>_YYYYMMDD.<ext>
Proposed location: /inputs/user_provided/YYYYMMDD_<descriptor>/
Purpose: [specific reason for saving]

Options:
1. ✅ Save permanently to repository
2. 📋 Use temporarily (auto-cleanup after task)
3. 📂 Save to different location
4. ❌ Skip saving (use in memory only)

May I proceed with option 1 at the proposed location?
```

### Directory Structure for User Inputs

```
inputs/                              # Primary user input location
├── user_provided/                   # All user-provided files
│   ├── YYYYMMDD_<descriptor>/      # Date-organized folders
│   │   ├── user_data_YYYYMMDD.csv
│   │   ├── metadata.md             # Describe contents
│   │   └── source.txt              # Origin information
│   └── README.md                    # Index of all inputs
├── external/                        # External system data
│   ├── api_responses/
│   ├── database_exports/
│   └── third_party/
└── temporary/                       # Pending review/cleanup
    └── .gitignore                   # Ignore all temp files
```

### Module-Specific Inputs

For module-specific user inputs:
```
specs/modules/<module>/
├── input/                           # Module-specific inputs
│   ├── user_<descriptor>_YYYYMMDD/
│   └── README.md
```

## Repository Root Hygiene

### ABSOLUTELY PROHIBITED in Root

The following MUST NEVER be placed in repository root:

```
❌ NEVER IN ROOT:
- User input files (*.csv, *.xlsx, *.json, *.txt)
- Test files (test_*.py, *_test.py)
- Data files (any data format)
- Temporary files (*.tmp, *.temp, *.bak)
- Generated outputs (*.out, *.log, results.*)
- Working files (scratch.*, temp.*, work.*)
- Personal files (my_*, personal_*)
- Unorganized configs (random *.yml, *.ini)
```

### ALLOWED in Root

```
✅ ONLY THESE IN ROOT:
Essential configs:
- .gitignore, .editorconfig
- pyproject.toml, uv.toml, setup.py
- .coveragerc, .gitmodules

Required docs:
- README.md
- LICENSE
- CLAUDE.md
- Makefile

Main directories:
- src/, tests/, docs/
- tools/, specs/
- .agent-os/, agents/
- inputs/, outputs/

Convenience scripts:
- *.sh (redirecting to tools/)
```

### Root Violation Response

If file MUST be in root temporarily:

```python
# 1. JUSTIFY
"""
ROOT EXCEPTION REQUIRED:
File: example.tmp
Reason: Build system requires root location
Duration: ~5 minutes
Cleanup: Automatic after build
"""

# 2. TRACK
# Add to .gitignore immediately
echo "*.tmp" >> .gitignore

# 3. NOTIFY
print("⚠️ Temporary root file created: example.tmp")
print("Will be cleaned up automatically after build")

# 4. CLEANUP
import atexit
atexit.register(lambda: os.remove("example.tmp"))
```

## File Operations Best Practices

### Before Creating Any File

```python
def validate_file_location(filepath: str) -> bool:
    """Validate file is not being created in root."""
    
    # Check if file would be in root
    if "/" not in filepath and "\\" not in filepath:
        raise ValueError(
            f"❌ Cannot create '{filepath}' in root directory.\n"
            f"Please specify appropriate subdirectory:\n"
            f"  - User inputs: inputs/user_provided/\n"
            f"  - Tests: tests/modules/<module>/\n"
            f"  - Tools: tools/\n"
            f"  - Docs: docs/domains/<module>/"
        )
    
    # Check for user input files
    if "user" in filepath or any(ext in filepath for ext in ['.csv', '.xlsx', '.json']):
        if not filepath.startswith(('inputs/', 'specs/modules/')):
            print(f"⚠️ User file should be in inputs/ directory")
            return False
    
    return True
```

### File Cleanup Protocol

```python
import os
import glob
from datetime import datetime, timedelta

def cleanup_repository_root():
    """Clean repository root of violations."""
    
    violations = []
    
    # Check for test files
    violations.extend(glob.glob("test_*.py"))
    violations.extend(glob.glob("*_test.py"))
    
    # Check for data files
    violations.extend(glob.glob("*.csv"))
    violations.extend(glob.glob("*.xlsx"))
    violations.extend(glob.glob("*.json"))
    
    # Check for temp files
    violations.extend(glob.glob("*.tmp"))
    violations.extend(glob.glob("*.temp"))
    violations.extend(glob.glob("*.bak"))
    
    if violations:
        print(f"🧹 Found {len(violations)} files violating root hygiene:")
        for file in violations:
            print(f"  - {file}")
            
        # Move or delete based on type
        for file in violations:
            if "test" in file:
                dest = f"tests/{file}"
            elif any(ext in file for ext in ['.csv', '.xlsx', '.json']):
                dest = f"inputs/temporary/{file}"
            else:
                os.remove(file)
                print(f"  ✓ Deleted: {file}")
                continue
                
            os.rename(file, dest)
            print(f"  ✓ Moved: {file} → {dest}")
```

## Automated Enforcement

### Git Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Check for root directory violations
violations=$(find . -maxdepth 1 -type f \
    \( -name "test_*.py" -o -name "*.csv" -o -name "*.xlsx" \
       -o -name "*.tmp" -o -name "*.bak" \) \
    2>/dev/null)

if [ ! -z "$violations" ]; then
    echo "❌ Commit blocked: Files violating root hygiene:"
    echo "$violations"
    echo ""
    echo "Please move these files to appropriate directories:"
    echo "  - Tests → tests/modules/<module>/"
    echo "  - Data → inputs/user_provided/"
    echo "  - Temp → Delete or move to inputs/temporary/"
    exit 1
fi
```

## File Tracking Requirements

### User Input Tracking

All user-provided files MUST be tracked in `inputs/README.md`:

```markdown
# User-Provided Input Files

## Active Files

| Date | File | Purpose | Module | Status |
|------|------|---------|--------|--------|
| 2025-01-21 | user_test_data_20250121.csv | Unit test data | orcaflex | Active |
| 2025-01-20 | user_config_20250120.yml | Batch configuration | signal_analysis | Archived |

## Cleanup Schedule

Files older than 30 days in `temporary/` will be automatically deleted.
```

## Exception Handling

### Valid Exceptions for Root Files

Only these scenarios justify temporary root placement:

1. **Build System Requirements**
   - Some build tools require specific root files
   - Must be cleaned immediately after build

2. **CI/CD Pipeline**
   - GitHub Actions may require root configs
   - Must be in .gitignore

3. **IDE Configuration**
   - VS Code settings that must be in root
   - Should be minimal and documented

### Documentation Requirement

Any exception MUST be documented in `CLAUDE.md`:

```markdown
## Root Directory Exceptions

### Temporary Build File
- **File**: build.tmp
- **Reason**: webpack requires root location
- **Duration**: Build time only (~2 minutes)
- **Cleanup**: Automatic via build script
```

## Compliance Metrics

Track file management compliance:

```python
def audit_file_compliance():
    """Generate file management compliance report."""
    
    metrics = {
        'root_violations': len(glob.glob("*.py") + glob.glob("*.csv")),
        'properly_named_inputs': len(glob.glob("inputs/**/user_*_[0-9]*.csv")),
        'temp_files': len(glob.glob("**/*.tmp", recursive=True)),
        'documented_inputs': check_input_documentation(),
    }
    
    compliance_score = calculate_compliance(metrics)
    print(f"File Management Compliance: {compliance_score}%")
    
    return metrics
```

## Quick Reference Commands

```bash
# Check for root violations
find . -maxdepth 1 -type f ! -name "README.md" ! -name "LICENSE" ! -name "CLAUDE.md" ! -name "Makefile" ! -name "*.toml" ! -name "*.sh" ! -name ".*"

# Clean temp files
find . -name "*.tmp" -o -name "*.temp" -o -name "*.bak" -delete

# Move test files to proper location
find . -maxdepth 1 -name "test_*.py" -exec mv {} tests/ \;

# Archive old user inputs
find inputs/user_provided -type d -mtime +30 -exec mv {} inputs/archive/ \;
```

---
**These standards are MANDATORY and override any conflicting practices.**