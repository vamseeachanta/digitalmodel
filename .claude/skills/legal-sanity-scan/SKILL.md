# Legal Sanity Scan Skill

## Description
Detect and prevent client-identifiable references (project names, vessel names, operator codes, user/machine identifiers) from leaking into committed code. Uses pattern-based scanning with deny lists at workspace and project levels.

## When to Use
- Before committing sanitized or ported code from client projects
- As a pre-PR gate to verify no client references remain
- After importing OrcaFlex models from external sources (e.g., rock-oil-field)
- When adding new data files, YAML models, or configuration derived from client work
- During code review to flag potential legal compliance issues

## Architecture

### Deny List Hierarchy
```
workspace-hub/.legal-deny-list.yaml      # Global patterns (all submodules)
  └── digitalmodel/.legal-deny-list.yaml  # Project-specific patterns (extends global)
```

Project deny lists **extend** (not replace) the global list. Both are scanned together.

### Deny List Format
```yaml
version: "1.0"
updated: "2026-02-11"

client_references:
  - pattern: "ClientProjectName"
    case_sensitive: false
    severity: block          # block = fail scan, warn = flag only
    description: "Client project codename"

exclusions:
  - ".legal-deny-list.yaml"  # Don't scan the deny list itself
  - ".git/"
  - "scripts/sanitize_*.py"  # Sanitization scripts legitimately contain patterns
```

### Severity Levels
| Level | Effect | Use Case |
|-------|--------|----------|
| `block` | Scan fails (exit 1), PR cannot proceed | Client names, operator codes, user IDs |
| `warn` | Flagged but scan passes | Generic terms that _might_ be client-related |

## Commands

### Scan specific repository
```bash
bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel
```

### Scan all submodules
```bash
bash scripts/legal/legal-sanity-scan.sh --all
```

### Scan only changed files (fast, for PRs)
```bash
bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --diff-only
```

### JSON output (for CI/CD integration)
```bash
bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --json
```

### Exit codes
| Code | Meaning |
|------|---------|
| 0 | PASS — no block-severity violations |
| 1 | FAIL — block violations found |
| 2 | ERROR — bad arguments or missing repo |

## Common Patterns to Watch For

### Client Project Names
Any project codename, field name, or development name that identifies a specific client engagement.

### Vessel Names
Installation vessel names, rig names, FPSO names — replace with generic identifiers (e.g., `installation_vessel_01`).

### Operator/Company Names
Oil company names, engineering contractor names, operator codes.

### User/Machine Identifiers
Windows usernames, machine hostnames, file paths containing user directories — often embedded in OrcaFlex `.yml` headers by `SaveData()`.

### OrcaFlex-Specific Metadata
OrcaFlex `SaveData()` embeds `User:`, `Machine:`, and `File:` lines in YAML headers. These must be stripped during sanitization.

## Sanitization Workflow

### For OrcaFlex Models (s7 extraction)
1. **Convert**: `.dat` → `.yml` via `OrcFxAPI.Model().SaveData()`
2. **Sanitize text**: Apply regex replacements from mapping table (longest patterns first)
3. **Strip metadata**: Remove `User:`, `Machine:`, `File:` header lines
4. **Organize**: Place in `docs/modules/orcaflex/<category>/monolithic/`
5. **Scan**: Run `legal-sanity-scan.sh --repo=digitalmodel`
6. **Fix**: Address any remaining violations
7. **Extract spec**: Only after scan passes (prevents client data in spec.yml)

### For General Code Porting
1. **Import**: Copy code to working branch
2. **Scan immediately**: `legal-sanity-scan.sh --repo=<name> --diff-only`
3. **Replace**: Swap client references with generic equivalents
4. **Re-scan**: Verify exit code 0
5. **Commit**: Only after clean scan

## Adding New Deny Patterns

1. Edit the appropriate deny list:
   - Global: `workspace-hub/.legal-deny-list.yaml`
   - Project: `<repo>/.legal-deny-list.yaml`
2. Add pattern with severity and description
3. Run full scan to check for existing violations:
   ```bash
   bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel
   ```
4. Fix all violations before committing the updated deny list

## Integration Points

### Pre-commit Hook
Can be integrated as a git pre-commit hook to prevent accidental commits:
```bash
# In .git/hooks/pre-commit or .husky/pre-commit
bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --diff-only
```

### CI/CD Pipeline
```yaml
# GitHub Actions example
- name: Legal sanity scan
  run: bash scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --json
```

### Cross-Review Gate
Legal scan is a mandatory pre-gate in the cross-review cycle (see `scripts/review/cross-review.sh`).

## Troubleshooting

### False Positives
If a legitimate term matches a deny pattern:
1. Add the file to `exclusions` in the deny list
2. Or use a more specific pattern (e.g., `"ACME Project"` instead of `"ACME"`)

### ripgrep Not Found
The scanner falls back to `grep` if `rg` is not available. Install ripgrep for faster scans:
```bash
cargo install ripgrep  # or: choco install ripgrep (Windows)
```

### Large File Skipping
Files >1MB are skipped by default (`--max-filesize 1M`). Binary `.dat` files are not scanned — only text-based files (`.yml`, `.py`, `.md`, etc.).

## Related Skills
- `/orcaflex-monolithic-to-modular` — Model conversion workflow (includes sanitization step)
- `/orcaflex-jumper-analysis` — Jumper models extracted from client data
- `/orcaflex-model-generator` — Modular generation from sanitized specs

## Related Rules
- `workspace-hub/.claude/rules/legal-compliance.md` — Full legal compliance rules
- `workspace-hub/.claude/rules/security.md` — Secret management (complementary)
