# Repository Structure Improvement Plan

## Summary
Comprehensive reorganization to clean up root directory clutter, consolidate redundant folders, and establish clear organizational conventions.

## Cross-Review Findings

### Review #1: Risk & Safety
- tmpclaude-* files are safe (contain only paths, <100 bytes each)
- `dat_files_list.txt` may be referenced - keep for now
- Moving test scripts could break `specs.modules` imports - check first
- `output/` → `outputs/` consolidation may break code references - create symlink
- Keep `.agent-runtime/` (may contain user config)

### Review #2: Completeness
- **Missed files**: `coverage.json`, `coverage.xml`, `.coverage`, `rainflow_analysis.log` (1.4MB), `tools`
- **Missed directories**: `temp_test_conversion/`, `temp_workflow_test/`, `htmlcov/`
- **Files that don't exist**: `nul`, `debug.txt`, `dm_vessel_output.log` - removed from plan
- tmpclaude files are in `data/orcaflex_examples/` subdirectories, not root
- Update docs referencing `run_validation.py` after move

### Review #3: Best Practices
- Use `scripts/testing/` instead of `tests/manual/`
- .gitignore has duplicates - consolidate
- Add folder structure to CONTRIBUTING.md as well

---

## Proposed Changes

### Phase 1: Clean Root Directory

```bash
# Delete generated coverage files (already in .gitignore but present)
rm -f coverage.json coverage.xml .coverage

# Delete stale reference file
rm -f tools

# Delete temp directories
rm -rf temp_test_conversion/ temp_workflow_test/

# Move large log file
mv rainflow_analysis.log logs/

# Move test output files to reports/
mv pytest_output.txt pytest_result.txt test_result.txt reports/ 2>/dev/null
mv rao_batch_results.txt temp_quality_check_output.txt reports/ 2>/dev/null

# Move test scripts to scripts/testing/
mkdir -p scripts/testing
mv test_complete_workflow.py test_converter_advanced.py test_converter_live.py test_model_generator_basic.py scripts/testing/
mv run_validation.py scripts/

# Move summary MDs to docs/modules/project-docs/
mv FINAL_TEST_BASELINE.md TEST_BASELINE_SUMMARY.md TEST_RESULTS_SUMMARY.md docs/modules/project-docs/
mv UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md IMPLEMENTATION_SUMMARY.md docs/modules/project-docs/
mv MODEL_GENERATOR_IMPLEMENTATION_SUMMARY.md RISER_PHASE1_SUMMARY.md docs/modules/project-docs/

# Consolidate output directories (create symlink for backward compatibility)
mv output/* outputs/ 2>/dev/null
rmdir output 2>/dev/null
# Note: Create symlink in separate step if needed for code compatibility
```

### Phase 2: Update .gitignore

Add/consolidate these patterns:
```gitignore
# Root temp directories
/temp_test_conversion/
/temp_workflow_test/

# Coverage files (consolidate - remove duplicates elsewhere)
/coverage.json

# Stale reference file
/tools

# Temp working directory files
tmpclaude-*-cwd
```

### Phase 3: Clean Hidden Folders (Conservative)

```bash
# Delete only empty/unused folders
rm -rf .workflow_cache/

# Keep .agent-runtime/ (may contain user config)
# Keep .swarm/, .hive-mind/, .agent-os/, .ai/ for future work
```

### Phase 4: Document Folder Purposes

Create `.claude/docs/FOLDER_CONVENTIONS.md`:
```markdown
## Repository Folder Conventions

### Root-Level Folders
- src/        - Source code (Python packages)
- tests/      - Test files (pytest)
- docs/       - Documentation (under docs/modules/)
- scripts/    - Utility scripts (including scripts/testing/)
- config/     - Configuration files
- data/       - Data files
- examples/   - Example code and workflows
- specs/      - Specifications and plans

### Agent-Related Folders
- agents/           - Domain-specific agents (orcaflex, aqwa, cad, etc.)
- .claude/agents/   - Framework/system agents (core, sparc, github, etc.)
- skills/           - Reusable skill definitions

### Runtime Folders (Not Tracked)
- .claude-flow/     - Agent runtime state and memory
- logs/             - Application logs
- outputs/          - Generated outputs
- reports/          - Generated reports
- cache/            - Cache files
```

### Phase 5: Update References

Update documentation referencing moved files:
- `docs/modules/mooring/CALM_BUOY_QUICK_START.md` - update `run_validation.py` path
- `docs/modules/mooring/CALM_BUOY_PROJECT_WORKFLOW.md` - update `run_validation.py` path
- `scripts/generate_calm_buoy_project.py` - update `run_validation.py` path

---

## Files to Modify

### .gitignore
Path: `.gitignore`
- Add temp directory patterns
- Add coverage.json pattern
- Consolidate duplicate patterns

### Folder Structure
- Delete: `.workflow_cache/`, `temp_test_conversion/`, `temp_workflow_test/`
- Create: `scripts/testing/`
- Move: `output/` contents → `outputs/`

---

## Verification

1. Run `ls *.py *.md *.log *.txt 2>/dev/null | wc -l` on root - should be minimal
2. Run `git status` - temp files should not appear
3. Verify `logs/` has `rainflow_analysis.log`
4. Verify `scripts/testing/` has moved test scripts
5. Verify `scripts/run_validation.py` exists
6. Run `uv run pytest tests/` - ensure no test regressions
7. Grep for broken references: `grep -r "run_validation.py" docs/`

---

## Risk Assessment

| Change | Risk | Mitigation |
|--------|------|------------|
| Delete temp files | LOW | Already confirmed safe |
| Delete coverage files | LOW | Regenerated on test run |
| Move test scripts | LOW | Use scripts/testing/, check imports first |
| Delete empty folders | LOW | Already empty |
| Move run_validation.py | MEDIUM | Update all doc references |

---

## Out of Scope (Future Work)

- Consolidating `.swarm/` into `.claude-flow/`
- Consolidating `.hive-mind/`
- Merging `.agent-os/` and `.ai/`
- Clarifying `agents/` vs `modules/` overlap
- Further .gitignore deduplication
