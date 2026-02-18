# Plan: Provider Adapters + Ecosystem Folder Structure

> WRK-200 (filesystem cleanup) + WRK-198 Phase 2 (thin provider adapters)
> Repos affected: workspace-hub, digitalmodel, assetutilities, worldenergydata, aceengineer-website

---

## Context

Three planning streams converged in the 2026-02-18 session:

1. **Thin provider adapters** — `.claude/skills/` stays canonical; Codex and Gemini get
   lightweight `.codex/` and `.gemini/` directories with a single `skills` symlink pointing
   at `.claude/skills/`. No migration, no disruption to existing Claude tooling.

2. **Folder structure cleanup** — Full audit of 5 repos found 30+ naming/duplication issues
   causing AI agent confusion (wrong output dir, duplicate module hierarchies, loose files,
   checked-in build artifacts).

3. **Architectural decisions** — Three open questions resolved below before implementation
   begins (specs vs plans, WRK item residence, module-based structure).

---

## Architectural Decisions (Resolved)

### Decision 1: specs/ vs docs/ vs plans/

| Directory | Purpose | Rule |
|-----------|---------|------|
| `specs/wrk/WRK-NNN/` | Execution spec for a specific work item (Route C) | One dir per complex WRK |
| `specs/repos/<repo>/` | Formal design decisions for a repo | Architecture ADRs live here |
| `specs/modules/` | System-level module specs (auto-generated plan files go here) | Design before build |
| `docs/modules/<domain>/` | Reference documentation — how something works | Written after build |
| `docs/guides/` | How-to guides for humans | Stable prose |
| `.claude/work-queue/` | WRK item tracking only | No specs/docs here |

**`plans/` does not exist as a concept.** Planning happens inside WRK items (`## Plan` section
for Route A/B) or in `specs/wrk/WRK-NNN/` (Route C). Remove any `docs/plans/` directories.

### Decision 2: Where WRK items reside

**Single location: `workspace-hub/.claude/work-queue/` only.**

- Submodule repos must NOT have their own WRK items
- If a repo has `.claude/work-queue/pending/` with items, those items belong at hub level
- Rationale: cross-repo orchestration requires a single queue; per-repo queues fragment context
- Enforcement: existing stop hook + AGENTS.md policy already requires this

### Decision 3: Module-based folder structure (canonical pattern)

```
src/<package>/
  <domain>/            ← top-level domain (bsee, marine_safety, structural...)
    <component>/       ← functional component (analysis, processors, reports...)
      __init__.py
      *.py

tests/
  <domain>/            ← mirrors src/<domain>/
    unit/              ← fast, isolated tests
    integration/       ← component interaction tests
    fixtures/          ← test data for this domain

docs/modules/<domain>/ ← reference docs for each src domain
data/modules/<domain>/ ← input data for each src domain
```

**Not** `tests/modules/<domain>/` — the `modules/` wrapper in tests is redundant.
**Not** `results/` or `outputs/` inside `tests/` — test output belongs in `reports/coverage/`.

---

## Part 1: Thin Provider Adapter Symlinks

### What to build

**At workspace-hub root:**
```
.codex/
  skills          → ../.claude/skills    (symlink)
  settings.json                          (new file)
  CODEX.md                               (new file, generated from AGENTS.md)

.gemini/
  skills          → ../.claude/skills    (symlink)
  settings.json                          (new file)
  GEMINI.md                              (new file, generated from AGENTS.md)
```

**At each submodule (25 repos) — relative paths escape the repo:**
```
<repo>/.codex/skills   → ../../.claude/skills   (symlink)
<repo>/.gemini/skills  → ../../.claude/skills   (symlink)
```

### settings.json — Codex
```json
{
  "provider": "codex",
  "model_ref": "config/agents/model-registry.yaml#codex.default_model",
  "focus": "code",
  "strengths": "single-file changes, algorithms, testing, refactoring, config",
  "skills_path": ".codex/skills",
  "permissions": {
    "allow": ["Bash(codex exec:*)", "Read(*)", "Write(*)", "Edit(*)"]
  }
}
```

### settings.json — Gemini
```json
{
  "provider": "gemini",
  "model_ref": "config/agents/model-registry.yaml#gemini.default_model",
  "focus": "research",
  "strengths": "research, data analysis, summarization, content, documents",
  "skills_path": ".gemini/skills",
  "permissions": {
    "allow": ["Bash(gemini:*)", "Read(*)", "Write(*)"]
  }
}
```

### CODEX.md / GEMINI.md format (mirrors CLAUDE.md)
```markdown
---
provider: codex
generated-from: AGENTS.md
contract-version: 1.0.0
---
# Codex Agent Adapter
## Required Gates
[same as CLAUDE.md gates — copied from AGENTS.md]
## Provider Strengths
Focused code tasks, single-file, algorithms, testing, refactoring.
## Skills
.codex/skills/ → .claude/skills/ (workspace-hub canonical)
```

### New script: `scripts/operations/compliance/generate_provider_adapters.sh`

Steps:
1. Read `AGENTS.md` gates section
2. For each provider in `[codex, gemini]`:
   - `mkdir -p .$provider`
   - Write `.$provider/settings.json`
   - Write `.$provider/<PROVIDER>.md` from template
   - `ln -sf ../.claude/skills .$provider/skills`
3. For each submodule repo:
   - Compute relative path: `realpath --relative-to="$repo/.$provider" ".claude/skills"`
   - `mkdir -p "$repo/.$provider"`
   - `ln -sf "$relative_path" "$repo/.$provider/skills"`

### Modify: `scripts/propagate-ecosystem.sh`

Add after existing skills propagation (≈ line 200):
```bash
for provider in codex gemini; do
  for repo in "${submodule_repos[@]}"; do
    adapter="$repo/.$provider"
    link="$adapter/skills"
    target=$(realpath --relative-to="$adapter" "$HUB_ROOT/.claude/skills")
    mkdir -p "$adapter"
    [[ ! -L "$link" ]] && ln -s "$target" "$link"
  done
done
```

### Modify: `.claude/hooks/ecosystem-health-check.sh`

Add after existing skills check:
```bash
for provider in codex gemini; do
  [[ ! -L "$HUB_ROOT/.$provider/skills" ]] && \
    emit_signal "warn" "provider_adapter_missing" ".$provider/skills symlink absent"
done
```

### Files to create/modify (Part 1)

| Action | Path |
|--------|------|
| CREATE | `.codex/settings.json` |
| CREATE | `.codex/CODEX.md` |
| CREATE | `.codex/skills` → `../.claude/skills` |
| CREATE | `.gemini/settings.json` |
| CREATE | `.gemini/GEMINI.md` |
| CREATE | `.gemini/skills` → `../.claude/skills` |
| CREATE | `scripts/operations/compliance/generate_provider_adapters.sh` |
| CREATE | `<each-repo>/.codex/skills` symlink × 25 |
| CREATE | `<each-repo>/.gemini/skills` symlink × 25 |
| MODIFY | `scripts/propagate-ecosystem.sh` (add adapter phase) |
| MODIFY | `.claude/hooks/ecosystem-health-check.sh` (add adapter check) |

---

## Part 2: Folder Structure Cleanup

### workspace-hub root

| Issue | Action | Priority |
|-------|--------|----------|
| `skills/` (2 items: data-analysis, devtools) conflicts with `.claude/skills/` | Move items into `.claude/skills/`, delete root `skills/` | HIGH |
| `modules/` (root): automation, ci-cd, config, development… | Rename to `src/` or merge into `scripts/` per content; remove | HIGH |
| `coordination/` (root): is a Python project with src/, tests/, pyproject.toml | Rename to `src/coordination/` or move whole project to its own dir | HIGH |
| `skills/` root dir now vacant | Delete | HIGH |
| Loose root scripts: `check_collisions.py`, `check_duplicates.py`, `generate_*.py` (5 files) | Move to `scripts/operations/` | MEDIUM |
| `Shared/` (capitalized) | Rename to `shared/` | MEDIUM |
| `ruv-swarm/`, `flow-nexus/`, `monitoring-dashboard/`, `test-framework-integrations/` | Clearly mark as experimental (add README) or move to `experimental/` dir | MEDIUM |
| `dryrun_debug.log` at root | Add to `.gitignore`; delete file | LOW |
| `specs/modules/` ← auto-generated slug names (~20 files: curious-foraging-tome.md etc.) | Rename files to meaningful names or delete if stale | LOW |
| `.claude/agent-library/` overlaps `.claude/skills/` domain structure | Audit: agent-library = agent *templates* (prompt templates), skills = invocable *commands*. Document boundary. No merge needed but add README distinguishing them | LOW |
| `docs/plans/` (inside docs/) | Delete; planning lives in `specs/wrk/` or WRK items | LOW |
| `.claude/outputs/` inside .claude | Move to root `reports/` | LOW |

### aceengineer-website

| Issue | Action | Priority |
|-------|--------|----------|
| `node_modules/` (57MB) checked in | Add to `.gitignore`, remove from git | CRITICAL |
| `dist/` (build output) checked in | Add to `.gitignore`, remove from git | CRITICAL |
| `.venv/` (700MB) checked in | Add to `.gitignore`, remove from git | CRITICAL |
| `.coverage`, `coverage.xml` checked in | Add to `.gitignore` | HIGH |
| HTML files at repo root (about.html, contact.html etc.) | These are generated output — add to `.gitignore` OR confirm they are source and move to `src/` | HIGH |
| `blog_output/` (empty generated dir) | Add to `.gitignore` | MEDIUM |
| `config/`, `demos/`, `logs/`, `.benchmarks/` — empty placeholder dirs | Remove `.gitkeep` and directories; add back only when content exists | LOW |
| `docs/api/`, `docs/guides/` — empty stubs | Remove stubs or populate | LOW |
| `specs/` has only README.md | Populate with feature specs or leave for now | LOW |
| `CASE_STUDY_TEMPLATE.md`, `GITHUB_ORG_SETUP.md`, `GOOGLE_ANALYTICS_SETUP.md`, `PHASE_4_AND_6_PLAN.md` at root | Move to `docs/guides/` | LOW |

### digitalmodel

| Issue | Action | Priority |
|-------|--------|----------|
| `D:\workspace-hub\digitalmodel\docs\charts\phase2\ocimf` Windows path dir | `git rm -rf` and delete immediately | CRITICAL |
| `projects/` (645MB) — large project cache | Add to `.gitignore`; confirm not source code | HIGH |
| `htmlcov/` (450MB) — coverage HTML | Add to `.gitignore`; move to `reports/coverage/` if needed | HIGH |
| `CLAUDE.md.backup-20251023-081047` | Delete; add `*.backup-*` to `.gitignore` | HIGH |
| `coverage.wrk149.focused.xml`, `coverage.wrk149.priority.xml`, `coverage.xml` at root | Move to `reports/coverage/`; add `coverage*.xml` to `.gitignore` | MEDIUM |
| `test_git_repo_analysis.py`, `test_git_repo_optimization.py` at root | Move to `tests/` (appropriate subdirectory) | MEDIUM |
| `outputs/` + `results/` + `reports/` — 3 output dirs | `results/` is empty → delete; keep `reports/` (structured HTML) and `outputs/` (raw analysis); document in README | MEDIUM |
| `memory/` (empty) | Delete | LOW |
| `cache/` (empty) | Add to `.gitignore`, delete | LOW |
| `verdict.txt` at root | Move to `reports/` or delete | LOW |
| Test structure: some tests inside `src/digitalmodel/*/tests/` AND `tests/` | Audit: tests inside src/ are unit co-location (acceptable); `tests/` is integration/system. Document this. | LOW |

### assetutilities

| Issue | Action | Priority |
|-------|--------|----------|
| **DUAL module structure**: `src/assetutilities/base_configs/modules/` AND `src/assetutilities/modules/` both exist with overlapping content (csv_utilities, yml_utilities etc.) | Audit both — determine if one is legacy. Merge into single `src/assetutilities/modules/`; update all imports | CRITICAL |
| TYPO: `tests/modules/yaml_utlities/` (should be `yml_utilities`) | Rename directory; update any test discovery config | HIGH |
| `tests/modules/agent-os/` AND `tests/modules/agent_os/` (both exist) | Merge into `tests/modules/agent_os/` (snake_case matches src/) | HIGH |
| `tests/unit/` AND `tests/units/` (redundant) | Merge into `tests/unit/`; delete `tests/units/` | MEDIUM |
| `CLAUDE.md.backup-20251023-081047`, `pyproject.toml.backup` | Delete; add `*.backup*` to `.gitignore` | MEDIUM |
| Loose root files: `AGENT_OS_COMMANDS.md`, `CLAUDE_CLI_COMMANDS.md`, `COMMANDS.md` | Move to `docs/commands/` | MEDIUM |
| `slash_commands.py`, `slash_completion.bash`, `slash` at root | Move to `scripts/cli/` | MEDIUM |
| `test_file.docx` at root | Delete (test artifact) | LOW |
| `docs/sub_*` naming (40+ dirs): sub_automation, sub_databases… | Long-term: rename to proper domain names; immediate: add `docs/README.md` with index | LOW |
| `agos` executable at root | Move to `scripts/` or `bin/` | LOW |

### worldenergydata

| Issue | Action | Priority |
|-------|--------|----------|
| `frontierdeepwater/Mktg/World Oil/` — client reference (legal concern) | Quarantine: `git mv frontierdeepwater/ .archived/frontierdeepwater/`; legal review before delete | CRITICAL |
| `backups/` dir (CLAUDE.md.backup, pyproject.toml.backup) | Delete contents; remove dir; add `backups/` to `.gitignore` | HIGH |
| Missing `AGENTS.md` | Create from workspace-hub template (1-line pointer to hub AGENTS.md) | HIGH |
| Root `agents/` (14 items) vs `.claude/agents/` (16 items, canonical) | Merge active agents (drilling-expert, financial-analysis, oil-and-gas-expert) into `.claude/skills/` or `.claude/agents/`; delete root `agents/` | HIGH |
| 4 output dirs: `output/` (1 file), `results/`, `reports/`, `test_output/` | Consolidate: `reports/` = structured HTML/MD output; `results/` = raw computation output; delete `output/` (move 1 file); delete empty `test_output/` | HIGH |
| Loose root docs: `QUICK_START.md`, `QUICK_START_MARINE_REPORTS.md`, `PHASE_1_TEST_DEVELOPMENT_PLAN.md`, `user_prompt.md` | Move to `docs/guides/` or `docs/modules/<domain>/` | MEDIUM |
| `archive/` (3 files) + `backups/` + `tests/_archived/` + `tests/_archived_tests/` + `tests/legacy_tests/` | Consolidate all stale content into `tests/_archived/` (tests) and `archive/` (non-test); document what's in each | MEDIUM |
| Root `modules/` (shell scripts: automation, config, reporting) vs `src/worldenergydata/modules/` | Rename root `modules/` → `scripts/modules/` or inline into `scripts/` | MEDIUM |
| `scripts/bsee/` AND `scripts/bsee_migration/` | Merge into `scripts/bsee/` | MEDIUM |
| `docs/data/`, `docs/data_science/`, `docs/data-sources/` — 3 data doc locations | Consolidate into `docs/data-sources/`; migrate `docs/data_science/` content | MEDIUM |
| `docs/modules/data_sodir/` AND `docs/data-sources/sodir/` — duplicate sodir docs | Merge into `docs/modules/sodir/`; delete duplicate | MEDIUM |
| `coverage.json`, `coverage.xml`, `COVERAGE_ANALYSIS.txt` at root | Move to `reports/coverage/`; add to `.gitignore` | LOW |
| `test_export.json`, `.test_performance.db` at root | Move to `tests/data/` or delete | LOW |
| `specs/` (empty) | Leave empty with `.gitkeep` for now; populate when route C specs are needed | LOW |
| `tests/` has 3 parallel organizational patterns (modules/, unit/, integration/) | Long-term: standardize to `tests/<domain>/unit/` and `tests/<domain>/integration/`; do not disrupt working tests | LOW |

---

## Part 3: Cross-Repo .gitignore Improvements

Add to `.gitignore` in repos that are missing these entries:

```gitignore
# Build output
dist/
build/
*.egg-info/

# Python env
.venv/
venv/

# Coverage
.coverage
coverage.xml
coverage.json
htmlcov/
*.xml   # (except explicit test fixtures)

# Node
node_modules/

# Backups
*.backup*
*.backup-*
CLAUDE.md.backup*

# Runtime / caches
*.db
cache/
__pycache__/
.hypothesis/

# Generated output (not source)
reports/          # optional — only if fully generated
outputs/          # optional
```

---

## Part 4: File Taxonomy Skill

### Problem

Every session produces the same question: "Where does this file go?" — results vs reports,
benchmarks vs data, intermediate files, format choices (yaml vs json vs csv). Without a
canonical answer, each agent decides differently, producing the multi-output-dir mess
documented in Part 2.

### Solution

A permanent, invocable skill at `.claude/skills/workspace-hub/file-taxonomy/SKILL.md` that
acts as a decision tree. Any agent (Claude, Codex, Gemini) can consult it before writing
output. The question is never asked again.

### Canonical File Taxonomy

#### By purpose — where it lives

| File Type | Canonical Dir | Rule |
|-----------|--------------|------|
| **Structured reports** (HTML, PDF, Markdown) | `reports/<domain>/` | Human-readable output; timestamped filenames |
| **Raw computation output** (arrays, matrices, series) | `results/<domain>/` | Machine-readable; intermediate for further processing |
| **Benchmark comparisons** (model vs reference) | `reports/benchmarks/<domain>/` | Always HTML or JSON with metadata |
| **Validated reference data** | `data/<domain>/` | Ground truth; checked in; version-controlled |
| **Test fixtures** | `tests/<domain>/fixtures/` | Input data for tests only |
| **Cache / temp** | `cache/` (gitignored) | Ephemeral; never committed |
| **Coverage** | `reports/coverage/` | HTML in htmlcov/; XML at reports/coverage/coverage.xml |
| **Specs / design docs** | `specs/wrk/WRK-NNN/` or `specs/repos/<repo>/` | Pre-build design; Route C only |
| **Reference docs** | `docs/modules/<domain>/` | Post-build; explains how something works |
| **Guides** | `docs/guides/` | How-to for humans; stable prose |
| **Config** | `config/` | Repo config; never output |

#### By format — which format to choose

| Use case | Format | Reason |
|----------|--------|--------|
| Structured config / schema | **YAML** | Human-readable, comment-friendly |
| API responses / machine data | **JSON** | Strict types, universal tooling |
| Tabular data (large) | **CSV** | Pandas-native, Excel-compatible |
| Tabular data (small, typed) | **YAML** | Inline, readable with units |
| Reports (human) | **HTML** or **Markdown** | Browsable / diffable |
| Binary data / matrices | **NumPy .npy** | Efficient, lossless |
| Geospatial | **GeoJSON** or **NetCDF** | Standard for domain |
| Model parameters | **YAML** | Editable, diffable |
| Hydrodynamic output | **.owd** (internal) | OrcaWave native; never convert to CSV |

#### Naming conventions

| Pattern | Example | Rule |
|---------|---------|------|
| Timestamped reports | `spar_fatigue_2026-02-18.html` | ISO date suffix for archives |
| Domain-scoped results | `results/fatigue/spar_sn_curves.npy` | Domain subdir always present |
| Benchmark files | `reports/benchmarks/wamit/ellipsoid_vs_wamit.html` | method_vs_reference pattern |
| Coverage | `reports/coverage/coverage.xml` | Singular, fixed name, gitignored |
| Fixtures | `tests/bsee/fixtures/cost_sample.yaml` | Domain-matched to src/ |

#### Gitignore policy

| Category | Always gitignore | Always track | Track with care |
|----------|-----------------|-------------|-----------------|
| `reports/` | If fully generated | If curated/reference | Mixed → track selectively |
| `results/` | Yes (computation output) | Never | — |
| `cache/` | Yes | Never | — |
| `data/` | Never (ground truth) | Yes | Large files → LFS |
| `tests/fixtures/` | Never | Yes | — |
| `htmlcov/` | Yes | Never | — |
| `coverage*.xml` | Yes | Never | — |
| `*.backup*` | Yes | Never | — |
| `node_modules/` | Yes | Never | — |
| `dist/`, `build/` | Yes | Never | — |
| `.venv/`, `venv/` | Yes | Never | — |

### Skill File to Create

**Path**: `.claude/skills/workspace-hub/file-taxonomy/SKILL.md`

```markdown
---
skill: workspace-hub:file-taxonomy
version: 1.0.0
invocation: /file-taxonomy
applies-to: [claude, codex, gemini]
---

# File Taxonomy — Where Does This File Go?

Consult this skill before writing any output file. Do not create new output dirs
without checking the canonical map below.

## Decision Tree

1. Is it human-readable output (HTML, PDF, Markdown report)? → `reports/<domain>/`
2. Is it raw computation (arrays, matrices, intermediate data)? → `results/<domain>/`
3. Is it a benchmark comparison vs reference? → `reports/benchmarks/<domain>/`
4. Is it validated ground truth / reference data? → `data/<domain>/`
5. Is it only used by tests? → `tests/<domain>/fixtures/`
6. Is it ephemeral / reproducible from source? → `cache/` (gitignored, never committed)
7. Is it a design spec (pre-build)? → `specs/wrk/WRK-NNN/` or `specs/repos/<repo>/`
8. Is it documentation of how something works? → `docs/modules/<domain>/`
9. Is it a how-to guide? → `docs/guides/`
10. Is it config? → `config/`

## Format Guide

- Config/schema → YAML
- API/machine data → JSON
- Tabular large → CSV
- Reports → HTML or Markdown
- Matrices → NumPy .npy

## Gitignore Policy

Always gitignore: `results/`, `cache/`, `htmlcov/`, `coverage*.xml`, `*.backup*`,
`node_modules/`, `dist/`, `.venv/`

Never gitignore: `data/`, `tests/fixtures/`, `config/`, `specs/`

`reports/` — gitignore if fully generated; track if curated.

## Canonical Map (quick reference)

See `specs/modules/synchronous-swinging-cat.md` Part 4 for full tables.
```

### New WRK Item

This skill requires its own WRK item (WRK-201) since it creates a new tracked skill file.
Add to work queue after plan approval.

---

## Phased Execution Order

### Phase A — Zero-risk (no src/test changes)
1. Delete Windows path dir in digitalmodel (`git rm -rf "D:\"...`)
2. Add `.gitignore` entries for node_modules, dist, .venv, *.backup* across all repos
3. Remove `node_modules/`, `dist/`, `.venv/` from aceengineer-website git tracking
4. Create `.codex/` and `.gemini/` adapters at hub + propagate to submodules
5. Worldenergydata: quarantine `frontierdeepwater/`

### Phase B — Low-risk structural moves
6. Move loose root-level scripts/docs to proper dirs (no code changes)
7. Delete empty dirs (memory/, cache/, empty stubs)
8. Create AGENTS.md in worldenergydata
9. Consolidate archive/backup dirs

### Phase C — Medium-risk (rename/merge)
10. workspace-hub: resolve `skills/` vs `.claude/skills/` (move 2 items, delete root `skills/`)
11. worldenergydata: merge root `agents/` into `.claude/`
12. worldenergydata: consolidate 4 output dirs to 2 (reports/, results/)
13. assetutilities: fix typo `yaml_utlities` → `yml_utilities`
14. assetutilities: merge `agent-os/` + `agent_os/` in tests
15. assetutilities: merge `unit/` + `units/` in tests

### Phase D — High-risk (requires import audit)
16. assetutilities: audit + merge dual module structure (`base_configs/modules/` vs `modules/`)
17. workspace-hub: resolve `coordination/` (rename or restructure)
18. Standardize test layout: move toward `tests/<domain>/unit/` pattern (non-breaking, incremental)

---

## Critical Files

| File | Change |
|------|--------|
| `scripts/operations/compliance/generate_provider_adapters.sh` | CREATE |
| `scripts/propagate-ecosystem.sh` | MODIFY (add adapter phase, ~10 lines) |
| `.claude/hooks/ecosystem-health-check.sh` | MODIFY (add adapter check, ~6 lines) |
| `.codex/settings.json`, `.codex/CODEX.md`, `.codex/skills` | CREATE (hub) |
| `.gemini/settings.json`, `.gemini/GEMINI.md`, `.gemini/skills` | CREATE (hub) |
| `<25 repos>/.codex/skills`, `<25 repos>/.gemini/skills` | CREATE via script |
| `aceengineer-website/.gitignore` | MODIFY (add node_modules, dist, .venv) |
| `digitalmodel/.gitignore` | MODIFY (add *.backup*, projects/, htmlcov/) |
| `assetutilities/.gitignore` | MODIFY (add *.backup*) |
| `worldenergydata/.gitignore` | MODIFY (add backups/, *.backup*) |

---

## Verification

```bash
# Part 1: Thin adapters
ls -la .codex/skills            # symlink → ../.claude/skills
ls -la .gemini/skills           # symlink → ../.claude/skills
ls -la digitalmodel/.codex/skills   # resolves through to hub .claude/skills/
bash .claude/hooks/ecosystem-health-check.sh   # no adapter warnings
bash scripts/propagate-ecosystem.sh --dry-run  # no errors
bash scripts/propagate-ecosystem.sh --apply && \
bash scripts/propagate-ecosystem.sh --apply    # second run: no changes (idempotent)

# Part 2: No broken symlinks after cleanup
find . -xtype l 2>/dev/null | head -20     # no dangling symlinks
git diff --cached --diff-filter=T          # no symlinks converted to files

# Tests still pass after structural moves
cd digitalmodel && PYTHONPATH="src:../assetutilities/src" \
  python3 -m pytest tests/ -v --tb=short --noconftest -x

cd assetutilities && python3 -m pytest tests/ -v --tb=short --noconftest -x

cd worldenergydata && PYTHONPATH="src:../assetutilities/src" \
  python3 -m pytest tests/ -v --tb=short --noconftest -x

# aceengineer-website builds clean
cd aceengineer-website && npm install && node build.js
```
