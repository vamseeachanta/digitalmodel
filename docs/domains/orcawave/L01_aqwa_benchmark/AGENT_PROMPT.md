# Agent Prompt — Fix L01 smoke test (digitalmodel#468)

## Goal
Make `tests/solver/smoke_test.py` L01 test pass by driving `.yml → .owr → .xlsx` via OrcFxAPI (not subprocess).

## Repo
`D:\workspace-hub\digitalmodel` (or `/d/workspace-hub/digitalmodel` in bash)

## Context

### What exists
- `docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.yml` — OrcaWave input
- `docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.owr` — GUI-generated result (reference only)
- `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_benchmark.py` — existing script, uses subprocess + OrcaWave.exe (NOT OrcFxAPI)
- `tests/solver/smoke_test.py` — smoke test; L01 currently loads pre-existing `.owr` as fixture

### What the smoke test expects (L01)
```python
L01_OWR_SOURCE = REPO_ROOT / "docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.owr"
# loads it, checks frequencyCount > 0, re-saves to tests/fixtures/solver/L01_001_ship_raos.owr
```

The smoke test file is at `tests/solver/smoke_test.py`.
`REPO_ROOT = Path(__file__).resolve().parent.parent.parent` → resolves to `digitalmodel/`

### OrcFxAPI pattern (proven working)
```python
import OrcFxAPI
diff = OrcFxAPI.Diffraction(str(yml_path))   # loads .yml or .owd
diff.Calculate()
diff.SaveData(str(owr_path))                 # writes .owr
diff.ExportResults(str(xlsx_path))           # writes .xlsx
```

Known quirks:
- `diff.frequencyCount` — use this, not `len(diff.frequencies)`
- `.frequencies` returns Hz descending — sort ascending if comparing
- `PanelAngleWarningLevel` key is unsupported in OrcFxAPI 11.6 — strip it before loading:
  write a filtered YAML to a temp file in the same directory as the source (so relative mesh paths resolve)

## Task

### Step 1 — Create `run_orcawave_api.py`
Location: `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_api.py`

Must expose:
```python
def run_orcawave_from_yml(
    yml_path: str | Path,
    owr_path: str | Path,
    xlsx_path: str | Path | None = None,
) -> bool:
    """Load yml, calculate, save .owr, optionally export .xlsx. Returns True on success."""
```

- Strip unsupported keys (`PanelAngleWarningLevel`) by writing a filtered YAML to a temp file in the same dir
- Print progress lines prefixed `[OW]`
- Also usable as CLI: `python run_orcawave_api.py orcawave_001_ship_raos_rev2.yml`

### Step 2 — Update `tests/solver/smoke_test.py` L01
Replace the current `run_l01_smoke_test()` body so it:
1. Calls `run_orcawave_from_yml(L01_YML, owr_path, xlsx_path)` to generate the `.owr` fresh
2. Then loads the generated `.owr` and checks `frequencyCount > 0`

Add constant:
```python
L01_YML = REPO_ROOT / "docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.yml"
```

Keep `L01_OWR_SOURCE` pointing at the same place for backward compat (or remove if unused).

### Step 3 — Verify
Run:
```bash
cd /d/workspace-hub/digitalmodel
python tests/solver/smoke_test.py
```
Both L00 and L01 must show PASS. Artifacts must appear in `tests/fixtures/solver/`.

### Step 4 — Commit and push
```
git add tests/solver/smoke_test.py
git add docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_api.py
git add tests/fixtures/solver/
git commit -m "feat(07-03): L01 smoke test drives .yml→.owr via OrcFxAPI (fixes #468)"
git push origin main
```

## Acceptance criteria
- [ ] `python tests/solver/smoke_test.py` exits 0
- [ ] `tests/fixtures/solver/L01_001_ship_raos.owr` exists and is non-empty
- [ ] `tests/fixtures/solver/L01_001_ship_raos.xlsx` exists (or WARNING logged — non-fatal)
- [ ] No subprocess / OrcaWave.exe used anywhere in the new code
- [ ] `run_orcawave_api.py` is importable as a module

## Notes
- This machine has OrcFxAPI installed and licensed
- Use `uv run python` if `python` is not in PATH
- Do NOT touch L00 logic in smoke_test.py
