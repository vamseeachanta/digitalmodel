# Plan: digitalmodel #552 — Align CRLF/LF line endings in AQWA backend

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/552
**Status:** plan-review
**Tier:** T1 (single targeted edit, two write sites in one file)

## Root cause
- `AQWABackend.generate_single()` writes via `open(..., newline='')` semantics, producing CRLF on output.
- `AQWABackend.generate_modular()` uses `Path.write_text(...)`, which normalizes to LF.
- The two paths produce byte-different output for the same logical input. Latent today (test `test_modular_concatenation_matches_single` strips lines before comparing) but breaks any future byte-equality assertion.
- Divergence sites: `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` — locate the two writers and align them.

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/hydrodynamics/diffraction/test_aqwa_backend.py::test_modular_concatenation_matches_single -xvs
```
Confirm passes today. Then add a temporary byte-equality assertion to demonstrate the divergence locally.

### Task 2 — Decide canonical line ending
Inspect a known-good AQWA-emitted `.dat`/`.lis` file (from `tests/...` fixtures or `data/aqwa/`) with `file -k`, `od -c | head`, or `xxd | head` to determine native AQWA convention.
- If AQWA emits CRLF: change `generate_modular()` to write CRLF (use `open(..., 'w', newline='')` and explicit `\r\n` joins, or `Path.write_bytes(text.replace('\n', '\r\n').encode())`).
- If AQWA emits LF (or unspecified): change `generate_single()` to write LF (use `Path.write_text()` symmetric with the modular path).

### Task 3 — Fix
Edit `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py`:
- Apply the chosen alignment to whichever writer is non-canonical.
- Both `generate_single()` and `generate_modular()` use the same writer helper (extract a small private `_write_deck(path, text)` if it shortens the diff).

Add a new test in `tests/hydrodynamics/diffraction/test_aqwa_backend.py` asserting **byte-equality** (not stripped-text equality) between concatenated modular output and single-path output for one representative input.

### Task 4 — Verify
```
uv run pytest tests/hydrodynamics/diffraction/test_aqwa_backend.py -xvs
```

### Task 5 — Confirm no regressions
```
uv run pytest tests/hydrodynamics/ -x
```

## Acceptance Criteria
- `generate_single()` and `generate_modular()` produce byte-identical output for the same inputs (excluding intentional per-deck filename/header differences if any — call out in PR body).
- New byte-equality test passes.
- Existing `test_modular_concatenation_matches_single` continues to pass.
- No regression in `tests/hydrodynamics/`.

## Open questions
- Canonical line-ending choice: confirm by inspecting AQWA-emitted reference file before implementing — choice is load-bearing for downstream tooling.
