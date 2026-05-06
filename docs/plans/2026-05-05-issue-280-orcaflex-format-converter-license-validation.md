# Plan: digitalmodel #280 — WRK-064 OrcaFlex format converter license-required validation

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/280
**Status:** plan-review
**Tier:** T2 (license-machine verification + small backward-compat wrapper edit)

## Context

Issue #280 (WRK-064) closes out the OrcaFlex three-way format converter by completing the license-gated steps the implementation could not do on dev-primary. The non-license work is **all done**: 102 tests passing across `src/digitalmodel/solvers/orcaflex/format_converter/` (verified via `ls`: `cli.py`, `__main__.py`, `format_detector.py`, `protocols.py`, `section_mapping.py`, `single_to_modular.py`, `modular_to_single.py`, `spec_to_modular.py`, `spec_to_single.py`, `modular_to_spec.py`, `single_to_spec.py`). The backward-compat wrapper at `scripts/conversion/yaml_to_include.py` already delegates to the canonical implementation (verified via head — it imports `OrcaFlexDumper`, `SingleToModularConverter`, `INPUT_PARAMETERS`).

What remains (from the issue body, sections "1", "2", "3"): (1) end-to-end OrcaFlex `LoadData` + `CalculateStatics` validation on a converted YAML, (2) round-trip binary-fidelity test through the OrcaFlex binary format, (3) update the backward-compat wrapper if step (2) reveals divergences. All three require an OrcFxAPI license and must run on `acma-ansys05`.

**Stale-flag:** Not stale; it's the routine close-out for a converter that landed on a non-licensed machine. The acceptance-criteria block in the issue body lists only the **completed** items (all `[x]`); the three pending license-gated items live in the body's free-text "Pending Tasks" section. Update the acceptance list at close-time to reflect actual scope.

## Plan

### Task 1 — End-to-end OrcaFlex load + statics
On `acma-ansys05`, run the converter against a representative input:
```
uv run python -m digitalmodel.solvers.orcaflex.format_converter \
    single2modular "docs/modules/orcaflex/examples/raw/A01/A01 Catenary riser.yml" \
    -o /tmp/a01_modular/
```
Then load the modular output and verify static analysis converges:
```
uv run python -c "
import OrcFxAPI
m = OrcFxAPI.Model()
m.LoadData('/tmp/a01_modular/master.yml')
m.CalculateStatics()
print('Static analysis OK')
"
```
Capture stdout + any OrcaFlex warnings into `tests/solvers/orcaflex/format_converter/license_validation_log.txt` (gitignored or under a fixture path) and reference it in the test added in Task 3.

### Task 2 — Round-trip binary fidelity
Run the four-step round-trip on the same A01 model: `A01.yml → single2modular → modular YAML → OrcaFlex Load → OrcaFlex Save (.sim) → reload → compare`. The comparison checks: (a) line/vessel object count parity, (b) all numeric `Stiffness`/`Mass`/`Length` properties within float epsilon, (c) `CalculateStatics` produces identical effective tension at the touchdown point (±0.1%). Repeat on a mooring case (e.g., `docs/modules/orcaflex/examples/raw/M01/`) if one exists; otherwise note the gap and scope a follow-on.

### Task 3 — Add license-marked tests
Add `tests/solvers/orcaflex/format_converter/test_license_validation.py` with two tests, both decorated `@pytest.mark.requires_orcaflex`, encoding the Task 1 + Task 2 procedures. Configure the marker in `pyproject.toml` so dev-primary CI skips them and `acma-ansys05` CI runs them. The fixture YAMLs already exist under `docs/modules/orcaflex/examples/raw/`.

### Task 4 — Update backward-compat wrapper if needed
Only if Task 1 or Task 2 reveals a behavioral divergence: update `scripts/conversion/yaml_to_include.py` to forward whatever new flag/option the canonical converter exposes. Otherwise no edit — the wrapper is already a thin delegate.

### Task 5 — Update issue acceptance list
Edit `digitalmodel#280` issue body to add three new acceptance bullets matching Tasks 1–3. The current acceptance list only enumerates completed work, which masks the still-open scope.

## Acceptance Criteria

- [ ] `tests/solvers/orcaflex/format_converter/test_license_validation.py` present, `requires_orcaflex` marker registered.
- [ ] On `acma-ansys05`, `uv run pytest -m requires_orcaflex tests/solvers/orcaflex/format_converter/` is green; both tests run.
- [ ] On dev-primary, the same command collects 0 tests (skip-by-marker semantics).
- [ ] A01 round-trip: effective tension at touchdown matches original within ±0.1%.
- [ ] If wrapper edit was made, all 102 pre-existing format-converter tests still pass on dev-primary.
- [ ] Issue body's acceptance list updated to enumerate the three license-gated bullets.

## Open questions

- Is there an existing `requires_orcaflex` pytest marker convention in the repo, or does this plan introduce it? If new, confirm the naming matches any planned cross-suite marker (e.g., AQWA, ANSYS).
- A01 is a riser case; is there an equivalently representative mooring case under `docs/modules/orcaflex/examples/raw/`? If not, defer the mooring round-trip to a follow-on rather than block this close-out.
