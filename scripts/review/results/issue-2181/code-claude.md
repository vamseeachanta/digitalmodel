# Code review: digitalmodel issue 2181 (`pipeline_defect_screen`)

**Verdict: MAJOR.** Review is of the inline packet only; no tools, edits or network were used, so nothing was executed.

## MAJOR

**M1. The allowable/demand ratio is published as an RSF.**
- `pipeline_defect_screen.py:180-187` passes the ratio to `decide(..., "pipeline")`, whose policy label is `"RSF"` (`ffs_decision.py:161-163`).
- The criterion text concatenated at `pipeline_defect_screen.py:193-196` and printed at `:293-295` therefore reads, for example, "allowable/demand=1.031; Screening ACCEPT; RSF=1.031 is within 0.05 of RSFa=1.00".
- `to_dict()` also emits `rsf` and `rsf_a=1.0` (`ffs_decision.py:330-331`) into the saved result.
- An RSF above 1 and an RSFa of 1.00 are not API 579 quantities, and the ESCALATE text repeats the label (`ffs_decision.py:401-402`).
- Fix: pass a ratio-specific label or policy and drop or rename `rsf`/`rsf_a` in the adapter output.

**M2. Pressure-RSF bands are applied to a demand ratio with no stated criterion.**
- `DecisionBands()` defaults (`ffs_decision.py:106-108`) give REPLACE below 0.50 and MONITOR within 0.05 of 1.0.
- For `test_axial_demand_can_govern` (`test_pipeline_defect_screen.py:117-121`) the margin is about 0.37, so the verdict is REPLACE: "Component must be replaced."
- The intact pipe would also fail that demand (0.72 × 52,000 = 37,440 psi against 100,000 psi), so the verdict comes from the caller's demand, not the defect.
- The test asserts only `margin < 1`, so this is unpinned.
- Fix: supply explicit `bands` with a documented basis, or restrict the verdict set for this screen.

**M3. Four of the six method surfaces are absent from the packet.**
- `corroded_pipe.py` (B31G, Modified B31G, RSTRENG) and `dnv_rp_f101.py` are not included, although `review-request.md:7` states the full method surfaces are attached.
- Not verifiable: `CorrodedPipeResult.applicability`, `safe_pressure_psi`, the DNV `usage_factor` keyword, `allowable_pressure_psi`, and the d/t = 0 and d/t = 1 behaviour that `test_pipeline_defect_screen.py:148-159` relies on.
- The "typed Applicability on all six" and "numeric outputs and signatures preserved" claims are therefore not established for these two modules.

**M4. `router` overwrites its own input block.**
- `inputs = cfg["pipeline_defect_screen"]` (`:324`) is replaced by `cfg[cfg["basename"]] = result` (`:331`), because the basename is the same key.
- The saved output YAML then holds no grid, factors or `data_origin`, so the result cannot be traced to its inputs from the artifact.
- Fix: store the result under a distinct key, or embed an `inputs` echo in the result.

**M5. The two examples collide on one report file.**
- The report filename is fixed (`:328`), and `README.md:10` says both `input.yml` and `synthetic.yml` write `results/pipeline-defect-screen.html`.
- Running both README commands leaves only the synthetic report at the path registered for the `input.yml` workflow (`review-request.md:54`).
- This assumes both inputs resolve to the same `result_folder`, which the README implies but the packet does not show.
- Fix: derive the filename from the input stem or `component_id`.

## MINOR

1. **Applicability contract broken.** `circumferential_defect.py:294-296` returns `ok=True` with a note and no flag. `applicability.py:54` defines one note per flag, and `_html_foot` pairs them with `zip` (`ffs_report.py:315`). Alignment holds here only because `circ` is last in `collect` (`pipeline_defect_screen.py:139`). The note also enters every ESCALATE criterion and makes `legacy_details()` return a note alongside `within_applicability=True`.
2. **Duplicate flags.** The four B31G-family results each raise `B31G_DT_GT_0.80`; `merge` does not de-duplicate, so the footer lists it four times.
3. **Date field misused.** `_html_head(component_id, "Current-demand screen")` (`:281`) renders "Date: Current-demand screen" under the heading "Fitness-for-Service Assessment Report" (`ffs_report.py:303-305`). The report carries no date, and the heading conflicts with the docstring at `:274`.
4. **Action contradicts the suppressed rerating.** When the axial screen governs, `rerated_mawp_psi` is nulled (`:191`) but `decision["action"]` remains `RE_RATE`.
5. **Pressure shortfall can be hidden.** If the axial row governs while pressure rows also have margin below 1, the decision reports only the axial limit.
6. **Level-1 extent screen dropped.** `level1_screen_ok` is computed but not carried into the row or the report (`:153-161`); a failed screen should be disclosed.
7. **Non-`ValueError` rejections.** A missing key raises `KeyError` and a string value raises `TypeError` at `:53-54`. `component_id` is required by `render_report` (`:278`) but never validated.
8. **Example verdict unpinned.** Hand calculation reproduces 1183, 1219 and 1334 psi. Standard effective-area arithmetic gives RSTRENG ≈ 1147 psi for the flat 0.150 in × 8 in profile, so margin ≈ 1.03 and the verdict is MONITOR with RSTRENG governing. This is approximate because `corroded_pipe.py` is absent. No test asserts the reference verdict or governing method; `test_pipeline_defect_screen.py:38-40` recomputes the same `min`.
9. **Encoding.** `test_durable_workflows.py:104` calls `read_text()` without `encoding="utf-8"`; the report contains em dashes and will mis-decode on a cp1252 host.
10. **Absolute path in output.** `report_path = str(report)` (`:330`) is written to the saved YAML; if `results/input.yml` is committed, a machine path enters the repository.
11. **Brittle disclaimer swap.** `.replace(...)` on a private footer string (`:313-316`) reverts silently if the upstream wording changes outside the tested path.
12. **Catalog inconsistency.** `corroded-pipe`, `rstreng-2d` and `dnv-f101` are promoted to `live` with the workflow, while `circumferential`, used by the same workflow, stays `validated` with none (`review-request.md:36,74`). The asymmetry should be either deliberate and stated, or corrected.

## Checked and found consistent

- The conditional at `:221` parses as intended.
- Ragged, NaN and non-monotonic inputs raise `ValueError`.
- The d/t = 1 path is JSON-safe with `allow_nan=False`.
- HTML escaping covers `component_id` and `data_origin`.
- The report makes no network fetch.
- The circumferential closed form in the test matches `Dm = OD − t`.

`★ Insight ─────────────────────────────────────`
- `ffs_decision.decide` is generic in its tree but not in its wording: each `AssetClassPolicy` carries a `margin_label` and `DecisionBands`. Reusing the `pipeline` policy for a different margin inherits both the RSF label and the RSF thresholds, which is the root of M1 and M2.
- `Applicability` keeps `flags` and `notes` as parallel lists, so one advisory note without a flag misaligns every later consumer that zips them. A separate `advisories` field would avoid this.
- Writing the result to `cfg[basename]` is convenient for the registry's `in_memory` key but removes the input from the durable artifact whenever the input block has the same name.
`─────────────────────────────────────────────────`
