# Spec Library Audit Snapshot

> Generated 2026-06-12 on current main + #716 branch (`scripts/audit_all_specs.py`).
> Supersedes the failure inventory in `SECTION_FIDELITY_ANALYSIS.md`
> (2026-04-10), whose builder-fidelity *matrix* remains useful but whose
> failure counts are obsolete. Regenerate this snapshot with:
>
> ```bash
> uv run python scripts/audit_all_specs.py --json /tmp/audit.json
> ```
>
> Tracking issue: [#716](https://github.com/vamseeachanta/digitalmodel/issues/716).

## Results (2026-06-12)

| Stage | OrcaFlex | OrcaWave |
|-------|----------|----------|
| YAML parseable | 91/91 | 13/13 |
| Schema validation (`ProjectInputSpec` / `DiffractionSpec`) | 91/91 | 13/13 |
| Generation (master.yml + includes) | 91/91 | 13/13 |
| YAML-strict validator | 91/91 | n/a |
| Post-validation (cross-references) | 91/91 | n/a |
| **ALL STAGES PASS** | **91/91 (100%)** | **13/13 (100%)** |

3 additional OrcaFlex specs use intentionally different schemas and are
excluded from the `ProjectInputSpec` denominator (1 passing-ship, 2 jumper
installation — see #506; the jumper specs convert via
`jumper_to_modular_spec.build_modular_spec()`, #602).

Model-type distribution: 77 generic, 8 riser, 5 pipeline, 1 minimal.

## Disposition of the 2026-04-10 failure inventory

Every failure category in the old fidelity analysis is resolved on current
main; none required new work in #716 except the last item:

| Old category (count) | Status | Where fixed |
|---|---|---|
| Schema: `seabed.stiffness.shear` null (≈11) | fixed | `environment.py` `_coerce_none_to_zero` validator |
| Generation: `equipment.ramp` vs `ramps` (≈5) | fixed | `shapes_builder.py:39` uses `ramps` |
| YAML-strict: `ImplicitVariableMaxTimeStep` (≈2) | fixed | `generic_builder.py` `_SKIP_GENERAL_KEYS` |
| Post-validator reference gaps (≈7 specs / 34 errors) | not reproducible | 91/91 post-validation pass; #717 should re-verify against its own fresh list before doing any work |
| `frps_ssr_global_riser`: placeholder `period: 0.0` rejected by `gt=0` | **fixed in #716** | spec placeholder set to 8.0 s ("SET per load case" comment retained) |

## Audit runtime

| Mode | Wall time | Notes |
|------|-----------|-------|
| Sequential (`--workers 1`, legacy behavior) | ~662 s | dominated by multi-MB extracted specs |
| Parallel (default, 31 workers on ace-linux-2) | 324 s | wall-bounded by the single slowest spec (~5.4 min for the largest extracted model) |
| `--changed-only` (pre-commit / PR gate) | seconds–90 s | audits only spec.yml files differing from origin/main; the #509 hook should use this |

The issue's aspirational <60 s full-library target is not reachable by
parallelism alone — the floor is per-spec Pydantic validation time on the
multi-MB extracted specs. That is the per-iteration `model_validate`
performance problem already tracked in
[#536](https://github.com/vamseeachanta/digitalmodel/issues/536); if #536
lands, rerun and update this table.

## CLI

```
--domain {all,orcaflex,orcawave}   restrict to one solver domain
--workers N                        parallel processes (1 = sequential)
--changed-only                     only specs changed vs origin/main
--json PATH                        machine-readable results
```
