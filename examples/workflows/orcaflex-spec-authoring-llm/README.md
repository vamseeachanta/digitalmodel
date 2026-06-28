# OrcaFlex LLM spec-authoring front-end

Turn a **project single-source-of-truth** (heterogeneous project data) plus a
natural-language request into an OrcaFlex **analysis SSOT**
(`ProjectInputSpec` + assumption ledger), and optionally generate the OrcaFlex
model YAML.

```
project_bundle.yml  +  "screen the mooring for storm strength"
        |  LLM authors a structured intent (extracts only STATED facts)
        v
    OrcaFlexIntent  ->  inverse_resolver.resolve  (fills gaps, ledgers each)
        v
    ProjectInputSpec  +  AssumptionLedger   <-- analysis SSOT
        |  (--generate-model)
        v
    OrcaFlex model YAML (master.yml + includes)
```

This mirrors the diffraction front-end
(`digitalmodel.hydrodynamics.diffraction.spec_author`) and reuses the shared
generic plumbing in `digitalmodel.common.spec_authoring`. Three license-free
model families are supported, dispatched by the chosen outcome's family:

| Family | Outcomes | Example bundle |
|---|---|---|
| Mooring | `mooring_strength`, `mooring_pretension` | `project_bundle.yml` |
| Riser | `riser_static`, `riser_dynamic` | `project_bundle_riser.yml` |
| Pipeline | `pipeline_onbottom`, `pipeline_lay` | `project_bundle_pipeline.yml` |

The LLM sets only the input fields that apply to the chosen family (e.g.
`riser_outer_diameter` for a riser outcome) and leaves the rest `null`.

## Responsibility split (why this is auditable)

- The **LLM** only extracts mooring facts the project data actually states and
  picks the matching outcome. Anything it cannot ground is left `null`, and it
  records a `provenance` entry for every value it does set. It never invents
  chain sizes, line counts, or water depths.
- The **deterministic resolver** fills every remaining field from maintained
  reference defaults and records each one in the `AssumptionLedger` -- the
  "no silent assumptions" contract. Engineering assumptions live in auditable
  code, not in the model output.

## Run it (default: `claude -p`, no API key)

The default backend shells out to the local Claude Code CLI (`claude -p`), so it
uses your existing Claude Code auth -- **no `ANTHROPIC_API_KEY` and no
`anthropic` dependency required**.

```bash
uv run python -m digitalmodel.solvers.orcaflex.spec_author \
    examples/workflows/orcaflex-spec-authoring-llm/project_bundle.yml \
    -r "Screen the spread mooring for storm strength" \
    -o authored_spec
```

Riser and pipeline requests use the same command with their bundles:

```bash
uv run python -m digitalmodel.solvers.orcaflex.spec_author \
    examples/workflows/orcaflex-spec-authoring-llm/project_bundle_riser.yml \
    -r "Check the steel catenary riser wave dynamics" \
    -o authored_spec_riser

uv run python -m digitalmodel.solvers.orcaflex.spec_author \
    examples/workflows/orcaflex-spec-authoring-llm/project_bundle_pipeline.yml \
    -r "Screen the pipeline installation lay" \
    -o authored_spec_pipeline
```

This writes into `authored_spec/`:

| File | What it is |
|---|---|
| `spec.yml` | The clean, reloadable analysis-SSOT `ProjectInputSpec`. |
| `assumptions.md` | Human audit: extracted-from-data vs assumed-by-resolver. |
| `assumption_ledger.json` | Machine-readable assumption ledger. |
| `intent.json` | The structured intent the LLM authored. |

## Also generate the OrcaFlex model

Add `--generate-model` to emit the OrcaFlex model YAML (`master.yml` + includes)
into `authored_spec/model/`:

```bash
uv run python -m digitalmodel.solvers.orcaflex.spec_author \
    examples/workflows/orcaflex-spec-authoring-llm/project_bundle.yml \
    -r "Screen the spread mooring for storm strength" \
    -o authored_spec \
    --generate-model
```

## Use the Anthropic SDK instead

To call the Anthropic API directly (structured outputs) instead of the CLI:

```bash
ANTHROPIC_API_KEY=sk-... uv run --with anthropic python -m \
    digitalmodel.solvers.orcaflex.spec_author \
    examples/workflows/orcaflex-spec-authoring-llm/project_bundle.yml \
    -r "Screen the spread mooring for storm strength" \
    --backend anthropic-sdk
```

## Offline / programmatic use

The LLM call is injected via the `IntentAuthor` protocol, so the pipeline is
fully testable without a network or API key -- pass a stub author that returns a
fixed `OrcaFlexIntent`. See `tests/solvers/orcaflex/test_spec_author.py`.
