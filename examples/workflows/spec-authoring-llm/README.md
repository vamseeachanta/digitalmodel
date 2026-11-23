# LLM spec authoring — project SSOT → analysis SSOT

Front-end of the model-generation pipeline (digitalmodel#1095). Turns a project
single-source-of-truth plus a natural-language request into the canonical
diffraction **analysis** single-source-of-truth (a `DiffractionSpec` YAML), with
a no-silent-assumptions audit trail.

```
project_bundle.yml  +  "give me transit RAOs"
        │  LLM authors a structured intent (extracts only stated facts)
        ▼
AuthoredIntent  ──►  resolver.resolve()  ──►  spec.yml  +  assumption_ledger.json
                       (fills + ledgers gaps)        (the analysis SSOT)
        │  spec_converter / orcawave_backend / aqwa_backend
        ▼
OrcaWave .yml / AQWA .dat   (licensed-program input)
```

## Run

By default the LLM call goes through the **`claude -p` CLI** (Claude Code) —
it uses your existing Claude Code auth, so **no `ANTHROPIC_API_KEY` and no extra
dependency**:

```bash
uv run python -m digitalmodel.hydrodynamics.diffraction.spec_author \
    examples/workflows/spec-authoring-llm/project_bundle.yml \
    --request "Give me first-pass motion RAOs for a transit screening study" \
    --out-dir /tmp/authored_spec
```

Alternative backend — the Anthropic SDK (needs `anthropic` + `ANTHROPIC_API_KEY`):

```bash
export ANTHROPIC_API_KEY=...
uv run --with anthropic python -m digitalmodel.hydrodynamics.diffraction.spec_author \
    examples/workflows/spec-authoring-llm/project_bundle.yml \
    --request "..." --backend anthropic-sdk --out-dir /tmp/authored_spec
```

> **Mesh note:** the resolver requires a hull mesh to complete the spec. This
> example bundle deliberately omits one — the author step extracts the
> particulars and **flags the missing mesh as an open question** (no silent
> assumptions). For a full run, add `vessel_particulars.mesh_file: <path.gdf>`
> to the bundle (or generate a parametric hull first).

Outputs in `--out-dir`:

| File | What it is |
|---|---|
| `spec.yml` | The analysis SSOT — a clean, reloadable `DiffractionSpec`. |
| `assumptions.md` | Human audit: what the LLM extracted vs what the resolver assumed. |
| `assumption_ledger.json` | Machine-readable assumption ledger (#622 contract). |
| `authored_intent.json` | The raw structured intent the LLM produced. |

Then convert to a licensed-program input:

```bash
uv run python -m digitalmodel.hydrodynamics.diffraction.spec_converter \
    /tmp/authored_spec/spec.yml --solver orcawave -o /tmp/orcawave_case
```

## Offline / programmatic use

The LLM call is injected via the `IntentAuthor` protocol, so the pipeline runs
fully offline with a stub author (see `tests/hydrodynamics/diffraction/test_spec_author.py`):

```python
from digitalmodel.hydrodynamics.diffraction.spec_author import (
    ProjectBundle, author_spec,
)
bundle = ProjectBundle.from_yaml("project_bundle.yml")
result = author_spec(bundle, "transit RAOs", author=my_author)
result.write("out/")
```
