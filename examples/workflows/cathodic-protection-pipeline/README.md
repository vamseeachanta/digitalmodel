# cathodic-protection-pipeline

Subsea-pipeline external CP via `calculation_type: DNV_RP_F103_2010`
(DNV-RP-F103, *Cathodic Protection of Submarine Pipelines by Galvanic Anodes* —
the pipeline-specific DNV code; the DNV-RP-B401 surface-area method is for
structures, not pipelines).

`input.yml` is registered as the durable
`cathodic-protection-pipeline` workflow in `docs/registry/workflows.yaml`.
The F103 citation resolves offline through the repo-local `knowledge/wikis`
overlay, so a bare engine invocation works without `LLM_WIKI_PATH`.

Run:

```bash
uv run python -m digitalmodel examples/workflows/cathodic-protection-pipeline/input.yml
```

Durable registry check:

```bash
uv run pytest "tests/workflows/test_durable_workflows.py::test_workflow_registry[cathodic-protection-pipeline]" -q
```
