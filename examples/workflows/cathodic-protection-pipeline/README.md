# cathodic-protection-pipeline (DEFERRED — not yet registered)

Subsea-pipeline external CP via `calculation_type: DNV_RP_F103_2010`
(DNV-RP-F103, *Cathodic Protection of Submarine Pipelines by Galvanic Anodes* —
the pipeline-specific DNV code; the DNV-RP-B401 surface-area method is for
structures, not pipelines).

`input.yml` here is complete and the engine math runs, **but this example is
intentionally NOT in `docs/registry/workflows.yaml`** because the F103 code path
is fail-closed on a citation it cannot resolve offline:

```
CitationResolutionError: code_id='dnv-rp-f103'
  wiki_path='wikis/engineering-standards/wiki/standards/dnv-rp-f103.md'
  reason=page_missing
```

`DNV_RP_F103_2010` calls `get_dnv_f103_reference()`, which calls
`validate_citation()`. With no `LLM_WIKI_PATH` set and no `citation_repo_root`,
the resolver parent-walks to the digitalmodel repo root, which has no `wikis/`
tree — so resolution fails. The page exists only as a test fixture
(`tests/citations/fixtures/knowledge/.../dnv-rp-f103.md`); the other four CP
demo paths (jacket / manifold / monopile / FPSO) emit no citation and run with
zero external dependencies.

## Prerequisite to register this example

Make the DNV-RP-F103 citation resolvable in the offline / registry environment,
one of:

1. Author `wikis/engineering-standards/wiki/standards/dnv-rp-f103.md` in the
   **llm-wiki** repo with frontmatter `code_id: dnv-rp-f103`, `publisher: DNV`,
   `revision: "2010"`, and wire `LLM_WIKI_PATH` (or the parent-walk knowledge
   base) into CI and the bot compute clone; **or**
2. Vendor that page into digitalmodel at a resolver-visible location (mirroring
   the existing test fixture) so a bare `engine()` call resolves it.

Once resolvable, add the registry row + a `test_workflow_registry` assertion
block and wire it into the Deckhand subsea-pipelines-integrity channel.
