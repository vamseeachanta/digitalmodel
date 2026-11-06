# Orcina help section retrieval

The retrieval command searches an explicitly configured private GitHub corpus
across OrcaFlex, OrcaWave and OrcFxAPI. It returns section pointers, source URLs
and Orcina attribution. Section text stays out of the JSON output.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-18 | Document section boundaries and synthetic regression evidence so code examples do not become false section pointers. |

## Invocation

From the repository root, supply the existing corpus repository and directory:

```sh
python scripts/retrieve_orcina_help.py "YAML inputs" \
  --wiki-repo "$HELP_REPOSITORY" --prefix "$HELP_PREFIX" \
  --revision "$HELP_REVISION"
```

The prefix contains product directories named `orcaflex`, `orcawave` and
`orcfxapi`. The revision resolves to a commit before topics are read. The
command requires existing `gh` authentication and read access. It does not
establish corpus placement or access rights. It has no local-corpus fallback.

Each match identifies `path`, `product`, `topic`, `section`, `line`, `source_url`
and `attribution`, alongside its lexical ranking score. `line` points to the
subsection heading in the pinned topic. For an introduction, it points to the
start of the document. A source URL and Orcina attribution are required.

## Boundary basis

The boundary rules use [CommonMark 0.31.2 (2024)](https://spec.commonmark.org/0.31.2/):

- [Section 4.2, ATX headings](https://spec.commonmark.org/0.31.2/#atx-headings)
  defines heading indentation, separators, levels and optional closing hashes.
  The locator splits subsections and keeps the document title as metadata.
  An attached trailing hash remains part of the section name.
- [Section 4.5, fenced code blocks](https://spec.commonmark.org/0.31.2/#fenced-code-blocks)
  defines matching fence characters, minimum lengths, closing whitespace and
  backtick restrictions in opening info strings. A fence-like line with trailing
  text cannot close a code block. An unclosed fence extends to the document end.

Code content remains in its containing section and can contribute search terms;
it cannot create a separate heading pointer. The tool is a bounded section
locator rather than a complete CommonMark parser. Setext headings, headings
inside list or blockquote containers, and inline formatting interpretation
are outside this slice. Lexical ranking is unchanged; scores are ordering
heuristics, not engineering acceptance limits or calibrated relevance measures.
No engineering equations or numerical solver criteria are introduced.

## Regression evidence

`tests/scripts/test_orcina_section_boundaries.py` uses synthetic text only.
Test docstrings count expected headings and source lines directly from literal
inputs, independently of the parser. Fence cases cover false closers, valid
longer closers, mixed markers and an unclosed block. Heading cases cover
indentation, tabs, closing hashes, empty headings and invalid syntax.

The cross-product cases each return `Synthetic inputs` in every product:

| Query | Expected named section |
|---|---|
| `yaml` | `YAML inputs` |
| `configuration` | `Model configuration` |
| `export` | `Results export` |

These cases also check topic paths, line pointers, attribution, source URLs and
the absence of section text from the emitted matches. They demonstrate offline
retrieval behavior; they do not demonstrate private-corpus availability.

## Remaining scope

Refs #2004. Later slices must settle corpus placement explicitly, wire the
retrieval consumers, and record live queries naming the returned topics for all
products. Ingestion coverage remains with #507. This slice neither reads a live
private corpus nor imports or calls a solver interface.
