---
status: accepted
---

# Sweep Config yaml is the source of truth; module constants are removed

demo_01's 680-case matrix and physical constants live as module constants today. Introducing a
Sweep Config (ADR-0001) created a two-sources-of-truth problem. We decided the committed
`inputs/demo_01_freespan.yml` **is** the spec: the loader always reads a yaml, the default run reads
a committed default that encodes today's exact matrix, and the in-code matrix/threshold constants
are removed (not kept as a silent fallback).

We rejected "constants-as-fallback" (yaml selectively overrides, absent yaml → constants) because it
leaves the real matrix buried in Python — defeating the GTM goal of a visible, client-editable
parametric spec — and merely defers the integration risk to subissue 5.

## Consequences

- subissue 2 modifies the demo's **run path** (constants → loaded config), not just adds a loader;
  larger blast radius, accepted.
- **Byte-identity is proven by a golden regression test**, the acceptance gate for subissue 2: run
  the demo from the committed default yaml and compare the sorted `cases[]` rows field-by-field
  against a frozen golden copy, plus assert the four summary counts (PASS / INLINE_ONLY / FAIL_CF /
  FAIL_LOCKIN). The volatile `metadata` block (timestamp, git SHA) is excluded from the diff.
- #633–#636 inherit "yaml-as-spec + golden test" as the template.
