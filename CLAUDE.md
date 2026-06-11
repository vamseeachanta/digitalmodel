# digitalmodel Agent Adapter
> Adapter for workspace-hub/AGENTS.md | Contract-Version: 1.0.0
> Canonical: workspace-hub/AGENTS.md — all gates and policies apply.

## Required Gates
1. Every task maps to a GitHub issue (`gh issue list`) — no local task IDs
2. Plan + explicit approval before implementation
3. Route B/C: cross-review before completion

## Plan Locality
Plans: `.planning/` (long-duration: `docs/plans/`) | Specs: `specs/`

## Repo Overrides
Add repo-specific rules below without weakening required gates.
