# digitalmodel Agent Adapter
> Adapter for workspace-hub/AGENTS.md | Contract-Version: 1.0.0
> Canonical: workspace-hub/AGENTS.md — all gates and policies apply.

## Required Gates
1. Every task maps to WRK-* in workspace-hub/.claude/work-queue/
2. Plan + explicit approval before implementation
3. Route B/C: cross-review before completion

## Plan Locality
Route A/B: WRK body | Route C: specs/wrk/WRK-<id>/ | Specs: specs/repos/<repo>/

## Repo Overrides
Add repo-specific rules below without weakening required gates.
