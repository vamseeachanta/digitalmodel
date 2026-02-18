# Digitalmodel Agent Adapter

> Generated from workspace-hub/AGENTS.md
> Contract-Version: 1.0.0
> Generated-At: 2026-02-17T16:39:59Z

## Adapter Role

This file is a provider-specific adapter for Claude-compatible tooling.
The canonical contract is in workspace-hub/AGENTS.md.

## Required Gates

1. Every non-trivial task must map to a WRK-* item in .claude/work-queue/.
2. Planning + explicit approval are required before implementation.
3. Route B/C work requires cross-review before completion.

## Plan and Spec Locality

1. Route A/B plan details can live in WRK body sections.
2. Route C execution specs: specs/wrk/WRK-<id>/.
3. Repository/domain specs: specs/repos/<repo>/.
4. Templates: specs/templates/.

## Compatibility

Legacy docs may exist during migration, but AGENTS.md is canonical.

## Repo Overrides

Add repo-specific details below this section without weakening required gates.
