---
name: compound
aliases: [compound-engineering, ce]
description: "Every.to's compound engineering loop - Plan→Work→Review→Compound where each feature makes the next easier"
category: workspace-hub
---

# Compound Engineering Command

Orchestrate the 4-phase compound engineering loop. Each phase is high-leverage: thorough planning and review mean implementation is the easy part.

## Subcommands

| Subcommand | Description |
|------------|-------------|
| (none) | Run the full loop: Plan → Work → Review → Compound |
| `plan <desc>` | Research-driven planning only (knowledge + archaeology + web research + synthesis) |
| `work` | TDD implementation from existing plan |
| `review` | 12-perspective parallel review of current changes |
| `learn` | Extract and store knowledge from current session |
| `resume <id>` | Resume a checkpointed session |
| `status` | Show current session state |

## Full Loop

```
/compound "Add OAuth2 authentication to aceengineer-website"
```

Runs all 4 phases sequentially:
1. **Plan (40%)** - Knowledge retrieval + codebase archaeology + web research + plan synthesis
2. **Work (10%)** - TDD implementation following the plan
3. **Review (40%)** - 12 parallel reviewer subagents + aggregation
4. **Compound (10%)** - Extract patterns/gotchas/tips, store as knowledge entries

## Phase Commands

### Plan
```
/compound plan "Add OAuth2 authentication"
```
Produces a research-informed plan at `specs/modules/<module>/plan.md`. Delegates to:
- `/knowledge advise` for prior learnings
- Explore subagent for codebase archaeology
- WebSearch for best practices
- core-planner for synthesis

### Work
```
/compound work
```
Picks up the current plan and implements via TDD. Delegates to core-coder.

### Review
```
/compound review
```
Spawns 12 isolated reviewer subagents in parallel, each examining changes from a single perspective:
Security, Performance, Correctness, Maintainability, Testability, Scalability, Accessibility, Error Handling, Dependencies, Consistency, Documentation, Deployment.

Aggregates findings and outputs to `.claude/compound-reviews/<session-id>.md`.

### Learn
```
/compound learn
```
Extracts patterns, gotchas, tips, decisions, and corrections from the current session. Stores via `/knowledge capture`.

### Resume
```
/compound resume flickering-rolling-rossum
```
Reads checkpoint from `.claude/compound-state/<session-id>.yaml` and resumes from the last completed phase.

### Status
```
/compound status
```
Shows current session checkpoint state.

## Storage

```
.claude/compound-state/          # Session checkpoints (ephemeral)
.claude/compound-reviews/        # Multi-perspective review reports
.claude/knowledge/entries/       # Reuses existing knowledge structure
specs/modules/<module>/plan.md   # Reuses existing spec location
```

## Skill Reference

@.claude/skills/coordination/workspace/compound-engineering/SKILL.md

## Related Commands

- `/knowledge` - Knowledge capture and retrieval
- `/work` - Work queue (items with `compound: true` route here)
- `/reflect` - Daily reflection incorporates compound session data
