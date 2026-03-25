# Available Agents Reference

> Load on-demand when spawning agents

## Core Development (5)
`coder`, `reviewer`, `tester`, `planner`, `researcher`

## Swarm Coordination (5)
`hierarchical-coordinator`, `mesh-coordinator`, `adaptive-coordinator`, `collective-intelligence-coordinator`, `swarm-memory-manager`

## Consensus & Distributed (7)
`byzantine-coordinator`, `raft-manager`, `gossip-coordinator`, `consensus-builder`, `crdt-synchronizer`, `quorum-manager`, `security-manager`

## Performance & Optimization (5)
`perf-analyzer`, `performance-benchmarker`, `task-orchestrator`, `memory-coordinator`, `smart-agent`

## GitHub & Repository (9)
`github-modes`, `pr-manager`, `code-review-swarm`, `issue-tracker`, `release-manager`, `workflow-automation`, `project-board-sync`, `repo-architect`, `multi-repo-swarm`

## SPARC Methodology (6)
`sparc-coord`, `sparc-coder`, `specification`, `pseudocode`, `architecture`, `refinement`

## Specialized Development (8)
`backend-dev`, `mobile-dev`, `ml-developer`, `cicd-engineer`, `api-docs`, `system-architect`, `code-analyzer`, `base-template-generator`

## Testing & Validation (2)
`tdd-london-swarm`, `production-validator`

## Migration & Planning (2)
`migration-planner`, `swarm-init`

---
**Total: 54 agents**

## Usage

```javascript
Task("Agent name", "Instructions", "agent-type")
```

## Agent Selection Guide

| Task Type | Recommended Agent |
|-----------|-------------------|
| Code implementation | `coder` |
| Code review | `reviewer` |
| Test writing | `tester` |
| Research/analysis | `researcher` |
| Architecture design | `system-architect` |
| Multi-agent coordination | `hierarchical-coordinator` |
