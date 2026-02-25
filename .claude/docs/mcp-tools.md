# MCP Tools Reference

> Load on-demand for MCP coordination setup

## Setup

```bash
claude mcp add claude-flow npx claude-flow@alpha mcp start
claude mcp add ruv-swarm npx ruv-swarm mcp start        # Optional
claude mcp add flow-nexus npx flow-nexus@latest mcp start  # Optional
```

## Tool Categories

### Coordination
| Tool | Purpose |
|------|---------|
| `swarm_init` | Initialize topology (mesh/hierarchical/ring/star) |
| `agent_spawn` | Register agent types |
| `task_orchestrate` | High-level workflow orchestration |

### Monitoring
| Tool | Purpose |
|------|---------|
| `swarm_status` | Track coordination health |
| `agent_list` | List active agents |
| `agent_metrics` | Performance metrics |
| `task_status` | Task progress |
| `task_results` | Completed task results |

### Memory & Neural
| Tool | Purpose |
|------|---------|
| `memory_usage` | Store/retrieve shared state |
| `neural_status` | Neural agent status |
| `neural_train` | Train patterns |
| `neural_patterns` | Get cognitive patterns |

### GitHub Integration
| Tool | Purpose |
|------|---------|
| `github_swarm` | Repository coordination |
| `repo_analyze` | Analyze repository |
| `pr_enhance` | Enhance pull requests |
| `issue_triage` | Triage issues |
| `code_review` | Code review |

### System
| Tool | Purpose |
|------|---------|
| `benchmark_run` | Performance benchmarks |
| `features_detect` | Detect capabilities |
| `swarm_monitor` | Real-time monitoring |

## Key Principle

**MCP tools coordinate, Claude Code Task tool executes.**

- MCP: Define topology, register agents, orchestrate workflows
- Task tool: Spawn real agents, create files, run code
