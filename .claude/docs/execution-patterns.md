# Execution Patterns Reference

> Load on-demand for complex multi-agent workflows

## Tool Responsibility

| Concern | MCP Tools | Task Tool |
|---------|-----------|-----------|
| Execute work | No | **Yes** |
| Coordinate | **Yes** | No |
| Create files | Never | **Always** |
| Run code | Never | **Always** |
| Define topology | **Yes** | No |

## Correct Pattern

```javascript
// Step 1: Optional MCP coordination setup
mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 6 }

// Step 2: REQUIRED Task tool execution (all in ONE message)
Task("Researcher", "Analyze requirements...", "researcher")
Task("Coder", "Implement features...", "coder")
Task("Tester", "Write tests...", "tester")
TodoWrite { todos: [...] }
```

## Hook Lifecycle

**Use hooks when:** 3+ agents, cross-session memory, complex workflows

### Before Work
```bash
npx claude-flow@alpha hooks pre-task --description "[task]"
```

### After Work
```bash
npx claude-flow@alpha hooks post-task --task-id "[task]"
```

## Memory Policies

| Type | Duration | Namespace |
|------|----------|-----------|
| Short-term | Session | `swarm/session/[id]` |
| Medium-term | 24-72h | `swarm/shared/[name]` |
| Long-term | Indefinite | `swarm/knowledge/[...]` |

## Batching Rules

**1 MESSAGE = ALL RELATED OPERATIONS**

- All `Task()` calls together
- All file operations together
- All Bash commands together
- All TodoWrite todos in ONE call
- All memory operations together
