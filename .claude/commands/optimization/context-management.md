# Context Management

Optimize Claude Code context usage through startup configuration, runtime hygiene, and understanding context rot.

## Golden Rule: Orchestrator Pattern

**The main Claude Code instance is the ORCHESTRATOR, not the executor.**

```
┌─────────────────────────────────────────────────────┐
│  ORCHESTRATOR (Main Instance)                       │
│  • Plans, coordinates, delegates                    │
│  • Stays lean (<20% context)                        │
│  • NEVER executes complex tasks directly            │
├─────────────────────────────────────────────────────┤
│  SUBAGENTS (Task tool spawned)                      │
│  • Execute all work                                 │
│  • Isolate context pollution                        │
│  • Disposable - discard when done                   │
│  • Return summaries, not verbose output             │
└─────────────────────────────────────────────────────┘
```

**Why this matters for context:**
- Subagent context is isolated and discarded after completion
- Verbose output (test logs, build output) never pollutes main context
- Orchestrator can `/clear` safely - subagent work is committed to git
- Main instance stays responsive even during long operations

**Always delegate:**
```javascript
// WRONG: Execute in main context
Bash("pytest -v")  // 200 lines pollute context

// RIGHT: Delegate to subagent
Task("Run tests", "Run pytest, return only failures", "tester")
```

**Rule of thumb:** If output might exceed 50 lines, delegate.

See: `.claude/docs/orchestrator-pattern.md` for full documentation.

---

## The Problem

### Startup Context Waste

| Category | Before | After | Savings |
|----------|--------|-------|---------|
| MCP tools | ~15,500 | ~3,000 | 12,500 tokens |
| Skills | ~2,500 | ~500 | 2,000 tokens |
| Agents | ~2,600 | ~2,600 | (keep all) |
| Memory files | ~2,000 | ~2,000 | (keep all) |
| **Total** | **~22,600** | **~8,100** | **~14,500 tokens (64%)** |

### Context Rot (Research Finding)

Context rot describes performance degradation as input length increases. Key findings:

1. **Non-uniform degradation** - Performance worsens unpredictably as context grows
2. **Distractor vulnerability** - Irrelevant content in context impairs responses
3. **Structural sensitivity** - Logically coherent content can hurt more than shuffled content
4. **Output degradation** - Even simple tasks fail at scale with hallucinations

**Implication**: More context ≠ better results. Less garbage = better outputs.

Source: [Context Rot Research](https://research.trychroma.com/context-rot)

---

## Analysis

```bash
# Check current context usage
/context

# Key metrics:
# - MCP tools token count
# - Custom agents token count
# - Memory files token count
# - Free space percentage
```

Target: Keep startup context under 20% (~40k tokens) to leave 80% for work.

---

## Startup Optimization

### MCP Lean Configuration

Most projects only need:
```json
{
  "mcpServers": {
    "claude-flow@alpha": {
      "command": "npx",
      "args": ["claude-flow@alpha", "mcp", "start"],
      "type": "stdio"
    }
  }
}
```

### When to Add More

| MCP Server | Add When |
|------------|----------|
| `ruv-swarm` | Multi-agent swarm coordination |
| `flow-nexus` | Cloud deployment, neural training |
| `playwright-mcp` | Browser automation testing |

### Remove by Project Type

| Project Type | Remove |
|--------------|--------|
| Data analysis | flow-nexus, agentic-payments, browser tools |
| Static websites | flow-nexus, agentic-payments |
| CLI tools | All except claude-flow@alpha |

### Propagation

```bash
./scripts/optimize-mcp-context.sh --lean
```

---

## Runtime Context Hygiene

### The `/clear` + `/catchup` Pattern

Best practice for session management:

```bash
# When context gets polluted
/clear

# Resume by reading changed files
/catchup
```

This resets context while preserving work via git.

### Document & Clear Pattern

For complex multi-step tasks:

1. Have Claude export progress to markdown
2. `/clear` to reset context
3. Resume from the documented plan

### Avoid `/compact`

The automatic compaction is opaque and error-prone. Prefer explicit `/clear` + `/catchup`.

### Sub-agents for Noisy Output

Trap verbose output in sub-agents:

```javascript
// BAD: 200 lines of test output in main context
Task("Run tests", "pytest -v", "Bash")

// GOOD: Sub-agent summarizes, returns only failures
Task("Run tests and summarize failures",
     "Run pytest, return only failing test names and stack traces",
     "tester")
```

Source: [Keep Context Clean](https://medium.com/@arthurpro/claude-code-keep-the-context-clean-d4c629ed4ac5)

---

## CLAUDE.md Optimization

### Token Budget Approach

Treat CLAUDE.md like ad space with strict token limits:

| Section | Max Tokens | Content |
|---------|------------|---------|
| Core rules | 200 | TDD, YAGNI, batch ops |
| Tech stack | 100 | Languages, frameworks |
| Commands | 150 | Essential scripts |
| References | 100 | Paths to detailed docs |
| **Total** | **~600** | ~1.5KB |

### Don't Embed, Reference

Instead of `@`-mentioning docs:

```markdown
# BAD (embeds entire file)
See @docs/api-reference.md for details

# GOOD (agent reads on-demand)
For FooBarError or complex usage, see docs/api-reference.md
```

Source: [How I Use Every Claude Code Feature](https://blog.sshh.io/p/how-i-use-every-claude-code-feature)

---

## Information Placement

Based on context rot research:

1. **Critical info first** - Place important details early in context
2. **Minimize distractors** - Remove topically related but irrelevant content
3. **Clear structure** - Use headings, but avoid overly nested hierarchies
4. **Focused prompts** - Only essential information, not maximum tokens

---

## Skill Optimization

### Archive Unused Skills

```
.claude/commands/_archive/
├── flow-nexus/     # 9 skills (MCP removed)
├── hive-mind/      # 12 skills (replaced by Task tool)
└── README.md
```

### Skill Registry Pattern

Search without loading all descriptions:

```bash
# Search skills
/skill-search swarm

# Load specific skill
/swarm:swarm-init
```

Registry: `.claude/skill-registry.yaml`

---

## Git Discipline

### Commit Frequently

Agents can drift mid-execution. Commit successful steps:

```bash
# After each successful change
git add -p && git commit -m "feat: step description"
```

Benefits:
- Easy rollback if agent drifts
- Clear history of what worked
- Context can be cleared safely

### Match Control to Task

| Task Type | Control Level |
|-----------|---------------|
| Patterned work | Plan Mode (let agent run) |
| Specific fixes | Iterate with commits |
| New subsystems | Do yourself, then hand off |

---

## Hooks for Context Control

### Block-at-Submit Pattern

Use PreToolUse hooks to enforce quality gates:

```json
{
  "PreToolUse": [{
    "matcher": "Bash(git commit*)",
    "hooks": [{
      "type": "command",
      "command": "test -f .tests-passed || exit 1"
    }]
  }]
}
```

**Don't block mid-write** - Blocking mid-plan confuses the agent.

---

## Session Analysis

Store sessions for meta-analysis:

```bash
# Sessions stored in
~/.claude/projects/<project>/sessions/

# Analyze for patterns
grep -r "Error:" ~/.claude/projects/*/sessions/*.json
```

Use findings to improve CLAUDE.md and agent-facing docs.

---

## Quick Reference

### Commands

| Command | Use When |
|---------|----------|
| `/context` | Check token usage |
| `/clear` | Reset polluted context |
| `/catchup` | Resume after clear |
| `/skill-search` | Find skills without loading all |

### Targets

| Metric | Target |
|--------|--------|
| Startup context | <20% (~40k tokens) |
| CLAUDE.md | <2KB |
| Free context | >50% for work |

### Anti-patterns

- Embedding large files with `@`
- Using `/compact` instead of `/clear`
- Keeping verbose tool output in main context
- Loading all MCP servers "just in case"
- Maximizing context usage (context rot)

---

## Sources

- [Context Rot Research](https://research.trychroma.com/context-rot) - Chroma
- [Keep Context Clean](https://medium.com/@arthurpro/claude-code-keep-the-context-clean-d4c629ed4ac5) - Arthur
- [How I Use Every Claude Code Feature](https://blog.sshh.io/p/how-i-use-every-claude-code-feature) - Shrivu Shankar
