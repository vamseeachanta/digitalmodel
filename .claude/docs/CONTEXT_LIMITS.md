# Context Management Limits

> **Version**: 1.0 | **Updated**: 2026-01-18

## Rationale

Large context files degrade AI quality by:
1. Diluting attention on relevant instructions
2. Consuming tokens that should be used for actual work
3. Including reference material that's rarely needed
4. Causing instruction conflicts and confusion

## Recommended Limits

### Active Context Files (Always Loaded)

| File | Max Size | Max Lines | Purpose |
|------|----------|-----------|---------|
| `~/.claude/CLAUDE.md` | **2KB** | 50 | Global user preferences |
| `workspace/CLAUDE.md` | **4KB** | 100 | Workspace delegation patterns |
| `project/CLAUDE.md` | **8KB** | 200 | Project-specific rules |
| `project/CLAUDE.local.md` | **2KB** | 50 | User overrides (not committed) |
| **Total Active** | **16KB** | 400 | ~4,000 tokens |

### Reference Documentation (Loaded On-Demand)

| Category | Location | Max Size | Load When |
|----------|----------|----------|-----------|
| Agent docs | `.claude/docs/agents.md` | 10KB | Agent spawning |
| MCP tools | `.claude/docs/mcp-tools.md` | 8KB | MCP coordination |
| Execution patterns | `.claude/docs/execution-patterns.md` | 12KB | Complex workflows |
| Memory policies | `.claude/docs/memory-policies.md` | 6KB | Cross-session work |
| Plan templates | `specs/templates/` | 12KB | Plan mode |

### Content Categories

**MUST be in CLAUDE.md (Essential):**
- Mandatory behavioral rules
- Critical safety constraints
- Plan mode conventions
- Cross-review requirements
- File organization rules
- Key delegation patterns

**SHOULD be in reference docs (Load on-demand):**
- Agent lists and descriptions
- MCP tool reference tables
- Execution workflow diagrams
- Code examples and patterns
- Memory namespace details
- Performance benchmarks

**SHOULD NOT be in context files:**
- Verbose ASCII art diagrams
- Repeated examples
- Historical information
- Marketing/feature lists
- Long code samples

## Token Budget Guidelines

Based on Claude 3.5/4 capabilities (200K context):

| Category | Token Budget | % of Context |
|----------|--------------|--------------|
| System prompt | ~8,000 | 4% |
| Active CLAUDE.md files | ~4,000 | 2% |
| Conversation history | ~50,000 | 25% |
| Code/file content | ~100,000 | 50% |
| Working space | ~38,000 | 19% |

**Rule**: Context files should never exceed 3% of total context.

## Enforcement

### Pre-commit Check
```bash
# Add to .claude/hooks/pre-commit
max_size=8192  # 8KB
for f in CLAUDE.md CLAUDE.local.md; do
  if [ -f "$f" ]; then
    size=$(wc -c < "$f")
    if [ "$size" -gt "$max_size" ]; then
      echo "ERROR: $f exceeds ${max_size} bytes (${size} bytes)"
      exit 1
    fi
  fi
done
```

### Metrics to Track
- Context file sizes (bytes)
- Token consumption per file
- Reference doc usage frequency
- Quality correlation with context size

## Migration Path

1. **Identify** content categories in existing CLAUDE.md
2. **Extract** reference material to `.claude/docs/`
3. **Condense** essential rules to fit limits
4. **Link** to reference docs using markdown links
5. **Validate** with size checks
6. **Sync** across all repositories

## Future Considerations

### Claude 4+ (2025-2026)
- 1M+ token context windows expected
- Limits may relax to 32KB active context
- Reference docs can grow to 50KB each
- Semantic loading based on task type

### Multi-Agent Systems
- Shared context budget across agents
- Per-agent context allocation
- Dynamic context loading based on role
- Context compression techniques

## Related Documents

- [Plan Template](../../../specs/templates/plan-template.md)
- [Cross-Review Policy](./CROSS_REVIEW_POLICY.md)
- [Agent Library](../agent-library/)
