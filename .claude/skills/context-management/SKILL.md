# Context Management Skill

> Version: 2.0.0
> Created: 2026-01-14
> Updated: 2026-01-18
> Purpose: Manage context files, enforce limits, and continuously improve based on work patterns

## Quick Reference

```
CLAUDE.md Limits:
- Global (~/.claude/CLAUDE.md): 2KB max
- Workspace CLAUDE.md: 4KB max
- Project CLAUDE.md: 8KB max
- CLAUDE.local.md: 2KB max
- Total Active: 16KB (~4K tokens)

Runtime Context:
- %ctx = (current_tokens / 200000) * 100
- Alert: >60% = archive older exchanges
- Critical: >80% = trim to essentials only
```

---

## Part 1: Context File Management

### Size Limits (MANDATORY)

| File | Max Size | Max Lines | Purpose |
|------|----------|-----------|---------|
| `~/.claude/CLAUDE.md` | 2KB | 50 | Global preferences |
| Workspace `CLAUDE.md` | 4KB | 100 | Delegation patterns |
| Project `CLAUDE.md` | 8KB | 200 | Project rules |
| `CLAUDE.local.md` | 2KB | 50 | User overrides |

### Validation Command

```bash
# Run validation across all repos
./scripts/context/validate_context.sh

# Check single repo
./scripts/context/validate_context.sh digitalmodel
```

### Content Categories

**MUST be in CLAUDE.md:**
- Mandatory behavioral rules (TDD, batching, etc.)
- Plan mode conventions
- Cross-review requirements
- File organization rules
- Key delegation patterns

**MUST be in .claude/docs/ (reference):**
- Agent lists and descriptions
- MCP tool reference tables
- Execution workflow diagrams
- Code examples and patterns
- Memory namespace details

### Automated Improvement

The skill analyzes past work to suggest improvements:

1. **Pattern Detection**: Identifies frequently used instructions
2. **Redundancy Removal**: Flags duplicate content across files
3. **Usage Analysis**: Tracks which docs are actually loaded
4. **Suggestion Generation**: Proposes optimizations

---

## Part 2: Runtime Context Management

### Response Format Rules

#### Output Constraints
- **Tables**: Maximum 10 rows, summarize remainder
- **Code blocks**: Maximum 50 lines, split larger into files
- **Lists**: Maximum 15 items, aggregate beyond
- **Large outputs**: Write to `.claude/outputs/`, return path only

#### Mandatory Response Ending
```
STATUS: [complete|in_progress|blocked] | NEXT: [action] | KEY: [metrics]
```

### Prohibited Actions

1. **No Echo**: Never repeat input data back
2. **No Redundancy**: Don't repeat previous findings
3. **No Over-Explanation**: Keep explanations ≤3 sentences
4. **No Raw Content**: Use file paths instead of pasting
5. **No Unbounded Lists**: Always cap with "and N more..."

### Context Health Indicators

| %ctx | Status | Action |
|------|--------|--------|
| 0-40% | 🟢 Healthy | Normal operation |
| 40-60% | 🟡 Elevated | Consider summarizing |
| 60-80% | 🟠 High | Archive older exchanges |
| 80-100% | 🔴 Critical | Trim to essentials |

### Recovery Patterns

When context exceeds threshold:
1. Create checkpoint with current state
2. Write to `.claude/outputs/session-state.json`
3. Clear verbose history
4. Continue with checkpoint reference only

---

## Part 3: Continuous Improvement

### Learning from Past Work

The skill tracks patterns across sessions:

```yaml
# .claude/state/context-patterns.yaml
patterns:
  frequently_referenced:
    - ".claude/docs/agents.md"  # 45 loads
    - ".claude/docs/execution-patterns.md"  # 32 loads
  rarely_used:
    - "verbose section X"  # 0 references in 30 days
  suggested_additions:
    - "Add shortcut for OrcaFlex batch processing"
improvement_suggestions:
  - "Move agent list to reference doc (saves 1.5KB)"
  - "Consolidate duplicate file org rules"
```

### Daily Analysis Tasks

1. **Size Check**: Validate all CLAUDE.md files against limits
2. **Pattern Analysis**: Analyze git commits for instruction patterns
3. **Usage Tracking**: Check which docs were loaded
4. **Suggestion Generation**: Create improvement proposals
5. **Report Generation**: Output daily health report

### Improvement Workflow

```
1. Validate sizes → Report violations
2. Analyze patterns → Identify redundancy
3. Check usage → Find unused content
4. Generate suggestions → Propose changes
5. Apply approved changes → Update files
6. Commit → Track improvements
```

---

## Part 4: Scripts

### validate_context.sh

Location: `scripts/context/validate_context.sh`

Validates context file sizes and generates report.

### analyze_patterns.sh

Location: `scripts/context/analyze_patterns.sh`

Analyzes git history to identify instruction patterns.

### improve_context.sh

Location: `scripts/context/improve_context.sh`

Applies approved improvements to context files.

### daily_context_check.sh

Location: `scripts/context/daily_context_check.sh`

Runs all checks and generates daily report.

---

## Part 5: Scheduled Execution

### Windows Task Scheduler

Task: `ContextManagementDaily`
Schedule: Daily at 6:00 AM
Action: `scripts/context/daily_context_check.sh`
Output: `.claude/reports/context-health-YYYY-MM-DD.md`

### Setup Command

```powershell
# Run as Administrator
schtasks /create /tn "ContextManagementDaily" /tr "D:\workspace-hub\scripts\context\daily_context_check.sh" /sc daily /st 06:00
```

---

## Version History

- **2.0.0** (2026-01-18): Add file management, continuous improvement, scheduled tasks
- **1.1.0** (2026-01-17): Add recovery patterns, auto-archive triggers
- **1.0.0** (2026-01-14): Initial context management skill
