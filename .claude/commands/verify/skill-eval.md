# skill-eval

Evaluate all workspace-hub skills for quality, structure, and consistency.

## Usage

```
/skill-eval [options]
```

## Options

| Option | Description |
|--------|-------------|
| `--skill <name>` | Evaluate a single skill by name |
| `--category <cat>` | Filter by top-level category directory |
| `--format json` | JSON output (default: human-readable) |
| `--severity <level>` | Minimum severity: critical, warning, info |
| `--summary-only` | Show summary counts only |
| `--output <path>` | Write report to file |

## Examples

```bash
# Full evaluation
/skill-eval

# Single skill check
/skill-eval --skill testing-tdd-london

# JSON for CI/CD
/skill-eval --format json --severity critical

# Category audit
/skill-eval --category development

# Summary only
/skill-eval --summary-only
```

## Execution

Run the evaluation script:

```bash
uv run .claude/skills/development/skill-eval/scripts/eval-skills.py
```

## Exit Codes

- `0` - All skills pass (no critical issues)
- `1` - Critical failures found
- `2` - Script error

## Related Commands

- `/verify check` - Code verification
- `/verify start` - Full verification system

## Skill Reference

@.claude/skills/development/skill-eval/SKILL.md
