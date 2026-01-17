# Codex Review Workflow Skill

> Version: 1.0.0
> Category: Workflows
> Triggers: Code review, commit review, quality checks

## Quick Reference

### Purpose

Automatically reviews all commits made by **Claude Code AND Google Gemini** using OpenAI Codex CLI.

### Flow

```
Claude/Gemini makes commit
         ↓
Post-commit hook triggers
         ↓
Detect AI signature
         ↓
Codex analyzes diff
         ↓
Review saved to pending
         ↓
Feedback needed?
├── No (Approved) → Present to user
└── Yes → Implement fixes → Re-review (max 3)
```

## Commands

```bash
# View pending reviews
./scripts/ai-review/review-manager.sh list

# Show specific review
./scripts/ai-review/review-manager.sh show <review_id>

# Approve a review
./scripts/ai-review/review-manager.sh approve <review_id>

# Implement approved suggestions
./scripts/ai-review/review-manager.sh implement <review_id>

# View statistics
./scripts/ai-review/review-manager.sh stats

# Clean old reviews (>30 days)
./scripts/ai-review/review-manager.sh clean
```

## Manual Review Commands

```bash
# Review HEAD commit in current repo
./scripts/ai-review/codex-review.sh

# Review specific commit
./scripts/ai-review/codex-review.sh abc123

# Review in specific repo
./scripts/ai-review/codex-review.sh -r /path/to/repo HEAD

# Quick review (code quality only)
./scripts/ai-review/codex-review.sh -q

# Full review (all aspects)
./scripts/ai-review/codex-review.sh -f
```

## Review Aspects

Codex reviews each commit for:

1. **Code Quality** - Style, readability, DRY, SOLID
2. **Security** - Injection vulnerabilities, hardcoded secrets
3. **Performance** - Algorithmic efficiency, memory usage
4. **Documentation** - Comments, docstrings, README updates
5. **Test Coverage** - Tests included, edge cases covered

## AI Commit Detection

### Claude Commits
- `Claude Code` or `claude.com/claude-code` in message
- `Co-Authored-By: Claude` in message
- `@anthropic` in author email

### Gemini Commits
- `Gemini` or `Google Gemini` in message
- `Co-Authored-By: Gemini` in message
- `@google` in author email

## Review Files Location

```
~/.codex-reviews/
├── pending/      # Reviews awaiting approval
├── approved/     # Approved for implementation
├── rejected/     # Rejected reviews
└── implemented/  # Already implemented
```

## Installation

```bash
# Install/reinstall hooks across all repositories
./scripts/ai-review/install-codex-hooks.sh
```

## Full Reference

See: @docs/modules/ai/CODEX_REVIEW_WORKFLOW.md

---

*Use this when managing code reviews, understanding review workflow, or checking review status.*
