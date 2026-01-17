# Gemini Review Workflow Skill

> Version: 1.0.0
> Category: Workflows
> Triggers: Gemini commits, initial review, pre-Codex review

## Quick Reference

### Important Note

**ALL work performed by Google Gemini MUST also be reviewed by OpenAI Codex** before presenting to user. Gemini reviews are optional initial reviews.

### Workflow

```
Gemini performs task and commits
         ↓
(Optional) Gemini initial review
         ↓
MANDATORY: Codex reviews Gemini's commit
         ↓
Implement Codex feedback (max 3 iterations)
         ↓
Present to user after Codex approval
```

## Commands

```bash
# View pending reviews
./scripts/ai-review/gemini-review-manager.sh list

# Show specific review
./scripts/ai-review/gemini-review-manager.sh show <review_id>

# Approve a review
./scripts/ai-review/gemini-review-manager.sh approve <review_id>

# Implement approved suggestions
./scripts/ai-review/gemini-review-manager.sh implement <review_id>

# View statistics
./scripts/ai-review/gemini-review-manager.sh stats

# Clean old reviews (>30 days)
./scripts/ai-review/gemini-review-manager.sh clean
```

## Manual Review Commands

```bash
# Review HEAD commit
./scripts/ai-review/gemini-review.sh

# Review specific commit
./scripts/ai-review/gemini-review.sh abc123

# Review in specific repo
./scripts/ai-review/gemini-review.sh -r /path/to/repo HEAD

# Quick review (code quality only)
./scripts/ai-review/gemini-review.sh -q

# Full review (all aspects)
./scripts/ai-review/gemini-review.sh -f
```

## Review Aspects

Same as Codex review:
1. **Code Quality** - Style, readability, DRY, SOLID
2. **Security** - Injection vulnerabilities, hardcoded secrets
3. **Performance** - Algorithmic efficiency, memory usage
4. **Documentation** - Comments, docstrings, README updates
5. **Test Coverage** - Tests included, edge cases covered

## Cross-Review Integration

When Gemini performs work:

```bash
# After Gemini commits, trigger cross-review
./scripts/ai-review/cross-review-loop.sh --source gemini --max-iterations 3

# Check iteration status
./scripts/ai-review/review-manager.sh iteration-status <review_id>
```

## Review Files Location

```
~/.gemini-reviews/
├── pending/      # Reviews awaiting approval
├── approved/     # Approved for implementation
├── rejected/     # Rejected reviews
└── implemented/  # Already implemented
```

## Installation

```bash
# Install/reinstall hooks across all repositories
./scripts/ai-review/install-gemini-hooks.sh
```

## Full Reference

See: @docs/modules/ai/GEMINI_REVIEW_WORKFLOW.md

---

*Use this when working with Gemini commits, understanding Gemini review workflow, or managing Gemini reviews.*
