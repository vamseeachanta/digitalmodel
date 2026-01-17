# Model Selection Skill

> Version: 1.0.0
> Category: Optimization
> Triggers: Starting tasks, choosing Claude model, usage optimization

## Quick Reference

### Model Selection Decision Tree

```
NEW TASK
    │
    ├── WORK REPO + COMPLEX → OPUS
    ├── WORK REPO + STANDARD → SONNET
    ├── PERSONAL + SIMPLE → HAIKU
    └── DEFAULT → SONNET
```

### Quick Selection Guide

| Model | Target % | Use For |
|-------|----------|---------|
| **OPUS** | 30% | Architecture, multi-file refactoring (>5 files), security review |
| **SONNET** | 40% | Standard implementations, code review, documentation |
| **HAIKU** | 30% | Quick queries, status checks, simple operations |

## Automated Model Suggestion

```bash
# Get model recommendation before each task
./scripts/monitoring/suggest_model.sh <repository> "<task description>"

# Examples:
./scripts/monitoring/suggest_model.sh digitalmodel "Design authentication architecture"
# → Recommends: OPUS (complexity score: 4)

./scripts/monitoring/suggest_model.sh aceengineercode "Implement user login"
# → Recommends: SONNET (complexity score: 1)

./scripts/monitoring/suggest_model.sh hobbies "Quick file check"
# → Recommends: HAIKU (complexity score: -3)
```

## Complexity Scoring

**Algorithm evaluates:**
1. **Keywords** - architecture/refactor → +3, implement/feature → +1, check/status → -2
2. **Repository Tier** - Work Tier 1 → +1, Personal → -1
3. **Task Length** - >15 words → +1, <5 words → -1

**Score Mapping:**
- Score ≥3: **OPUS**
- Score 0-2: **SONNET**
- Score <0: **HAIKU**

## Repository Tiers

### Work Repositories

**Tier 1 (Production):** 60% Opus, 30% Sonnet, 10% Haiku
- digitalmodel, energy, frontierdeepwater

**Tier 2 (Active):** 30% Opus, 50% Sonnet, 20% Haiku
- aceengineercode, assetutilities, worldenergydata

**Tier 3 (Maintenance):** 10% Opus, 30% Sonnet, 60% Haiku
- doris, saipem, OGManufacturing

### Personal Repositories

**Active:** 20% Opus, 40% Sonnet, 40% Haiku
**Experimental:** 5% Opus, 25% Sonnet, 70% Haiku
**Archive:** 0% Opus, 20% Sonnet, 80% Haiku

## Usage Monitoring

**Check before starting work:** https://claude.ai/settings/usage

**Alert Thresholds:**
- Sonnet >70% → Switch to Opus/Haiku
- Session >80% → Batch work or wait
- Overall >80% → Defer non-critical

## OPUS Use Cases

✅ Multi-file refactoring (>5 files)
✅ Architecture decisions
✅ Complex algorithm design
✅ Security-critical code review
✅ Cross-repository coordination
✅ Performance optimization strategies

## SONNET Use Cases

✅ Standard feature implementation
✅ Code review (single PR)
✅ Documentation writing
✅ Test generation
✅ Bug fixing (standard complexity)
✅ Configuration updates

## HAIKU Use Cases

✅ File existence checks
✅ Simple grep/search operations
✅ Quick status updates
✅ Log analysis (pattern matching)
✅ Template generation
✅ Format validation

## Emergency Protocols

### If Sonnet >80%
```
⛔ STOP using Sonnet immediately
✅ Switch to Opus for critical work
✅ Switch to Haiku for everything else
📅 Defer non-urgent work to Tuesday
```

### If Session >80%
```
⏸️  Pause AI tasks
⏰ Wait for session reset (~3-4 hours)
📦 Batch work for next session
```

## Full Reference

See: @docs/AI_MODEL_SELECTION_AUTOMATION.md
See: @docs/CLAUDE_MODEL_SELECTION_QUICK_REFERENCE.md

---

*Use this when starting tasks, selecting models, or optimizing AI usage.*
