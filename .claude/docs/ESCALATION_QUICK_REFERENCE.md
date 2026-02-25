# 🚨 ESCALATION QUICK REFERENCE CARD

## STOP IMMEDIATELY AND ESCALATE IF:

### 🔴 RED FLAGS (Must Escalate)
- [ ] Don't know file format
- [ ] Missing domain formulas  
- [ ] Safety-critical task
- [ ] Regulatory compliance
- [ ] Knowledge level < 50%
- [ ] **Compute time > 10 minutes**

### 🟡 YELLOW FLAGS (Verify First)
- [ ] Partial understanding (50-79%)
- [ ] Making assumptions
- [ ] No validation method
- [ ] Missing documentation
- [ ] First time doing task

### ✅ GREEN FLAGS (OK to Proceed)
- [ ] Full understanding (80%+)
- [ ] Have documentation
- [ ] Clear validation method
- [ ] Using existing examples
- [ ] Non-critical task

## INSTANT ESCALATION TRIGGERS

```python
# Copy-paste this check
if any([
    "create GDF" in task,
    "engineering calculation" in task,
    "safety factor" in task,
    "compliance" in task,
    "don't understand" in thoughts,
    knowledge_level < 50
]):
    ESCALATE_TO_USER()
```

## ESCALATION MESSAGE TEMPLATE

```
🚨 SPECIALIZED KNOWLEDGE REQUIRED

Task: [what you're trying to do]
Issue: [what you don't know]
Knowledge Level: [0-100%]

Need:
- [ ] Documentation
- [ ] Expert help
- [ ] Examples
- [ ] Different approach

Impact: Cannot proceed without [specific need]
```

## DOMAINS REQUIRING EXPERTISE

| Domain | Always Escalate For |
|--------|-------------------|
| **Geometry** | GDF, mesh generation, panels |
| **Engineering** | Fatigue, structures, loads |
| **Marine** | Hydrodynamics, stability, mooring |
| **Regulatory** | DNV, API, ABS standards |
| **Numerical** | FEA, CFD, convergence |
| **Hardware** | Sensors, PLCs, interfaces |
| **Safety** | Factors, margins, critical systems |

## KNOWLEDGE LEVEL GUIDE

| Level | Your Understanding | Action |
|-------|-------------------|---------|
| 0-20% | Don't know | 🚨 STOP |
| 20-49% | Limited | 🔴 Escalate |
| 50-79% | Partial | 🟡 Verify |
| 80-99% | Good | ⚠️ Document assumptions |
| 100% | Complete | ✅ Proceed |

## REMEMBER

**When in doubt → ESCALATE**

Better to ask than to break something!

---
*This card is MANDATORY for all tasks*