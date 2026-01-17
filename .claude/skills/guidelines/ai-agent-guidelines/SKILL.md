# AI Agent Guidelines Skill

> Version: 1.0.0
> Category: Guidelines
> Triggers: AI work, policy, review

## Quick Reference

### MANDATORY RULES
- **Before ANY implementation:** Read requirements, ask clarifying questions, wait for approval
- **Never assume:** Always ask when uncertain
- **Question triggers:** Ambiguous requirements, technical choices, trade-offs, edge cases
- **Cross-review MUST happen:** Claude/Gemini work reviewed by Codex before presenting to user

### Core Workflow
1. **READ** user_prompt.md thoroughly
2. **ASK** clarifying questions (scope, technical choices, constraints)
3. **WAIT** for user approval
4. **CONFIRM** understanding before proceeding
5. **IMPLEMENT** following spec

### When to Ask Questions
✅ Requirements are ambiguous
✅ Multiple implementation approaches exist
✅ Trade-offs need consideration (perf vs. simplicity)
✅ Dependencies/external systems involved
✅ User intent not explicit
✅ Configuration has multiple valid options
✅ Error handling strategy unclear
✅ Performance requirements undefined

### Never Assume
❌ Implementation details from vague requirements
❌ User preferences without asking
❌ Default values for critical parameters
❌ Error handling strategies
❌ Performance targets
❌ That "it's obvious what user wants"

## Question Templates

### Template 1: Requirement Clarification
```
I've reviewed user_prompt.md. Before proceeding to [NEXT_PHASE], I need clarification:

[CATEGORY]:
1. [SPECIFIC_QUESTION_WITH_OPTIONS]
   - Option A: [DESCRIPTION]
   - Option B: [DESCRIPTION]
   - Your preference?

2. [QUESTION_ABOUT_EDGE_CASE]

Please provide guidance before I proceed.
```

### Template 2: Implementation Choices
```
Based on approved pseudocode, I need to make decisions:

**Technology Choices:**
1. For [FUNCTIONALITY], should we use:
   - Library A: [PROS] / [CONS]
   - Library B: [PROS] / [CONS]
   - Your preference?

**Design Patterns:**
2. Should we implement [FEATURE] using:
   - Pattern X: [BENEFITS]
   - Pattern Y: [BENEFITS]

Please advise before implementation.
```

## Cross-Review Policy (MANDATORY)

**ALL work by Claude Code or Gemini MUST be reviewed by OpenAI Codex:**

1. **Commit changes** immediately after task completion
2. **Submit for Codex review** via post-commit hook
3. **Implement feedback** (max 3 iterations)
4. **Present to user** after Codex approval OR 3 iterations complete

See `/cross-review-policy` skill for full workflow.

## Agent-Specific Rules

| Agent | When to Use | Key Rules |
|-------|-------------|-----------|
| **Claude/Sonnet** | Standard implementations | Ask before code, TDD mandatory |
| **Claude/Opus** | Complex architecture | Deep analysis, multiple options |
| **Claude/Haiku** | Quick tasks | Direct execution, minimal questions |
| **OpenAI/GPT-4o** | Advanced reasoning | Check constraints, validate approach |
| **Gemini** | Specific domains | Requires Codex cross-review |
| **Factory Droids** | Automation | Reference spec files, validate config |

## Compliance Checklist

Before proceeding with ANY work:
- [ ] I have READ user_prompt.md completely
- [ ] I have IDENTIFIED all ambiguous requirements
- [ ] I have ASKED clarifying questions
- [ ] I have WAITED for user response
- [ ] I have RECEIVED user approval
- [ ] I understand the EXACT requirements
- ❌ I am NOT making assumptions
- ❌ I am NOT skipping the question phase
- ❌ I am NOT proceeding without approval

## Full Reference

See: @docs/modules/ai/AI_AGENT_GUIDELINES.md (comprehensive version)

---

*Use this when setting up AI workflows, reviewing policy compliance, or training new agents.*
