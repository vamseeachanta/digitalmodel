---
# =============================================================================
# PLAN METADATA - Required fields marked with (*)
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# Core Identification (*)
# ─────────────────────────────────────────────────────────────────────────────
title: ""                          # (*) Plan title
description: ""                    # (*) Brief summary (1-2 sentences)
version: "1.0"                     # (*) Plan version (semver)
module: ""                         # (*) Target module (e.g., orcaflex, orcawave)

# ─────────────────────────────────────────────────────────────────────────────
# AI Agent Session Tracking
# ─────────────────────────────────────────────────────────────────────────────
session:
  id: ""                           # AI agent session ID for continuity
  agent: ""                        # Agent type (claude-opus, claude-sonnet, etc.)
  started: ""                      # Session start timestamp
  last_active: ""                  # Last activity timestamp
  conversation_id: ""              # Conversation/thread ID if applicable

# ─────────────────────────────────────────────────────────────────────────────
# Cross-Review Process (MANDATORY) - Minimum 3 iterations before complete
# ─────────────────────────────────────────────────────────────────────────────
review:
  required_iterations: 3           # Minimum review iterations before completion
  current_iteration: 0             # Current iteration count
  status: "pending"                # pending | in_progress | approved | rejected
  reviewers:
    openai_codex:
      status: "pending"            # pending | reviewing | approved | changes_requested
      iteration: 0
      last_reviewed: ""
      feedback: ""
    google_gemini:
      status: "pending"            # pending | reviewing | approved | changes_requested
      iteration: 0
      last_reviewed: ""
      feedback: ""
  approval_gate:                   # All must be true to proceed
    min_iterations_met: false      # current_iteration >= required_iterations
    codex_approved: false          # openai_codex.status == "approved"
    gemini_approved: false         # google_gemini.status == "approved"
    ready_for_next_step: false     # All above conditions met

# ─────────────────────────────────────────────────────────────────────────────
# Status & Progress
# ─────────────────────────────────────────────────────────────────────────────
status: "draft"                    # draft | review | revision | approved | in_progress | blocked | complete
progress: 0                        # Percentage complete (0-100)
phase: 1                           # Current phase number
blocked_by: []                     # List of blocking issues/dependencies

# ─────────────────────────────────────────────────────────────────────────────
# Timeline
# ─────────────────────────────────────────────────────────────────────────────
created: ""                        # Creation date (YYYY-MM-DD)
updated: ""                        # Last update date (YYYY-MM-DD)
target_completion: ""              # Target completion date
timeline: ""                       # Duration estimate (e.g., "2 weeks")
milestones:                        # Key milestones
  - name: ""
    target_date: ""
    status: "pending"              # pending | in_progress | complete

# ─────────────────────────────────────────────────────────────────────────────
# Team & Ownership
# ─────────────────────────────────────────────────────────────────────────────
author: ""                         # Plan author
reviewers: []                      # List of reviewers
assignees: []                      # Assigned team members/agents

# ─────────────────────────────────────────────────────────────────────────────
# Technical Context
# ─────────────────────────────────────────────────────────────────────────────
technical:
  language: "python"               # Primary language
  python_version: ">=3.10"         # Python version requirement
  dependencies: []                 # Required packages/modules
  test_coverage: 80                # Target test coverage percentage
  platforms: []                    # Target platforms (windows, linux, macos)

# ─────────────────────────────────────────────────────────────────────────────
# Classification & Discovery
# ─────────────────────────────────────────────────────────────────────────────
priority: "medium"                 # high | medium | low
complexity: "medium"               # low | medium | high | critical
risk: "low"                        # low | medium | high
tags: []                           # Searchable keywords

# ─────────────────────────────────────────────────────────────────────────────
# Related Resources
# ─────────────────────────────────────────────────────────────────────────────
links:
  spec: ""                         # Link to specification document
  branch: ""                       # Git branch name
  pr: ""                           # Pull request URL
  issues: []                       # Related issue numbers
  docs: []                         # Related documentation links

# ─────────────────────────────────────────────────────────────────────────────
# History & Audit Trail
# ─────────────────────────────────────────────────────────────────────────────
history:
  - date: ""
    action: "created"
    by: ""
    notes: ""
---

# {{ title }}

> **Module**: {{ module }} | **Status**: {{ status }} | **Priority**: {{ priority }}
> **Created**: {{ created }} | **Target**: {{ target_completion }}

## Executive Summary

{{ description }}

<!-- Expand with 2-3 paragraphs covering:
- Primary objective and business value
- Technical approach overview
- Key deliverables
-->

---

## Technical Context

| Aspect | Details |
|--------|---------|
| Language | {{ technical.language }} |
| Python Version | {{ technical.python_version }} |
| Test Coverage | {{ technical.test_coverage }}% |
| Platforms | {{ technical.platforms }} |

### Dependencies

<!-- List required modules, packages, and external dependencies -->

- [ ] Dependency 1
- [ ] Dependency 2

### Prerequisites

<!-- What must be complete before this plan can proceed -->

- [ ] Prerequisite 1
- [ ] Prerequisite 2

---

## Phases

### Phase 1: Research & Design

**Objective**: [Define the research and design goals]

**Tasks**:
- [ ] Task 1.1:
- [ ] Task 1.2:
- [ ] Task 1.3:

**Deliverables**:
- [ ] Design document
- [ ] Architecture diagram

**Exit Criteria**:
- [ ] Design approved
- [ ] Dependencies identified

---

### Phase 2: Implementation

**Objective**: [Define implementation goals]

**Tasks**:
- [ ] Task 2.1:
- [ ] Task 2.2:
- [ ] Task 2.3:

**Deliverables**:
- [ ] Source code
- [ ] Unit tests

**Exit Criteria**:
- [ ] All tests passing
- [ ] Code review complete

---

### Phase 3: Testing & Validation

**Objective**: [Define testing and validation goals]

**Tasks**:
- [ ] Task 3.1:
- [ ] Task 3.2:
- [ ] Task 3.3:

**Deliverables**:
- [ ] Test report
- [ ] Validation results

**Exit Criteria**:
- [ ] {{ technical.test_coverage }}% coverage achieved
- [ ] All acceptance criteria met

---

### Phase 4: Documentation & Release

**Objective**: [Define documentation and release goals]

**Tasks**:
- [ ] Task 4.1:
- [ ] Task 4.2:

**Deliverables**:
- [ ] User documentation
- [ ] API reference
- [ ] Release notes

**Exit Criteria**:
- [ ] Documentation complete
- [ ] Release approved

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Risk 1 | Low/Med/High | Low/Med/High | Mitigation strategy |
| Risk 2 | Low/Med/High | Low/Med/High | Mitigation strategy |

---

## Cross-Review Process (MANDATORY)

> **REQUIREMENT**: Plan must complete minimum **3 review iterations** with both OpenAI Codex and Google Gemini before proceeding to implementation.

### Review Status

| Gate | Requirement | Status |
|------|-------------|--------|
| Minimum Iterations | >= 3 iterations completed | ⬜ Not Met |
| OpenAI Codex | Approved | ⬜ Pending |
| Google Gemini | Approved | ⬜ Pending |
| **Ready for Next Step** | All gates passed | ⬜ **BLOCKED** |

### Review Iteration Log

| Iteration | Date | Reviewer | Status | Key Feedback | Changes Made |
|-----------|------|----------|--------|--------------|--------------|
| 1 | | OpenAI Codex | Pending | | |
| 1 | | Google Gemini | Pending | | |
| 2 | | OpenAI Codex | Pending | | |
| 2 | | Google Gemini | Pending | | |
| 3 | | OpenAI Codex | Pending | | |
| 3 | | Google Gemini | Pending | | |

### Review Instructions

**For OpenAI Codex Review**:
1. Submit plan to Codex with prompt: "Review this implementation plan for completeness, technical accuracy, and feasibility. Provide specific feedback and improvement suggestions."
2. Document feedback in iteration log
3. Implement changes and update plan version
4. Resubmit for next iteration

**For Google Gemini Review**:
1. Submit plan to Gemini with prompt: "Review this implementation plan. Assess: (1) technical approach, (2) risk assessment, (3) timeline feasibility, (4) missing considerations. Provide actionable feedback."
2. Document feedback in iteration log
3. Implement changes and update plan version
4. Resubmit for next iteration

### Approval Checklist

- [ ] Iteration 1: Codex reviewed
- [ ] Iteration 1: Gemini reviewed
- [ ] Iteration 1: Feedback incorporated
- [ ] Iteration 2: Codex reviewed
- [ ] Iteration 2: Gemini reviewed
- [ ] Iteration 2: Feedback incorporated
- [ ] Iteration 3: Codex reviewed
- [ ] Iteration 3: Gemini reviewed
- [ ] Iteration 3: Feedback incorporated
- [ ] **FINAL**: Both reviewers approved
- [ ] **FINAL**: Ready for implementation

---

## Progress Tracking

### Milestones

| Milestone | Target Date | Status | Notes |
|-----------|-------------|--------|-------|
| Plan Draft Complete | | Pending | |
| Review Iteration 1 | | Pending | |
| Review Iteration 2 | | Pending | |
| Review Iteration 3 | | Pending | |
| Plan Approved | | Pending | |
| Phase 1 Complete | | Pending | |
| Phase 2 Complete | | Pending | |
| Phase 3 Complete | | Pending | |
| Phase 4 Complete | | Pending | |

### Session Log

<!-- AI agent session history for continuity -->

| Date | Session ID | Agent | Actions | Notes |
|------|------------|-------|---------|-------|
| {{ created }} | {{ session.id }} | {{ session.agent }} | Plan created | Initial plan |

---

## Appendix

### A. Glossary

<!-- Define technical terms and acronyms -->

### B. References

<!-- List reference documents, standards, and resources -->

### C. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | {{ created }} | {{ author }} | Initial plan |

---

*Plan generated using workspace-hub plan template v1.0*
