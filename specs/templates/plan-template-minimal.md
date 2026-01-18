---
# =============================================================================
# PLAN METADATA (Minimal Template)
# =============================================================================

# Core (Required)
title: ""
description: ""
version: "1.0"
module: ""

# AI Session
session:
  id: ""
  agent: ""

# Cross-Review (MANDATORY - Min 3 iterations)
review:
  required_iterations: 3
  current_iteration: 0
  status: "pending"
  reviewers:
    openai_codex:
      status: "pending"
      iteration: 0
      feedback: ""
    google_gemini:
      status: "pending"
      iteration: 0
      feedback: ""
  ready_for_next_step: false

# Status
status: "draft"
progress: 0

# Timeline
created: ""
updated: ""
target_completion: ""

# Classification
priority: "medium"
tags: []

# Links
links:
  spec: ""
  branch: ""
---

# {{ title }}

> **Module**: {{ module }} | **Status**: {{ status }} | **Created**: {{ created }}

## Summary

{{ description }}

---

## Cross-Review Process (MANDATORY)

> **REQUIREMENT**: Minimum **3 review iterations** with OpenAI Codex and Google Gemini before implementation.

### Review Status

| Gate | Status |
|------|--------|
| Iterations (>= 3) | ⬜ {{ review.current_iteration }}/3 |
| OpenAI Codex | ⬜ {{ review.reviewers.openai_codex.status }} |
| Google Gemini | ⬜ {{ review.reviewers.google_gemini.status }} |
| **Ready** | ⬜ {{ review.ready_for_next_step }} |

### Review Log

| Iter | Date | Reviewer | Status | Feedback Summary |
|------|------|----------|--------|------------------|
| 1 | | Codex | Pending | |
| 1 | | Gemini | Pending | |
| 2 | | Codex | Pending | |
| 2 | | Gemini | Pending | |
| 3 | | Codex | Pending | |
| 3 | | Gemini | Pending | |

### Approval Checklist

- [ ] Iteration 1 complete (both reviewers)
- [ ] Iteration 2 complete (both reviewers)
- [ ] Iteration 3 complete (both reviewers)
- [ ] **APPROVED**: Ready for implementation

---

## Phases

### Phase 1: [Name]

- [ ] Task 1
- [ ] Task 2

### Phase 2: [Name]

- [ ] Task 1
- [ ] Task 2

---

## Progress

| Phase | Status | Notes |
|-------|--------|-------|
| Review Iteration 1 | Pending | |
| Review Iteration 2 | Pending | |
| Review Iteration 3 | Pending | |
| Plan Approved | Pending | |
| Phase 1 | Pending | |
| Phase 2 | Pending | |

---

## Session Log

| Date | Session ID | Agent | Notes |
|------|------------|-------|-------|
| {{ created }} | {{ session.id }} | {{ session.agent }} | Plan created |
