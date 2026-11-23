# Plan: digitalmodel #273 — WRK-666 WhatsApp → WRK capture pipeline

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/273
**Status:** plan-review
**Tier:** T3 (cross-system pipeline: OpenClaw skill + capture script + message-classifier)

## Context

Issue #273 (WRK-666) builds a lightweight WhatsApp listener that turns inbound messages from a designated number / group into WRK items in the repo ecosystem — the same surface as `/work add <desc>`. Architecture (per the issue body): WhatsApp → OpenClaw Narada agent (WRK-665) → capture handler → `next-id.sh` + WRK file → git commit + push → INDEX regeneration. Sources monitored: self-number (with Whisper-transcribed voice notes) and a "Work Capture" group. Components: an OpenClaw skill at `~/.openclaw/workspace-whatsapp-ai-group/skills/wrk-capture.js`, a shell wrapper at `scripts/work-queue/whatsapp-capture.sh`, an acknowledgement reply, and a config block in `.env` / `openclaw.json`.

The deliverables live in **workspace-hub**, not digitalmodel — verified by the issue body's `Repo: ['workspace-hub']` and the `scripts/work-queue/` path. The work-queue infrastructure (`next-id.sh` etc.) was not surfaced under `/mnt/local-analysis/workspace-hub/scripts/work-queue/` in the search; it likely lives under a different prefix or has been renamed since the issue was filed.

**Stale-flag (mis-filed + possibly landed):** This issue belongs in workspace-hub. Status reads `Stage 17: Reclaim (n)` with WRK stages 1–16 marked done, suggesting the implementation has already shipped. Also: `scripts/work-queue/next-id.sh` was not located in HEAD — the entire WRK-as-flat-file workflow may have been replaced by GitHub-issues-only per the user's `feedback_no_reserved_wrk_ids.md` memory note. Recommend close-as-stale unless the executor confirms `scripts/work-queue/` is still the canonical surface.

## Plan

### Task 1 — Verify capture-pipeline canonical surface
Run `git ls-files | grep -E '(work-queue|whatsapp-capture)'` in workspace-hub. If `scripts/work-queue/next-id.sh` is absent and the user's memory note ("local task IDs deprecated, GitHub issues only") is now policy, document the deprecation and skip to Task 5 (close-as-stale).

### Task 2 — Capture script (only if Task 1 confirms work-queue surface still exists)
Implement `scripts/work-queue/whatsapp-capture.sh <title> <repo> <priority>`:
- Calls `next-id.sh` to allocate WRK-NNN.
- Writes a minimal WRK markdown frontmatter (`title`, `repo`, `priority`, `source: whatsapp`, `captured_at`).
- `git add` the new file, commit with message `wrk(<repo>): WRK-NNN — <title> [whatsapp-capture]`, push.
- Logs every invocation to `.claude/work-queue/logs/whatsapp-capture.log` (timestamp, message-id, WRK-id, exit code).
- Enforces rate limit (10 captures/hour) and dedup (skip if same normalized title captured within 24h).

### Task 3 — OpenClaw skill (only if Task 1 confirms canonical surface)
Author `~/.openclaw/workspace-whatsapp-ai-group/skills/wrk-capture.js`:
- Triggers on every message from the configured source number / group JID.
- Runs a classifier (`classify_message(text) → {is_wrk, title, repo, priority}`) — start with regex heuristics, leave a Claude API call as a feature flag for ambiguous messages.
- If `is_wrk`, runs the capture script via `child_process.exec`, captures the WRK-NNN from stdout, replies to WhatsApp with `"✓ Captured as WRK-NNN: <title> (repo: <repo>, priority: <priority>)"`.
- Voice notes routed through Whisper (or OpenAI audio) before classification.

### Task 4 — Configuration + setup doc
Add config keys to `.env.example` (or `openclaw.json` extension): `WRK_CAPTURE_SOURCE_NUMBER`, `WRK_CAPTURE_REPO_DEFAULT`, `WRK_CAPTURE_PRIORITY_DEFAULT`, `WRK_REPO_PATH`. Document the full setup in `docs/agents/narada-openclaw-setup-guide.md` (the WRK-665 deliverable referenced in the acceptance list). Source-filter must reject any message from a non-configured JID — fail-closed.

### Task 5 — Close-as-stale path (if Task 1 says deprecated)
Post a close comment on #273 stating: (a) WRK-as-flat-file workflow has been deprecated in favor of GitHub issues, (b) the equivalent capability would now be "WhatsApp → `gh issue create` → workspace-hub", (c) recommend filing a fresh issue against the new architecture if the WhatsApp ingestion need is still active.

## Acceptance Criteria

- [ ] If implemented: `scripts/work-queue/whatsapp-capture.sh` creates a valid WRK file and pushes within 10s of invocation.
- [ ] OpenClaw skill classifies messages and routes to the capture script; non-WRK messages are ignored.
- [ ] Source filter rejects messages from unrecognised JIDs (verified by an injected non-configured-source test).
- [ ] Rate limit (10/hr) and dedup (24h) guards hit on synthetic flood.
- [ ] Capture log present at `.claude/work-queue/logs/whatsapp-capture.log`.
- [ ] If close-as-stale: #273 closed with a redirect note; no source landed.
- [ ] Legal scan green; no API keys / source numbers in committed files.

## Open questions

- **Stale check:** is the WRK flat-file workflow still active, or has it been deprecated per `feedback_no_reserved_wrk_ids.md`? Resolution decides whether this issue ships or closes-as-stale.
- **Repo scope:** confirm transfer to workspace-hub before execution.
- **Whisper transcription cost:** voice-note path adds OpenAI Whisper API calls — confirm the cost ceiling is acceptable.
