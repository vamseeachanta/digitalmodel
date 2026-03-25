# /today - Interactive Daily Productivity Review

You are guiding the user through their daily productivity review. This is an interactive session.

## Mode Detection

Determine the mode based on current time or user argument:
- **morning** (default before noon): Full review + priority setting
- **midday** (noon-5pm): Quick check-in + blockers
- **evening** (after 5pm or `--eod`): End-of-day wrap-up

## Step 1: Generate Report

First, run the daily report script to gather fresh data:

```bash
./scripts/productivity/daily_today.sh 2>/dev/null || echo "Script not found, gathering data manually"
```

Then read today's log file at `logs/daily/{TODAY}.md`.

## Step 2: Interactive Review by Mode

### Morning Mode

Walk through these sections interactively:

**A. Yesterday's Activity**
- Present git commits summary
- Ask: "Anything from yesterday you want to continue today?"

**B. Current State**
- Show open TODOs, active specs, branch status
- Ask: "Any of these blocked or need attention?"

**C. Set Priorities**
- Based on the data, suggest 3 priorities
- Ask user to confirm or modify using AskUserQuestion:
  ```
  Header: "Priority 1"
  Question: "What's your #1 priority today?"
  Options based on: open TODOs, in-progress work, suggestions
  ```
- Repeat for priorities 2 and 3

**D. Time Blocking (optional)**
- Ask: "Want to block focus time for deep work today?"
- If yes, suggest time blocks based on priorities

### Midday Mode

Quick 2-minute check-in:

**A. Progress Check**
- Ask: "How's priority #1 going?" with options: [On track, Blocked, Pivoted, Done]
- If blocked: "What's the blocker?" - log it

**B. Adjust if Needed**
- If pivoted or blocked, ask about reprioritizing
- Update the daily log with notes

### Evening Mode (--eod)

End-of-day wrap-up:

**A. Priority Review**
- For each morning priority, ask: [Completed, Partial, Blocked, Deferred]
- Log the status

**B. Capture Blockers**
- Ask: "Any blockers to log for tomorrow?"
- Add to daily log

**C. Tomorrow's Focus**
- Ask: "One thing you want to start with tomorrow?"
- Add to daily log

**D. Update Log**
- Mark `reviewed: true` in frontmatter
- Save all responses to the daily log file

## Step 3: Save Updates

After each interactive session, update the daily log file at `logs/daily/{TODAY}.md` with:
- Selected priorities (morning)
- Progress notes (midday)
- Completion status and tomorrow's focus (evening)

## Output Format

Keep responses conversational but concise. Use formatting:

```
## Morning Review - Jan 21, 2026

**Yesterday:** 11 commits across 8 repos - solid progress!

**Carrying forward:**
- Analytics dashboard (branch: feature/analytics)
- Mobile spec waiting on design review

**Suggested priorities:**
1. Complete analytics tests (high impact)
2. Review 2 open PRs (quick wins)
3. Follow up on mobile design review (unblock)

Ready to set your priorities?
```

## Ecosystem Report Mode (`--reflect`)

When invoked with `--reflect` or when the user asks for the "daily-reflect report table":

1. Read the latest reflect-state.yaml from `.claude/state/reflect-state.yaml`
2. If not found, fall back to `~/.claude/state/reflect-state.yaml`
3. Parse the `checklist` section and present a markdown table with all 21 checks

### Report Table Format

Present the ecosystem health as a table:

```
## Ecosystem Health Report - {date}

| # | Check | Status | Detail |
|---|-------|--------|--------|
| 1 | Cross-Review | pass/fail | {gemini}G {codex}C {claude}Cl pending |
| 2 | Skills Dev | pass/none | {created} new, {enhanced} enhanced |
| 3 | File Structure | pass/fail | {orphan_docs} orphan docs, {orphan_scripts} scripts |
| 4 | Context Mgmt | pass/warn | {avg_session} msgs, {correction_rate} corrections |
| 5 | Best Practices | pass/warn | {repos_with_tests} repos w/tests, {uncommitted} uncommitted |
| 6 | Submodule Sync | pass/warn | {dirty} dirty, {unpushed} unpushed |
| 7 | CLAUDE.md Health | pass/fail | {oversized}/{total} oversized |
| 8 | Hook Coverage | pass/warn/fail | {coverage}% coverage |
| 9 | Stale Branches | pass/warn | {count} stale (>30 days) |
| 10 | GitHub Actions | pass/warn/fail | {failing}/{total} failing |
| 11 | Folder Structure | pass/fail | {issues} structural issues |
| 12 | Test Coverage | pass/warn/fail | {avg}% avg across {repos} repos |
| 13 | Test Pass/Fail | pass/fail | {pass} pass, {fail} fail |
| 14 | Refactor | pass/warn | {large_files} large, {todos} TODOs |
| 15 | Aceengineer Cron | pass/warn/fail | stats {date}, report {date} |
| 16 | Session RAG | pass/warn/fail | {sessions} sessions, {events} events |
| 17 | CC Insights | pass/warn | v{version} reviewed: {last} |
| 18 | Work Queue | pass/warn/fail | {pending}P {working}W {blocked}B |
| 19 | Skill Eval | pass/warn/fail | {passed}/{total} pass, {critical} critical |
| 20 | Capability Map | pass/warn/fail | {repos} repos, {domains} domains |
| 21 | Knowledge Base | pass/warn | {total} entries, {stale} stale |
```

Use status symbols: `pass` = check, `warn` = warning, `fail` = X, `none` = circle

After the table, show:
- **Action Items**: Any checks with `fail` status
- **Recommendations**: Any checks with `warn` status
- **RAGS Loop**: repos analyzed, commits, patterns, sessions

### Quick Reflect Script

You can also run the reflect report data gatherer directly:

```bash
./scripts/productivity/daily-reflect-report.sh 2>/dev/null
```

This script reads reflect-state.yaml and outputs the table in markdown format for quick review.

## Arguments

- `/today` - Auto-detect mode by time
- `/today morning` - Force morning mode
- `/today midday` - Force midday check-in
- `/today --eod` or `/today evening` - Force end-of-day
- `/today --week` - Weekly review instead of daily
- `/today --reflect` - Show ecosystem health report table from daily-reflect data
