# Plan: digitalmodel #271 — WRK-631 Microsoft Teams chatbot Phase 2 deployment

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/271
**Status:** plan-review
**Tier:** T3 (cross-system: Azure Bot Service + Teams app + Claude API + per-client tenant isolation)

## Context

Issue #271 (WRK-631) deploys engineering AI chatbots via Microsoft Teams for Client 1 (Rigid Jumper Assistant — system prompt from WRK-625) and Client 2 (Structural Engineering Assistant — system prompt from WRK-629; see plan for #269). Architecture: Teams → Azure Bot Service → Claude API with per-client system prompt as bot persona, audit log to Azure Table Storage. Cost: ~$20–200/mo per team in Phase 2 (no RAG); RAG upgrade deferred to Phase 3.

Deliverables: `deployment/teams-bot/{manifest.json, bot.py, config.yaml, system_prompts/}`, two deployed Teams bots (one per client), per-client Azure AD app registrations for auth isolation, Azure Monitor usage dashboard stub, and a 1-page deployment guide for client IT admins.

The deliverables live in **frontierdeepwater** and a TBD client-2 repo, not digitalmodel — verified by the issue body's `Repo: ['frontierdeepwater', 'TBD-client2-repo']`. The issue's `Status: Stage 17: Reclaim (n)` and stages 1–16 done suggests the deployment may already have shipped — verify before re-doing.

**Stale-flag (mis-filed + possibly landed):** Misfiled in digitalmodel (should be frontierdeepwater for Client 1, separate repo for Client 2). Status indicates close-out gate. Plan supports both verify-and-close and fresh-deploy paths. The TBD client-2 repo is a known dependency — the issue can't fully close until that repo is provisioned.

## Plan

### Task 1 — Verify deployment artifact existence
In `frontierdeepwater`, run `git ls-files deployment/teams-bot/`. If `bot.py`, `manifest.json`, and `config.yaml` exist with non-stub content, this becomes verify-and-close (skip to Task 6). Otherwise proceed.

### Task 2 — Bot service skeleton
Implement `deployment/teams-bot/bot.py` (Python via `botbuilder-core`):
- Receive Teams `TurnContext`, extract user message, append to per-conversation history (Azure Cosmos DB or in-memory cache for Phase 2).
- Call Claude API with the bot's system prompt (loaded from `deployment/teams-bot/system_prompts/`) + last N turns.
- Return the response to Teams; persist `(user_id, channel_id, ts, prompt_tokens, completion_tokens, response_excerpt)` to Azure Table Storage.
- Rate limit per user (configurable, default 30 messages/hour).
- All credentials from env vars: `CLAUDE_API_KEY`, `AZURE_STORAGE_CONNECTION`, `BOT_APP_ID`, `BOT_APP_PASSWORD`. Fail-closed on missing env.

### Task 3 — Teams manifest + config
Author `deployment/teams-bot/manifest.json` per the Teams app schema (id, name, description, bots[], permissions). Author `deployment/teams-bot/config.yaml` declaring per-bot model selection (default `claude-sonnet-4`), max-tokens, and rate-limit. Symlink (or check-in) `system_prompts/` from the AI initiatives repo so each client's prompt is the source-of-truth.

### Task 4 — Per-client deployment
- **Client 1 (frontierdeepwater):** bot name "Rigid Jumper Assistant", system prompt `rigid_jumper_assistant.md` (WRK-625), pilot to 2–3 engineers in SME1's channel.
- **Client 2 (TBD repo):** blocks on the client-2 repo being provisioned. If not yet created, this task collapses to "stub the deployment in `frontierdeepwater/deployment/teams-bot/clients/client-2-pending/` and link the blocker".
- Each client gets its own Azure AD app registration; resource group `aceengineer-ai-clients` is shared. Per-client Azure Table Storage table for audit isolation.

### Task 5 — Pilot validation + dashboard
For each deployed bot: run 3 validation Q&A turns. Confirm: (a) multi-turn context preserved, (b) audit log entry written per turn, (c) Azure Monitor sees the function invocations. Author the IT-admin deployment guide as a 1-page README under `deployment/teams-bot/README.md`.

### Task 6 — Close (verify or fresh)
Either verify-and-close path or post-deployment close. The close comment must enumerate which client(s) actually went live and which are blocked.

## Acceptance Criteria

- [ ] `deployment/teams-bot/bot.py` implemented with Claude API integration; no hardcoded keys.
- [ ] `deployment/teams-bot/manifest.json` valid against Teams schema.
- [ ] Client 1 bot deployed to a test Teams tenant, 3 validation questions returned engineering-coherent answers.
- [ ] Client 2 bot deployed (or blocker explicitly documented if TBD repo not provisioned).
- [ ] Conversation threading (multi-turn context) verified by a 4-turn test.
- [ ] Audit log persists Q&A pairs in per-client Azure Table Storage.
- [ ] Azure Monitor dashboard stub shows daily token consumption per client.
- [ ] 1-page deployment guide present at `deployment/teams-bot/README.md`.
- [ ] Legal scan green; no API keys / tenant IDs in committed files.

## Open questions

- **Repo scope:** confirm transfer to frontierdeepwater (Client 1) and identify the client-2 repo. The issue is clearly mis-filed in digitalmodel.
- **Phase-3 RAG upgrade trigger:** issue body says "graduate when usage grows" — quantify (e.g., >X messages/day for N consecutive weeks) so the next implementation issue has a concrete trigger.
- **Cost containment:** is there a monthly Claude API ceiling per client to enforce in `bot.py` (hard-stop above $X/day)? Recommend yes.
