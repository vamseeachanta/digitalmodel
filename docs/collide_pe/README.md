# Collide — PE Problem of the Day (solver program)

Solve the [Collide](https://app.collide.io) **PE Problem of the Day** series
(petroleum-engineering challenges, ~161 posts and counting), hottest first,
marching progressively. Each problem becomes a small data file, a Deckhand-
letterhead HTML brief, and a Claude-API-recorded solution.

## Layout

| Path | What |
|---|---|
| `backlog.yml` | Ranked backlog (hotness = `2*comments + likes`) + capture metadata. The "what to do next" list. |
| `problems/<id>.yml` | One problem: statement, `given`, `find`, and the recorded `solution`. Config-as-data. |
| `templates/letterhead.html` | Deckhand letterhead — self-contained HTML, no build, no assets. |
| `render_brief.py` | `problem YAML -> briefs/<id>.html` (the shareable link). |
| `solve.py` | Calls the Claude API (`claude-opus-4-8`) and records the worked solution into the YAML. |
| `briefs/<id>.html` | Rendered briefs. |

## Workflow per problem

```bash
# 1. Solve it (records solution back into the YAML)
uv run docs/collide_pe/solve.py docs/collide_pe/problems/pe-2026-06-19.yml

# 2. Render the Deckhand-letterhead brief (shareable HTML)
uv run docs/collide_pe/render_brief.py docs/collide_pe/problems/pe-2026-06-19.yml
# -> docs/collide_pe/briefs/pe-2026-06-19.html
```

`solve.py` needs `ANTHROPIC_API_KEY` (or an `ant auth login` profile).
`render_brief.py` has no API dependency — it works on whatever solution is in the YAML
(deterministic, hand-checked, or Claude-recorded).

## Two epics

- **Epic A — Solve the series.** This directory. One GitHub issue per problem,
  ordered by hotness; we work top-of-`backlog.yml` first and append as new problems post.
- **Epic B — Deckhand-routed solver, Telegram transport.** `solve.py:solve_problem()`
  is the transport-free engine. Epic B wraps it as a Deckhand `solve <slug>` command on
  the Hermes gateway: the operator sends `solve pe-2026-06-19` in Telegram, Deckhand runs
  the solver + renderer on the host, and replies with the answer and the brief link.
  The Claude API call stays in `solve.py` so it is testable independent of transport.

## Sharing the brief as a link

The brief is a single static HTML file. To turn it into a link, publish `briefs/<id>.html`
to any static host (the `aceengineer-website` repo, GitHub Pages, or an object store). Epic B's
Deckhand command automates "render → publish → reply with link".
