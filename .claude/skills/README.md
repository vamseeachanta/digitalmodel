# digitalmodel — repository skills

## Where the marine/offshore skills actually live

This directory used to list 12 marine and offshore engineering skills, each
linking to `<name>/SKILL.md`. **None of those files existed.** They were macOS
Finder alias files — committed as small binary blobs — pointing at
`ws/.claude/skills/engineering/marine-offshore/`, a path that does not exist on
any machine. Git stored them, nothing resolved them, and every link in this
README was dead.

They have been removed rather than repaired, because pointing a repository at a
sibling checkout that may not be present trades one broken assumption for
another.

The corpus they were meant to reference is real and maintained in two places:

| Where | What |
|---|---|
| [`vamseeachanta/workspace-hub`](https://github.com/vamseeachanta/workspace-hub) — `.claude/skills/engineering/marine-offshore/` | Source of truth. 26 skill families: mooring, riser, VIV, fatigue, diffraction, hydrodynamics, ship dynamics, wave theory. |
| [`aceengineer/aceengineer-agents`](https://github.com/aceengineer/aceengineer-agents) | The same corpus packaged as an installable Claude Code plugin, with provenance and a byte-identical rebuild check. |

Install the packaged form:

```bash
/plugin marketplace add aceengineer/aceengineer-agents
/plugin install ace-marine-dynamics@aceengineer
```

## A caveat worth reading before you trust a skill

Parts of that corpus documented a `digitalmodel` API that does not exist —
**134 module paths and 33 file paths across 28 skills** were dead, mostly because
this repository reorganised `src/digitalmodel/modules/` into `solvers/`,
`marine_ops/`, `structural/` and `hydrodynamics/` and the corpus never followed.

Most have been repointed. What could not be repointed is now marked in place with
an explicit warning naming every absent path, and the intended-but-unbuilt
OrcaWave API is specified here under
[`docs/domains/orcawave/intended-api/`](../../docs/domains/orcawave/intended-api/).
Both corpora now carry automated checks that fail on any new unresolved
reference.

## What is still in this directory

| Path | |
|---|---|
| `engineering/`, `converted-agents/` | Repository-local skills |
| `interactive-plotting-best-practices.md`, `module-based-refactor.md` | Standalone notes |
| `guidelines/`, `meta/`, `workflows/` | Shared skills, git-ignored (see `.gitignore`) |

`skills-catalog.json` was removed. It contained `{"skills": {}}` with a
`generated` path from a machine that no longer figures, and an empty catalog is
worse than no catalog: it answers "what skills exist here" with "none" in a tone
of authority.
