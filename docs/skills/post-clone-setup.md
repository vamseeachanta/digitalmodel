# Post-clone skill pointer setup

`bash scripts/setup-claude-skills.sh` regenerates ignored skill links and a dated
propagation marker. It uses the existing shared checkout and does not install
packages or clone repositories. This completes the local regeneration scope of
issue #2009; the artifact policy is recorded in the
[earlier plan](../plans/2026-05-05-issue-469-ai-agent-session-artifact-policy.md).

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-20 | Document default regeneration, conflict handling and a worked example so a fresh clone can restore ignored pointers. |

## Inputs and method

Setup requires Bash, Python 3.10+ and filesystem support for directory symlinks.
The default repository root comes from the script location, independent of the
working directory. The default shared checkout is its `workspace-hub` sibling.
Each of these source directories must exist:

```text
workspace-hub/.claude/skills/_internal/guidelines
workspace-hub/.claude/skills/_internal/meta
workspace-hub/.claude/skills/_internal/workflows
```

For another layout, supply `--workspace-hub <checkout>`. Use `--repo <checkout>`
when setting up a repository other than the one containing the script.
`--skills-dir <directory>` selects an alternate destination inside that
repository; omit it for normal installation. Relative option paths are resolved
from the working directory.

Setup validates all sources, the calendar date, destination parents and all
existing entries before it writes an artifact. It derives each relative target
from the destination directory to its source using `os.path.relpath`, creates or
repairs the links, then writes the date marker. Source names, the marker format
and idempotence requirements come from issue #2009's Scope and Acceptance
criteria, rather than an engineering equation or numerical acceptance limit.

The default date is today's local calendar date, formatted as `YYYYMMDD`.
`--date YYYYMMDD` supplies an explicit valid calendar date for reproducible
fixtures. `--dry-run` prints a JSON recipe without writing any artifacts; its
`mode` is `plan`. A completed setup prints the recipe with `mode` set to `apply`.
`--apply` remains a compatible explicit request for the default behavior and
cannot be combined with `--dry-run`.

## Worked example

Input: an existing checkout and sibling shared checkout, arranged as follows.
The explicit date is a synthetic test input; a normal invocation omits it.

```text
workspace/
  checkout/
    scripts/setup-claude-skills.sh
    scripts/plan_claude_skills.py
  workspace-hub/
    .claude/skills/_internal/
      guidelines/
      meta/
      workflows/
```

From `workspace/checkout`, run:

```bash
bash scripts/setup-claude-skills.sh --date 20261014
```

Hand derivation: from `checkout/.claude/skills`, traverse to `.claude`, then
`checkout`, then `workspace`. Enter `workspace-hub` and append the internal
skill path. This gives the following answer, independently of the script:

```text
.claude/skills/guidelines -> ../../../workspace-hub/.claude/skills/_internal/guidelines
.claude/skills/meta       -> ../../../workspace-hub/.claude/skills/_internal/meta
.claude/skills/workflows  -> ../../../workspace-hub/.claude/skills/_internal/workflows
.claude/skills/.ecosystem-propagated contains: 20261014 followed by a newline
```

Rerunning the command produces the same links and marker contents. In a layout
with `workspace/trees/checkout`, the destination has an extra directory level;
passing `--workspace-hub ../../workspace-hub` produces targets beginning with
`../../../../workspace-hub/`. Paths containing spaces are supported when quoted.
The script tests reproduce both layouts and compare link text and resolved
sources against these independently derived answers.

## Conflicts and failure behavior

- A missing shared checkout or any missing internal skill directory causes a
  nonzero exit that tells the reader to clone `workspace-hub` as a sibling or
  select an existing checkout with `--workspace-hub`. No artifact is written.
- Matching links are retained. Stale links, including dangling links, are replaced
  without changing or removing their old targets.
- Real files and directories at any skill pointer path require manual review.
  Setup preserves them and exits before updating links or the marker.
- A symlink or directory at the marker path requires manual review. A regular
  marker file is refreshed after the pointer preflight passes.
- Destination directories and their parents inside the repository cannot be
  symlinks or regular files. Setup refuses these redirects before writing.
- Invalid dates fail before any artifact is written. Permission errors and
  unsupported symlinks produce a nonzero exit; there is no text-pointer fallback.

Preflight prevents writes for known conflicts; it does not provide a transaction
across all artifacts if an operating-system error occurs during writing. The
setup command assumes no concurrent process changes the same destination.

## Tracking policy and evidence

The `.claude/skills/.gitignore` rules cover all three link names; the root
`.gitignore` covers `.ecosystem-propagated`. The clone-fixture regression uses
those actual rules, runs setup twice, checks the links and date, and asserts that
`git status --porcelain --untracked-files=all` stays empty. No generated pointer
or date marker is committed.

The existing `.codex/skills` and `.gemini/skills` entries are tracked mode-120000
symlinks to `../../workspace-hub/.claude/skills`. They expose the whole shared
skill root. The ignored Claude links expose selected internal subdirectories
instead. This asymmetry is deliberate: preserving those existing tracked entry
points does not require committing locally regenerated Claude links.

The inspected clone initially had none of the ignored Claude artifacts, while
the tracked root links retained the symlink shape above. The Linux symlink
observation recorded in issue #2009 and those tracked entries support using
symlinks. No claim is made about other systems. Filesystems requiring additional
symlink privileges need those privileges before setup can succeed. Auditing
other systems or changing shared propagation tooling remains outside this scope.
