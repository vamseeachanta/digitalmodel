# Plan for [#617](https://github.com/vamseeachanta/digitalmodel/issues/617): Citation-resolver hardening for `LLM_WIKI_PATH` env var + fail-closed UX

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-05-20
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/617
> **Review artifacts:** scripts/review/results/2026-05-20-plan-617-claude.md | ...-codex.md | ...-gemini.md (pending)

---

## Resource Intelligence Summary

### Existing repo code

- Found: `src/digitalmodel/citations/schema.py` — `validate_citation(citation, *, repo_root: Path)` reads the wiki page at `repo_root / citation.wiki_path`; raises `CitationResolutionError` (with `code_id`, `wiki_path`, `reason`) if the page is missing or frontmatter mismatches.
- Found: `src/digitalmodel/citations/registry.py` — `get_mooring_safety_factor(condition, *, repo_root: Path)` hardcodes `wiki_path = "knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md"` and takes `repo_root` as a required kwarg. **No env-var lookup in this module.**
- Found: `src/digitalmodel/orcaflex/mooring_design.py:38` — `_default_repo_root()` already implements a layered fallback: (1) explicit kwarg, (2) **`DIGITALMODEL_REPO_ROOT`** env var, (3) bounded parent walk (cap `_REPO_ROOT_WALK_HARD_CAP = 8`, addresses [`feedback_path_parent_infinite_loop`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/memory/feedback_path_parent_infinite_loop.md)), (4) `None` → standalone mode with one-shot `RuntimeWarning`.
- Gap 1: env-var name is `DIGITALMODEL_REPO_ROOT`, **not** `LLM_WIKI_PATH` as the issue and routing rule prescribe. Routing rule [`codes-standards-data-routing.md`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/codes-standards-data-routing.md) §4 explicitly names `LLM_WIKI_PATH` as the contract surface.
- Gap 2: env-var resolution lives in `mooring_design.py` (caller), not in `citations/` (the contract surface). External callers of `get_mooring_safety_factor()` directly bypass the env-var fallback entirely.
- Gap 3: the hardcoded `wiki_path` carries the `knowledge/wikis/` prefix — that prefix is a **workspace-hub mount artifact**, not the llm-wiki repo's own layout. A standalone `llm-wiki` clone has `wikis/engineering/wiki/standards/dnv-os-e301.md` (no `knowledge/` prefix). See Evidence below.
- Gap 4: `CitationResolutionError` messages are opaque (`reason=page_missing`) — no mention of `LLM_WIKI_PATH`, no mention that `llm-wiki` is private, no pointer to the repo URL or routing rule.
- Gap 5: no logging at INFO/ERROR level (issue scope item 3).

### Standards

| Standard | Status | Source |
|---|---|---|
| DNV-OS-E301 | LIVE pilot ([#2685](https://github.com/vamseeachanta/workspace-hub/issues/2685)) | `src/digitalmodel/orcaflex/mooring_design.py:check_mbl_with_safety_factor` |

### LLM Wiki pages consulted

- `wikis/engineering/wiki/standards/dnv-os-e301.md` (in the private llm-wiki clone) — confirmed present at `/mnt/local-analysis/llm-wiki/wikis/engineering/wiki/standards/dnv-os-e301.md` with frontmatter `code_id: DNV-OS-E301`, `publisher: DNV`, `revision: 2021-07`. Path **does NOT** start with `knowledge/`.

### Documents consulted

- [`.claude/rules/codes-standards-data-routing.md`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/codes-standards-data-routing.md) §4 — names `LLM_WIKI_PATH` as the documented env-var; states resolver "fails closed for unauthenticated external users since the wiki is now private. This is by design — `pip install digitalmodel` users see `CitationResolutionError` and must configure `LLM_WIKI_PATH` to a local clone they have authorized access to."
- [`.claude/rules/calc-citation-contract.md`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/calc-citation-contract.md) — unchanged contract; resolver must remain **fail-closed at calc time** per #2481 D2.
- Issue [#617](https://github.com/vamseeachanta/digitalmodel/issues/617) (live body) — scope: env-var resolution, fail-closed actionable message, INFO/ERROR logging, tests on all branches, update `docs/data/OCIMF_CORPUS_README.md`.
- Umbrella [#2774](https://github.com/vamseeachanta/workspace-hub/issues/2774) — OPEN; tracks the post-privacy-flip ingest program.
- Pilot [#2685](https://github.com/vamseeachanta/workspace-hub/issues/2685) — CLOSED; landed DNV pilot.
- `docs/data/OCIMF_CORPUS_README.md` — currently references the citation registry (line near bottom: `Citation registry | src/digitalmodel/citations/registry.py`); does NOT mention `LLM_WIKI_PATH` or the private-wiki dependency.

### Gaps identified

1. No `LLM_WIKI_PATH` env-var support in the codebase (only `DIGITALMODEL_REPO_ROOT`).
2. Env-var fallback is in the caller (`mooring_design.py`), not in `citations/registry.py` where the contract belongs.
3. Hardcoded `wiki_path` prefix `knowledge/wikis/` is wrong for standalone llm-wiki clones — only correct under workspace-hub overlay.
4. Error message gives no actionable guidance about `LLM_WIKI_PATH` / private repo / routing rule.
5. No INFO/ERROR logging on resolve success/failure.
6. `OCIMF_CORPUS_README.md` does not mention the env-var contract.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-05-20 via `gh issue view`):

- `vamseeachanta/digitalmodel#617` — OPEN — "Citation-resolver hardening for LLM_WIKI_PATH env var + fail-closed UX (post llm-wiki privacy flip)" — label `enhancement`.
- `vamseeachanta/workspace-hub#2774` — OPEN — "Private llm-wiki corpus-ingest program (post-2026-05-20 privacy flip)".
- `vamseeachanta/workspace-hub#2685` — CLOSED — "Citation pilot contradiction…".

**File existence** (`ls -la` 2026-05-20):

- EXISTS: `/mnt/local-analysis/digitalmodel/src/digitalmodel/citations/schema.py` (4467 B)
- EXISTS: `/mnt/local-analysis/digitalmodel/src/digitalmodel/citations/registry.py` (2004 B)
- EXISTS: `/mnt/local-analysis/digitalmodel/src/digitalmodel/citations/__init__.py` (432 B)
- EXISTS: `/mnt/local-analysis/digitalmodel/tests/citations/test_schema.py`
- EXISTS: `/mnt/local-analysis/digitalmodel/tests/citations/test_registry.py`
- EXISTS: `/mnt/local-analysis/digitalmodel/tests/citations/fixtures/knowledge/` (vendored wiki fixture tree)
- EXISTS: `/mnt/local-analysis/digitalmodel/tests/orcaflex/test_mooring_design_citations.py` (env-var coverage for `DIGITALMODEL_REPO_ROOT`)
- EXISTS: `/mnt/local-analysis/digitalmodel/docs/data/OCIMF_CORPUS_README.md`
- EXISTS: `/mnt/local-analysis/llm-wiki/wikis/engineering/wiki/standards/dnv-os-e301.md` (standalone clone — note absence of `knowledge/` prefix)
- MISSING (this plan creates): `src/digitalmodel/citations/resolver.py` (new — central env-var/precedence resolver)

**Line excerpts**:

`src/digitalmodel/citations/registry.py:26-31` (hardcoded prefix in template — to be updated per Q1 resolution below):
```
_DNV_OS_E301_CITATION_TEMPLATE: Final = {
    "code_id": "DNV-OS-E301",
    "publisher": "DNV",
    "revision": "2021-07",
    "wiki_path": "knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md",  # → "wikis/engineering/wiki/standards/dnv-os-e301.md" after Q1 resolution
}
```

`src/digitalmodel/orcaflex/mooring_design.py:51-63` (current env-var, wrong name):
```
env = os.environ.get("DIGITALMODEL_REPO_ROOT")
if env:
    env_path = Path(env)
    if (env_path / "knowledge" / "wikis").is_dir():
        return env_path
    raise CitationResolutionError(
        code_id="DNV-OS-E301",
        wiki_path="knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md",
        reason=(
            f"DIGITALMODEL_REPO_ROOT={env_path!s} does not contain knowledge/wikis/; "
            "set to the workspace-hub root or unset to fall through to standalone mode"
        ),
    )
```

**Gap proofs**:

- `grep -rn "LLM_WIKI_PATH" /mnt/local-analysis/digitalmodel/src /mnt/local-analysis/digitalmodel/tests /mnt/local-analysis/digitalmodel/docs` → no matches. Confirms `LLM_WIKI_PATH` is not implemented anywhere.
- `ls /mnt/local-analysis/llm-wiki/knowledge/wikis/` → "No such file or directory". Confirms the `knowledge/` prefix is workspace-hub-specific.

**Reproduction proofs** (verify-against-repo-state, per Step 1.5 of `issue-planning-mode`):

```
$ cd /tmp/repro-617 && env -u DIGITALMODEL_REPO_ROOT -u LLM_WIKI_PATH PYTHONPATH=/mnt/local-analysis/digitalmodel/src python3 -c "
from digitalmodel.citations.registry import get_mooring_safety_factor, MooringCondition
from pathlib import Path
try:
    cv = get_mooring_safety_factor(MooringCondition.INTACT_QUASI_STATIC, repo_root=Path('/tmp/repro-617'))
except Exception as e:
    print('ERROR TYPE:', type(e).__name__)
    print('ERROR MSG:', str(e))
"

ERROR TYPE: CitationResolutionError
ERROR MSG: CitationResolutionError: code_id='DNV-OS-E301' wiki_path='knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md' reason=page_missing
```

- Reproduced at: 2026-05-20 local
- Failure mode observed matches issue claim: YES — the current message is opaque (`page_missing`), with no mention of `LLM_WIKI_PATH`, no mention that the wiki is now private, and no actionable remediation. This is exactly the UX the issue scopes a fix for.

<!-- Source count: issue body, schema.py, registry.py, mooring_design.py, codes-standards-data-routing rule, calc-citation-contract rule, OCIMF_CORPUS_README, llm-wiki standalone clone, reproduction = 9 distinct sources. -->

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `digitalmodel/docs/plans/2026-05-20-issue-617-citation-resolver-hardening.md` |
| New resolver module | `digitalmodel/src/digitalmodel/citations/resolver.py` |
| New resolver tests | `digitalmodel/tests/citations/test_resolver.py` |
| Existing registry (modify) | `digitalmodel/src/digitalmodel/citations/registry.py` |
| Existing schema (modify — error message) | `digitalmodel/src/digitalmodel/citations/schema.py` |
| Existing caller (modify — defer to resolver) | `digitalmodel/src/digitalmodel/orcaflex/mooring_design.py` |
| Existing registry tests (extend) | `digitalmodel/tests/citations/test_registry.py` |
| Caller integration tests (extend) | `digitalmodel/tests/orcaflex/test_mooring_design_citations.py` |
| Docs update | `digitalmodel/docs/data/OCIMF_CORPUS_README.md` |
| Plan review — Claude | `workspace-hub/scripts/review/results/2026-05-20-plan-617-claude.md` |
| Plan review — Codex | `workspace-hub/scripts/review/results/2026-05-20-plan-617-codex.md` |
| Plan review — Gemini | `workspace-hub/scripts/review/results/2026-05-20-plan-617-gemini.md` |

---

## Deliverable

A new `digitalmodel.citations.resolver` module that resolves the wiki base path via `LLM_WIKI_PATH` env var (with documented precedence chain falling back to known local-clone heuristics, and fail-closed if none resolve), emitting an actionable `CitationResolutionError` that names the env var, the private repo URL, and the routing-rule pointer; consumed by `get_mooring_safety_factor()` so external `pip install digitalmodel` users see the actionable message rather than the opaque `page_missing` reason.

---

## Pseudocode

```
# src/digitalmodel/citations/resolver.py

WIKI_REPO_URL = "https://github.com/vamseeachanta/llm-wiki"
ROUTING_RULE_URL = "https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/codes-standards-data-routing.md"
_WALK_HARD_CAP = 8
_KNOWN_LOCAL_CLONES = ("/mnt/local-analysis/llm-wiki",)  # documented user convention
_RESOLUTION_CACHE = {}  # thread-safe one-shot warn key set
_LOGGER = logging.getLogger("digitalmodel.citations.resolver")

def resolve_wiki_base(*, override: Optional[Path] = None) -> Path:
    """Resolve the wiki base directory using documented precedence.

    Precedence (first match wins):
      1. explicit override kwarg
      2. LLM_WIKI_PATH env var
      3. DIGITALMODEL_REPO_ROOT env var (legacy alias; logs DeprecationWarning)
      4. bounded parent-walk from __file__ for a `knowledge/wikis/` overlay
         (workspace-hub developer convention)
      5. each path in _KNOWN_LOCAL_CLONES if it contains `wikis/`
      6. fail closed with an actionable CitationResolutionError

    Returns:
      Path to the directory containing the wiki tree. The returned path
      will satisfy: (returned / 'wikis') is a directory OR
      (returned / 'knowledge' / 'wikis') is a directory.
      Layout is recorded on the returned object via a sidecar attribute
      (see resolve_wiki_path() for join semantics).

    Raises:
      CitationResolutionError with reason starting `resolver_unconfigured:`
      and a message naming LLM_WIKI_PATH, WIKI_REPO_URL, and ROUTING_RULE_URL.
    """
    ...

def resolve_wiki_path(citation_wiki_path: str, *, override=None) -> Path:
    """Resolve a citation wiki_path (canonical form: 'wikis/<domain>/...') to
    an absolute file path on disk. Handles both layouts:

      - llm-wiki standalone clone: <base>/wikis/<domain>/...
      - workspace-hub overlay:     <base>/knowledge/wikis/<domain>/...

    Strips the optional 'knowledge/' prefix iff the resolved base does NOT
    have it (and vice versa). Emits INFO log on success."""
    ...

def _validate_clone(base: Path) -> Optional[Path]:
    """Return base if it looks like a real wiki clone (contains `wikis/` or
    `knowledge/wikis/` with at least one expected subdir like
    `engineering/wiki/`). Returns None otherwise (no exception so
    precedence chain can keep walking)."""
    ...
```

```
# src/digitalmodel/citations/schema.py — validate_citation() modification

def validate_citation(citation: Citation, *, repo_root: Optional[Path] = None) -> None:
    """Fail-closed resolution check.

    If repo_root is None, defer to resolver.resolve_wiki_path()
    (which applies the LLM_WIKI_PATH precedence chain).

    If repo_root is provided, preserve current behavior for backwards
    compatibility with the existing test suite + the orcaflex pilot caller.
    """
    if repo_root is None:
        path = resolver.resolve_wiki_path(citation.wiki_path)
    else:
        path = repo_root / citation.wiki_path
    # ... rest unchanged: frontmatter mismatch checks etc.
```

```
# src/digitalmodel/citations/registry.py — registry signature

def get_mooring_safety_factor(
    condition: MooringCondition,
    *,
    repo_root: Optional[Path] = None,  # was: required
) -> CitedValue:
    """Now-optional repo_root. None defers to LLM_WIKI_PATH resolver chain."""
    ...
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `digitalmodel/src/digitalmodel/citations/resolver.py` | central env-var precedence chain + actionable fail-closed message + INFO/ERROR logging |
| Create | `digitalmodel/tests/citations/test_resolver.py` | TDD for all 4+ precedence branches |
| Modify | `digitalmodel/src/digitalmodel/citations/__init__.py` | export `resolve_wiki_base`, `resolve_wiki_path`, `WIKI_REPO_URL` for callers |
| Modify | `digitalmodel/src/digitalmodel/citations/schema.py` | `validate_citation()` accepts `repo_root=None`; defers to resolver |
| Modify | `digitalmodel/src/digitalmodel/citations/registry.py` | `get_mooring_safety_factor()` accepts `repo_root=None`; defers to resolver. Also update `_DNV_OS_E301_CITATION_TEMPLATE["wiki_path"]` from `knowledge/wikis/...` to `wikis/...` per Q1 resolution. |
| Modify | `digitalmodel/src/digitalmodel/orcaflex/mooring_design.py` | `_default_repo_root()` delegates to resolver; existing `DIGITALMODEL_REPO_ROOT` handled by resolver as legacy alias with deprecation warning |
| Extend | `digitalmodel/tests/citations/test_registry.py` | add tests for `repo_root=None` deferring to resolver |
| Extend | `digitalmodel/tests/orcaflex/test_mooring_design_citations.py` | rename/extend existing env-var tests to also cover `LLM_WIKI_PATH`; preserve `DIGITALMODEL_REPO_ROOT` legacy path |
| Update | `digitalmodel/docs/data/OCIMF_CORPUS_README.md` | add section "Citation env-var contract" naming `LLM_WIKI_PATH`, the private repo URL, the routing rule |
| Update | `workspace-hub/docs/plans/README.md` | add row for this plan |

---

## TDD Test List

**Tests will be written BEFORE implementation** per `.claude/rules/` TDD mandate.

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_resolver_explicit_override_wins` | explicit kwarg beats env vars | `override=tmp_path` (valid clone), `LLM_WIKI_PATH` set to garbage | resolves to `tmp_path` |
| `test_resolver_llm_wiki_path_env_valid_standalone_layout` | env var pointing to standalone llm-wiki clone (no `knowledge/` prefix) | `LLM_WIKI_PATH=/tmp/llm-wiki` with `wikis/engineering/...` | resolves successfully; resolved path joins correctly |
| `test_resolver_llm_wiki_path_env_valid_workspace_hub_layout` | env var pointing to workspace-hub root (with `knowledge/wikis/`) | `LLM_WIKI_PATH=/tmp/workspace-hub` with `knowledge/wikis/...` | resolves successfully |
| `test_resolver_llm_wiki_path_env_invalid_path` | env var set but path doesn't contain a wiki tree | `LLM_WIKI_PATH=/tmp/nonexistent` | raises `CitationResolutionError`; reason starts `llm_wiki_path_invalid:` ; message names env var |
| `test_resolver_llm_wiki_path_env_stale_clone_detection` | env var path exists but is missing expected subdirs | `LLM_WIKI_PATH=/tmp/empty-dir` | raises `CitationResolutionError`; reason starts `llm_wiki_path_stale_clone:` |
| `test_resolver_digitalmodel_repo_root_legacy_alias` | legacy env var still works, emits DeprecationWarning | `DIGITALMODEL_REPO_ROOT=/tmp/workspace-hub` (no `LLM_WIKI_PATH`) | resolves successfully; one `DeprecationWarning` recorded |
| `test_resolver_parent_walk_finds_overlay` | bounded parent walk finds workspace-hub overlay with sentinel | resolver invoked from within nested workspace-hub checkout | resolves to workspace-hub root |
| `test_resolver_parent_walk_has_sentinel` | walk halts at hard cap, does NOT infinite-loop at `/` | invoked from `/tmp/deep/path` outside any clone | raises `CitationResolutionError`, walk count ≤ 8 |
| `test_resolver_known_local_clone_fallback` | falls back to `/mnt/local-analysis/llm-wiki` when env vars unset and parent walk fails | unset env, fake `/mnt/local-analysis/llm-wiki` via monkeypatched `_KNOWN_LOCAL_CLONES` | resolves to the patched path |
| `test_resolver_fail_closed_no_env_no_clone` | env unset, no clone anywhere | clean tmp dir, all env unset, `_KNOWN_LOCAL_CLONES=()` | raises `CitationResolutionError`; message contains `LLM_WIKI_PATH`, `vamseeachanta/llm-wiki`, `codes-standards-data-routing` |
| `test_resolver_fail_closed_message_is_actionable` | error message string contract | trigger fail-closed | `str(exc)` contains: `LLM_WIKI_PATH=`, the GH URL `https://github.com/vamseeachanta/llm-wiki`, the routing-rule pointer string |
| `test_resolver_info_log_on_success` | INFO-level log when resolution succeeds | valid env-var resolution | one INFO record at `digitalmodel.citations.resolver` naming the resolved base |
| `test_resolver_error_log_on_failure` | ERROR-level log when resolution fails | trigger fail-closed | one ERROR record at `digitalmodel.citations.resolver` |
| `test_resolver_layout_join_workspace_hub` | `resolve_wiki_path()` joins `knowledge/wikis/...` correctly under workspace-hub layout | layout has `knowledge/wikis/` | absolute path under `knowledge/wikis/` |
| `test_resolver_layout_join_standalone` | `resolve_wiki_path()` strips/inserts `knowledge/` prefix to match standalone layout | layout has `wikis/` only | absolute path under `wikis/` (no `knowledge/`) |
| `test_resolver_one_shot_warning_cached` | repeated calls produce one warning, not N | call resolver 3 times in standalone-fail mode | exactly 1 warning recorded |
| `test_registry_get_mooring_safety_factor_repo_root_none_defers_to_resolver` | `repo_root=None` integrates with resolver | `LLM_WIKI_PATH` set, `repo_root=None` | returns valid `CitedValue` |
| `test_registry_get_mooring_safety_factor_explicit_repo_root_preserved` | back-compat: explicit `repo_root=Path(...)` still works | `repo_root=fixtures_dir` | returns valid `CitedValue`, env vars ignored |
| `test_schema_validate_citation_repo_root_none_defers_to_resolver` | schema-level integration | `validate_citation(c, repo_root=None)` with env set | passes |
| `test_mooring_design_llm_wiki_path_env_var_drives_pilot` | end-to-end: `check_mbl_with_safety_factor` honors `LLM_WIKI_PATH` | env set, no `DIGITALMODEL_REPO_ROOT` | returns expected SF + citation |
| `test_mooring_design_legacy_env_var_still_works` | back-compat for existing users | `DIGITALMODEL_REPO_ROOT` set, `LLM_WIKI_PATH` unset | returns expected SF + citation; one DeprecationWarning |

---

## Acceptance Criteria

- [ ] All new tests pass: `cd digitalmodel && uv run pytest tests/citations/test_resolver.py tests/citations/test_registry.py tests/citations/test_schema.py tests/orcaflex/test_mooring_design_citations.py -v`
- [ ] No regression: `cd digitalmodel && uv run pytest tests/citations/ tests/orcaflex/test_mooring_design_citations.py` is fully green (existing 14+ cases preserved).
- [ ] Reproduction script from this plan's "Reproduction proofs" block, when run after merge with `LLM_WIKI_PATH` unset and no local clone discoverable, prints an error message containing the literal substrings: `LLM_WIKI_PATH`, `https://github.com/vamseeachanta/llm-wiki`, and `codes-standards-data-routing`.
- [ ] Reproduction script, when run with `LLM_WIKI_PATH=/mnt/local-analysis/llm-wiki` (standalone clone with `wikis/` not `knowledge/wikis/`), resolves successfully and emits one INFO log.
- [ ] `docs/data/OCIMF_CORPUS_README.md` contains a new section naming `LLM_WIKI_PATH`, the private repo URL, and a pointer to the routing rule.
- [ ] Existing `DIGITALMODEL_REPO_ROOT` env-var users see a `DeprecationWarning` but the calc still resolves.
- [ ] No `from <module> import *` of resolver internals from outside the citations package; resolver's public API is documented in `__init__.py`.
- [ ] Review artifacts posted to `scripts/review/results/2026-05-20-plan-617-{claude,codex,gemini}.md`.

---

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | — |
| Codex | PENDING | — |
| Gemini | PENDING | — |

**Overall result:** PENDING (plan in `draft`; not surfaced for approval until cross-provider review lands)

Pre-empted findings (the plan author's own adversarial pass):

1. **Stale-clone hazard.** What if a user sets `LLM_WIKI_PATH` to a directory that exists but is a stale or wrong checkout (e.g., a sibling project's `wikis/` directory)? — Mitigated: `_validate_clone()` checks for at least one expected canonical subdirectory (`<base>/wikis/engineering/wiki/` or `<base>/knowledge/wikis/engineering/wiki/`). If the env var is set but the clone doesn't validate, raise `CitationResolutionError` with reason `llm_wiki_path_stale_clone:`. Tested.

2. **CI without wiki access (legitimate).** What if a CI environment legitimately runs digitalmodel tests with no wiki access? — Mitigated: (a) `validate_citation()` keeps the explicit-`repo_root` path for tests-with-fixtures, so the existing vendored test fixtures (`tests/citations/fixtures/knowledge/...`) continue to work without any env var; (b) the resolver only raises at calc invocation, not at import — import remains side-effect-free. Tested via `test_registry_get_mooring_safety_factor_explicit_repo_root_preserved`.

3. **Thread safety of the resolution cache.** `_REPO_ROOT_RESOLUTION_CACHE` in `mooring_design.py` and the new resolver's `_RESOLUTION_CACHE` are plain dicts. Concurrent first-resolution in a threaded test runner could double-warn. — Acceptable for v1: warning is idempotent (same message, one-shot is "best-effort"), not a correctness defect. Documented in module docstring. If we ever observe a test flake, promote to `threading.Lock` in a follow-on.

4. **Parent-walk sentinel.** Per [`feedback_path_parent_infinite_loop`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/memory/feedback_path_parent_infinite_loop.md), an unbounded `Path.parent` walk can infinite-loop at `/`. — Mitigated: hard cap `_WALK_HARD_CAP = 8` (matches the existing `mooring_design.py:_REPO_ROOT_WALK_HARD_CAP` precedent). Tested via `test_resolver_parent_walk_has_sentinel`.

5. **Hardcoded `knowledge/wikis/` prefix in registry template.** The current `_DNV_OS_E301_CITATION_TEMPLATE["wiki_path"]` embeds the workspace-hub overlay prefix. — **Resolved (user, 2026-05-20)**: change canonical form to `wikis/<domain>/...` (no `knowledge/` prefix). Standalone `vamseeachanta/llm-wiki` is canonical post-privacy-flip; the resolver `_prepend_knowledge_if_workspace_hub_layout()` adds the prefix when the resolved base is a workspace-hub root. Implementation: update `_DNV_OS_E301_CITATION_TEMPLATE["wiki_path"]` in registry.py to drop the `knowledge/` prefix.

6. **Error-message stability.** Tests assert literal substrings in the error message. — Acceptable: documented as part of the public contract. Future message updates must update tests in lockstep.

7. **`DIGITALMODEL_REPO_ROOT` legacy aliasing.** Removing it would break existing users; keeping it forever bloats the resolver. — Mitigated: keep with `DeprecationWarning`, mark for removal in a future release tracked by a follow-on issue (TBD; do not file in this plan to avoid scope creep).

8. **Logging side-effects at import.** Issue scope item 3 mentions INFO log on success. — Mitigated: logger is module-scoped (`logging.getLogger("digitalmodel.citations.resolver")`), no handler attached by default — quiet by default per stdlib convention, callers opt in via their own logging config.

---

## Risks and Resolved Decisions

- **Risk:** Back-compat with the 14 existing `tests/citations/` cases + the `tests/orcaflex/test_mooring_design_citations.py` suite. **Mitigation:** explicit `repo_root` kwarg remains a public API path; only `repo_root=None` triggers the new resolver. Run full suite as part of acceptance.
- **Risk:** Documentation drift between this plan and `calc-citation-contract.md` (workspace-hub rule). **Mitigation:** rule already references the resolver contract abstractly; no rule-text change needed for this hardening. If reviewers disagree, file a workspace-hub rule-update follow-on.
- **Risk:** CI environments that previously relied on the parent-walk auto-discovery (workspace-hub developers) — the resolver preserves this branch (precedence step 4).
- **Risk introduced by Q1 resolution:** changing canonical `wiki_path` form requires updating `_DNV_OS_E301_CITATION_TEMPLATE` in `registry.py`. **Mitigation:** the change is mechanical (one line); the resolver's layout-detection handles both standalone and overlay layouts so the migration is back-compatible at the consumer-API level.

### Resolved (user, 2026-05-20)

- **Q1 — canonical citation `wiki_path` form**: **CHANGE to `wikis/<domain>/...`** (no `knowledge/` prefix). The standalone `vamseeachanta/llm-wiki` repo is the canonical source post-privacy-flip; its layout has no `knowledge/` prefix. The workspace-hub overlay (`knowledge/wikis/...`) is the legacy convention. The resolver detects layout and prepends `knowledge/` ONLY when the resolved base is a workspace-hub root (i.e., contains `knowledge/wikis/` directly). Implementation impact: `_DNV_OS_E301_CITATION_TEMPLATE["wiki_path"]` changes from `"knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md"` to `"wikis/engineering/wiki/standards/dnv-os-e301.md"`; resolver gains a `_prepend_knowledge_if_workspace_hub_layout()` helper.
- **Q2 — `_KNOWN_LOCAL_CLONES` cross-platform**: **YES**. Default set: `(Path("/mnt/local-analysis/llm-wiki"), Path.home() / "workspace-hub" / "llm-wiki")`. The `Path.home()` form resolves to `~/workspace-hub/llm-wiki` on Linux/macOS and `C:\Users\<user>\workspace-hub\llm-wiki` on Windows. Per `feedback_path_parent_infinite_loop`, paths are validated via `is_dir()` before use (no walk hazard).
- **Q3 — fail-closed shell command**: **YES, include `export LLM_WIKI_PATH=/path/to/your/llm-wiki` in the error message** as a copy-pasteable example. Full message contract:
  ```
  CitationResolutionError: code_id='<id>' wiki_path='<path>' reason=resolver_unconfigured

  The citation resolver cannot find a wiki clone. The vamseeachanta/llm-wiki repo
  is private as of 2026-05-20 (codes-standards-data-routing rule §4).

  To fix: clone the private repo (requires GitHub auth) and set the env var:
      git clone https://github.com/vamseeachanta/llm-wiki.git
      export LLM_WIKI_PATH=$(pwd)/llm-wiki

  Or set the env var to point at an existing local clone:
      export LLM_WIKI_PATH=/path/to/your/llm-wiki

  See: https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/codes-standards-data-routing.md §4
  ```
  The test `test_resolver_fail_closed_message_is_actionable` asserts each of these substrings.

---

## Out of Scope

- Building a two-tier "public stub + private detail" resolver (deferred per issue body; current fail-closed is sufficient).
- Mirroring wiki content into `digitalmodel` (intentional anti-goal — preserves the licensing boundary per [routing rule](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/codes-standards-data-routing.md) item 3).
- Making `digitalmodel` private (no — public surface preserved).
- Migrating to the wiki-MCP resolver per [workspace-hub#2400](https://github.com/vamseeachanta/workspace-hub/issues/2400) — that's the v2 path; the schema does not change.
- Removing `DIGITALMODEL_REPO_ROOT` outright (kept as legacy alias with `DeprecationWarning`; a future follow-on issue tracks removal).
- Wiring OCIMF-MEG4 into the citation registry — separate scope tracked by digitalmodel#563.

---

## Related

- Umbrella: [workspace-hub#2774](https://github.com/vamseeachanta/workspace-hub/issues/2774) — private llm-wiki corpus-ingest program
- Pilot precedent: [workspace-hub#2685](https://github.com/vamseeachanta/workspace-hub/issues/2685) — DNV-OS-E301 citation pilot LIVE
- Calc-citation contract: [`.claude/rules/calc-citation-contract.md`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/calc-citation-contract.md)
- Routing rule: [`.claude/rules/codes-standards-data-routing.md`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/rules/codes-standards-data-routing.md) §4
- Parent-walk hazard: [`feedback_path_parent_infinite_loop`](https://github.com/vamseeachanta/workspace-hub/blob/main/.claude/memory/feedback_path_parent_infinite_loop.md)
- Schema decisions: [workspace-hub#2481](https://github.com/vamseeachanta/workspace-hub/issues/2481) D1/D2/D3
- Wiki-MCP migration target: [workspace-hub#2400](https://github.com/vamseeachanta/workspace-hub/issues/2400)

---

## Complexity: T2

New module + multi-file modifications (3 source files modified, 1 created; 2 test files modified, 1 created; 1 docs file modified). TDD required. Cross-cuts an existing pilot caller, so back-compat is load-bearing. Two-provider cross-review minimum (T2 default per AGENTS.md AI Review Policy); recommend T3 (3 providers) given the user-facing contract change.
