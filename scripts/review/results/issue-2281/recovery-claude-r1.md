# PLAN: REQUEST CHANGES · CODE: REQUEST CHANGES

I reviewed the diff only. I used no tools and modified no files. Line numbers come from the diff hunks.

## Blocking findings

**B1. A commit or checkout during the build can record a revision that never ran.** `build_cp_review_portfolio.py:220` reads HEAD fresh for each case, but the modules were already loaded into memory by `main` (`:365-367`) or by an earlier case. If someone commits or checks out another branch between cases, the new checkout is clean at R2. The check at `:220` returns R2, and the tracked-file checks at `:221/233/249` pass because the paths are still tracked. The receipt then says R2 while the in-memory R1 code ran. The same gap exists between `main`'s check at `:365` and `build_case`'s check at `:220`.
- **Fix:** capture the revision once in `main`, before `prepare_execution_source` runs, and pass it into `build_case`. Every later check should require that exact revision. When `build_case` is called directly, it should record the revision before its first import, or refuse if `digitalmodel.*` is already loaded. A portfolio with mixed revisions should be impossible.

**B2. Files hidden from `git status` give a "clean" checkout that isn't.** `:72` (`status --porcelain`) and `:95` (`ls-files --error-unmatch`) only see what the index reports:
- Files marked `--assume-unchanged` or `--skip-worktree` (sparse checkout sets this) can be edited without status showing a change, and `ls-files` still passes.
- Gitignored data or config files that the engine reads (anything not imported as a module) are never checked.
- **Fix:** compare the contents of each imported file with HEAD (`git hash-object --path=<p>` against `git rev-parse HEAD:<p>`). Reject if `git ls-files -v` shows any lowercase or `S` flags. Either add `--ignored` checks for `src/` and `scripts/reporting/`, or narrow the guide's wording.

**B3. The guide claims more than the code does.** `report-portfolio-guide.html`, new lines 17-18, say: "record the full commit of the clean checkout actually executed … imports from another checkout are rejected before calculation."
- B1 and B2 make the first half untrue.
- Modules that load lazily during `run_cathodic_protection` (`:236`) and aren't in the preload list (`:103-110`) are only rejected after the calculation, at `:249`. That is before any output is written, but not before calculation.
- **Fix:** either fix B1/B2 and say "rejected before writing output", or soften the wording.

**B4. The old test it restores now exercises a different code path.** `test_cp_review_document.py:219-224` reloads the UI script and loads `saved` into an empty session. The pre-existing assertion `comments.length,1` used to check reloading into a live session (the merge and de-duplication path). Now it only checks a first load and passes trivially.
- **Fix:** rebuild the earlier live session before reloading `saved`, or run the new snapshot block in a separate harness.

## Non-blocking findings (should fix)

- **N1. Foreign code runs before the origin check.** `prepare_execution_source` (`:111`) imports each module, which runs its top-level code, before `:112` validates where it came from. A shadowing package's top-level code therefore runs before rejection. Checking `importlib.util.find_spec(name).origin` first fails closed with less executed. Parent packages still get imported, so say so plainly.
- **N2. The real preload list is never tested.** `test_lazy_offshore_import…` mocks both `import_module` and the validator, so nothing proves the eight real module names at `:103-110` exist. A rename would crash every build. Add a test that runs `prepare_execution_source(ROOT)` on the real repository; it only imports modules and runs no solver.
- **N3. Git treats the path as a pattern.** At `:95`, `ls-files -- <relative>` interprets glob characters. Use `--literal-pathspecs`. Each loaded module also costs one Git process, and this runs about 4 times × 11 cases on Windows. Use one `ls-files -z` listing per check instead.
- **N4. The package version may come from the wrong checkout.** `importlib.metadata.version` (import at `:16`) reads the installed package metadata. If any receipt or release field records `version("digitalmodel")`, that value can come from the shadowing editable install's metadata rather than the checkout that ran. Please check.
- **N5. Missing provenance is not disclosed.** The receipts don't record the `LLM_WIKI_PATH` wiki revision, and they don't cover hash-only `.pyc` files or `.pth`/`sitecustomize` monkey-patching. State this as a limit rather than claiming full provenance.
- **N6. A dirty checkout produces a traceback.** In `main`, `:365` raises a `ValueError` traceback where the other input errors use `parser.error`. This is a usability issue only.
- **N7. Possible garbled dashes in the guide.** The changed A and E rows show `ΓÇö`. If that is in the file's bytes and isn't just console display, the edit re-saved a garbled em-dash.
- **N8. Check the public links.** The guide now publicly says a private A report needs a correction, and it links to `issuecomment-6002874483`. Make sure that comment contains no private payload or source hashes.

## What holds up

- **UI snapshots:** the deep copies in `cp_review_ui.js:69-77` cut the sharing between archived rounds and the live state. The new assertions at `:203-218` cover editing after a merge and appending further rounds.
- **Git setup:** removing inherited `GIT_*` variables, checking the top-level path, and requiring the full 40-character HEAD are correct. The disposable-repo tests check that dirty checkouts (unstaged, staged and untracked) are rejected before any output exists.
- **Guide statements:** the guide states correctly that the eleven published packs keep `bf638ed1` and their old embedded UI.
- **Scope:** I saw no reruns, private payloads, deletions or history rewrites in the diff.

## Plan changes needed

1. Pin one revision for the whole process, recorded before the first `digitalmodel` import.
2. Verify file contents against HEAD, including the hidden-flag and ignored-file cases.
3. Treat the preload list only as a way to fail early; the check before writing output is the real guarantee, and the guide should say so.
4. Keep the existing live-session reload test.
