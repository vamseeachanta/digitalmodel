# Security hook checks

The security hooks use repository-owned configuration and upstream executables.
Bandit scans changed Python files under `src/`; Gitleaks checks staged changes.
The startup corrections are already present in `.pre-commit-config.yaml`.
The contracts guard configuration paths, dependencies and executable identity.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Document direct startup checks and regression coverage for standalone checkouts. |

## Configuration and method

Bandit retains the pinned revision in `.pre-commit-config.yaml` and declares
`pbr>=2.0.0` in `additional_dependencies`. The dependency requirement is recorded
in that configuration; the upstream [Bandit initialization module](https://github.com/PyCQA/bandit/blob/1.7.5/bandit/__init__.py)
imports `pbr.version`. Installing only the scanner can therefore leave its
executable unable to start. A project environment is unnecessary for a static scan.

Gitleaks uses `.gitleaks.toml`, whose `[extend]` section sets `useDefault = true`.
This retains the upstream detection rules without importing a sibling checkout's
configuration. The pinned [Gitleaks configuration documentation](https://github.com/gitleaks/gitleaks/blob/v8.21.2/README.md#configuration)
defines this setting and distinguishes it from an external `extend.path`.
Existing `.gitleaksignore` fingerprints carry individual false-positive reasons;
new findings require assessment before any fingerprint is added.

The [Bandit hook manifest](https://github.com/PyCQA/bandit/blob/1.7.5/.pre-commit-hooks.yaml)
provides the `bandit` executable and Python environment. The
[Gitleaks hook manifest](https://github.com/gitleaks/gitleaks/blob/v8.21.2/.pre-commit-hooks.yaml)
provides the staged scan with redacted output. Contract tests reject local
executable, interpreter, source or dependency overrides for either scanner.
Each scanner must occur exactly once.

Path checks cover separate and attached configuration arguments, parent traversal,
absolute paths, Windows paths and symlinks that resolve outside the checkout.
For example, `-c../config.toml` is rejected, whereas `-cconfig.toml` is contained.
The short configuration option is documented in the pinned Gitleaks README's
Usage section. Only an explicit `python` system hook receives the inline-program
exception for the argument following `-c`.

## Worked Bandit example

Prerequisites: `uv`, Python and an existing writable `TMPDIR` outside the checkout.
Run from the repository root. Keep the environment's cache settings in effect.

```sh
example_dir=$(mktemp -d "${TMPDIR:?}/security-hook-example.XXXXXX")
printf 'enabled = True\n' > "$example_dir/clean.py"
printf 'enabled = True\nassert enabled\n' > "$example_dir/assertion.py"

uv run --no-project --with bandit==1.7.5 --with 'pbr>=2.0.0' \
  bandit -f json -o "$example_dir/clean.json" "$example_dir/clean.py"

uv run --no-project --with bandit==1.7.5 --with 'pbr>=2.0.0' \
  bandit -f json -o "$example_dir/assertion.json" "$example_dir/assertion.py"
```

The clean input yields no findings and exit status zero. The assertion input
yields one `B101` finding on its second line, with LOW severity, HIGH confidence
and a nonzero exit status. The expected finding follows directly from the
published [Bandit assertion example and rule](https://github.com/PyCQA/bandit/blob/1.7.5/bandit/plugins/asserts.py):
the input contains exactly one `assert` statement and no other rule trigger.
The executable must both start and reject the assertion input; a version check
alone does not establish detection behavior. No engineering coefficients or
acceptance thresholds are involved.

## Direct checks

```sh
uv run --no-project --with pytest --with pytest-asyncio --with pyyaml \
  python -m pytest tests/contracts/test_hook_paths.py \
  tests/contracts/test_bandit_hook_entry.py -q -rA -p no:cacheprovider \
  --basetemp="${TMPDIR:?}/security-hook-tests"

# Requires the already available pinned Gitleaks executable.
gitleaks dir --config .gitleaks.toml --redact --no-banner \
  --report-format json --report-path "${TMPDIR:?}/gitleaks-tree.json" .
```

The Gitleaks command scans the working tree, rather than historical commits.
Its directory command and redaction option are defined in the pinned README's
Usage and Dir sections. Record missing tools as unexecuted checks; static
configuration tests do not prove that an unavailable scanner runs.

For a whole-source Bandit inventory, run `bandit -r src/ -f json` with its report
directed outside the checkout. Do not publish source excerpts or credential
values from reports. Existing source findings are tracked in #2453; this change
adds no baseline, rule exclusions or severity filters. Changed source files
continue to block a commit when Bandit reports findings.
