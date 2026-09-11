# Actual Claude focused code review r2

Requested model sonnet, effort medium, tools disabled, exit 0.

## Reviewed hashes
src/digitalmodel/solvers/ghs/__init__.py: 92694625a8108a6bfd920e8842cebfc1805f0b018eba06943aea11f79cabb355
src/digitalmodel/solvers/ghs/_canonical.py: b5888a05501700f1a452ee6a8235c024c241c20e5615472340bf5955ea16794c
src/digitalmodel/solvers/ghs/comparison.py: b193f80ea8476815a81499ff9372902d7a3721e540cbf4b8346ae7cc020e0676
src/digitalmodel/solvers/ghs/contracts.py: 9e68b82cd9db608b088327f8f1451c1e46d4b690f279a57d12176917973b8d0e
src/digitalmodel/solvers/ghs/runner.py: 349fb4aa1a0e42ba01606d1361d1afb697a6f57bc6a3093ddfd35bcc273155b7
tests/solvers/ghs/__init__.py: 8654a7c4e0d97c3c0299eb9acc2a2becb9597900f0433c194c40e9b0d0ff4e05
tests/solvers/ghs/test_boundaries.py: 827198bbdc3d5f253996cb84cc161e1dbe4455146b3239cf367d26588d41d585
tests/solvers/ghs/test_comparison.py: 0f893bfa68544681bb62aed7c6fa3cce59d8c551bd3c92be106baf85ed8c571f
tests/solvers/ghs/test_contracts.py: 95c43038133b176225c9219ca41e7f92ae32a2561ae57af4ce9f65867627e84f
tests/solvers/ghs/test_runner.py: 3de585da6ba96a64b657d0250be35997237498d75e4d670dbb04c6c31409fabc

# Adversarial Review — GHS Milestone 1

**Verdict: MAJOR**

## Confirmed correct (checked carefully, not defects)
- `type(x) is not int/str` (not `isinstance`) throughout `_canonical.py` correctly rejects `bool` masquerading as `int`/exclusion from numeric fields — a common pitfall this code avoids.
- `decimal()` bounds string length *before* running its regex, so no ReDoS there.
- Box-oracle math (volume, mass, LCB/LCF/TCB/TCF, KB=depth/2, BM_T=L·B³/12/V, BM_L=B·L³/12/V) is dimensionally and numerically correct for a rectangular box.
- `run_approved_capture` unconditionally raises; `licensed_execution_verified` is `False` everywhere; `preview_approval`/`validate_packet` never set `launch_allowed`/approval true. No false-licensed-claim path found.
- `stage_synthetic`/`_verify_stage`/`_read_verified` staging defenses (ancestor symlink scan, exclusive `mkdir`, exact fileset checks, inode/size/mtime re-verification around the read, bounded read instead of `read_bytes()`) are solid and correctly tested (`test_oversize_substitution_rejected_before_read`, `test_stage_substitution_rejects`).
- `shape()` strict key-set equality closes off parameter injection (`extra_args`, unknown evidence keys, etc.).

## Concrete defects

**1. `text()` performs unbounded work before its length bound (`_canonical.py:text`)**
```python
if type(value) is not str or not value.strip() or len(value)>limit or not value.isprintable():
```
`value.strip()` and (if reached) `isprintable()` are O(n) and execute *before* `len(value)>limit` due to short-circuit ordering. An attacker-controlled field such as `case_id` (validated via plain `text()` in `normalized_request`) can be an arbitrarily large string; `.strip()` will scan/copy the whole thing before it's ever rejected for length. This is exactly the "unbounded validation" class the review asked to hunt for — the length check must run first.

**2. `path_text` does not itself reject a bare trailing separator (`contracts.py:path_text`)**
For an input like `'C:/Some/Dir/'`, `ntpath.splitdrive` yields a tail whose `split('/')` produces a trailing empty string that gets filtered out by `if part`, so no `.`/`..`/trailing-dot/space check ever fires. The parametrized test `test_runtime_directory_alias_rejected` only demonstrates rejection for `'C:/Synthetic Work/'` because it happens to normalize to the same value as the fixed `work_dir='C:/Synthetic Work'`, tripping the *separate* `work_dir == temp_dir` equality guard in `runtime_record` — not because `path_text` itself closes the alias. If `path_text` is ever reused to validate a lone path with no counterpart to collide against, a trailing-separator variant slips through unflagged, undermining the "closed canonical path" guarantee the function's name implies.

**3. `_read_verified`'s `O_NOFOLLOW` protection silently no-ops on the target platform (`runner.py:_read_verified`)**
```python
flags=os.O_RDONLY|getattr(os,'O_BINARY',0)|getattr(os,'O_NOFOLLOW',0)
```
`os.O_NOFOLLOW` does not exist on Windows, so `getattr` falls back to `0` there — precisely the platform this module targets (`runtime_record` hard-requires `platform=='windows'`). The remaining defense is a non-atomic `lstat`-then-`open` symlink check (`_is_link` before/after), which is a real but strictly weaker TOCTOU mitigation than the atomic kernel-enforced `O_NOFOLLOW` that protects the POSIX path. This should be flagged rather than silently accepted, since it means the strongest documented protection against artifact-file substitution is inactive on Windows.

**4. Oracle metadata inconsistent with computed rows (`comparison.py:oracle_material`)**
```python
def oracle_material():
    return {'oracle_version':1,'box_m':['20','10','4'], ...}
```
but `box_oracle()` only ever computes rows at `depth ∈ {1,2,3}` (length=20, beam=10, no use of "4" anywhere). The recorded oracle metadata implies a box height of 4 m that is never exercised by any comparison row. This is baked into `oracle_sha256` and shipped as part of the "independent oracle" evidence — misleading for anyone auditing what was actually validated, even though it doesn't affect the arithmetic itself.

**5. Minor off-by-one in reference-line bound (`comparison.py:row_record`)**
```python
integer(ref['line'],1,artifacts[artifact]+1)
```
allows a claimed line number one past the maximum plausible for the declared `byte_count` (minimum 1 byte/line ⇒ max line count == byte_count, not `byte_count+1`). Not exploitable given the synthetic-only usage, but it's a real, avoidable looseness in an otherwise tight bound.

## Recommendation
Fix #1 and #3 before treating this as containment-ready even at the "synthetic" milestone level, since #1 is a genuine unbounded-cost validation gap and #3 undermines the one Windows-specific safety property the staging code advertises. #2 and #4 should be tightened/corrected for consistency; #5 is optional. None of these break the stated invariant that live execution is unreachable, but #1 and #3 are concrete, exploitable-in-principle weaknesses in the "provisional" surface that is otherwise carefully hardened.

## stderr
