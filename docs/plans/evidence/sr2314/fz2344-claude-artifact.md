VERDICT: APPROVE

I found no actionable defect in the frozen bytes. Each owner requirement binds to a governing case below.

| Requirement | Governing case | Result |
|---|---|---|
| Default `cite=True` returns EN400 only, ignoring a missing ITTC page | `friction_scaling.py:337-338` routes any non-`"strict"` value through `ittc57_cf_cited`, which never calls `get_ittc78_transfer_reference`. `test_default_transfer_without_procedure_page_matches_uncited` copies only `en400.md` and asserts `["EN400"]`. | Met |
| Standalone default warns once and returns empty citations | `resistance.py:83-92` keeps the `_EN400_STANDALONE_WARNED` guard and the empty list. `test_default_standalone_transfer_warns_once_and_returns` covers both transfer functions, asserts one warning, then none on repeat, with values equal to `cite=False`. | Met |
| A configured EN400 error still raises | `resistance.py:93-94` re-raises when the reason is not `resolver_unconfigured`. `test_transfer_citation_fail_closed_on_missing_page` asserts `page_missing` for both directions. | Met |
| `cite="strict"` validates both citations and has no fallback | `friction_scaling.py:339-345` calls both resolvers directly, bypassing the warn-and-degrade wrapper. Strict also sets `unresolved_citations=()`. Tests cover the missing procedure page, a revision mismatch and an unconfigured resolver. | Met |
| Legacy non-string truthiness stays compatible | The only guard is `isinstance(cite, str) and cite != "strict"` (line 226). `1`, `0` and `None` pass through, and `cited=bool(cite)` is tested. | Met |
| Unknown strings raise `ValueError` | `"Strict"`, `"false"` and `"off"` are tested. `""` is also refused because it is a `str` other than `"strict"`. The check runs before any citation resolution. | Met |
| Mapping is `collections.abc.Mapping` | `mesh_hydrostatics.py:69` imports it, and `test_facade_mapping_identity_matches_main` asserts identity. | Met |
| Parity evidence binds to these bytes | `current_source_sha256` in `fz2344-main-parity.json:35` equals the packet digest of `friction_scaling.py`. | Met |

**Residual gaps, none blocking:**

- Strict mode with a missing or stale EN400 page is not tested separately. The strict EN400 failure tests inject the error by patching the resolver or `get_en400_reference`. Those tests cannot tell an EN400 failure from an ITTC failure, so the ITTC-specific unconfigured case is not isolated either. The code path is straight-line, so I rate this low risk.
- `registry.py`, `resolver.py` and the fixture frontmatter are outside the packet. Whether `resolver.resolve_wiki_path` is looked up dynamically (so the monkeypatches take effect) and what `get_ittc78_transfer_reference` validates are therefore not established here. The CI results for those tests are the check.

`★ Insight ─────────────────────────────────────`
- The default path reuses `ittc_1957_cf_cited` unchanged. This is why main parity holds: one-shot warning state and fail-closed semantics live in one place, and strict only adds a separate resolver call.
- Guarding with `isinstance(cite, str)` rather than `cite not in (True, False, "strict")` is what keeps legacy `1`/`0`/`None` working while still catching string typos.
`─────────────────────────────────────────────────`
