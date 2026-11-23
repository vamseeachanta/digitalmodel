# Cathodic protection: session handoff (2026-09-24 to 2026-09-27)

## What was done
- **Review** (2026-09-24): four-reviewer technical quality review, `technical-quality-review-2026-09-24.md`; owner decisions recorded in `technical-quality-review-2026-09-25-human-decisions.html` (first round D1–D12, second round Table 6).
- **Epic #2206** (closed 2026-09-27): all eight children merged.

| Issue | Pull request(s) | Delivered |
|---|---|---|
| #2207 | #2216 | Cited, edition-keyed DNV-RP-B401 / DNV-RP-F103 table modules with CSV-backed tests |
| #2208 | #2228, llm-wiki #921 | Real editions: B401 2005/2010/2017/2021, F103 2010/2019 |
| #2209 | #2215 | Four arithmetic fixes; four models quarantined |
| #2210 | #2220 | Engine adapter, PASS/FAIL status, demos retired to fixtures |
| #2211 | #2217 | Single formula kernel, F103 bracelet module, full B401 Sec. 7 loop |
| #2212 | #2219, #2237, #2238 | Standard HTML/PDF report engine; CP report adapters; `report:` router hook |
| #2213 | #2221 | Consumed test vectors, property-based and published-value tests |
| #2214 | #2218 | Executable worked examples, index, brochure removed |
| #2155 (D8) | #2236 | Identifier redaction |
| follow-up | #2246, #2250 | Client-use status with engineer-of-record check; F103 default 2019; `DNV_RP_F103` key |
| #2247 | #2248, #2251 | Stray current, galvanic and ICCP life re-modelled from open literature (provisional, experimental); fuel-system check removed |

## Current status
- **Client use** (with engineer-of-record check): DNV-RP-B401 offshore, DNV-RP-F103 bracelet, and rebuilt ABS ships route.
- **Legacy, uncited, independent check required:** ABS ships legacy alias and ABS offshore route.
- **Experimental** (`experimental=True`): stray current, galvanic, ICCP anode life.
- Defaults: B401 edition 2021, F103 edition 2019.

## Open items
1. Obtain EN 50162, ISO 18086, ISO 15589-1, NACE SP0169 and SP0572 (none are in the standards store). The checklists at the end of `standards-inventory.md` list every provisional value each standard must confirm; the models leave experimental status only after that check.
2. Run benchmark #1852 on the first real CP job (condition of the client-use approval).
3. New structure types (offshore-wind monopile internals, hull ICCP, retrofit sleds, flexible risers and mooring chain, quay walls, tank internals, concrete CP, AC interference) sequenced by client demand; see section 8 of the review.
4. The worked examples still call the old hydrodynamics router with `DNV_RP_F103_2010`; migrate them to the engine adapter when that router is retired.
5. ABS offshore: cite its tables (the guidance notes are on file) to lift the independent-check condition. The rebuilt ABS ships route was promoted by owner decision 2026-10-01 after acceptance of its three recorded project interpretations and addition of its generic-wiki citation page.

## Working notes for the next session
- Worktree tests: the main checkout's editable install shadows worktree sources; run pytest through a runner that strips the editable finder and prepends the worktree `src/`.
- Never delete a branch that is the base of stacked PRs before retargeting them to main; GitHub auto-closes the dependants.
- The repository has no auto-merge; wait for all four workflows (Quality Gates, Quality Gates by Domain, Build API Docs, Parametric atlas drift) before merging.
- The absolute-path gate rejects `/mnt/...` paths in added lines; cite standards by wiki page instead.
