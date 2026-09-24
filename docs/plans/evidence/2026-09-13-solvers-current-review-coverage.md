# SOLVERS exact-content review coverage — 13 September 2026

This record supersedes earlier pending coverage statements for the artifacts identified below. It does not approve implementation or establish numerical results.

## Reusable analysis criteria

The user requested ecosystem analysis criteria, including parametric studies and output lookup datasets. The criteria are routed from `docs/README.md`. Ecosystem discovery remains tracked in [workspace-hub issue 3853](https://github.com/vamseeachanta/workspace-hub/issues/3853); only the digitalmodel documentation branch is updated.

| Artifact | Canonical SHA-256 |
|---|---|
| `docs/standards/analysis-parametric-lookup-criteria.html` | `1cebe3d4c8577b9795e67024a05a58bda7514c5d2334bea7d39f281f96af5bd0` |
| `docs/README.md` | `888975b62d4a735d7205a2cc505c90768944b9464708d9ee2aee8d7c3f7ffbdc` |

Table 1. Exact reviewed criteria and routing content; Git canonical bytes.

Claude reviewed tree `81cc68c76259a02be007e2b5c923b4c885fee05e`: MAJOR. The independent non-author Codex content review returned APPROVE; its subsequent disposition of Claude findings returned MINOR with no supported blocking defect. These are differing verdicts, not consensus. The retained Claude result SHA-256 is `dcee415d17c56a3564ca673e8dce31c45360066ceba4eab2c794fa0d3d41c47c`; Codex disposition SHA-256 is `4afd44c4e4445ac335af99c5e30bc1a1260c7a95632c65d5e0f8a9a325954d76`.

Finding dispositions:

- CLA-M1: additional authorization is not required for the requested documentation. Existing user direction supplies authority; issue-specific implementation approval remains separate. Review evidence is retained separately rather than embedded in a self-referential document hash.
- CLA-M2: the proposed quota-only fallback restriction is not supported by the governing hub `docs/standards/AI_REVIEW_ROUTING_POLICY.md`, Optional Review Reduction Rules, or `config/ai-tools/provider-routing-policy.yaml`, `provider_unavailable`. Both permit continuing with a documented missing provider. Agy/Gemini is UNAVAILABLE through configured local tooling; no quota failure or completed third review is claimed.
- CLA-M3: explicit approval-reference fields would improve retrieval. Recorded standing authority, exact scope, approver/date and shared-policy precedence are already required. No unrecorded waiver is permitted.
- CLA-M4: a more direct private inventory locator would improve traceability. The private SOLVERS criteria-coverage record exists; the public document reports only 12 first-level checkout observations and expressly disclaims ecosystem compliance. The routing issue remains the durable public checkpoint.
- Remaining minor findings concern caption markup, navigation and consolidation of already-required fields. They are retained as editorial improvements, not engineering acceptance failures.

An earlier malformed Claude response is INVALID_OUTPUT and is not counted as review coverage. The valid exact-content review above supersedes it. Repeated provider disagreement is surfaced rather than automatically cycling further.

## Parametric benchmark plan

The current [issue 2117 plan](../2026-09-13-issue-2117-padeye-parametric.html) has SHA-256 `17f6d2c9bf1f2d2afb8bbcda453bf9c2d7c5d65f2ab7910795311dcc8fe93e19`. Claude reviewed exact tree `c7c5e91b1054f5233f604a380257a8401fd56ac7`: MAJOR. Findings address independent-check sampling breadth, explicit load/source hash binding, and additional method/advisory fields. Review disposition remains a separate checkpoint; implementation approval remains pending. The plan proposes 225 rows and nine demands, but no lookup dataset, calculation campaign or native solve has been produced by this scope.

The retained Codex author disposition has SHA-256 `8e0a31f9f5508eb967f9065fd62ee232c9d853b4debf4e130b3fcc977a12f6ae`. It accepts the need to clarify response-family sampling redundancy, complete source/load metadata binding and section-to-weld moment references. It rejects the claimed inevitable hash collision and unsupported imported weld-size constants. The author disposition does not override Claude MAJOR. The next plan revision will resolve these bounded clarifications before another exact-current review and user implementation approval.

## Earlier pressure-preparation documentation

Bounded report and receipt corrections received exact Claude review R3: MINOR, bundle SHA-256 `7883fde1fa872a8641bbce7d4c2b24c0e8ed1ad25aed66ae4cffaa455bf8bc42`. The report working-byte SHA-256 is `df6a798c02d945d817cece246774625fabea0c725682c15aba555ce7d994dddc`; receipt working-byte SHA-256 is `651a5f2dd4b1cb274d65ff5d55fa72cb35bcf03b66a93da44e7ec7b88b38e4c3`. Canonical Git blobs are respectively `6e2634e5573785bc65fe24fe3d0071bf0b89a572` and `7f082dce8bd06e1ccf8803ed66ad21c13b9fcbbc`.

The correction distinguishes historical evidence from current preparation checks. Historical receipt fields remain unchanged after removal of the added `review_annotation`. Broader implementation/report reviews remain MAJOR and unresolved; this bounded correction does not clear [issue 2094](https://github.com/vamseeachanta/digitalmodel/issues/2094).

## Verification and retained state

Eight existing documentation-routing tests pass. Whitespace checks pass. The benchmark HTML has six section headings, one captioned table, no horizontal overflow at the tested viewport and no page errors. These checks establish document integrity only. The companion verification receipt records the scoped legal scan and exact input digests.

Private review bundles, provider outputs, dispositions, inventory and screenshots remain retained operational evidence. This coverage record is a main-session factual collation; it is not represented as an additional independent review. No shared harness changes, new persistent sessions or solver execution were introduced.
