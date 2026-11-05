# Subsea power cable standards metadata

The FOWT watch-circle screen references DNV-ST-0359 for wind power cable
system scope. The MBR limit is supplied by the caller. Public publisher
metadata establishes neither a dynamic MBR clause nor a design factor.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-11-05 | Correct cable editions and scope and distinguish public metadata from engineering requirements. |

## Publisher metadata

The following public product pages were accessed on 2026-11-05; both returned
HTTP 200. Only product metadata was consulted.

| Reference | Title | Edition | Scope established by the public page |
| --- | --- | --- | --- |
| [DNV-RP-0360](https://www.dnv.com/energy/standards-guidelines/dnv-rp-0360-subsea-power-cables-in-shallow-water/) | Subsea power cables in shallow water | 2016-03, amended 2021-10 | Focus on static service in shallow water throughout the cable life cycle. |
| [DNV-ST-0359](https://www.dnv.com/energy/standards-guidelines/dnv-st-0359-subsea-power-cables-for-wind-power-plants/) | Subsea power cables for wind power plants | 2025-12 | Offshore wind power plant cable systems, including inter-array and export systems, components and interfaces. |

The ST-0359 product page does not specify static or dynamic service. Naming it
as the wind-cable reference does not establish dynamic applicability or
validate the circular-arc model. The previous RP-0360 edition labels and
attribution of dynamic MBR and fatigue clauses were unsupported.

## Calculation citation and resolver

`mooring_design_fowt.py` emits `DNV-ST-0359`, revision `2025-12`, section
`Scope (public product page)`. Its note states the public-source limits and
the caller-supplied MBR basis. No numerical calculation changes.

A configured wiki requires
`wikis/engineering-standards/wiki/standards/dnv-st-0359.md` with matching
`code_id`, `publisher` and `revision` frontmatter. Missing or mismatched
metadata raises `CitationResolutionError`. Existing RP-0360 pages do not
satisfy this dependency. The separately maintained wiki must provide the
ST-0359 resolver page. An unconfigured installation retains the existing
warning and empty-sidecar behaviour.

## Archived atlas metadata

Archived manifests and their recorded basis fields are immutable publication
evidence. Changing a recorded edition would invalidate the source fingerprint
and artifact digests without changing any numerical result. The following
historical manifests retain their original bytes. Their entries in
[`historical-provenance.json`](../../../../atlases/historical-provenance.json)
carry an explicit `citation_correction` with the verified RP-0360 edition,
amendment and static scope, and the ST-0359 wind-cable reference.

| Archived FOWT atlas | Treatment |
| --- | --- |
| `0af02c3aaf14` | Original basis retained; citation correction recorded separately. |
| `15372d97a52f` | Original basis retained; citation correction recorded separately. |
| `7ac02c8bc7f0` | Original basis retained; citation correction recorded separately. |
| `b4046a89d219` | Original basis retained; citation correction recorded separately; existing reconstruction gap remains. |
| `1f1871cd817d` | Original basis retained and indexed from its retained publication; citation correction recorded separately. |

The current atlas is regenerated with ST-0359 edition 2025-12 as scope
metadata. The correction is not retroactive qualification of historical
calculations. Historical `basis.standards` and manifest `provenance.standards`
remain records of the original, incorrect labels, rather than current
publisher metadata. Consumers reading those raw fields must also consult the
correction; automated application of correction notes is not implemented.

## What is not established

Dynamic service applicability, an MBR clause, fatigue criteria, design factors
and standards compliance remain open. They require a separately verified
engineering basis beyond the public product pages. The geometric screen and
its caller-supplied limits provide no such qualification.
