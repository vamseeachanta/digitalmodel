# API 579 coverage intake analysis

The intake records evidence references for Parts 8, 14, 13 and 11 and returns
`NOT_EVALUATED` for every engineering assessment. Missing evidence categories
are listed explicitly. Recorded references establish neither inspection
sufficiency nor suitability for continued operation. The calculation offerings
remain planned; the catalog workflow status describes intake availability only.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-20 | Added non-calculating intake and routing to expose missing evidence and unimplemented criteria. |

## Basis and method

[API 579-1/ASME FFS-1, December 2021, public table of contents](https://www.asme.org/getmedia/5726e673-eb65-456e-913d-3d0dce0359ff/asme-ffs-1_2021_toc.pdf)
was opened on 2026-10-20. Each part's section .3 locates data requirements and
section .4 locates assessment criteria. These headings support clause routing;
they do not provide normative equations or limits. No numerical acceptance
criterion is implemented by this workflow.

The three evidence categories are `design_data`, `operating_history` and
`inspection_data`, corresponding to the published .3.1, .3.2 and .3.3 headings.
Values are nonempty reference strings. The workflow does not open the references
or attest to their content. `RECORDED` means all three references were supplied;
`INCOMPLETE` means at least one is absent. Both states return `NOT_EVALUATED`.
The requested level is recorded without establishing eligibility for that tier.

## Coverage and next steps

| Slice | Coverage | Assessment work remaining |
| --- | --- | --- |
| 1: Part 8 | Geometry evidence intake and pointer to the existing piping feature module | Broader distortion criteria, coupled stress assessment and published-case validation |
| 2: Part 14 | Load-history intake and pointers to existing S-N and cycle-counting libraries | Edition-specific screening, qualified fatigue curves, stress treatment and published-case validation |
| 3: Parts 13 and 11 | Lamination and fire-damage evidence intake with separate next actions | Lamination criteria; metallurgical investigation and heat-zone assessment; published-case validation |

The existing `shell_distortion_screen` implements bounded 2016 piping feature
checks. It is neither invoked nor promoted to a 2021 component assessment here.
The existing `fatigue.sn_curves` and `fatigue.rainflow` modules are preparation
pointers, not executed Part 14 procedures. DNV curves are not automatically
qualified for API 579 fatigue assessment. Part 10 creep-fatigue interaction also
remains planned under the companion tracker.

## Worked intake example

The committed input is
[the workflow configuration](../../../examples/workflows/api579-coverage-screen/input.yml).
Its reference labels are synthetic; it contains no measurements or licensed
numerical criteria. Run the standalone command from the repository root:

```sh
PYTHONPATH=src uv run python -m digitalmodel.asset_integrity.api579_coverage examples/workflows/api579-coverage-screen/input.yml
```

The method is evidence-set subtraction: missing categories are the three intake
categories minus supplied keys. The independent expected answer is:

| Part | Supplied categories | Missing categories | Evidence status | Engineering verdict |
| --- | --- | --- | --- | --- |
| 8 | design, operating history, inspection | none | RECORDED | NOT_EVALUATED |
| 14 | design, operating history | inspection_data | INCOMPLETE | NOT_EVALUATED |
| 13 | none | design_data, operating_history, inspection_data | INCOMPLETE | NOT_EVALUATED |
| 11 | none | design_data, operating_history, inspection_data | INCOMPLETE | NOT_EVALUATED |

The batch verdict is `NOT_EVALUATED`; there is no acceptance, pressure rating or
remaining-life answer. The tests assert these states, input rejection and route
registration. They are software contract tests, not validation against a
published engineering solution. The standalone command imports no solver
integration. The legacy engine adapter is registered separately; that engine's
eager imports prevent its use under a prohibition on solver imports.

The broader source survey remains in
[the public-source review](../../research/ffs-api-579-parts-8-11-13-14-public-source-review.md).
The next calculation slice requires operative, edition-matched criteria and a
published worked solution before an engineering acceptance result can be issued.
