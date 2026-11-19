# Method and data

Article: "Fitness for service of corroded pipelines: strength and remaining useful life (RUL)", The Deckhand Chronicles, revision 5 of 19 Nov 2026. This page holds in full what Appendix A of the article summarises.

Method. Section A is rated by the simplified local metal-loss relation of API 579-1/ASME FFS-1, Parts 2 and 5 [6], from the ASME B31.8 rating before loss [7]. The burst pressures come from the five methods of References 1 to 5, with the allowable factors stated in "Five methods give four different pressures". Life is projected at a constant thinning rate to the wall at which the reduced rating falls to the design pressure.

Editions and evaluation levels. The editions are those the library cites; the linked publisher pages may show later editions. The evaluation level for each method, as implemented, is to be checked by the Corrosion Deckhand against the evaluation levels of ASME B31G-2012, the edition the library cites:

- B31G and Modified B31G [1, 2]: Level 1 evaluation (to be checked).
- RSTRENG [3]: Level 2 effective-area evaluation (to be checked).
- RSTRENG-2D [4]: Level 2 effective-area evaluation (to be checked); the implementation projects each axial row to its greatest depth.

Public data set: our corroded-pipeline worked example in digitalmodel, published by AceEngineer, MIT licence, worked-example readings, retrieved 12 Nov 2026 at commit 09089a121a16069c4b657d1a320a630a23ab1dd5. <https://github.com/vamseeachanta/digitalmodel/tree/09089a121a16069c4b657d1a320a630a23ab1dd5/examples/workflows/pipeline-ffs-assessment>

Code: our public code repositories, <https://github.com/vamseeachanta/digitalmodel> (MIT licence), pinned to the same commit.

## References

1. B31G method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
2. Modified B31G method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
3. RSTRENG method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
4. RSTRENG-2D method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
5. DNV-RP-F101, *Corroded Pipelines*, January 2015 as implemented, Part B (allowable stress approach); current edition September 2019, amended September 2025. <https://www.dnv.com/energy/standards-guidelines/dnv-rp-f101-corroded-pipelines/>
6. API 579-1/ASME FFS-1, *Fitness-For-Service*, 2021; Part 2, *Fitness-For-Service Engineering Assessment Procedure*; Part 5, *Assessment of Local Metal Loss*. The local-loss relation is simplified. 2021 is the current edition. <https://www.asme.org/codes-standards/find-codes-standards/fitness-for-service>
7. ASME B31.8, *Gas Transmission and Distribution Piping Systems*, 2022 as implemented, 841, *Steel Pipe*; current edition 2025. <https://www.asme.org/codes-standards/find-codes-standards/b31-8-gas-transmission-distribution-piping-systems>
8. AceEngineer, pipeline fitness-for-service assessment, study page, October 2026. <https://vamseeachanta.github.io/digitalmodel/ffs/pipeline-ffs-assessment.html>
