# Vessel Suitability Reference Material

## Canonical usage-boundary location

This README is the canonical usage-boundary surface for the vessel-suitability reference set. The material in this directory is retained for internal research and future methodology design only unless a later approved issue explicitly changes that boundary.

## CTV operability reference set

The `ctv-operability-kincardine-print-redline.*` asset family is retained as **reference material** for future vessel-operability work in the repo ecosystem.

Current files:

- `ctv-operability-kincardine-print-redline.html` — editable/rendered source artifact
- `ctv-operability-kincardine-print-redline.svg` — vector export
- `ctv-operability-kincardine-print-redline.png` — image preview/export
- `ctv-operability-kincardine-print-redline.pdf` — print/share export
- `ctv-sov-w2w-operability-reference-synthesis.md` — internal synthesis note for CTV/SOV/W2W operability methodology design
- `data/ctv_operability_kincardine.json` — internal reference metadata and digitized values retained for traceability only

## Structured reference data boundary

`data/ctv_operability_kincardine.json` is **internal reference data only**. Its digitized numerical values are rights-unresolved and no-external-use. The values are not design-basis engineering data and must not be used for project analysis, client reports, public collateral, or marketing material.

Before any external or engineering reuse, regenerate the analysis from source-native, licensed or public datasets and open a separate approved issue that covers:

1. source-rights and attribution review;
2. data provenance and confidence classification;
3. vessel/access criteria and environmental limits;
4. unit handling and validation tests;
5. original report wording and original visual design.

## Usage boundary

Use this material for:

1. Research into CTV/vessel operability reporting patterns.
2. Internal engineering-methodology documentation.
3. Future vessel-operability ecosystem design, including report structure, visual conventions, and evidence packaging.
4. Competitive/reference comparison when building our own original workflows.

Do **not** treat this as approved GTM collateral or project-delivery collateral by default. Reuse for project work, client work, public collateral, or marketing requires explicit user approval, source-rights review, and a separate approved issue.

## Competitive context

SeaOps is a competitor. Any SeaOps-derived or SeaOps-adjacent material should be handled as competitive research/reference material only:

- document observations in our repo ecosystem;
- synthesize original methodology and implementation patterns;
- avoid copying competitor wording, layouts, or client-facing claims into AceEngineer deliverables;
- keep downstream deliverables clearly original and evidence-backed;
- do not export digitized values into external artifacts.

## Source-native engineering implementation path

Source-native engineering implementation belongs under later approved issues, not this reference package. Known integration candidates:

- digitalmodel #578 — W2W gangway operability module path;
- digitalmodel #467 — real-time vessel motion feedback and vessel-performance integration path.

This reference set should inform questions and data contracts for those issues, not replace their engineering inputs or validation.
