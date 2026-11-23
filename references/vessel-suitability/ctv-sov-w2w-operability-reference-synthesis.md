# CTV/SOV/W2W operability reference synthesis

## Purpose and boundary

This note captures internal reference learnings from the retained Kincardine CTV operability material and related vessel-access questions. It is **internal reference material only**. It is not approved for client, project, public, or marketing reuse.

The retained SeaOps-adjacent assets and structured JSON are useful for studying report patterns and data-shape requirements, but they cannot be promoted into deliverables by default. Any external reuse requires:

1. explicit user approval;
2. source-rights and attribution review;
3. a separate approved issue;
4. original AceEngineer/digitalmodel wording, visuals, and calculations;
5. source-native data regeneration.

## Observed reference patterns

These are high-level observations, not copied wording, layout, or claim language:

- **Monthly operability by access configuration** — a useful report can show how an access arrangement performs month by month instead of relying on a single annual summary.
- **Annual/headline operability uplift concept** — a changed access configuration can be summarized as an annual availability delta, but the supporting monthly profile is needed to explain seasonality.
- **Daylight and environmental thresholds** — daylight-only transfer constraints and wind/wave limits must be explicit inputs, not hidden report assumptions.
- **Directional/landing arrangement sensitivity** — access availability may depend on landing direction, approach sector, or transfer geometry; static vessel capability is insufficient by itself.
- **Bottleneck-month identification** — the most constrained months can drive maintenance planning and vessel/access selection more than annual averages.
- **Caveat block requirement** — infographic-derived values must be separated from source-native metocean analysis and cannot be treated as design basis.

## Reusable engineering concepts

Future source-native operability work should model:

- access mode: CTV landing, SOV gangway, W2W gangway, daughter craft, or other access mode;
- landing point / approach geometry: landing side, heading sector, relative position, and approach constraints;
- vessel/access limits: wind, wave, current, visibility, daylight, personnel-transfer criteria, and gangway operating envelope;
- metocean climatology: source dataset, time span, spatial coordinate, sampling interval, units, and quality controls;
- persistence/weather windows: minimum access duration, recovery time, and monthly/seasonal persistence statistics;
- relative motion path: vessel RAOs, platform motions, gangway compensation, landing relative motion, and real-time sensor feedback where applicable.

## Original digitalmodel methodology requirements

A future digitalmodel implementation should provide a source-native data contract rather than relying on digitized reference values:

### Data schema

- site metadata: project/site name, coordinates, timezone, water depth if relevant;
- environmental time series or climatology: wind speed, wave height/period/direction, current, visibility, daylight;
- vessel/access criteria: thresholds, access mode, landing geometry, and operating envelope;
- calculation assumptions: transfer duration, persistence window, limiting variable priority, and seasonal aggregation rules;
- provenance: source name, license status, retrieval date, processing version, confidence class.

### Provenance and confidence

Each reported value should carry a confidence class:

- `source_native_verified` — regenerated from licensed/public source data with reproducible processing;
- `source_stated_reference` — stated by a source artifact but not independently regenerated;
- `digitized_reference_only` — approximated from an image or chart; internal use only;
- `assumption_placeholder` — used for workflow design only, never engineering decision-making.

### Unit handling

- Store native units and normalized units explicitly.
- Validate coordinate sign conventions and dataset coordinate reference.
- Treat source notation ambiguities, such as wave/wind shorthand, as unresolved until source-native data is inspected.

### Chart/report outputs

Ship-today internal chart candidates from the retained JSON:

- monthly as-built vs modified access-configuration bar chart;
- annual availability summary with clear internal-only caveat;
- bottleneck-month table showing winter/daylight sensitivity and peak uplift month.

Future source-native chart candidates:

- time-series access/no-access classification from metocean windows;
- monthly persistence/weather-window heatmap;
- access-mode comparison matrix for CTV, SOV, and W2W options;
- relative-motion or gangway-envelope exceedance chart tied to vessel RAOs/motions;
- real-time go/no-go dashboard fed by metocean and motion telemetry.

### Tests

Minimum guardrails:

- prove reference-derived values cannot be loaded from demo/collateral paths by default;
- require provenance, confidence class, and usage boundary metadata;
- fail if internal-only data is described as externally reusable;
- validate monthly series ordering, percent bounds, and source-callout consistency;
- require source-native workflows to document dataset license and processing version.

## Integration points

- **#578 — W2W gangway operability**: use the access-mode, landing-geometry, and vessel/access-limit concepts when developing gangway operability criteria. Do not reuse the Kincardine digitized values as input data.
- **#467 — real-time vessel motion feedback**: use the relative-motion path as a bridge between static metocean operability and live motion/RAO-based go/no-go monitoring.
- **Reference fixture**: `data/ctv_operability_kincardine.json` remains a traceability fixture for methodology design only, with `rights_unresolved_no_external_use` digitized values.

## Promotion gate

Promotion into any external or design-basis workflow requires all of the following:

1. explicit user approval naming the target use;
2. source-rights and attribution review;
3. separate approved GitHub issue;
4. source-native data regeneration;
5. documented provenance and confidence class;
6. original wording and visual design;
7. focused tests showing the internal reference boundary is not bypassed.

## Anti-laundering checklist

Before using any idea from this reference set elsewhere, verify:

- [ ] No copied SeaOps wording.
- [ ] No replicated SeaOps visual hierarchy or layout.
- [ ] No client-facing claims derived from competitor material.
- [ ] No export of digitized values.
- [ ] No use as project/design-basis data.
- [ ] Source-native replacement data exists for engineering or external use.
- [ ] Attribution and rights review are complete.
- [ ] A separate issue explicitly approves the target artifact.
