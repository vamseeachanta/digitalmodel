---
status: accepted
---

# Sweep Config is a distinct surface from Prospect Intake, sharing only validation machinery

The GTM parametric work needs an analyst-editable input that drives a demo's full case matrix
(the **Sweep Config**, `inputs/demo_0N_*.yml`). The repo already has a client-facing
**Prospect Intake** layer (`prospect_adapter.py` + `prospect-*.yaml`) that declares demo_01 but
leaves its materializer a deliberate `NotImplementedError` stub.

We decided to keep the two surfaces **explicitly separate** — Sweep Config is the analyst
"what-to-compute" matrix; Prospect Intake remains the client "who/what-for-whom" single-design-point
intake — rather than (a) building a fully standalone sweep loader that ignores the adapter and
spawns a second validation stack, or (b) overloading the sales-intake schema to express a 680-case
engineering matrix.

## Consequences

- Sweep Config **reuses the adapter's validation approach** (jsonschema via `_load_schema`), so
  there is one validation idiom, not two.
- Completing the demo_01 prospect-adapter materializer stub is reframed: it should **emit a Sweep
  Config from a Prospect Intake** (a later follow-up / subissue 5), not feed the demo directly.
- demo_01 sets the de-facto template; #633–#636 inherit this Sweep-Config-vs-Prospect-Intake split.
