---
status: accepted
---

# Every sweep axis is a list; the engine computes the cross-product; study method is user data, not code

The priority is a running, yaml-driven program where parametrics are easy to express by user
preference. We deliberately do **not** hardcode a study method (one-factor-at-a-time vs full
factorial vs reference-point sensitivity). Instead, every axis in the Sweep Config is a **list**,
and the runner computes the cross-product of whatever lists it is given:

- a **frozen** axis is a length-1 list (the user's judged most-likely value),
- a **swept** axis is a length-N list,
- the user gets OAT by making one axis multi-valued and leaving the rest length-1, or a factorial by
  making several multi-valued — same code path, no modes.

We rejected building explicit study-method modes (an `--oat` flag, an L1-base / L2-sensitivity split,
a dedicated "reference point" block) because they bake engineering policy into the engine. Policy
belongs in the data: the user edits lists. ADR-0003's async/next-day latency model absorbs the cost
when a user opts into more values.

## Consequences

- Today's matrix = 4 multi-valued axes + length-1 lists for the rest → reproduces the 680-case
  Baseline Run exactly, so the ADR-0002 golden test holds.
- subissue 2 stays small: generalize the nested loop to iterate per-axis lists.
- subissue 3 ("agree parameters to vary") becomes "the user edits lists" — no engine change.
- A frozen axis *is* the reference point; there is no separate most-likely-point concept to design.
- **Promotable vs locked:** only geometry/loading/operational/modeling factors are list-valued axes
  (sizes, spans, currents, gaps, content density, boundary condition, WT selection). Fundamental
  physical constants (steel/seawater density, E, Cₐ) and screening thresholds (VR onset bands, γ_f)
  stay **scalar and locked** — a client does not re-derive them.
- **Per-sub-sweep, not global:** demo_01's two heterogeneous sub-sweeps (`sweeps.pipelines`,
  `sweeps.jumpers`) each carry their own axis lists, since they differ (jumper gap includes `.inf`,
  different spans) and a client may sweep a factor on one but not the other.
