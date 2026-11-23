# Connector SCF → DNV-RP-C203 Fatigue Procedure (FEA Method)

## Scope

A generic finite-element procedure for deriving the **Stress Concentration
Factor (SCF)** of a threaded/mechanical connector (or any geometry with a local
hot-spot) and converting it into **allowable fatigue cycles** via a DNV-RP-C203
S-N curve. The output is an **SCF-vs-tension-range** relationship and an
equivalent **moment-vs-cycles (M-N)** curve usable in global fatigue analysis.
The method is generic (no client/component-specific data) and applies to
connectors, threaded couplings and similar stress-raising features in risers and
pipe.

## Why connector SCF matters

A connector's threads, shoulders and section changes locally amplify the nominal
(reference) stress. Fatigue life depends on the **local hot-spot stress range**,
not the nominal range, so the SCF (local range ÷ reference range) must be measured
by detailed FE before the connector can be assessed against an S-N curve.

## Procedure

### 1. Choose the FE model fidelity
Use the simplest model that captures the hot-spot:
1. **2D axisymmetric**, elastic-plastic material model (preferred).
2. If a 2D model is not possible, use a **sector model**.
3. If neither simplification is possible, use a **half or full 3D model with
   sub-modelling**.

### 2. Mesh sensitivity study
Refine the mesh until the **hot-spot stress** (highest 1st-principal stress) at
the key local features (e.g. thread roots) **stops changing appreciably — within
5%**. Use the converged mesh for all subsequent steps.

### 3. Ratcheting analysis (shakedown)
Apply and remove the **maximum allowable working load** cyclically until
progressive (ratcheting) strain ceases — i.e. the connector shakes down to a
stable elastic-plastic cycle. This establishes the stabilised stress state before
the fatigue load steps are read.

### 4. Load steps
Apply the connector preload and hydrotest, then a symmetric ladder of tension
load steps about the preload. With `P` = connector preload:

```
Preload
Hydrotest
Preload
Hydrotest
±0.2P, ±0.4P, ±0.6P, ±0.8P, ±1.0P, ±1.2P, ±1.4P
```

(applied as the pairs −0.2P,+0.2P, −0.4P,+0.4P, … −1.4P,+1.4P).

### 5. Extract stress tensors
At the hot spot, output the full **6-component stress tensor** for **each** load
step:

```
σxx, σyy, σzz, σxy, σyz, σzx
```

### 6. Tension ranges
For load-step pair `i` (the +i and −i steps), the tension range is:

```
ΔTi = T(+i) − T(−i)
```

Example: ΔT1 = +0.2P − (−0.2P) = 0.4P; ΔT2 = +0.4P − (−0.4P) = 0.8P; etc.

### 7. Stress-component ranges
For each component:

```
Δσxx_i = σxx(+i) − σxx(−i)        (and likewise for yy, zz, xy, yz, zx)
```

### 8. Von Mises stress range
For each load range, combine the component ranges into a von Mises range:

```
Δσvm_i = sqrt( ½·[ (Δσxx−Δσyy)² + (Δσyy−Δσzz)² + (Δσzz−Δσxx)²
                   + 6·(Δσxy² + Δσyz² + Δσzx²) ] )
```

(all terms at load range `i`).

### 9. Reference stress range
Convert the tension range to a nominal **reference stress range** using the
reference cross-section:

```
Δσref_i = ΔTi / Aref
Aref    = (π/4)·(ODref² − IDref²)
```

where `ODref`/`IDref` are the chosen reference outer/inner diameters.

### 10. SCF per load range
```
SCF_i = Δσvm_i / Δσref_i
```

Plot **SCF_i vs ΔTi** — this is the SCF-vs-tension-range characteristic.

### 11. Tension ↔ moment conversion (for M-N curves)
If an M-N (moment) curve is wanted, convert the tension range to an equivalent
bending-moment range that produces the **same reference stress**:

```
ΔMi = ΔTi · (ODref² + IDref²) / (8 · ODref)
```

Check consistency — the moment range should reproduce the reference stress via:

```
Δσref_i = ΔMi · (ODref/2) / Iref
Iref    = (π/64)·(ODref⁴ − IDref⁴)
```

### 12. Allowable cycles from DNV-RP-C203
Solve the S-N relation for the allowable cycles at each load range, using the
intercept `log a` and slope `m` of the appropriate DNV-RP-C203 S-N curve:

```
log N = log a − m · log Δσvm
=>  Ni = 10^( log a − m · log(Δσvm_i) )
```

Plot the **M-N curve (ΔMi vs Ni)** for use in downstream fatigue accumulation
(Palmgren-Miner; see `riser-fatigue-fundamentals.md`).

## Outputs

- SCF vs tension range (and the equivalent moment range).
- An M-N curve consistent with the chosen DNV-RP-C203 S-N detail category.
- A converged, shaken-down FE hot-spot stress state for the connector.

## Standards referenced

- **DNV-RP-C203** — Fatigue Design of Offshore Steel Structures (S-N curves;
  `log a`, `m`).

## Provenance

Derived and de-identified from internal AceEngineer offshore project work
(~2013–2016).
