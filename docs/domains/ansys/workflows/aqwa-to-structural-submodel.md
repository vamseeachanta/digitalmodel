# Workflow: `aqwa-to-structural-submodel`

Hydrodynamic diffraction to local structural detail, as a stage-by-stage contract.

| Field | Value |
| --- | --- |
| Workflow identifier | `aqwa-to-structural-submodel` |
| Domain | Offshore and marine structural analysis |
| Products consumed | Ansys AQWA (Hydrodynamic Diffraction), Ansys Mechanical (Static Structural), Ansys Workbench |
| Release basis | Documented behaviour checked against Ansys release 2024 R2 / 2025 R1 help content |
| Status | Definition issued; end-to-end validation against a physical or published comparator is **not established** (see §12) |

Table 1 — Workflow identification. Caption below object per house style.

---

## 1. Scope

This workflow defines the chain by which a wave field is converted into a stress
state in a local structural detail:

```
Panel model of wetted hull
  -> AQWA Hydrodynamic Diffraction (RAOs over frequency and heading)
    -> Design wave / load case selection (equivalent design wave)
      -> Hydrodynamic pressure transfer onto the structural mesh
        -> Equilibrium and mass reconciliation
          -> Global structural solve (Static Structural)
            -> Local submodel (cut-boundary interpolation)
              -> Results extraction and code check
```

Each stage below is written as a contract: purpose, inputs, outputs, the Ansys
mechanism that performs the work, the decisions the analyst must make, the
failure modes, and the acceptance check that shall pass before the next stage is
started. A stage whose acceptance check fails shall not be carried forward; the
defect shall be resolved at the stage in which it arises.

The scope covers first-order diffraction-radiation loading mapped to a linear
static structural solve. Slamming, sloshing, green water, ice, collision and
fully nonlinear free-surface effects are outside scope and are not represented by
this chain.

---

## 2. When NOT to use this workflow

This chain is warranted only where hydrodynamic pressure genuinely drives the
structural response — that is, where the load acting on the structure is the
diffracted and radiated pressure field over a wetted surface, and where the
spatial distribution of that field over the structure is what governs the result.

Most finite element work in an offshore and marine consultancy does not meet that
condition and shall use a direct load definition instead. The following are
explicitly outside the intended use:

| Analysis type | Governing load | Recommended treatment |
| --- | --- | --- |
| Padeye, trunnion, lifting lug | Sling load, resolved at a point or over a bearing area | Direct force / bearing pressure, with DAF applied per the lifting standard |
| Foundation, mudmat, pile cap | Reactions from a separate global model, soil reaction | Direct force and moment, elastic foundation or soil springs |
| Pressure vessel, separator, filter housing | Internal design pressure, nozzle loads | Direct internal pressure and nozzle load set |
| Bolted or welded connection | Member end forces from a frame model | Direct force and moment at the cut |
| Topside frame, skid, module | Self weight, equipment weight, acceleration set | Direct acceleration and mass loads |
| Riser and pipeline local detail | Effective tension, bending moment, internal pressure | Direct load definition from the global dynamic analysis |
| Fixed jacket member check | Wave and current forces on slender members | Morison-type loading in the structural model, not panel diffraction |

Table 2 — Analysis types for which the diffraction-to-structure chain is not the correct route.

The distinguishing test is stated directly: if the structural load can be written
as a force, moment, acceleration or uniform pressure without loss of engineering
content, the diffraction chain adds cost, licence contention and failure modes
without adding accuracy. The chain is justified where **all** of the following
hold:

1. The structure is floating or wet-surface-dominated, and its response depends
   on the pressure distribution over the wetted surface rather than on a resultant.
2. The wetted surface is large relative to the wavelength such that diffraction is
   significant. Where the characteristic member diameter is small relative to the
   wavelength, Morison-type loading is the applicable model and panel diffraction
   is not required.
3. The structural conclusion is sensitive to the spatial gradient of the pressure —
   for example hull girder stress, shell buckling, or a stiffened panel check.

Where any of the three fails, a direct load definition is recommended.

---

## 3. Stage 1 — Hydrodynamic diffraction (AQWA)

### 3.1 Purpose
A panel representation of the wetted hull is solved for the first-order
diffraction-radiation problem, producing the response amplitude operator (RAO)
matrix over wave frequency and heading, together with the panel pressure field
associated with each frequency-heading pair.

### 3.2 Inputs

| Input | Units | Note |
| --- | --- | --- |
| Wetted-surface geometry at the analysis draught | m | Surface bodies to the waterline; internal tanks excluded from the diffracting surface |
| Mass | te | Total displacement mass at the analysis condition |
| Centre of gravity | m | In the hydrodynamic model axis system |
| Radii of gyration or inertia tensor | m, te·m² | Consistent with the mass and COG above |
| Water depth | m | Finite or infinite depth as applicable |
| Water density | te/m³ | Stated explicitly; a density mismatch propagates to every downstream pressure |
| Gravitational acceleration | m/s² | Stated explicitly |
| Frequency range and increment | rad/s | See §3.5 |
| Heading range and increment | deg | See §3.5 |
| Additional damping | — | Roll damping in particular; see §3.6 |

Table 3 — Stage 1 inputs.

### 3.3 Outputs
- RAO matrix for the six rigid-body motions, amplitude and phase, over the
  frequency-heading grid.
- RAOs for any derived dominant load parameter requested (for example vertical
  bending moment at a section, where a hydrodynamic section cut is defined).
- First-order panel pressure field, amplitude and phase, at panel centroids.
- Hydrostatic stiffness, added mass and radiation damping matrices.
- Equilibrium and hydrostatic report for the analysis condition.

### 3.4 Ansys mechanism
Hydrodynamic Diffraction system in Workbench, solved through the AQWA solver.
The diffracting surface is meshed with panels; the panel mesh is separate from
and independent of the structural mesh created later.

### 3.5 Decisions the analyst must make

- **Panel size.** The panel mesh shall resolve the shortest wave of interest. A
  panel edge length not exceeding one seventh of the shortest wavelength is a
  commonly applied working rule; the governing requirement is convergence
  demonstrated by refinement, not the rule of thumb. The defining maximum element
  size in AQWA is derived from the highest frequency requested, and the solve will
  report where the mesh does not satisfy it.
- **Frequency grid.** The grid shall span the range over which the dominant load
  parameter RAO is non-negligible, and shall be fine enough that the RAO peak is
  resolved rather than straddled. A grid that steps over a lightly damped roll
  peak will under-report the peak and therefore over-report the design wave
  amplitude derived from it in Stage 2.
- **Heading grid.** Headings shall cover the range required by the load case
  matrix. Where the hull and loading are symmetric, a half-range with symmetry
  may be used; where mooring, thrusters, or asymmetric mass break that symmetry,
  the full range shall be used.
- **Draught and loading condition.** Each structural condition to be assessed
  requires its own hydrodynamic solve at the corresponding draught, trim, mass and
  inertia. A hydrodynamic model solved at one condition shall not be mapped onto a
  structural model representing a different condition.
- **Forward speed.** Where forward speed is relevant, the applicable AQWA
  treatment and its limitations shall be identified before the grid is chosen.
- **Irregular frequency removal.** Where the lid or interior free-surface panel
  treatment is available, its use is recommended for hulls with a large waterplane;
  irregular frequencies produce spurious spikes in the pressure field that will be
  mapped to the structure without warning.

### 3.6 Failure modes

| Failure mode | Symptom | Consequence if undetected |
| --- | --- | --- |
| Panel mesh too coarse for the highest frequency | Solver warning on defining maximum element size | Pressure field under-resolved; mapped structural load wrong in distribution and magnitude |
| Irregular frequencies not suppressed | Narrow spikes in added mass, damping or pressure at isolated frequencies | A spurious frequency is selected as the design wave |
| Roll damping not specified | Roll RAO peak unphysically large | Design wave amplitude from a roll-driven DLP is non-conservative (amplitude divided by an inflated RAO) |
| Mass properties inconsistent with the structural model | None at this stage | Detected only at Stage 4, after the mapping cost has been incurred |
| Waterline not closed / leaking panels | Solver warning or failed hydrostatic check | Displacement and pressure both wrong |
| Density or gravity differs from the structural basis | None | Systematic pressure error carried silently to the structure |

Table 4 — Stage 1 failure modes.

### 3.7 Acceptance check — all shall pass before Stage 2

1. Computed hydrostatic displacement agrees with the declared mass of the
   condition to within 1 percent, and the computed centre of buoyancy is
   consistent with the declared centre of gravity such that the reported trim and
   heel at equilibrium are within the tolerance stated for the condition.
2. The panel mesh satisfies the defining maximum element size for the highest
   requested frequency, with no outstanding mesh warnings.
3. Added mass and damping curves are smooth over the frequency grid; any isolated
   spike is identified as an irregular frequency and suppressed, or the affected
   frequency is excluded with justification.
4. The dominant load parameter RAO peak is resolved by at least three grid points
   across the peak, not straddled by two.
5. A refinement comparison is recorded: the RAO of the governing dominant load
   parameter changes by less than a stated tolerance — 5 percent is a commonly
   applied acceptance band — between the working panel mesh and a refined mesh.

---

## 4. Stage 2 — Design wave and load case selection

### 4.1 Purpose
The frequency-domain RAO matrix is reduced to a small number of deterministic
regular waves, each of which reproduces the extreme value of one dominant load
parameter. This is the equivalent design wave (EDW) method. The output of this
stage is the set of `(amplitude, frequency, heading, phase)` tuples that Stage 3
will map.

### 4.2 Inputs
- RAO matrix from Stage 1, including the RAO of each dominant load parameter.
- Long-term or short-term extreme value of each dominant load parameter, from a
  spectral or long-term response analysis against the applicable metocean basis.
- The governing design standard or class rule defining the return period, the
  extreme value definition (most probable extreme value, or a stated fractile),
  and the load case matrix.

### 4.3 Outputs

| Output | Units | Definition |
| --- | --- | --- |
| Dominant load parameter (DLP) | varies | The response whose extreme the design wave is constructed to reproduce |
| Design wave amplitude | m | See §4.5 |
| Wave frequency / period | rad/s, s | Frequency at which the DLP RAO attains its maximum |
| Wavelength | m | From the dispersion relation at that frequency and water depth |
| Heading | deg | Heading at which the DLP RAO attains its maximum |
| Wave phase angle | deg | Phase of the DLP RAO at the selected frequency and heading |
| Crest position | m | Longitudinal position of the crest implied by the phase angle |

Table 5 — Stage 2 outputs, per design wave.

### 4.4 Dominant load parameters
The DLP set is defined by the governing standard. For a ship-shaped or
barge-shaped hull the DLP set typically includes:

- vertical bending moment, hogging and sagging, at the governing section;
- horizontal bending moment at the governing section;
- vertical and horizontal shear force;
- torsional moment, where the section is open or the hull is torsionally soft;
- vertical acceleration at a governing location;
- lateral acceleration at a governing location;
- roll angle;
- relative wave elevation at a governing location.

Each DLP produces its own design wave, and each design wave produces its own
structural load case. The DLP set shall be taken from the governing standard and
shall not be reduced without stating the reduction and its justification.

### 4.5 Construction of the equivalent design wave

**Amplitude.** The design wave amplitude is the extreme value of the dominant
load parameter divided by the peak value of that parameter's RAO:

```
a_EDW = DLP_extreme / max( RAO_DLP(omega, beta) )
```

where the maximum is taken over the frequency-heading grid, `DLP_extreme` is the
most probable extreme value of the DLP for the specified return period and sea
state, and `RAO_DLP` is the DLP response per unit wave amplitude. The units of
`DLP_extreme` and of `RAO_DLP` shall be consistent such that `a_EDW` is a length.

**Frequency and wavelength.** The design wave frequency is the frequency at which
the DLP RAO attains its maximum. The wavelength follows from the dispersion
relation at that frequency and the analysis water depth:

```
omega^2 = g * k * tanh(k * d)
lambda  = 2 * pi / k
```

reducing to `lambda = 2*pi*g / omega^2` in the deep-water limit. The wavelength
shall be reported, because the ratio of wavelength to hull length is the physical
statement of what the design wave is doing to the structure and is the quantity a
reviewer checks first.

**Heading.** The design wave heading is the heading at which the DLP RAO attains
its maximum.

**Crest position.** The crest position is determined by the phase angle of the
DLP RAO at the selected frequency and heading. The wave is positioned in
longitudinal space such that the DLP attains its peak value at the instant that
is mapped. A design wave mapped at the wrong phase produces a structurally
plausible but physically wrong load case, and the error is not detectable from the
stress result alone.

**Accompanying loads — the point that governs.** The pressure field transferred
to the structure is the field acting **simultaneously with the peak of the
dominant load parameter**. It is not the pressure field's own maximum, and it is
not the envelope of pressure over the wave cycle. At the phase instant at which
the DLP peaks, some regions of the hull are at or near their own pressure maximum
and others are not; the correct field is the instantaneous one. Constructing a
load case from the pressure envelope superimposes instants that never co-exist,
producing a load set that is not in equilibrium with any real wave and that will
fail the Stage 4 check or, worse, pass it after an unjustified correction.

The same principle applies to every accompanying response: the accelerations,
the motions and the internal tank pressures applied in the structural load case
shall all be evaluated at the same phase instant as the DLP peak.

### 4.6 Decisions the analyst must make

- The extreme value definition — most probable extreme value against a stated
  fractile — shall be taken from the governing standard, stated explicitly, and
  applied consistently across all DLPs. A mixed basis across DLPs is a defect.
- Whether one phase instant per design wave is sufficient, or whether a sweep of
  phase angles is required. Where the structural region of interest is remote
  from the DLP location, the instant of peak DLP is not necessarily the instant of
  peak local stress; a phase sweep is then recommended, and the governing instant
  is selected on the local response.
- Whether the linear RAO basis is adequate. The EDW method is linear by
  construction. Where nonlinearity is significant — large relative motion, bow
  flare, non-wall-sided geometry near the waterline — the linear design wave
  understates the response, and this limitation shall be stated in the report
  rather than absorbed silently.
- Whether the still-water load component is carried separately and superimposed,
  or included in the mapped case.

### 4.7 Failure modes

| Failure mode | Consequence |
| --- | --- |
| Pressure envelope mapped instead of the simultaneous field | Load set not in equilibrium with any wave; Stage 4 check fails or is forced |
| Phase angle omitted or defaulted to zero | Wave crest in the wrong longitudinal position; DLP does not attain its extreme |
| RAO peak straddled by the frequency grid | Peak RAO understated, design wave amplitude overstated |
| Design wave amplitude exceeding a physically admissible steepness | Linear theory applied outside its range; the wave is not realisable |
| DLP extreme and RAO in inconsistent units or sign convention | Amplitude wrong by a fixed factor, undetected downstream |
| Single DLP assumed governing without demonstration | Governing structural case missed |

Table 6 — Stage 2 failure modes.

### 4.8 Acceptance check — all shall pass before Stage 3

1. For each design wave, the reconstructed DLP — the DLP RAO at the selected
   frequency and heading multiplied by the design wave amplitude — reproduces the
   target extreme value to within 1 percent. This is an arithmetic identity by
   construction, and a failure indicates a unit, sign or interpolation error.
2. The wave steepness `H/lambda` of each design wave is computed and reported,
   and is within the limit of applicability of linear wave theory stated for the
   analysis. A steepness at or beyond the breaking limit shall be reported as a
   limitation of the linear basis, not accepted silently.
3. The wavelength-to-hull-length ratio is reported for each design wave and is
   consistent with the physical mechanism the DLP represents — for example a
   sagging vertical bending moment design wave with a wavelength close to the hull
   length.
4. The crest position implied by the phase angle is reported in model
   coordinates and is consistent with the DLP mechanism.
5. Each design wave is documented with all seven quantities of Table 5. A design
   wave carried forward with any of the seven undocumented shall be rejected.

---

## 5. Stage 3 — Pressure transfer to the structural mesh

### 5.1 Purpose
The first-order pressure field from the hydrodynamic solve is evaluated on the
structural finite element mesh for the selected design wave, and applied as a
structural load.

### 5.2 Inputs
- Solved Hydrodynamic Diffraction system from Stage 1, linked in the project
  schematic.
- Structural finite element model of the global structure, meshed.
- Design wave parameters from Stage 2: incident wave amplitude, wave frequency or
  period, wave direction, wave phase angle.
- The face selection defining the wetted surface of the structural model.

### 5.3 Outputs
- Mapped pressure distribution on the selected structural faces.
- Accelerations and motion state consistent with the mapped instant.
- Mapping diagnostics, including the mass and inertia comparison used in Stage 4.

### 5.4 Ansys mechanism
The **Hydrodynamic Pressure** object, inserted under a Static Structural analysis
in Mechanical, where that Static Structural system is linked to a solved
Hydrodynamic Diffraction system in the project schematic. The object is
configured with wave frequency or period, wave direction, wave phase angle and
incident wave amplitude, and the transfer is started by generating the load on
the object.

### 5.5 Direct and Interpolated mapping

Two mapping methods are available, and the choice is constrained by the model,
not by preference.

| Property | Direct | Interpolated |
| --- | --- | --- |
| Source of the pressure | Evaluated at the position of each selected structural node directly from the diffracting panel source strengths | Interpolated at each structural node from the element-centred pressures of the hydrodynamic mesh |
| Line bodies in the mapping selection | Not permitted | Permitted |
| Outer surfaces of solid bodies | Permitted | Not permitted |
| Hydrodynamic model containing more than one structure | Permitted; pressures may be mapped onto one of those structures | Not permitted |
| Sensitivity to hydrodynamic mesh resolution | Avoids the interpolation loss | Accuracy depends on the hydrodynamic mesh resolution |

Table 7 — Mapping method capabilities and restrictions.

The selection rule follows directly from Table 7 and is not discretionary:

- A model whose wetted surface is carried on **line bodies** shall use
  Interpolated, and therefore shall be a single-structure hydrodynamic model with
  no mapping onto solid outer surfaces.
- A model requiring mapping onto **solid outer surfaces**, or drawing from a
  **multi-structure** hydrodynamic model, shall use Direct, and therefore shall
  carry no line bodies in the mapping selection.
- Where neither constraint applies, Direct is recommended, because it avoids the
  accuracy loss of interpolation.
- Where **both** constraints apply simultaneously — line bodies together with
  solid outer surfaces or a multi-structure hydrodynamic model — no mapping method
  satisfies the model as built. The structural model shall be restructured, or the
  hydrodynamic model split into single-structure solves, before the transfer is
  attempted.

### 5.6 Limits of the mapped load

**Second-order pressure terms on diffracting panels are not calculated.** Line
body loads do include a second-order viscous drag component, but the diffracting
panel pressures carried into the structural model are first-order only. Any
analysis whose conclusion depends on mean drift, slowly varying drift or
second-order sum-frequency pressure on the wetted surface is not served by this
transfer, and that limitation shall be stated in the report.

**Only one Hydrodynamic Pressure object may be added to a Static Structural
analysis.** Each design wave and each phase instant therefore requires its own
Static Structural system. A load case matrix of *n* design waves is *n* Static
Structural systems, and this drives the licence and scheduling position in §10.

### 5.7 The silent-transfer setup error

The hydrodynamic solve shall be started by updating the Hydrodynamic Diffraction
**Solution cell from the project schematic**, with the Mechanical editor closed.

Where the Hydrodynamic Diffraction analysis is solved from inside the AQWA
Workbench editor only, the output files are **not transferred** to the Static
Structural system. The hydrodynamic solve completes, the AQWA results are
correct and viewable, and nothing indicates a fault — but the structural system
has received no pressure data. This is the most consequential silent failure in
the chain, because every downstream artefact is produced without error and is
wrong.

The required sequence is:

1. A link exists in the project schematic between the Hydrodynamic Diffraction
   system and the Static Structural system.
2. The Mechanical editor is closed.
3. The Hydrodynamic Diffraction Solution cell is updated from the project
   schematic. This solves the diffraction analysis if required and transfers the
   output files into the Static Structural solver files.
4. Mechanical is then opened, and the Hydrodynamic Pressure object is configured
   and generated.

### 5.8 Decisions the analyst must make

- Mapping method, per the rule in §5.5.
- The face selection defining the wetted surface. Faces above the mapped
  waterline and internal faces shall be excluded; a selection that includes dry
  faces will receive a pressure value that has no physical meaning.
- Whether the structural mesh and the hydrodynamic panel mesh are sufficiently
  compatible in the regions that matter. The two meshes are independent by
  design, but a structural mesh markedly finer than the panel mesh under
  Interpolated mapping recovers no additional pressure detail.
- Treatment of the region near the waterline, where the instantaneous wetted
  surface differs from the mean wetted surface. The linear transfer is defined on
  the mean wetted surface; the treatment adopted shall be stated.
- Whether internal tank pressures and other accompanying loads are applied in the
  same Static Structural system at the same phase instant.

### 5.9 Failure modes

| Failure mode | Detection |
| --- | --- |
| Diffraction solved inside the AQWA editor only; nothing transferred | No pressure contour on the structural model after Generate; check the Solution cell update state |
| Mapping method incompatible with the model content | Generate fails or reports an unmapped selection |
| Dry or internal faces included in the wetted selection | Pressure contour extends above the waterline |
| Second-order dependence assumed but not present in the mapped field | Not detectable from the result; shall be caught by review of the analysis basis |
| More than one design wave attempted in one Static Structural system | Blocked by the one-object limit |
| Mechanical open during the schematic update | Update does not transfer as expected |
| Wave phase angle in the object not matching the Stage 2 phase | Pressure distribution inconsistent with the intended crest position |

Table 8 — Stage 3 failure modes.

### 5.10 Acceptance check — all shall pass before Stage 4

1. The pressure contour on the structural model is non-zero, continuous over the
   wetted selection, and terminates at the intended waterline.
2. The structure panel pressure reported in AQWA at the element centroids is
   compared against the mapped pressure in Mechanical at corresponding locations.
   Agreement within a stated tolerance shall be demonstrated; a sampled
   comparison at a minimum of five locations distributed over the hull is
   recommended, covering bow, stern, both sides and the bottom.
3. The integrated vertical component of the mapped pressure over the wetted
   surface is compared against the buoyancy-plus-wave vertical force from the
   hydrodynamic solve at the same instant, and agrees within a stated tolerance.
4. The wave frequency, direction, phase angle and amplitude configured on the
   Hydrodynamic Pressure object are verified item-by-item against the Stage 2
   design wave record.
5. The mapping method used is recorded, together with the model property that
   made it the required choice.

---

## 6. Stage 4 — Equilibrium and mass reconciliation

### 6.1 Purpose
The mapped external pressure is a set of surface tractions in equilibrium with
the *hydrodynamic* model's mass and inertia. The structural model carries its own
mass, which in general differs. This stage establishes that the applied load set
is in equilibrium to within an accepted residual, so that the boundary conditions
of Stage 5 carry no load that physically belongs on the structure.

### 6.2 Inputs
- Mapped pressure field and accompanying accelerations from Stage 3.
- Structural model mass, centre of gravity and inertia tensor, as built from the
  element density and any applied point masses.
- Hydrodynamic model mass, centre of gravity and inertia tensor from Stage 1.

### 6.3 Outputs
- Mass and inertia comparison table, structural against hydrodynamic.
- Any point-mass correction applied, with its magnitude and location.
- Residual unbalanced force and moment in the three global directions.

### 6.4 Ansys mechanism
The load transfer compares the structural mass properties to the hydrodynamic
model properties as part of the mapping. Where the two disagree, correction is
applied by adding point masses to the structural model, positioned and sized such
that the corrected total mass, centre of gravity and inertia tensor of the
structural model match those of the hydrodynamic model. Residual imbalance is
carried by inertia relief or by a soft restraint set whose reactions are then
checked.

### 6.5 The reconciliation

The structural model of a hull typically represents plate, stiffener and girder
steel but not outfit, machinery, cargo, ballast, consumables or marine growth.
The hydrodynamic model represents the full displacement. The two therefore
disagree on mass by construction, and the disagreement is a normal feature of the
workflow, not a modelling error.

The correction is applied so that:

```
m_structural + sum(m_point)          = m_hydrodynamic
COG of the corrected structural model = COG of the hydrodynamic model
I  of the corrected structural model  = I  of the hydrodynamic model
```

Point masses shall be distributed to represent the actual distribution of the
missing mass — cargo in the cargo spaces, ballast in the ballast tanks,
machinery in the machinery space — and shall not be concentrated at a single
convenient node solely to satisfy the totals. A point-mass correction that
matches the three global quantities while placing mass in physically wrong
locations will satisfy the equilibrium check and still produce a wrong local
stress field, because the inertial load is applied where the mass is placed.

Where the structural model is a partial model rather than a full-hull model, the
mass reconciliation is performed over the modelled extent only, and the retained
boundary conditions carry the balance. The basis adopted shall be stated.

### 6.6 Acceptance criterion — residual unbalanced force

The applied hydrodynamic external pressure shall be in equilibrium with the other
applied loads. The unbalanced force in each of the three global directions shall
be computed for each load case and checked against the following criterion,
expressed as a percentage of the displacement:

| Sea condition | Residual unbalanced force limit (% of displacement) |
| --- | --- |
| Head seas | 1 |
| Oblique seas | 2 |
| Beam seas | 2 |

Table 9 — Residual unbalanced force acceptance limits, per class-society Dynamic Loading Approach guidance.

Residual forces within these limits may be balanced by adding suitably
distributed inertial forces — inertia relief — before the structural solve is
carried out. Residual forces exceeding these limits shall not be balanced by
inertia relief; the source of the imbalance shall be found and corrected, because
inertia relief applied to a large residual distributes a fictitious body force
over the whole structure and contaminates the stress field everywhere.

### 6.7 Decisions the analyst must make

- The distribution of the corrective point masses, per §6.5.
- Whether inertia relief or a soft restraint set is used to carry the residual.
  Where a restraint set is used, its reactions shall be reported and shall satisfy
  Table 9; where inertia relief is used, the applied relief accelerations shall be
  reported.
- Whether added mass is to be represented in the structural model. The
  hydrodynamic added mass is not structural mass; whether and how it is
  represented shall be stated, and shall be consistent with the way the mapped
  accelerations were derived.
- The treatment of free-surface effects in partially filled tanks, where these
  affect the inertia tensor.

### 6.8 Failure modes

| Failure mode | Consequence |
| --- | --- |
| Mass mismatch uncorrected | The structure accelerates under the mapped load; large residual reactions at the restraints |
| Point masses correct in total but wrongly located | Equilibrium check passes; local inertial load field wrong |
| Inertia relief applied to a residual exceeding Table 9 | Fictitious body force distributed over the whole model; stress field contaminated with no visible symptom |
| Restraint set over-stiff | Residual absorbed as restraint reaction; local stress concentration at the restraint reported as a structural result |
| Displacement basis inconsistent between the two models | Percentage criterion evaluated against the wrong denominator |

Table 10 — Stage 4 failure modes.

### 6.9 Acceptance check — all shall pass before Stage 5

1. The mass, centre of gravity and inertia tensor of the corrected structural
   model are reported alongside the hydrodynamic values, and the differences are
   within the tolerance stated for the analysis.
2. The residual unbalanced force in each of the three global directions satisfies
   Table 9 for the applicable sea condition.
3. The residual unbalanced moment about each of the three global axes is reported.
   Where the governing standard states a moment criterion, it is applied; where it
   does not, the moment residual shall be reported and its structural significance
   assessed rather than omitted.
4. Where a restraint set is used, the reactions at every restraint are reported
   and are consistent with item 2.
5. Where inertia relief is used, the relief accelerations are reported and are
   consistent with the rigid-body accelerations from the hydrodynamic solve at
   the mapped instant. A material discrepancy between the two indicates that the
   load set does not represent the intended instant.

---

## 7. Stage 5 — Global structural solve

### 7.1 Purpose
The balanced load set is solved on the global structural model to produce the
displacement and stress field from which the local submodel boundary conditions
will be taken, and against which global acceptance criteria are checked.

### 7.2 Inputs
- Global structural model with the mapped pressure, accompanying loads, corrective
  point masses and restraint or inertia-relief definition from Stages 3 and 4.
- Material definition with the modulus, Poisson ratio and density used for mass.
- Acceptance criteria from the governing standard.

### 7.3 Outputs
- Nodal displacement field.
- Element stress field, including membrane and bending components where shell
  elements are used.
- Reaction set at all restraints.
- Result file retained in a form suitable for cut-boundary interpolation at
  Stage 6.

### 7.4 Ansys mechanism
Static Structural analysis system solved through the Mechanical APDL solver.

### 7.5 Element choice

| Structure type | Element family | Note |
| --- | --- | --- |
| Plating, shell, stiffened panel | Shell elements with membrane and bending behaviour, 4-node or 8-node | The standard choice for a global hull model |
| Primary stiffeners, secondary framing | Beam elements, or shell elements where web buckling or local web stress is assessed | Beam representation does not resolve web local stress |
| Castings, thick brackets, connection detail | Solid elements | Generally reserved for the submodel rather than the global model |
| Transitions between shell and solid regions | Constraint equations or bonded contact | The transfer of rotational degrees of freedom requires explicit treatment |

Table 11 — Element selection for the global model.

The mesh density of the global model shall be sufficient to resolve the global
load path and to provide a well-converged displacement field at the intended
submodel cut boundary. It is not required to resolve local stress concentrations,
which is the function of Stage 6. A commonly applied basis for a stiffened-hull
global model is one element between stiffeners; the governing requirement is the
convergence of the cut-boundary displacement field, which shall be demonstrated
rather than assumed.

### 7.6 Boundary conditions

The mapped load set is, after Stage 4, self-equilibrated to within the Table 9
residual. The boundary condition set therefore exists to remove rigid-body
motion, not to carry load. Two admissible treatments:

- **Inertia relief.** Rigid-body motion is removed by the relief formulation and
  no external restraint carries load. This is the treatment consistent with a
  freely floating structure.
- **Minimum restraint set.** Six degrees of freedom are restrained at points
  remote from the region of interest, sufficient to remove rigid-body motion and
  no more. The reactions at these points shall be small per Table 9 and shall be
  reported.

A restraint set that over-constrains the model — for example a fully fixed edge —
shall not be used, because it carries load that physically belongs in the
structure, redistributes the stress field, and produces a spurious concentration
that may be mistaken for a structural finding.

### 7.7 Solver selection
The sparse direct solver is the default and is recommended for models of the size
typical of a global hull model with shell elements. An iterative solver may be
selected where model size makes the direct solver impractical, in which case the
convergence tolerance shall be stated and the residual reported. Where inertia
relief is used, solver compatibility with the relief formulation shall be
confirmed before the production run.

The analysis is linear elastic unless a nonlinearity is explicitly required.
Where contact, large deflection or material nonlinearity is introduced, the
superposition implicit in combining the mapped wave case with a still-water case
is no longer valid, and the load cases shall be combined before the solve rather
than after.

### 7.8 Failure modes

| Failure mode | Detection |
| --- | --- |
| Over-constrained boundary set | Large restraint reactions; stress concentration at the restraint |
| Rigid-body motion not fully removed | Solver reports a singular or near-singular matrix, or pivot warnings |
| Mesh too coarse at the intended cut boundary | Cut-boundary displacements not converged; Stage 6 boundary conditions unreliable |
| Shell-to-beam or shell-to-solid transition without rotational continuity | Artificial hinge at the transition; load path wrong |
| Units inconsistent between the mapped pressure and the material definition | Stress wrong by a fixed factor throughout |
| Linear superposition applied to a nonlinear solve | Combined results invalid |

Table 12 — Stage 5 failure modes.

### 7.9 Acceptance check — all shall pass before Stage 6

1. The solve completes with no pivot, element-formulation or convergence warnings
   outstanding, or each outstanding warning is assessed and recorded as benign.
2. Total reaction in each global direction equals the applied load resultant to
   within the numerical tolerance of the solve, and satisfies Table 9.
3. The deformed shape is inspected and is consistent with the design wave
   mechanism — for example a hogging deflection for a hogging vertical bending
   moment design wave. A deformed shape inconsistent with the DLP indicates a
   phase or sign error upstream.
4. A hand check is recorded for at least one global quantity. For a hull girder
   case, the section force or moment recovered from the finite element model at
   the governing section is compared against the target DLP extreme from Stage 2,
   and agrees within a stated tolerance. This is the single most valuable check in
   the chain, because it closes the loop from the hydrodynamic DLP back to the
   structural response.
5. Mesh convergence at the intended cut boundary is demonstrated: the displacement
   field at the cut-boundary location changes by less than a stated tolerance
   between the working mesh and a refined mesh.

---

## 8. Stage 6 — Local submodeling

### 8.1 Purpose
A locally refined model of a structural detail is solved using displacements
interpolated from the global result onto its cut boundary, resolving stress
concentrations that the global mesh cannot represent.

### 8.2 Inputs
- Global result file from Stage 5, retained with the displacement field.
- Fine-mesh local model, geometrically coincident with the corresponding region of
  the global model in the same coordinate system.
- The cut-boundary face or edge set on the local model.
- Any load acting directly on the local region — in particular the mapped
  hydrodynamic pressure on any wetted face within the submodel extent.

### 8.3 Outputs
- Local displacement and stress field at the refined mesh density.
- Cut-boundary comparison data supporting the validity check of §8.7.

### 8.4 Ansys mechanism
Submodeling by cut-boundary interpolation. Degree-of-freedom values from the
global result are interpolated onto the nodes of the submodel cut boundary and
applied as imposed displacements. In Mechanical APDL this is performed by the
cut-boundary interpolation operation; in Mechanical the Submodeling object
performs the equivalent transfer from an upstream Static Structural result.

For **shell-to-solid** submodeling, where the global model is built of shell
elements and the submodel of solid elements to resolve through-thickness
behaviour, the transfer accounts for the difference in degrees of freedom: a
shell node carries three translations and three rotations, a solid node carries
three translations only. Each cut-boundary node of the solid submodel is
projected onto the nearest element in the shell plane, and the degree-of-freedom
values at the projected point are interpolated and assigned to the solid node.
The shell rotations are thereby converted into the through-thickness variation of
translation on the solid cut face.

### 8.5 Decisions the analyst must make

- **Where to cut.** See §8.7; this is the governing decision of the stage.
- **Shell-to-shell or shell-to-solid.** Shell-to-solid is required where the
  conclusion depends on through-thickness stress variation — weld toe stress, a
  thick insert plate, a cast or forged component, a bolted flange, or any detail
  where the plane-stress assumption of a shell fails. Where the detail is
  thin-walled and the conclusion is a membrane-plus-bending stress, shell-to-shell
  refinement is sufficient and is recommended, being substantially cheaper.
- **Mesh density at the hot spot.** The density shall be set by the acceptance
  criterion. Where the governing standard specifies a stress-reading mesh — for
  example an element size equal to the plate thickness for hot-spot stress
  extraction — that specification governs and shall be applied exactly, because
  the allowable stress is calibrated to that mesh.
- **Mesh transition.** The transition from the cut-boundary mesh density to the
  hot-spot density shall be graded, not abrupt. A commonly applied basis is a
  size ratio not exceeding 2 between adjacent element layers, with at least three
  transition layers between the cut boundary and the region of interest. Abrupt
  transition introduces a numerical artefact whose magnitude is not distinguishable
  from a real stress concentration.
- **Element order.** Quadratic elements are recommended in the region of interest;
  linear elements under-predict stress gradients at a given mesh density.
- **Re-application of direct loads.** Any load acting within the submodel
  extent — pressure, thermal, bolt preload — shall be re-applied to the submodel.
  Cut-boundary interpolation transfers only the boundary displacements; it does
  not transfer interior loads. A submodel of a wetted region solved without
  re-applying the mapped pressure is a frequent and consequential error.

### 8.6 Mesh and geometry rules

1. The submodel geometry shall be coincident with the global geometry in the same
   coordinate system. Where the submodel adds geometric detail absent from the
   global model — a fillet, a cutout, a bracket toe — that detail shall be remote
   from the cut boundary.
2. Every cut-boundary node of the submodel shall lie within the global mesh. A
   node lying outside the global mesh is extrapolated rather than interpolated,
   and extrapolation at a cut boundary can introduce error without warning.
3. The submodel shall be in equilibrium under its imposed cut-boundary
   displacements and re-applied interior loads.
4. Where the submodel spans a material or thickness change, the change shall be
   represented identically in the global model, or the cut boundary shall be moved
   clear of it.

### 8.7 Verifying that the cut boundary is far enough away

Submodeling rests on Saint-Venant's principle: at a distance from a region of
disturbance, the response depends only on the resultant. The cut boundary shall
therefore be placed far enough from the region of interest that the local detail
does not influence the response at the boundary. The following checks establish
this, and the first two are the operative ones:

1. **Cut-boundary result comparison.** The displacement and stress computed by the
   submodel at the cut boundary are compared against the global model results at
   the same locations. Close agreement demonstrates that the local refinement has
   not disturbed the boundary, and therefore that the imposed displacements remain
   valid. Material disagreement demonstrates the opposite: the cut boundary is
   inside the zone of influence of the local detail, and shall be moved outward.
   This check shall be performed for every submodel. A stress agreement within 5
   percent at the cut boundary is a commonly applied working band; the governing
   requirement is that the disagreement be small relative to the margin on the
   local result.
2. **Cut-boundary relocation test.** The submodel is re-solved with the cut
   boundary moved outward by a stated distance. Where the stress at the region of
   interest changes by less than a stated tolerance, the original boundary
   position is demonstrated adequate. Where it changes materially, the boundary
   was too close and the enlarged model governs. This test shall be performed at
   least once per class of detail, and is recommended for every submodel whose
   result governs a design decision.
3. **Distance rule of thumb.** The cut boundary should be at least two to three
   times the characteristic dimension of the local detail away from the region of
   interest. This is a starting position for the mesh, not an acceptance criterion;
   checks 1 and 2 are what establish adequacy.

### 8.8 Failure modes

| Failure mode | Detection |
| --- | --- |
| Cut boundary inside the zone of influence of the local detail | Cut-boundary stress disagrees between global and submodel; relocation test changes the local result |
| Interior loads not re-applied to the submodel | Local stress understated; the pressure load on a wetted submodel face is the usual omission |
| Cut-boundary nodes outside the global mesh | Extrapolated boundary values; silent error |
| Abrupt mesh transition | Artificial gradient indistinguishable from a real concentration |
| Global mesh too coarse to provide a converged boundary displacement field | Submodel refined onto an unconverged boundary; the refinement adds precision without accuracy |
| Shell-to-solid transfer across a location of high shell bending gradient | Projected through-thickness distribution unrepresentative |
| Submodel coordinate system offset from the global | Interpolation returns wrong or null values |

Table 13 — Stage 6 failure modes.

### 8.9 Acceptance check — all shall pass before Stage 7

1. Every cut-boundary node lies within the global mesh; no extrapolated node is
   present.
2. The cut-boundary result comparison of §8.7 item 1 is performed and recorded,
   and the agreement is within the stated band.
3. The cut-boundary relocation test of §8.7 item 2 is performed and recorded for
   the governing detail, and the local result is demonstrated insensitive to the
   boundary position.
4. All loads acting within the submodel extent are re-applied, and the list of
   re-applied loads is recorded against the global load set to demonstrate
   completeness.
5. Mesh convergence at the region of interest is demonstrated: the governing
   stress changes by less than a stated tolerance between the working mesh and a
   refined mesh. Where the governing standard prescribes the stress-reading mesh
   size, the prescribed mesh is used and the convergence study establishes that
   the prescribed size lies in the converged range or, where it does not, the
   prescribed size governs and the departure is recorded.
6. The submodel reaction resultant at the cut boundary is compared against the
   corresponding internal force resultant in the global model at the same section
   and agrees within a stated tolerance.

---

## 9. Stage 7 — Results extraction and code check

### 9.1 Purpose
The stress and displacement results are reduced to the quantities the governing
standard defines, compared against the allowable values that standard specifies,
and reported as dispositions bound to their criteria.

### 9.2 Inputs
- Global and submodel result fields from Stages 5 and 6.
- The governing standard, identified by title, edition and clause.
- Material allowable values with their basis — yield, ultimate, design factor,
  temperature derating, and any weld or fatigue class.

### 9.3 Outputs
- Result table, one row per checked location per load case.
- Disposition for each row, bound to its criterion and comparator.
- Statement of limitations.

### 9.4 Stress extraction

The stress measure extracted shall be the measure the acceptance criterion is
written against. A criterion written against von Mises equivalent stress is
checked against von Mises; a criterion written against membrane stress is checked
against membrane stress and not against the membrane-plus-bending total; a
criterion written against a hot-spot stress at a defined extrapolation distance is
checked against a stress extrapolated at exactly that distance from exactly the
prescribed mesh. Substituting one stress measure for another invalidates the
comparison even where the substituted value is numerically larger.

Averaged and unaveraged nodal results shall be distinguished. Averaging across a
material change, a thickness change or a geometric discontinuity produces a value
that represents neither side of the discontinuity. Unaveraged results are
recommended at and adjacent to such locations.

Singular locations — re-entrant corners, point loads, restraint points — do not
converge under mesh refinement and shall not be reported as stress results. The
treatment adopted at a singularity shall be stated: either the geometry is
modelled with its actual radius, or the location is excluded and the criterion is
applied at the nearest valid location.

### 9.5 Binding the conclusion to the criterion

Each reported result shall carry, in one row: the location, the load case, the
stress measure, the computed value, the allowable value, the clause of the
standard that sets the allowable, and the resulting utilisation and disposition.

| Location | Load case | Stress measure | Computed (MPa) | Allowable (MPa) | Criterion source | Utilisation (—) | Disposition |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Bracket toe, transverse frame | EDW-01 sagging VBM | von Mises, unaveraged | 268 | 315 | Governing standard, cl. X.Y, 0.9·σ_y | 0.85 | Acceptable against cl. X.Y |
| Plate field, midship bottom | EDW-01 sagging VBM | Membrane, averaged | 141 | 189 | Governing standard, cl. X.Z, 0.6·σ_y | 0.75 | Acceptable against cl. X.Z |
| Weld toe, longitudinal termination | EDW-03 lateral acceleration | Hot-spot, t/2–3t/2 extrapolation | 212 | Fatigue class D, S-N check | Governing standard, fatigue annex | — | Fatigue check required; static criterion not applicable |

Table 14 — Result reporting format. Values shown are illustrative and carry no project meaning.

The disposition "acceptable" shall never appear without the criterion that makes
it so. A bare verdict is not a conclusion.

### 9.6 Failure modes

| Failure mode | Consequence |
| --- | --- |
| Stress measure not matching the criterion's basis | Comparison invalid regardless of the margin |
| Result read at a singularity | Value is mesh-dependent and has no allowable |
| Averaging across a discontinuity | Reported value represents neither adjacent region |
| Utilisation reported without the governing load case identified | The controlling case cannot be traced or rechecked |
| Load case matrix incomplete relative to the DLP set | The governing case may not have been solved |
| Partial safety factors applied twice, or omitted | Margin wrong by the factor |

Table 15 — Stage 7 failure modes.

### 9.7 Acceptance check

1. Every DLP in the Stage 2 set has a corresponding solved load case, or its
   omission is stated with justification.
2. Every reported row carries all eight fields of Table 14.
3. The governing load case is identified for each checked location.
4. Every result read from a location assessed as singular is either excluded or
   supported by a modelled radius, and the treatment is stated.
5. The limitations of the chain are stated explicitly in the report, at minimum:
   the first-order-only pressure basis (§5.6), the linear equivalent design wave
   basis (§4.6), the mass reconciliation basis (§6.5), and the residual imbalance
   actually achieved against Table 9.

---

## 10. Licence and compute

The chain consumes **one AQWA solve seat and one Mechanical seat**. These are
consumed at different stages, but the chain as a whole is bounded by whichever is
scarcer.

The practice holds a **single usable Mechanical seat at the current release**.
The consequences are operational and shall be planned for:

1. **The chain serialises.** Two design waves cannot be solved concurrently. A
   load case matrix of *n* design waves is *n* sequential Static Structural
   solves, because §5.6 permits only one Hydrodynamic Pressure object per Static
   Structural analysis.
2. **Submodels also serialise** against the same seat, and each submodel requires
   the global result to be present. The critical path is therefore
   `AQWA solve -> n x (map + solve) -> m x submodel solve`, fully serial in the
   Mechanical seat.
3. **Seat contention with unrelated work is real.** Any other analysis requiring
   Mechanical blocks this chain and is blocked by it. Runs shall be scheduled, and
   a long global solve shall not be started without the seat being reserved for its
   expected duration.
4. **Schedule estimation shall be based on the serial path**, not on the sum of
   solve times assuming parallelism.
5. Interactive use of Mechanical for model building also holds the seat. Model
   preparation and production solving compete for the same resource, and
   separating them in time is recommended.

Compute sizing is model-dependent and is **not established** in this document. A
sizing basis shall be recorded from the first production run of each model class:
degrees of freedom, wall-clock solve time, peak memory, and scratch disk consumed.

| Resource | Quantity held | Consequence |
| --- | --- | --- |
| AQWA solve seat | 1 | Diffraction solves serialise |
| Mechanical seat | 1 usable at the current release | Mapping, global solves and submodel solves all serialise against one another and against unrelated Mechanical work |

Table 16 — Licence position and its scheduling consequence.

---

## 11. Automation constraint

The governing constraint on automating this workflow is stated directly:

**PyMAPDL cannot drive AQWA, and PyMAPDL cannot drive the Hydrodynamic Pressure
object.**

PyMAPDL is a client to the Mechanical APDL solver. AQWA is a separate solver with
its own input deck and is not addressable through the MAPDL command stream. The
Hydrodynamic Pressure object is not an MAPDL entity at all — it lives in
Mechanical's own object model, is configured through Mechanical's tree, and its
generation depends on Mechanical processing the object state. It is therefore
outside PyMAPDL's reach by construction, not by omission.

The consequence is that **the Stage 3 transfer is the automation boundary of this
chain**. Stages 5, 6 and 7 are MAPDL-side and are scriptable through PyMAPDL once
the load exists; Stages 1 to 3 are not.

Two routes exist for a headless chain, and one of the two shall be selected before
any automation is built:

### Route A — Workbench journaling or PyMechanical
Workbench journaling and PyMechanical drive the Workbench and Mechanical object
models, and therefore can reach the Hydrodynamic Pressure object and the project
schematic update. This route preserves the Workbench chain exactly as defined in
Stages 1 to 3, including the §5.7 update sequence.

Reported behaviour is that updating a Hydrodynamic Pressure object through
scripting depends on the Mechanical application processing the change to set the
object and solution states, and that loops over successive configurations may
update the configured parameter without the load value following. This route is
therefore **viable but not established as robust for unattended batch operation**,
and any implementation shall be validated by comparing scripted results against
interactively produced results before it is relied upon.

### Route B — AqwaWave neutral-file route
AqwaWave maps pressures from the AQWA hydrodynamic model onto a structural model
and writes structural load cases in a file format the structural solver reads.
AqwaWave is driven by a configuration file that defines the structural model
selection, the hydrodynamic model selection, the output format and the load case
selection, and the surrounding steps are file-based and deck-driven. This route
does not involve the Mechanical object model at any point and is therefore
**CLI-drivable end to end**.

Route B is the route to select where unattended batch operation over a large load
case matrix is the requirement. The precise command-line invocation, exit codes
and the full set of configuration-file directives at the current release are
**not established in this document** and shall be confirmed against the release
documentation before the route is built.

| Route | Reaches Hydrodynamic Pressure object | Headless | Status |
| --- | --- | --- | --- |
| PyMAPDL | No — object is outside the MAPDL object model | n/a | Excluded for Stages 1–3 |
| Workbench journaling / PyMechanical | Yes | Yes, with application session | Viable; batch robustness not established |
| AqwaWave neutral file | Not applicable — bypasses the object | Yes | Fully CLI-driven; release-specific details to be confirmed |

Table 17 — Automation routes for the diffraction-to-structure transfer.

A hybrid is admissible and is recommended as the first implementation: Stages 1
to 3 driven by Route A or Route B, and Stages 5 to 7 driven by PyMAPDL against
the resulting load cases.

---

## 12. Validation

This workflow definition is issued without an end-to-end validation. The
comparator that would establish the chain is working correctly is named here so
that the validation can be executed rather than described.

### 12.1 What a valid comparator must do
A comparator shall exercise every stage, and shall provide an independently known
answer at a point downstream of the pressure transfer. A comparator that ends at
the RAO validates Stage 1 only, and Stage 1 is not where this chain fails.

### 12.2 Candidate comparators, ranked

1. **A published benchmark hull with measured or independently computed structural
   response.** A hull for which panel geometry, mass distribution, RAOs and hull
   girder response are published — for example a benchmark container-ship or
   tanker hull used in class-society or ITTC comparative studies — solved through
   the full chain, with the recovered hull girder bending moment compared against
   the published value at the same wave condition. This is the strongest available
   comparator because it tests Stages 1 to 5 against an external answer.
   Recommended as the primary validation.

2. **Analytical closure on a simple floating body.** A rectangular barge or a
   floating cylinder of uniform section, for which the hydrostatic and long-wave
   limits admit a closed-form pressure distribution and hull girder moment. The
   chain is run in the long-wave limit, where the wave pressure reduces to a
   quasi-static head, and the recovered bending moment is compared against the
   analytical value. This is the cheapest comparator and the one that isolates
   unit, sign and phase errors most cleanly. Recommended as the first validation
   step, executed before the benchmark hull.

3. **Internal closure between the hydrodynamic DLP and the structural response.**
   For any model, the section force or moment recovered from the global finite
   element model at the DLP section is compared against the DLP extreme value
   targeted in Stage 2. This is the check already required at Stage 5 acceptance
   item 4. It is a necessary condition and it is internal — it does not establish
   that the hydrodynamic solve itself is correct, but it does establish that
   Stages 2 to 5 are consistent. It shall be performed on every analysis.

4. **Cross-solver comparison of the pressure field.** The AQWA panel pressure at a
   given frequency and heading is compared against an independent boundary-element
   solver on the same panel geometry. This validates Stage 1 only and does not
   address the transfer, but it is inexpensive where a second solver is available.

5. **Submodel closure against a single fine global model.** A model small enough to
   be solved at submodel mesh density throughout is solved both ways: once
   globally at fine density, and once as a coarse global plus submodel. Agreement
   of the local stress within a stated tolerance establishes that the Stage 6
   cut-boundary interpolation and the mesh transition are correct. This validates
   Stage 6 independently of Stages 1 to 4 and is recommended as a separate exercise.

### 12.3 Acceptance for the validation
The chain shall be regarded as validated when comparators 2, 3 and 5 are executed
and pass within stated tolerances, and comparator 1 is executed with the recovered
hull girder response agreeing with the published value within a tolerance stated
in advance. Until that is done, results from this chain shall be reported as
carrying an unvalidated toolchain, and the conclusion shall be supported by an
independent check at Stage 5 acceptance item 4.

---

## 12A. Restrictions verified after first drafting

The five restrictions below were confirmed against Ansys documentation after this
document was first written. Each constrains the chain materially, and each is
absent from the stage sections above.

### 12A.1 Moonpool pressures exist only on the frequency-domain route

Ansys states verbatim that moonpool pressures cannot be included in or mapped
from Hydrodynamic Response systems. From a Diffraction system they are
available, and they always use direct mapping irrespective of the object's
mapping setting.

For any structure carrying a moonpool, turret well or internal free surface, the
frequency-domain design-wave route is therefore the **only** supported path to
moonpool wall pressure. Where the governing case requires time-domain effects,
the moonpool load shall be obtained separately and the superposition stated as
an assumption, or the omission declared. No single-run answer exists.

### 12A.2 The time-domain route forces Interpolated mapping

For a time-domain Hydrodynamic Response source the pressure mapping option is
set automatically to Interpolated; Direct is unavailable. The time-domain route
is therefore both less complete (12A.1) and less accurate in mapping than the
frequency-domain route. Selection between the two shall record which of the two
losses the analysis accepts.

### 12A.3 Diffraction pressures act on the mean wetted surface only

Pressures from a Hydrodynamic Diffraction analysis are computed under the mean
wetted surface. The wave surface shape is not reproduced in the pressure
distribution, and the field scales strictly linearly with wave amplitude. For an
extreme design wave this is the dominant modelling error of the whole chain —
larger than any mapping or mesh effect — because the wetted surface above the
mean waterline is not represented at all.

Every report from this chain shall state that the pressure field is linear and
defined on the mean wetted surface, and shall say what treatment, if any, was
applied above the waterline. Section 5 already requires the treatment to be
stated; this adds that the limitation shall be stated whether or not a treatment
was applied.

A related reporting artifact: the *Structure Interpolated Pressure* contour shows
interpolation artifacts near the free surface. *Structure Panel Pressure* shows
the computed per-panel values and is the quantity to inspect when judging whether
a solve is sound.

### 12A.4 A shell-to-solid cut-boundary file requires two read operations

The file written for shell-to-solid submodeling contains two command blocks — the
nodal rotations and the displacement values — separated by an end-of-file marker
and a block label. Reading it once applies rotations without displacements, or
the reverse, and the submodel then solves without error against a wrong boundary
condition. **Two separate read operations are required.** This is a silent
failure: nothing in the solve reports it.

Related constraint: shell-to-solid submodeling is not supported where the coarse
shell model uses section offsets. A coarse model built with offset sections must
be re-modelled to the midplane before this route is available.

### 12A.5 Automated wave-case sweeps are not established

The Mechanical pressure-import object is scriptable, but behaviour reported by
practitioners is that a script loop updates the load correctly on the first
iteration and fails to update it on subsequent iterations, with a change
notification call offered as a partial workaround. No documented, supported API
for the object was located.

**Status: not established.** A sweep over many wave cases shall not be assumed
automatable. Until a sweep is demonstrated on the release in use, with evidence
that the mapped load actually changed between iterations, wave cases shall be
treated as separate runs. Verification that the load changed shall be by the
mapped pressure resultant, not by the absence of an error.

Under Workbench, mapping validation is additionally unavailable when the transfer
is shell-to-solid — the automated quality check is absent in precisely the case
where mapping is most delicate. This is an argument for executing shell-to-solid
work through the command-language route, where the interpolation tolerances are
explicit and the transferred constraint set can be inspected.

---

## 13. Register of claims and their verification status

| Claim | Status | Basis |
| --- | --- | --- |
| Direct method evaluates pressure at each selected structural node from diffracting panel source strengths; permits solid outer surfaces and multi-structure; cannot map onto line bodies | Verified | Ansys help, Importing Hydrodynamic Diffraction Pressures and Loads to Mechanical |
| Interpolated method interpolates from element-centred hydrodynamic pressures; permits line bodies; cannot map onto solid outer surfaces or multi-structure | Verified | As above |
| Second-order pressure terms on diffracting panels are not calculated; line body loads do include a second-order viscous drag component | Verified, quoted | As above |
| Only one Hydrodynamic Pressure object may be added to a Static Structural analysis | Verified, quoted | As above |
| Solving the diffraction analysis from inside the AQWA editor only does not transfer output files to the Static Structural system; the Solution cell shall be updated from the project schematic with Mechanical closed | Verified | Ansys Learning Forum guidance on Aqwa-to-Mechanical pressure mapping |
| Residual unbalanced force limits of 1 percent of displacement in head seas and 2 percent in oblique and beam seas; residual may be balanced by distributed inertial forces | Verified | Class-society Dynamic Loading Approach guidance notes |
| Equivalent design wave amplitude equals the DLP extreme divided by the peak DLP RAO, at the frequency and heading of that peak | Verified | Class-society design wave guidance and published EDW literature |
| Accompanying pressure is the field acting simultaneously with the DLP peak, not the pressure field's own maximum | Consistent with the DLA accompanying-load concept; exact wording not quoted from a single source | Inferred from DLA method description |
| Cut-boundary interpolation transfers global degree-of-freedom values onto submodel boundary nodes; nodes outside the global mesh are extrapolated, which can induce error | Verified | Ansys help, Using Submodeling |
| Shell-to-solid submodel cut-boundary nodes are projected onto the nearest element in the shell plane and interpolated there | Verified | Ansys help, Shell-to-Solid Submodels |
| Submodeling rests on Saint-Venant's principle, and validity is established by comparing cut-boundary results between global and submodel | Verified as principle; the 5 percent band is a working practice figure, not a documented Ansys criterion | Ansys help and practitioner guidance |
| PyMAPDL cannot drive AQWA | Not established as a documented statement; asserted from the architecture — PyMAPDL is a client to the MAPDL solver and AQWA is a separate solver | Reasoned |
| PyMAPDL cannot drive the Hydrodynamic Pressure object because that object lives in Mechanical's object model | Not established as a documented statement; supported by reported behaviour that updating the object through scripting requires Mechanical to process the object and solution states | Reasoned, with forum-reported behaviour |
| AqwaWave is driven by a configuration file and the route is file-based end to end | Verified in outline; the full current-release command-line interface is not established | AQWA documentation and applied-usage descriptions |
| Panel edge length not exceeding one seventh of the shortest wavelength | Working rule of thumb, not a documented Ansys requirement; the documented control is the defining maximum element size derived from the highest requested frequency | Practitioner practice |
| Mesh transition size ratio not exceeding 2, minimum three transition layers, cut boundary at two to three times the detail dimension | Working practice figures, not documented criteria | Practitioner practice |
| Compute sizing for this chain | Not established | To be recorded from first production runs |
| End-to-end validation of this chain | Not established | See §12 |

Table 18 — Verification status of the claims made in this document.

---

## 14. References

Source documents are cited by title rather than by local path. The applicable
edition shall be confirmed at the time of use.

- Ansys AQWA User's Manual, current release — Hydrodynamic Diffraction, results,
  and the Mechanical extensions.
- Ansys AQWA Theory Manual, current release — diffraction-radiation formulation
  and pressure recovery.
- Ansys Help, *Importing Hydrodynamic Diffraction Pressures and Loads to
  Mechanical* — mapping methods, restrictions, second-order limitation, one-object
  limit.
- Ansys Help, *Importing Time Domain Hydrodynamic Response Pressures and Loads to
  Mechanical* — the time-domain counterpart to Stage 3, outside the scope of this
  workflow.
- Ansys Mechanical User's Guide, *Submodeling* and *Shell-to-Solid Submodels*.
- Ansys Mechanical APDL Advanced Analysis Guide, *Using Submodeling*.
- Class-society Guidance Notes on the Dynamic Loading Approach, applicable to the
  structure type — dominant load parameters, equivalent design wave construction,
  load balance and residual unbalanced force criteria.
- Class-society Guidance Notes on selecting the design wave by long-term
  stochastic method — extreme value basis, phase angle and crest position.
- The governing structural design standard for the analysis, identified by title,
  edition and clause in the analysis report.
