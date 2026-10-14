# Pointing the #1173 calm-water resistance case at a client hull

Scoping only. Nothing here is built, and nothing here should be started before
the KCS validation result exists — the entire value of this capability is that
a client number arrives attached to a validated case, and the validation is the
asset, not the mesh.

Written at the end of #1173's Stage 4, against the case as actually built.

---

## What carries over unchanged

These are hull-independent and cost nothing to re-point.

| Component | Why it carries |
|---|---|
| `runner.py` execution model — ranks, detach, declared budgets, the poller | Nothing in it refers to a hull. The 15-stage pipeline is the interFoam/snappyHexMesh workflow, not a KCS workflow. |
| `parse_hull_force()` and the pressure/viscous split | Reads the forces function object's own columns. The half-domain doubling is a property of the domain, not the body. |
| The gate *machinery* — `v1_*`, `v2a_*`, `v2b_*`, `v3_*` | Each takes a referent and a measurement. Swapping the referent is a fixture edit. |
| The frozen `@TOKEN@` templates and the declared-deviation diff test | The template set is the DTCHull tutorial's; a client hull is another set of token values. The diff test keeps a new hull honest in exactly the way it kept KCS honest. |
| Domain construction by scaling the tutorial domain to the hull's length | Already parameterised on `hull_scale`. A client hull of different proportions gets the same treatment, subject to the caveat below. |
| The watertight-surface checks — closure, orientation, single-face edge count | Generic mesh-topology properties. |
| The `restore0Dir`, `redistributePar`, `renumberMesh` staging | Required by any parallel interFoam case. |

The cost model carries too, now that it rests on a **measured** 2.392 µs per
cell-iteration at 8 ranks rather than an assumption. A client hull at the same
mesh density and iteration budget costs the same ~25 h per level.

## What does not carry, and what it would take

### 1. Geometry ingestion — the real work

KCS was solved by a route that will **not** generalise: the Tokyo 2005 workshop
publishes a structured surface grid, which is a tessellation already. A client
will supply IGES, STEP, or a hull-form definition, and this repository has no
CAD kernel.

`kcs_geometry.py` splits cleanly along this line. The *reader*
(`read_tecplot_zones`) is KCS-specific. Everything downstream —
`_triangulate_structured`, `_ruled_cap`, `orient_consistently`,
`check_surface`, `wetted_surface_area`, `enclosed_volume`,
`transform_surface`, `write_stl` — operates on triangles and structured
patches and is hull-agnostic. A new front end feeds the same pipeline.

Three ingestion routes, ranked:

1. **Offsets table → lofted surface.** Most likely for a client with a
   traditional hull definition. Needs station interpolation and longitudinal
   lofting, which is the one genuinely new piece of geometry code. Moderate
   effort; fully in-repo; no new dependency. This is also the route that would
   let the existing `hull_form.py` analytic work be reused.
2. **IGES/STEP via `gmsh`.** `gmsh` is pip-installable and bundles
   OpenCASCADE, so IGES → tessellation → STL is available without commercial
   CAD. Low effort to wire; the risk is not the import but the *quality* of the
   tessellation, which must still pass the closure and orientation checks
   already written. Note #1161 Phase 3 already owns "a general IGES/STEP import
   capability" — this should go there, not here.
3. **Client-supplied STL.** Cheapest, and the most likely to arrive
   non-watertight. `check_surface()` already reports open-edge and
   non-manifold counts explicitly, so triage is immediate; repair is not
   currently implemented and can be unbounded work on a bad asset. The Wigley
   finding in this issue's own plan — 392 single-face edges behind a one-line
   "not closed" verdict — is the cautionary case.

### 2. The referent — and the part that cannot be bought

**A client hull has no published Ct.** This is the load-bearing difference, and
it is not a technical gap.

Everything #1173 built is a *validation* apparatus: V1 scores a computed
coefficient against a measured one, V2a/V2b against a decomposition of it, V3
against a second mesh level. For a client hull the first three have no
reference to score against. What remains is:

- **V3 alone still works.** Two-level self-consistency needs no experiment. It
  is the only one of the four criteria that transfers unchanged, and it should
  be run.
- **V2b partially transfers.** The ITTC-57 line is a correlation, not a
  measurement, so the computed viscous coefficient can still be scored against
  it. That catches wall-function and roughness errors — the class V2b was
  written for — without any client data.
- **V1 and V2a become predictions, not validations.** They report a number; they
  do not gate it.

The honest framing for a client deliverable is therefore: *the method is
validated on KCS to within X%, and the same method applied to your hull
predicts Ct = Y, with self-consistency demonstrated across two mesh levels.*
The KCS result is what licenses the client number. Presenting a client Ct with
a tolerance band derived from KCS is defensible **only if** the client hull is
in the same regime — similar block coefficient, Froude number, and appendage
state. A bulk carrier at Fr 0.15 is not covered by a container ship validated
at Fr 0.26, and saying so is part of the deliverable.

### 3. Condition and configuration

- **Froude number.** #1173 deliberately solves one. A client will want a
  speed–power curve, which is N solves at ~25 h each. This is the single
  largest cost driver and it is where the deferred multi-Froude follow-on
  becomes relevant.
- **Attitude.** #1173 is fixed even keel, because that is what the referent is.
  A client hull at realistic conditions is free to sink and trim, which means
  `sixDoFRigidBodyMotion` and a moving mesh — a materially different case, and
  a real increase in both setup and solve cost. The repo has the machinery
  (the floating-body and wave-excited cases use it) but it is not wired into
  this case.
- **Appendages.** Rudder, bilge keels, propeller. Each changes the wetted
  surface — and, per this issue's own referent work, *the normalising area must
  match the force*, or the coefficient is meaningless. The bookkeeping hazard
  documented for KCS applies directly.
- **Full-scale extrapolation.** #1173 stops at the model-scale coefficient. A
  client almost certainly wants delivered power, which requires ITTC-78
  extrapolation with a form factor and correlation allowance. Explicitly out of
  scope here; it is a well-defined addition and is mostly arithmetic, but it
  needs its own references and its own gate.

### 4. Domain proportions — a caveat worth checking early

The domain is the DTCHull domain scaled by hull length. A test asserts the
placed hull fits inside every refinement box, and it passes for KCS. A client
hull with a materially different beam-to-length or draft-to-length ratio could
fail that check — a very full tanker form, for instance. The failure is loud
and arithmetic, before meshing, which is the correct place for it. But the
remedy is to parameterise the boxes on hull *proportions* rather than on
length alone, and that work has not been done.

---

## Recommended sequencing, if the owner wants this

1. Finish #1173 to a KCS result. Without it there is nothing licensing a client
   number.
2. Pick the ingestion route from the client's actual file format. Do not build
   all three.
3. Re-point with **V3 and V2b live, V1 and V2a reporting only**, and say so
   explicitly in the deliverable.
4. Treat the speed–power curve as the cost driver it is and scope the Froude
   points deliberately.

The thing worth protecting: #1173's value to a client engagement is the
*validated method*, and the discipline that produced it — the condition tuple,
the declared deviations, the decomposition gate. A client case that quietly
drops that discipline inherits the credibility without the substance.
