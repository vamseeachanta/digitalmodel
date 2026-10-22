# Native FEA tensor extraction audit

The offline audit preserves each coherent element-node tensor, checks an
independent sample and path inventory, retains raw interior peaks, and gates
engineering criteria on force **and** moment balance. It extends the scalar
parser checks discussed in issue #2134. The supported native adapter is the
retained PLANE183 listing profile; this does not qualify other adapters.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Add an offline tensor and plot evidence audit to expose missing samples and peaks concealed by face reconstruction. |

## Slices and adapter coverage

| Slice | Interface | Evidence and limits |
| --- | --- | --- |
| 1: tensor identity | `parse_presol_plane183`, `TensorSample`, `ResultSet` | The retained v261 PLANE183 profile visits every listed corner contribution and keeps SX, SY, SZ, SXY, SYZ and SXZ together. Units are required inputs. Every page must match the existing strict native step/substep/time/frame profile. Other native element types and shell layouts remain unsupported. |
| 2: coverage and balance | `audit_tensors`, `PathSpec`, `Equilibrium` | Exact element/node/surface/layer coverage against a separately supplied inventory; every expected sample must occur on a supplied path. Missing, duplicate, unexpected, repeated-position and truncated radial inventories refuse. Forces and moments are summed independently. |
| 3: peaks and evidence | Cartesian component decomposition, `validate_plot_evidence` | Raw signed traces and equivalent stress maxima remain separate from reconstructed faces. Plot metadata is bound to retained result/export bytes and reconciled to the raw tensor maximum. Annotation overlap and contour pixel equality remain `NOT_EVALUATED`. |

The existing `ResultsExtractor.parse_prnsol_block` remains a scalar interface:
its nodal values do not establish unaveraged tensor or shell-surface identity.
The existing cylinder contribution checker validates selected stations and
recovers selected components for readback comparisons. Its guarantees are not
expanded by this change. The new adapter retains all six components at all
listed element-node pairs instead of discarding unused shears.

The retained fixture contains 96 elements with four corner rows each: 384
element-node contributions. This is listed-corner coverage, not integration-point
or midside-node coverage. Supplying those additional expected keys causes a
coverage failure. The fixture's mesh is axisymmetric; it supplies parser evidence
only, and is **not** the Cartesian linearization worked example below.

## Evidence contract

Import the interfaces from `digitalmodel.ansys.native_tensor_audit`. Build
`expected` from the independently retained mesh connectivity, requested shell
layers/surfaces and sampling plan. Build each `PathSpec` from that plan, including
both endpoints, every interior radial sample, positions and an explicit method.
Never construct an expected inventory by collecting keys from parsed results.

Each row carries one result set, one element, one node, one surface, one layer,
one recovery mode and all six stress components in the same frame and units.
Arithmetic is isolated from the caller's Decimal precision and rounding mode.
Explicit decimal and E/D exponents are accepted; implicit exponents, nonfinite
tokens and overflow markers refuse. Invalid constructor inputs raise
`EvidenceError`; coverage and identity failures return `raw_status=INCOMPLETE`.

Normalized shell samples require `TOP`, `BOTTOM` or `MID`, a positive layer,
and `surface_basis="explicit"` with the adapter's field reference. Row ordering
alone does not establish identity. References are evidence declarations, not
authenticated native captures. `surface_basis="verified-format"` refuses:
no native shell parser or verified shell witness is supplied here, and a reference
string alone cannot establish format compatibility. Native shell coverage is
`NOT_EVALUATED` until an adapter and fixtures establish it.

`parse_presol_plane183(raw, stress_unit="MPa", length_unit="mm")` uses the
existing strict v261 envelope parser. Its supported result identity is step 1,
substep 1, time 1 in the global component frame, with four corner rows per
element. These are observed format restrictions, not analysis defaults.
Other result sets, element types or grammars refuse. MPa and mm in this call
are caller declarations: a native text listing does not establish units.

Load and reaction inputs to `Equilibrium` use N, mm and N mm in global Cartesian
coordinates and include an explicit moment origin and cited absolute tolerances.
The caller must independently establish the complete applied-load and support
inventories, common result identity and unit consistency. Missing or incompatible
balance evidence blocks all engineering criteria. A zero residual calculated from
an incomplete supplied force list cannot prove native model equilibrium.

`raw_status=COMPLETE` means the supplied records satisfy this contract.
`NON_PASS` means an evaluated, cited engineering criterion or equilibrium check
exceeded its supplied limit. Missing limits or prerequisite evidence yield
`NOT_EVALUATED`. No design-code allowable, edition or classification is inferred;
`code_acceptance` always remains `NOT_EVALUATED` in this implementation.

## Equations and sources

All geometric and acceptance values are required inputs. The coefficients below
are mathematical definitions or integration results, not empirical constants.

1. [Mechanical APDL Command Reference, 2024 R2, PRESOL Notes and Table 221](https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_cmd/Hlp_C_PRESOL.html):
   element-node component identity, coordinate-system dependence, shell surface
   ordering and PowerGraphics surface-only limitations. This is the format basis;
   the retained fixture establishes compatibility with the narrow v261 profile.
2. [Mechanical APDL Theory Reference, 2024 R2, section 17.4.1, equations 17-16 through 17-19](https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_thry/thy_post4.html):
   Cartesian component membrane and bending decomposition. For thickness
   `t=b-a`, center `c=(a+b)/2`, membrane `M=integral(s dx)/t` and outer bending
   `B=6 integral(s (x-c) dx)/t²`; faces are `M-B` and `M+B`.
   Integration is exact for the **piecewise-linear supplied samples**, rather
   than claiming an exact underlying FE field. On a segment with endpoints
   `u,v` relative to `c`, stresses `p,q`, and `h=v-u`, direct integration of
   `p+(q-p)(x-u)/h` gives area `h(p+q)/2` and first moment
   `h[(2u+v)p+(u+2v)q]/6`. All six components are integrated separately.
   The bending factor follows from `integral(x² dx)=t³/12` on the centered
   interval and evaluation at `x=t/2`. Axisymmetric, curved and code-specific
   alternatives remain `NOT_EVALUATED` when the requested method is not
   `CARTESIAN`; raw peaks remain available.
3. [ContinuumMechanics.org, Von Mises Stress, component-form derivation](https://www.continuummechanics.org/vonmisesstress.html)
   (undated; accessed 2026-10-22):
   `VM²=[(SX-SY)²+(SY-SZ)²+(SZ-SX)²]/2+3(SXY²+SYZ²+SXZ²)`.
   The signed trace is `SX+SY+SZ`; its absolute maximum is computed across
   **all** samples, with every tied governing key retained. Hydrostatic stress
   can therefore have zero VM and a nonzero trace demand.
4. [OpenStax, University Physics Volume 1 (2016), section 12.1, equations 12.2 and 12.5](https://openstax.org/books/university-physics-volume-1/pages/12-1-conditions-for-static-equilibrium):
   static force and torque equilibrium. Moments are accumulated as
   `(r-origin) cross F + couple`. For `r=(x,y,z)` and `F=(Fx,Fy,Fz)`, the cross
   product is `(yFz-zFy, zFx-xFz, xFy-yFx)`. Each residual component is compared
   to the supplied force or moment tolerance.
5. [Mechanical APDL Command Reference, 2024 R2, PLESOL Notes](https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_cmd/Hlp_C_PLESOL.html)
   and [/DSCALE argument descriptions](https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_cmd/Hlp_C_DSCALE_sl.html):
   `PLESOL,S,EQV` supplies unaveraged element contours. `/DSCALE,1,OFF` removes
   deformation; `0` or `AUTO` selects automatic magnification, while `1` uses
   true displacement scale. Neither alternative establishes an undeformed plot.

All linked pages were retrieved successfully on 2026-10-22. No licensed standard
values or code acceptance multipliers are reproduced.

## Worked example: an interior peak missed by reconstructed faces

Run `PYTHONPATH=src python examples/ansys/native_tensor_audit.py` in an environment
with the repository's Python dependencies. The example imports no solver.

Constructed inputs are a straight Cartesian path at positions 0.0, 1.0 and
2.0 mm, normal stresses `(0,0,0)`, `(20,20,20)` and `(0,0,0)` MPa, and zero
shears. The independently supplied inventory contains the three sample keys.
The synthetic trace limit is 40.0 MPa, deliberately chosen between the face
and raw demands; it is **not a design-code allowable**.

Each normal component has two triangular areas of 10.0 MPa mm, giving membrane
`20.0/2.0=10.0 MPa`. The first moment vanishes by symmetry, so each reconstructed
face has trace `3×10.0=30.0 MPa`. The actual interior trace is
`3×20.0=60.0 MPa`; VM is 0.0 MPa at every sample. The applied force of 10.0 N
and reaction of -10.0 N act at the same position 2.0 mm from the moment origin,
giving force residual 0.0 N and moment residual `20.0-20.0=0.0 N mm`.

Expected output:

```json
{
  "raw_evidence": "COMPLETE",
  "raw_abs_trace_mpa": "60.0",
  "linearized_face_abs_trace_mpa": "30.0",
  "raw_von_mises_mpa": "0.0",
  "raw_trace_criterion": "NON_PASS",
  "face_trace_criterion": "PASS",
  "design_code_acceptance": "NOT_EVALUATED"
}
```

This result demonstrates why an interior raw failure must remain visible even
when a separate linearized criterion passes. It does not make an engineering
design-code assessment.

## Native screenshot evidence

`validate_plot_evidence` requires retained result bytes before and after export,
the export bytes, recorded SHA-256 digests, selected result set, pressure, native
plot command, camera, surface, averaging, displacement command, legend endpoints
and native raw equivalent-stress maximum. Supply the same source-result digest
to `audit_tensors(source_result_sha256=...)` after independently binding the
listing to that result file. Its raw tensor selection must match the plotted
surface. The native maximum must reconcile within a supplied cited tolerance;
the legend must contain that maximum. Optional `expected_legend` requires an
identical scale for comparisons. No image bytes are edited or decoded.

These checks establish byte and metadata consistency only. Native command
execution, camera correctness, unchanged contour-region pixels and absence of
title/legend overlap require retained native evidence and independent inspection.
Pixel equality and annotation overlap are explicitly `NOT_EVALUATED` here.

## Verification and remaining adapters

The tests use written hand calculations for triangular hydrostatic profiles,
nonuniform linear profiles, pure shear and force-balanced moment imbalance;
literal byte/digest comparisons test provenance. Adversarial fixtures exercise
coverage gaps, ambiguous exponents, mixed identities, averaged rows, missing
shell surface evidence and misleading displacement commands. Expected values
are not generated by the functions under test.

Further native adapters require their own retained fixtures, independently
enumerated element/node/layer/surface/path inventories and edition-specific
engineering assessment sources. In particular, scalar PRNSOL, native shell
layouts, other native element types, full midside/integration-point coverage,
axisymmetric linearization and native pixel/layout inspection remain unqualified.
