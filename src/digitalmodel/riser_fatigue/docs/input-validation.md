# Riser fatigue input evidence

Both the touchdown API and the combined router raise `ValueError` before
returning an engineering verdict when loading or controls are incomplete,
non-finite or physically invalid. Existing numerical formulas are retained.

## Required evidence

The direct `TouchdownFatigueInput` requires explicit `sn_class`, `environment`,
`sn_basis`, `scf`, `design_life_years`, `dff`, `histogram_period_years` and
`histogram_basis`. Unity SCF also requires `scf_basis`, explaining why stress
already includes concentration or why unity applies. `histogram_basis` is a
mapping containing finite positive `period_years` and nonempty `evidence`.
Its duration must equal `histogram_period_years`; evidence identifies the
observation or synthesis and confirms its cycle-count basis. For example:

```yaml
histogram_period_years: 1.0
histogram_basis:
  period_years: 1.0
  evidence: Synthetic annual cycle counts
```

These fields record evidence for engineering review; the
calculation cannot independently verify the upstream observation or protection.

Histograms must be nonempty, one-dimensional, finite and non-negative, with at
least one bin containing positive stress and positive cycles. Known empty bins
may accompany loaded bins. An entirely unloaded histogram is rejected: this API
has no qualified no-load assessment mode. Unknown bins are never discarded.
Calculated damage must remain finite and positive before a verdict is produced.

Geometry must be finite and positive, with outer diameter exceeding twice the
wall thickness. Diameter is informational for this stress-based calculation;
wall thickness affects the thickness correction. An explicit exponent requires
`thickness_exponent_basis` and cannot reduce the selected class penalty. A
reference-thickness override cannot weaken the selected correction method.

## Combined workflow

The root `sn_curve` is a mapping containing `curve`, `environment` and `basis`.
Segments inherit this validated selection. A segment override must provide the
complete mapping, including its basis. A scalar, partial mapping or missing
root selection is rejected; protection is never inferred.

Each wave block supplies explicit `scf`, `histogram_period_years` and
`histogram_basis`, plus `scf_basis` for unity. Optional thickness settings use the
same names and guards as the direct API. Each VIV case supplies exactly one
stress alias, one frequency alias and one exposure representation. Duplicate
aliases are rejected even if their values agree. Stress, frequency, exposure
and the derived design-life cycles must be finite and positive.

Both wave and VIV loading are required unless the absent contribution has an
explicit exclusion. For example, a wave-only analytical case can omit
`viv_cases` and supply:

```yaml
viv_exclusion:
  reason: VIV is outside the supplied stress-history study
  evidence: Referenced applicability assessment for the evaluated segment
```

Use `wave_exclusion` in the same form to omit `wave`. An exclusion cannot coexist
with its data key, including null or empty data. Both contributions cannot be
excluded. The segment summary and summary CSV retain the exclusion reason,
evidence and `not_applicable` status. `evaluated` means the contribution passed
input validation and produced positive damage.

Packaged life, DFF and S-N settings are unset. A configured case must supply
its engineering controls; a null DFF is invalid. The standalone router retains
cited DFF resolution only when the DFF key is absent, with failure if its source
cannot be resolved. Results identify whether DFF was explicit or cited.

## Verdict and limits

The direct PASS criterion is design-life damage multiplied by DFF no greater
than one. The router PASS criterion is fatigue life at least the finite required
life (design life multiplied by DFF), for every segment. Missing evidence never
produces a verdict. Unqualified zero damage never produces infinite life.

Evidence adequacy, exclusion applicability and the upstream stress basis need
engineering review. VIV stress ranges retain their existing interpretation as
supplied assessment ranges; the wave SCF and thickness adapter is not applied
to VIV ranges. No solver or independent physical validation is provided here.
