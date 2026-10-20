# FFS inspection campaign

One synthetic inspection campaign composes the existing FFS coordinator,
measurement sufficiency, qualitative RBI, composite repair selector and
inspection planning. Inputs are inches, psi, years and degrees Fahrenheit;
RBI converts corrosion rate to millimetres per year internally.

```python
from digitalmodel.workflow_api import run_workflow

result = run_workflow("ffs-campaign-demo", verify_reproducible=True)
assert result.status == "ok"
campaign = result.result["value"]
report = campaign["ffs_report"]
```

The default run is offline and writes temporary outputs outside the example.
For direct Python use, `assess_campaign()` returns the same payload. The
engine router can write `ffs-campaign.html` to an explicitly supplied
`ffs_campaign.output_dir`. No output directory is inferred by that router.

The [input](input.yml) contains all RBI and repair practice parameters and
three components: uniform loss, repair-required loss and an under-sampled
local thin area. The uniform component interval is 8 years; the unrepaired
component interval is zero; the local-loss interval is withheld pending more
measurements. Repair review timing never authorizes unrepaired operation.

The [committed report](report.html),
[method and independent hand calculations](../../../docs/domains/asset-integrity/campaign-screening-validation.html)
and [readiness matrix](../../../docs/api/ffs/ffs-readiness.html) state the
qualification gaps. API RP 580 published-case validation and ISO 24817 / PCC-2
category qualification remain `NOT_EVALUATED`. Internal A-E repair labels
are software selector labels, not categories claimed from either standard.
All example values are synthetic inputs, not measured data or code limits.
