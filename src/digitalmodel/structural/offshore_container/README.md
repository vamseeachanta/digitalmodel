# Offshore-container structural calculations

The public package exports ideal SHS section properties, centred sling geometry,
caller-factor allowable stress and reduced member utilization calculations.

See the [module analysis](../../../../docs/reports/modules/offshore_container/index.html)
for the reviewed API inventory, input and unit basis, independent hand arithmetic
and open method findings. The [rebuild instructions](../../../../docs/reports/modules/offshore_container/README.md)
describe the generated evidence and exact-byte checks.

The [section and sling derivation](docs/worked-shs-sling-geometry.md) establishes
geometry only. Report factors are synthetic arithmetic inputs. Strength, padeye
resistance, actual load sharing and certification remain `NOT_EVALUATED`.

## Plan-lite

Scope is one module analysis report for a single synthetic example. Independent
checks use section integrals, the centred sling triangle and explicit
caller-factor arithmetic. Deterministic rebuild checks compare two external
deliveries byte for byte and independently read back source and artifact digests.
Calculation changes, physical design qualification, standard-factor selection,
parameter-range coverage, licensed solver runs and certification are excluded.
