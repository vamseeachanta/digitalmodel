# Pint Physical Units Library — Evaluation

**Issue:** vamseeachanta/workspace-hub#1459
**Date:** 2026-03-29
**Version tested:** 0.24.4

## Overview

[Pint](https://github.com/hgrecco/pint) is a BSD-3-Clause Python library for physical unit handling. It provides a `Quantity` type that wraps numeric values (including NumPy arrays) with unit metadata, enabling automatic conversion, dimensional analysis, and unit validation.

## Installation

```bash
uv pip install pint              # or: pip install pint
uv pip install "pint[numpy]"     # for NumPy integration (usually auto-detected)
```

## Key API Patterns

```python
import pint

ureg = pint.UnitRegistry()       # one per application
Q_ = ureg.Quantity               # shorthand

# Create quantities
depth = Q_(100, "meter")
force = Q_(500, "kN")

# Convert
depth.to("feet")                 # → 328.08 foot
depth.to("inch").magnitude       # → 3937.0 (raw number)

# Arithmetic — units propagate
P = Q_(1025, "kg/m**3") * Q_(9.81, "m/s**2") * depth  # → Pa

# Validation — incompatible units raise DimensionalityError
depth + force                    # raises pint.DimensionalityError

# String parsing (config-driven)
ureg.parse_expression("25.4 mm") # → Quantity(25.4, 'millimeter')
```

## NumPy Integration

Pint wraps NumPy arrays seamlessly:

```python
import numpy as np
depths = Q_(np.array([10, 50, 100]), "m")
pressures = rho * g * depths     # element-wise, returns Quantity array
pressures.magnitude              # raw numpy array
```

**Performance note:** Operations on `Quantity` arrays add ~2-5x overhead vs raw NumPy for simple operations. For tight inner loops, extract `.magnitude`, compute, then re-wrap.

## Pandas Integration

Pint provides a pandas extension type via `pint-pandas` (separate package):

```bash
pip install pint-pandas
```

```python
import pandas as pd
import pint_pandas
df = pd.DataFrame({"depth": pd.array([10, 50, 100], dtype="pint[m]")})
```

**Status:** `pint-pandas` works but has rough edges — not recommended for production use yet.

## Gotchas & Limitations

1. **Single UnitRegistry:** All quantities must share the same `UnitRegistry` instance. Mixing registries raises errors. Create one at module level and import it.

2. **Performance overhead:** Unit tracking adds overhead. For large arrays in tight loops, extract `.magnitude`, compute in raw NumPy, then re-wrap with units.

3. **Serialization:** `Quantity` objects are not JSON-serializable by default. Use `str(q)` or `(q.magnitude, str(q.units))` for persistence.

4. **String representations:** Unit names can be verbose (`kilogram / meter ** 3`). Use format specs like `f"{q:.2f~P}"` for compact output.

5. **Temperature conversions:** Offset units (°C, °F) require `Quantity.to()`, not multiplication. Pint handles this correctly but the mental model differs from ratio units.

6. **Thread safety:** `UnitRegistry` is not thread-safe for modifications. Safe for read-only use across threads.

## Offshore Engineering Relevance

Pint is well-suited for digitalmodel because:

- **Hydrostatic calculations:** ρ·g·h with automatic unit propagation
- **Code-as-documentation:** `Q_(1025, "kg/m**3")` is self-documenting
- **Config-driven inputs:** Parse units from YAML/JSON configs
- **Error prevention:** Catches unit mismatches at runtime before they become wrong results
- **Standards compliance:** API codes mix SI and imperial — Pint handles conversion

## Recommendation

**Adopt for new calculation modules.** Pint is mature (v0.24.4), well-maintained, BSD-3 licensed, and directly addresses a recurring source of engineering errors. Start by using it in new modules; retrofit existing code opportunistically.

Suggested integration pattern:
```python
# src/digitalmodel/units.py
import pint
ureg = pint.UnitRegistry()
Q_ = ureg.Quantity
```

Import `ureg` and `Q_` from this single module throughout the codebase.

## See Also

- [PoC script](../../scripts/integrations/pint_poc.py) — runnable demonstrations
- [Pint docs](https://pint.readthedocs.io/)
- [GitHub](https://github.com/hgrecco/pint) — 2700+ stars, active development
