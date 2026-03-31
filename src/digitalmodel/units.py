"""Shared Pint UnitRegistry for digitalmodel.

Provides a single UnitRegistry instance so all modules use compatible units.
See: https://github.com/vamseeachanta/workspace-hub/issues/1484
"""

import pint

ureg = pint.UnitRegistry()
Q_ = ureg.Quantity

# --- Offshore / structural engineering custom units ---
# ksi is built into pint (1000 psi). Only define missing/overridden units.
ureg.define("pcf = pound / foot**3")     # pounds per cubic foot
ureg.define("ppg = pound / gallon")      # pounds per gallon (drilling mud weight)
ureg.define("bbl = 42 * gallon")         # petroleum barrel (overrides pint's wine barrel)
