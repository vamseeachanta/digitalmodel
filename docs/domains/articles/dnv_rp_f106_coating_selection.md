# DNV-RP-F106 Coating Selection

`digitalmodel.cathodic_protection.dnv_rp_f106` provides a bounded helper layer
for factory-applied external pipeline coatings. It is intended to sit upstream
of CP sizing: F106 selects and verifies the coating system, while B401/F103 use
coating breakdown factors in current-demand calculations.

The module covers the six encoded F106 2003 data-sheet families: FBE, 3LPE,
3LPP, asphalt enamel, coal-tar enamel, and polychloroprene. Fixed
minimum-thickness checks are encoded where the local F106 data sheets provide
them. Coatings whose data sheets defer thickness or holiday detection to the
project ITP require project-specific input instead of silently inventing a
standard value.

Temperature selection is intentionally conservative: service at or above
110 deg C selects 3LPP, mechanically demanding or long-life warm service selects
3LPE, and lower-demand cold service selects FBE. The helper does not replace a
project coating specification; it gives engineering callers a standards-cited
default and fails closed when the requested service falls outside the encoded
envelope.
