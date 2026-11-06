# Pipe capacity

## Summary

The package contains pipe sizing and standards-specific capacity routes.
The [Module report](../../../../docs/reports/modules/pipe_capacity/report.html)
maps their source evidence and independently checks a synthetic annulus through
the common sizing method. Pressure containment, collapse, combined loading,
material strength and code compliance remain **NOT_EVALUATED** in that report.
The parent issue remains open for broader numerical qualification.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-28 | Added the bounded report pointer to distinguish geometry evidence from capacity qualification. |

## Documentation

- [Report index and rebuild instructions](../../../../docs/reports/modules/pipe_capacity/README.md)
- [Knowledge pack](docs/agent-pack.md)
- [Existing workflow](../../../../examples/workflows/pipe-capacity/README.md)

The report calls `common.PipeSizing.PipeSizing.pipe_section_properties` directly.
The public router selects custom implementations; the report does not establish
their numerical equivalence or qualify their engineering use.
