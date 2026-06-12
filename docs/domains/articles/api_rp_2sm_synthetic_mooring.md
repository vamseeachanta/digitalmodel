# API RP 2SM Synthetic Rope Mooring

`digitalmodel.orcaflex.synthetic_rope_design` provides a bounded helper layer
for synthetic fiber rope segments used in offshore mooring systems. It keeps
API RP 2SM logic separate from the data-procurement rope database: procurement
clients describe candidate vendor-like rope records, while this module performs
standards-cited design screening.

The first slice models the engineering distinction that matters most for
OrcaFlex-style preliminary design: synthetic ropes do not have one universal
axial stiffness. The module exposes static, post-installation, storm dynamic,
and mean-load adjusted stiffness so callers can avoid treating polyester, HMPE,
aramid, or nylon segments like chain or wire.

The helper also includes narrow screening checks for long-term creep,
low-tension axial-compression fatigue, and QA evidence. These checks are not a
replacement for project rope qualification. They provide a fail-closed,
citation-backed default that points engineers to the API RP 2SM clauses that
need project confirmation.
