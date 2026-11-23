"""In-air S-N parameters from the canonical October 2011 C203 table.

This adapter covers plated welded details, not the tubular-joint T curve.
C203 §2.4, Table 2-1 and Eqs. (2.4.1), (2.4.3) govern endurance and thickness.
"""
from typing import NamedTuple

from digitalmodel.citations import Citation
from digitalmodel.fatigue.c203_sn_tables import BILINEAR, N_KNEE_AIR


class SNParameters(NamedTuple):
    log_a1: float
    m1: float
    log_a2: float
    m2: float
    knee: float
    k: float


# Reuse the verified table; no independent second set of engineering values.
SN_CURVES = {
    f'DNV-{name}': SNParameters(
        row.log_a1_air, row.m1, row.log_a2, row.m2, N_KNEE_AIR, row.k,
    )
    for name, row in BILINEAR.items() if name != 'B2'
}

SN_CITATION = Citation(
    code_id='DNV-RP-C203', publisher='DNV', revision='2011-10',
    section='Table 2-1; Section 2.4, Eqs. (2.4.1) and (2.4.3)',
    wiki_path='wikis/engineering-standards/wiki/standards/dnv-rp-c203-2011.md',
    source_sibling='generic',
    note='In-air bilinear curves; MPa; 25 mm welded-detail reference thickness. '
         'Continued m2 branch for variable-amplitude damage; no endurance cutoff. '
         'The T curve and seawater environments are outside this adapter.',
)
