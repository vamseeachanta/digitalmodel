"""Observed v261 constrained-UY PRRSOL layout; absent FX stays absent."""
import re
from decimal import Decimal
from digitalmodel.ansys.cylinder_results import EvidenceError,parse_e24,decimal_context
from digitalmodel.ansys.cylinder_results_native_listings import _rows,_words
from digitalmodel.ansys.cylinder_results_native_status import page_header
from digitalmodel.ansys.cylinder_results_native_values import summary_fields


def _prefix(rows):
    if not rows or _words(rows[0])!=b'PRINT F REACTION SOLUTIONS PER NODE':
        raise EvidenceError('Unknown PRRSOL preamble')
    page_header([_words(r) for r in rows[1:5]])
    expected=[b'***** POST1 TOTAL REACTION SOLUTION LISTING *****',
        b'LOAD STEP= 1 SUBSTEP= 1',b'TIME= 1.0000 LOAD CASE= 0',
        b'THE FOLLOWING X,Y,Z SOLUTIONS ARE IN THE GLOBAL COORDINATE SYSTEM',b'NODE FX FY']
    if [_words(r) for r in rows[5:10]]!=expected:
        raise EvidenceError('Unknown reaction state/frame/columns')
    return rows[10:]


def _value(field,expected):
    value,quantum=parse_e24(field,'N')
    limit=max(Decimal('1e-12')*max(abs(value),abs(expected)),Decimal('1e-8'))
    if quantum>limit/100 or abs(value-expected)>limit:
        raise EvidenceError('PRRSOL/support export disagreement or low precision')
    return value


@decimal_context
def verify_reactions(raw,expected):
    rows=_prefix(_rows(raw));found={}
    while rows and re.match(rb' *[0-9]',rows[0]):
        row=rows.pop(0)
        if (len(row)!=58 or not re.fullmatch(rb' *[1-9][0-9]* *',row[:10])
                or row[10:34]!=b' '*24):
            raise EvidenceError('Reaction row width or absent unconstrained FX differs')
        node=int(row[:10])
        if node not in expected or node in found:raise EvidenceError('Wrong or duplicate reaction node')
        found[node]=_value(row[34:58],expected[node])
    if set(found)!=set(expected):raise EvidenceError('Missing native reactions')
    if len(rows)!=2 or _words(rows[0])!=b'TOTAL VALUES':
        raise EvidenceError('Missing or extra native reaction total')
    total=summary_fields(rows[1], 2)
    if parse_e24(total[0],'N')[0]!=0:raise EvidenceError('Unexpected unconstrained reaction total')
    _value(total[1],sum(found.values(),Decimal(0)))
