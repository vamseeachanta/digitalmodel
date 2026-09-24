"""Observed v261 unloaded-control prologue and shared strict DLIST reader."""
import re
from digitalmodel.ansys.cylinder_results import EvidenceError, decimal_value
from digitalmodel.ansys.cylinder_results_native_status import lines, page_header


def verify_zero_loads(raw, case):
    rows=lines(raw)
    if decimal_value(case['pressure_mpa'])!=0:
        raise EvidenceError('Native no-load branch is only valid for zero control')
    note=rb'\*\*\* NOTE \*\*\* ELAPSED TIME = [0-9]+(?:\.[0-9]+)? TIME= [0-9]{2}:[0-9]{2}:[0-9]{2}'
    if (len(rows)<13 or not re.fullmatch(note,rows[0]) or not re.fullmatch(note,rows[2])
            or rows[1]!=b'No surface loads to list.' or rows[3]!=b'No nodal forces to list.'):
        raise EvidenceError('Missing native no-surface/no-force records')
    verify_constraints(rows[4:], case)


def verify_constraints(rows, case, *, case_token=None):
    """Consume the complete DLIST through FINISH; no ignored trailing state."""
    if len(rows) < 9:
        raise EvidenceError('Missing native constraint listing')
    selection=re.fullmatch(rb'LIST CONSTRAINTS FOR SELECTED NODES 1 TO ([0-9]+) BY 1',rows[0])
    if (not selection or int(selection[1])!=max(n['node_id'] for n in case['nodes'])
            or rows[1]!=b'CURRENTLY SELECTED DOF SET= UX UY'):
        raise EvidenceError('Native DLIST selection differs')
    page_header(rows[2:6])
    if case_token is not None and rows[5] != ('Open cylinder verification ' + case_token).encode('ascii'):
        raise EvidenceError('Native constraint case title differs')
    if rows[6]!=b'NODE LABEL REAL IMAG' or rows[-1]!=b'FINISH SOLUTION PROCESSING':
        raise EvidenceError('Unknown DLIST header or trailing state')
    supports=set()
    for row in rows[7:-1]:
        match=re.fullmatch(rb'([1-9][0-9]*) UY ([+-]?[0-9]+\.[0-9]{8}) ([+-]?[0-9]+\.[0-9]{8})',row)
        if not match or any(decimal_value(v.decode())!=0 for v in match.groups()[1:]):
            raise EvidenceError('Native support has wrong DOF/nonzero real or imaginary value')
        node=int(match[1])
        if node in supports: raise EvidenceError('Duplicate native constraint')
        supports.add(node)
    if supports!=set(case['bottom_node_ids']):
        raise EvidenceError('Native support set differs from frozen bottom row')
