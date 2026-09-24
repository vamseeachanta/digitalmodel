"""Synthetic neutral v261 zero-load listing; no pressure-case generalization."""
import pytest
from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_audit import verify_load_audit


def native_loads():
    return (b' *** NOTE *** ELAPSED TIME = 1.0 TIME= 01:00:00\nNo surface loads to list.\n'
            b' *** NOTE *** ELAPSED TIME = 1.0 TIME= 01:00:00\nNo nodal forces to list.\n'
            b'LIST CONSTRAINTS FOR SELECTED NODES 1 TO 8 BY 1\nCURRENTLY SELECTED DOF SET= UX UY\n'
            b'*** MAPDL - ENGINEERING ANALYSIS SYSTEM RELEASE 2026 R1.01 26.1 ***\n'
            b'Ansys Mechanical Enterprise\n'
            b'00000000 VERSION=WINDOWS x64 01:00:00 SEP 14, 2026 Elapsed Time= 1.0\n'
            b'Open cylinder verification CTRL16\n'
            b'NODE LABEL REAL IMAG\n1 UY 0.00000000 0.00000000\n'
            b'FINISH SOLUTION PROCESSING\n')


def case():
    return dict(pressure_mpa='0',bottom_node_ids=[1],pressure_faces=[],nodes=[{'node_id':8}])


def test_observed_zero_load_and_support_layout():
    verify_load_audit(native_loads(),case())


@pytest.mark.parametrize('damage',['surface','force','nonzero','imaginary','missing','duplicate','dof','pressure','unknown'])
def test_native_zero_branch_refuses_unsupported_state(damage):
    raw=native_loads();model=case()
    if damage=='surface':raw=raw.replace(b'No surface loads to list.',b'Surface loads present.')
    elif damage=='force':raw=raw.replace(b'No nodal forces to list.',b'Nodal forces present.')
    elif damage=='nonzero':raw=raw.replace(b'1 UY 0.00000000',b'1 UY 0.00000001')
    elif damage=='imaginary':raw=raw.replace(b'0.00000000\nFINISH',b'0.00000001\nFINISH')
    elif damage=='missing':raw=raw.replace(b'1 UY 0.00000000 0.00000000\n',b'')
    elif damage=='duplicate':raw=raw.replace(b'1 UY',b'1 UY 0.00000000 0.00000000\n1 UY')
    elif damage=='dof':raw=raw.replace(b'1 UY',b'1 UX')
    elif damage=='pressure':model['pressure_mpa']='10'
    else:raw+=b'UNKNOWN STATE\n'
    with pytest.raises(EvidenceError):verify_load_audit(raw,model)
