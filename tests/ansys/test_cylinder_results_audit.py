"""Synthetic audit tables; their exact native compatibility remains unestablished."""
import pytest

from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_audit import verify_load_audit, parse_configuration
from tests.ansys.test_cylinder_results import e24


def audit(pressure='10'):
    loads=('ELEMENT FACE KVAL P1 P2\n'+f'{1:8d}{4:8d}{1:8d}'+e24(pressure)*2+'\n'
           if pressure!='0' else 'NO SURFACE LOADS\n')
    return (loads+'NO NODAL FORCES\nNODE DOF VALUE\n'+
            f'{1:8d}{"UY":8}'+e24('0')+'\n').encode()


def case(pressure='10'):
    return {'pressure_mpa':pressure,'nodes':[{'node_id':1,'x_mm':'750','y_mm':'0'}],
            'bottom_node_ids':[1], 'pressure_faces':[] if pressure=='0' else [{'element_id':1,'face':4}]}


def test_complete_empty_control_is_distinct_from_missing_or_zero_sfe():
    verify_load_audit(audit('0'),case('0'))
    with pytest.raises(EvidenceError):verify_load_audit(b'',case('0'))
    with pytest.raises(EvidenceError):verify_load_audit(audit('10').replace(e24('10').encode(),e24('0').encode()),case('0'))


@pytest.mark.parametrize('fault',['sign','pressure','face','nodalforce','support','missing'])
def test_wrong_live_load_or_support_refuses(fault):
    raw=audit()
    if fault=='sign':raw=raw.replace(e24('10').encode(),e24('-10').encode())
    if fault=='pressure':raw=raw.replace(e24('10').encode(),e24('9').encode())
    if fault=='face':raw=raw.replace(f'{4:8d}'.encode(),f'{3:8d}'.encode())
    if fault=='nodalforce':raw=raw.replace(b'NO NODAL FORCES',b'NODE FX VALUE')
    if fault=='support':raw=raw.replace(b'UY      ',b'UX      ')
    if fault=='missing':raw=raw[:-1]
    with pytest.raises(EvidenceError):verify_load_audit(raw,case())


def test_pressure_audit_complete():
    verify_load_audit(audit(),case())


def config():
    return (b'RESUPREC = 0\nRESUPREC = 0\n'
            b'OCV_FORMAT_BEGIN\nNDIGIT=8\nFTYPE=E\nNWIDTH=24\nDSIGNF=16\nLINE=100\nCHAR=240\nOCV_FORMAT_END\n'
            b'OCV_NERR_BEGIN\nNMERR=200\nOCV_NERR_END\n'
            b'OCV_OUTRES_BEGIN\nALL=ALL\nNAR=NONE\nOCV_OUTRES_END\n')


def test_configuration_requires_two_zero_precision_captures_and_outres():
    assert parse_configuration(config().replace(b'RESUPREC = 0\nRESUPREC = 0\n',b'')+window())['nerr_nmerr']==200
    for raw in [config().replace(b'RESUPREC = 0\n',b'',1),
                config().replace(b'RESUPREC = 0',b'RESUPREC = 1'),
                config().replace(b'NAR=NONE',b'NAR=ALL')]:
        with pytest.raises(EvidenceError):parse_configuration(raw)


def transition():
    return (b' ***** ROUTINE COMPLETED *****  ELAPSED TIME = 1.0\n'
            b'RESUPREC = 0\n ***** MAPDL SOLUTION ROUTINE *****\n'
            b' ***** MAPDL SOLVE COMMAND *****\n'
            b' FINISH SOLUTION PROCESSING\n'
            b' ***** ROUTINE COMPLETED *****  ELAPSED TIME = 2.0\n'
            b'RESUPREC = 0\n ***** MAPDL RESULTS INTERPRETATION (POST1) *****\n')


def window():
    return b'OCV_LOAD_AUDIT_BEGIN\n'+audit()+transition()+b'OCV_SOLVED_STATE_BEGIN\n'


def test_load_window_splits_at_native_finish_and_retains_transition():
    from digitalmodel.ansys.cylinder_results_audit import split_load_audit
    result=split_load_audit(window())
    assert result['loads']==audit()
    assert result['transition']==transition()
    verify_load_audit(result['loads'],case())


def test_configuration_zeros_must_be_bound_to_pre_and_post_solve_sections():
    raw=config().replace(b'RESUPREC = 0\nRESUPREC = 0\n',b'')+window()
    assert parse_configuration(raw)['nerr_nmerr']==200
    for bad in [raw.replace(b'RESUPREC = 0\n',b'',1),
                raw.replace(b'RESUPREC = 0\n',b'RESUPREC = 0\nRESUPREC = 0\n',1),
                config(), raw.replace(b'RESUPREC = 0\n',b'UNRECOGNIZED CONFIG HEADER\nRESUPREC = 0\n',1)]:
        with pytest.raises(EvidenceError):parse_configuration(bad)


@pytest.mark.parametrize('fault',['missing_finish','duplicate_finish','reordered','missing_solve','duplicate_solve'])
def test_transition_boundary_mutations_refuse(fault):
    raw=config().replace(b'RESUPREC = 0\nRESUPREC = 0\n',b'')+window()
    finish=b' ***** ROUTINE COMPLETED *****  ELAPSED TIME = 1.0\n'
    solve=b' ***** MAPDL SOLVE COMMAND *****\n'
    if fault=='missing_finish':raw=raw.replace(finish,b'')
    if fault=='duplicate_finish':raw=raw.replace(finish,finish+finish)
    if fault=='reordered':raw=raw.replace(solve,b'').replace(finish,solve+finish)
    if fault=='missing_solve':raw=raw.replace(solve,b'')
    if fault=='duplicate_solve':raw=raw.replace(solve,solve+solve)
    with pytest.raises(EvidenceError):parse_configuration(raw)
