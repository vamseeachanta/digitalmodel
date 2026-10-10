"""Neutral native-status layout fixtures; no solver execution or native acceptance."""
import pytest
from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_native_status import config_section, format_status, outres_status, nerr_status

CONFIG = b"\n\n\n\n *****  CURRENT ANSYS CONFIGURATION  *****\n\n\n MAXIMUM NUMBER OF DATA SETS ON RESULT FILE(NRES)=     10000\n NUMBER OF BUFFERS PER SOLUTION FILE       (NBUF)=         4\n\n KEEP SOLUTION FILES OPEN THROUGHOUT RUN(LOCFL)\n\n\n SIZE OF BINARY FILES                     (SZBIO)=     16384  (  64.00 KB)\n MAXIMUM NUMBER OF CONTACTING SURFACES    (NCONT)=      1000\n\n USE BOTH WSORT,ALL AND WAVES FOR DEFAULT REORDERER(ORDER)\n\n\n\n DO NOT USE UNDO OPTION\n SIZE OF KEYPOINT HEADER                         =         0\n SIZE OF LINE HEADER                             =         0\n SIZE OF AREA HEADER                             =         0\n SIZE OF VOLUME HEADER                           =         0\n SIZE OF NODE HEADER                             =         0\n SIZE OF ELEMENT HEADER                          =         0\n SIZE OF REAL HEADER                             =         0\n SIZE OF COUPLE HEADER                           =         0\n SIZE OF CONSTRAINT EQUATIONS HEADER             =         0\n WRITE .DBB DATABASE BACKUP WHEN APPROPRIATE (NODBB,0)\n DEFAULT EXTENSION FOR DATABASE FILES (DBEXT)=db  \n New Nonlinear Solution Control Options is active \n Solution file split size (MWords) (fsplt)=         0\n POISSON'S RATIO IS REQUIRED INPUT FOR ISOTROPIC STRUCTURAL MATERIALS\n Using new (LONG) result file format\n Using calculation of the mode sup. stresses\n USING DEFAULT I/O BYTE SWAPPING LOGIC\n MAXIMUM NUMBER OF LOAD VECTORS ON SUB FILE =        31\n USE DOUBLE PRECISION RESULTS FILE FORMAT\n PROGRAM AUTOMATICALLY CONTROLS WHEN TO DYNAMICALLY GROW THE NUMBER OF FILE BUFFERS\n MAXIMUM NUMBER OF LOAD VECTORS ON MODE FILE =      1000\n DO NOT SUPPRESS DUPLICATE ELEMENT NODAL QUANTITIES FROM RESULT FILE\n     AND BYPASS NST1 EXPANSION LOGIC\n AUTOMATICALLY CHECK OUT A MECHANICAL BATCH LICENSE\n     DURING SOLVE IF CAPABLITY IS NOT ENABLED\n\n"

FORMAT = b'\r\n CHARACTERS IN INTEGER FIELD    =   8\r\n OUTPUT DISPLAY TYPE            =   E \r\n CHARACTERS PER OUTPUT FIELD    =  24\r\n CHARACTERS AFTER DECIMAL POINT =  16\r\n LINES PER PAGE                 = 100\r\n CHARACTERS PER LINE            = 240\r\n\r\n CHARACTERS IN INTEGER FIELD    =   8\r\n OUTPUT DISPLAY TYPE            =   E \r\n CHARACTERS PER OUTPUT FIELD    =  24\r\n CHARACTERS AFTER DECIMAL POINT =  16\r\n LINES PER PAGE                 = 100\r\n CHARACTERS PER LINE            = 240\r\n'

OUTRES = b'\r\n ERASE THE CURRENT DATABASE OUTPUT CONTROL TABLE.\r\n\r\n\r\n WRITE ALL  ITEMS TO THE DATABASE WITH A FREQUENCY OF ALL \r\n   FOR ALL APPLICABLE ENTITIES\r\n\r\n WRITE NAR  ITEMS TO THE DATABASE WITH A FREQUENCY OF NONE\r\n   FOR ALL APPLICABLE ENTITIES\r\n\r\n LIST THE CURRENT DATABASE OUTPUT CONTROL TABLE:\r\n\r\n     ITEM     FREQUENCY   COMPONENT\r\n      ALL         ALL      \r\n     NAR         NONE      \r\n'

NERR = b'\r\n *** STATUS OF ERRORS ***\r\n NUMBER OF NOTES DISPLAYED=            0\r\n NUMBER OF WARNINGS DISPLAYED=         0\r\n NUMBER OF ERRORS DISPLAYED=           0\r\n NUMBER OF DISPLAYED ERRORS ALLOWED PER COMMAND=                200\r\n NUMBER OF ERRORS ALLOWED PER COMMAND BEFORE ANSYS ABORT=     10000\r\n ALL ERRORS WRITTEN TO synthetic/file.err\r\n DO NOT TERMINATE /INPUT UPON FIRST ERROR\r\n'


def test_retained_layout_reports_all_required_native_states():
    assert config_section(CONFIG) == '0'
    assert format_status(FORMAT) == {'NDIGIT':'8','FTYPE':'E','NWIDTH':'24','DSIGNF':'16','LINE':'100','CHAR':'240'}
    assert outres_status(OUTRES) == {'ALL':'ALL','NAR':'NONE'}
    assert nerr_status(NERR) == 200


@pytest.mark.parametrize('damage',['precision','unknown','duplicate','missing'])
def test_configuration_unsupported_or_changed_state_refuses(damage):
    raw=CONFIG
    if damage=='precision': raw=raw.replace(b'DOUBLE PRECISION',b'SINGLE PRECISION')
    elif damage=='unknown': raw+=b'UNKNOWN STATUS\n'
    elif damage=='duplicate': raw+=b'USE DOUBLE PRECISION RESULTS FILE FORMAT\n'
    else: raw=raw.replace(b' USE DOUBLE PRECISION RESULTS FILE FORMAT\n',b'')
    with pytest.raises(EvidenceError): config_section(raw)


@pytest.mark.parametrize('which', ['format','outres','nerr'])
@pytest.mark.parametrize('damage', ['changed','duplicate','unknown','missing'])
def test_observed_status_cannot_drop_duplicate_or_mutate_fields(which,damage):
    raw,reader,old,new = {'format':(FORMAT,format_status,b'=  16',b'=  15'),
        'outres':(OUTRES,outres_status,b'NAR         NONE',b'NAR         ALL '),
        'nerr':(NERR,nerr_status,b'200',b'-200')}[which]
    if damage=='changed': raw=raw.replace(old,new)
    elif damage=='duplicate': raw+=raw
    elif damage=='unknown': raw+=b'UNKNOWN STATUS\n'
    else: raw=b''
    with pytest.raises(EvidenceError): reader(raw)


def native_output():
    from tests.ansys.test_cylinder_results_audit import window
    raw=window().replace(b'RESUPREC = 0\n',CONFIG)
    raw=b' USE DOUBLE PRECISION RESULTS FILE FORMAT\n'+raw
    for name,value in [('FORMAT',FORMAT),('OUTRES',OUTRES),('NERR',NERR)]:
        raw+=('OCV_'+name+'_BEGIN\n').encode()+value+('OCV_'+name+'_END\n').encode()
    return raw


def test_native_echo_plus_two_bound_config_captures():
    from digitalmodel.ansys.cylinder_results_audit import parse_configuration
    assert parse_configuration(native_output())['nerr_nmerr']==200


def test_extra_unbound_precision_state_refuses():
    from digitalmodel.ansys.cylinder_results_audit import parse_configuration
    with pytest.raises(EvidenceError):
        parse_configuration(native_output()+b'USE DOUBLE PRECISION RESULTS FILE FORMAT\n')


def test_input_listing_marker_text_is_not_executed_marker():
    from digitalmodel.ansys.cylinder_results_audit import parse_configuration
    raw=b'     4 /COM,OCV_LOAD_AUDIT_BEGIN\n'+native_output()
    assert parse_configuration(raw)['nerr_nmerr']==200


def test_native_page_titles_must_bind_current_case():
    from digitalmodel.ansys.cylinder_results_native_status import verify_case_titles
    verify_case_titles(b' Open cylinder verification CTRL16\n','CTRL16')
    for raw in [b' Open cylinder verification P10N4\n',b'no native title\n',
                b' Open cylinder verification CTRL16\n Open cylinder verification P10N8\n']:
        with pytest.raises(EvidenceError):verify_case_titles(raw,'CTRL16')


@pytest.mark.parametrize('cue', [b'PRINT S', b'PRINT U', b'PRINT F',
    b'PRINT ELEMENT', b'No surface loads to list.'])
def test_native_adapter_cues_bind_case_even_without_config(cue):
    from tests.ansys.cylinder_synthetic_protocol import synthetic_protocol
    from digitalmodel.ansys.cylinder_results_validation import validate_native_evidence
    case, artifacts = synthetic_protocol('ocv-zero-t60-n16')
    artifacts['native.out'] += cue + b'\n Open cylinder verification P10N4\n'
    result = validate_native_evidence(case, artifacts, 'a'*64, 'a'*64)
    assert result['status'] == 'INCOMPLETE'
    assert 'Native page case identity differs' in '; '.join(result['errors'])


def test_native_precision_state_exposes_inference_and_source_version():
    from digitalmodel.ansys.cylinder_results_audit import parse_configuration
    result = parse_configuration(native_output())
    assert result['resuprec'] == '0'
    assert result['resuprec_basis'] == 'inferred_from_native_double_precision_status'
    assert result['resuprec_source_version'] == 'v261'
