"""Strict v261 status layouts observed in the retained neutral zero-control log.

Every status line is consumed. This narrow branch preserves existing required
states and rejects unrecognized layouts rather than inferring requested state.
"""
import re
from digitalmodel.ansys.cylinder_results import EvidenceError

CONFIG_LINES = (b'***** CURRENT ANSYS CONFIGURATION *****', b'MAXIMUM NUMBER OF DATA SETS ON RESULT FILE(NRES)= 10000', b'NUMBER OF BUFFERS PER SOLUTION FILE (NBUF)= 4', b'KEEP SOLUTION FILES OPEN THROUGHOUT RUN(LOCFL)', b'SIZE OF BINARY FILES (SZBIO)= 16384 ( 64.00 KB)', b'MAXIMUM NUMBER OF CONTACTING SURFACES (NCONT)= 1000', b'USE BOTH WSORT,ALL AND WAVES FOR DEFAULT REORDERER(ORDER)', b'DO NOT USE UNDO OPTION', b'SIZE OF KEYPOINT HEADER = 0', b'SIZE OF LINE HEADER = 0', b'SIZE OF AREA HEADER = 0', b'SIZE OF VOLUME HEADER = 0', b'SIZE OF NODE HEADER = 0', b'SIZE OF ELEMENT HEADER = 0', b'SIZE OF REAL HEADER = 0', b'SIZE OF COUPLE HEADER = 0', b'SIZE OF CONSTRAINT EQUATIONS HEADER = 0', b'WRITE .DBB DATABASE BACKUP WHEN APPROPRIATE (NODBB,0)', b'DEFAULT EXTENSION FOR DATABASE FILES (DBEXT)=db', b'New Nonlinear Solution Control Options is active', b'Solution file split size (MWords) (fsplt)= 0', b"POISSON'S RATIO IS REQUIRED INPUT FOR ISOTROPIC STRUCTURAL MATERIALS", b'Using new (LONG) result file format', b'Using calculation of the mode sup. stresses', b'USING DEFAULT I/O BYTE SWAPPING LOGIC', b'MAXIMUM NUMBER OF LOAD VECTORS ON SUB FILE = 31', b'USE DOUBLE PRECISION RESULTS FILE FORMAT', b'PROGRAM AUTOMATICALLY CONTROLS WHEN TO DYNAMICALLY GROW THE NUMBER OF FILE BUFFERS', b'MAXIMUM NUMBER OF LOAD VECTORS ON MODE FILE = 1000', b'DO NOT SUPPRESS DUPLICATE ELEMENT NODAL QUANTITIES FROM RESULT FILE', b'AND BYPASS NST1 EXPANSION LOGIC', b'AUTOMATICALLY CHECK OUT A MECHANICAL BATCH LICENSE', b'DURING SOLVE IF CAPABLITY IS NOT ENABLED')

OUTRES_LINES = (b'ERASE THE CURRENT DATABASE OUTPUT CONTROL TABLE.', b'WRITE ALL ITEMS TO THE DATABASE WITH A FREQUENCY OF ALL', b'FOR ALL APPLICABLE ENTITIES', b'WRITE NAR ITEMS TO THE DATABASE WITH A FREQUENCY OF NONE', b'FOR ALL APPLICABLE ENTITIES', b'LIST THE CURRENT DATABASE OUTPUT CONTROL TABLE:', b'ITEM FREQUENCY COMPONENT', b'ALL ALL', b'NAR NONE')

FORMAT_LINES = (b'CHARACTERS IN INTEGER FIELD = 8', b'OUTPUT DISPLAY TYPE = E', b'CHARACTERS PER OUTPUT FIELD = 24', b'CHARACTERS AFTER DECIMAL POINT = 16', b'LINES PER PAGE = 100', b'CHARACTERS PER LINE = 240')


def lines(raw):
    if not isinstance(raw,bytes) or not raw.endswith(b'\n'):
        raise EvidenceError('Missing or truncated native status')
    return [b' '.join(line.split()) for line in raw.splitlines() if line.strip()]


def page_header(rows):
    """Consume exactly the observed four-line page identity, not arbitrary banners."""
    patterns = [rb'\*\*\* MAPDL - ENGINEERING ANALYSIS SYSTEM RELEASE 2026 R1\.01 26\.1 \*\*\*',
        rb'Ansys Mechanical Enterprise',
        rb'[0-9]+ VERSION=WINDOWS x64 [0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{3} [0-9]{1,2}, [0-9]{4} Elapsed Time= [0-9]+(?:\.[0-9]+)?',
        rb'Open cylinder verification (?:CTRL16|P10N4|P10N8|P10N16)']
    if len(rows)!=4 or any(re.fullmatch(p,row) is None for p,row in zip(patterns,rows)):
        raise EvidenceError('Unknown native page identity')


def config_section(raw):
    rows=lines(raw)
    count=len(CONFIG_LINES)
    if tuple(rows[:count])!=CONFIG_LINES:
        raise EvidenceError('Unknown or changed native CONFIG status')
    if rows[count:]: page_header(rows[count:])
    return '0'  # Official /CONFIG: entire result file double precision means RESUPREC=0.


def format_status(raw):
    rows=lines(raw)
    if tuple(rows)!=FORMAT_LINES+FORMAT_LINES:
        raise EvidenceError('Native FORMAT echo/STAT are missing, duplicated or changed')
    return dict(NDIGIT='8',FTYPE='E',NWIDTH='24',DSIGNF='16',LINE='100',CHAR='240')


def outres_status(raw):
    if tuple(lines(raw))!=OUTRES_LINES:
        raise EvidenceError('Native OUTRES command/table disagree or changed')
    return dict(ALL='ALL',NAR='NONE')


def nerr_status(raw):
    rows=lines(raw)
    patterns=[rb'\*\*\* STATUS OF ERRORS \*\*\*',rb'NUMBER OF NOTES DISPLAYED= [0-9]+',
        rb'NUMBER OF WARNINGS DISPLAYED= 0',rb'NUMBER OF ERRORS DISPLAYED= 0',
        rb'NUMBER OF DISPLAYED ERRORS ALLOWED PER COMMAND= ([0-9]+)',
        rb'NUMBER OF ERRORS ALLOWED PER COMMAND BEFORE ANSYS ABORT= 10000',
        rb'ALL ERRORS WRITTEN TO [^\x00\r\n]+[/\\]file\.err',
        rb'DO NOT TERMINATE /INPUT UPON FIRST ERROR']
    if len(rows)!=len(patterns): raise EvidenceError('Missing or duplicate NERR status')
    matches=[re.fullmatch(p,row) for p,row in zip(patterns,rows)]
    if not all(matches): raise EvidenceError('Changed or unsupported NERR status')
    return int(matches[4][1])


def verify_case_titles(raw,case_token):
    titles=re.findall(rb'(?m)^ *Open cylinder verification ([A-Z0-9]+) *\r?$',raw)
    if not titles or any(t!=case_token.encode() for t in titles):
        raise EvidenceError('Native page case identity differs or absent')
