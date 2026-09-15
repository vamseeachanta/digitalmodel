"""Planted synthetic signature tests; no actual credentials."""
import importlib.util
from pathlib import Path
import pytest


def screen():
    path=Path(__file__).resolve().parents[2]/'scripts/screen_solver_evidence.py'
    spec=importlib.util.spec_from_file_location('screen_solver_evidence',path)
    result=importlib.util.module_from_spec(spec);spec.loader.exec_module(result)
    return result.screen_bytes


@pytest.mark.parametrize('encoding',['ascii','utf-16-le','utf-16-be'])
@pytest.mark.parametrize('text,rule',[
    ('-----BEGIN '+'PRIVATE KEY-----','private-key'),
    ('aws_access_key_id = '+'AKIA'+'A'*16,'aws-access-key'),
    ('password = "'+'synthetic-secret-value'+'"','credential-assignment')])
def test_ascii_and_utf16_signatures_return_only_ids(encoding,text,rule):
    result=screen()(text.encode(encoding))
    assert rule in result
    assert all(item in {'private-key','aws-access-key','credential-assignment'} for item in result)
    assert text not in str(result)


@pytest.mark.parametrize('raw',[b'',b'synthetic neutral stress: 0 MPa',b'password = null',b'password = ""'])
def test_negative_fixtures(raw):
    assert screen()(raw)==[]


def test_external_deny_rules_case_and_ids():
    rules=[dict(id='synthetic-deny',pattern='neutral-name',case_sensitive=True,severity='high')]
    assert screen()(b'NEUTRAL-NAME',rules)==[]
    rules[0]['case_sensitive']=False
    assert screen()('NEUTRAL-NAME'.encode('utf-16-le'),rules)==['synthetic-deny']


@pytest.mark.parametrize('rule',[{},dict(id='x',pattern='[',case_sensitive=True,severity='high'),
    dict(id='x',pattern='test',case_sensitive='false',severity='high')])
def test_invalid_deny_controls_refuse(rule):
    with pytest.raises(ValueError):screen()(b'neutral',[rule])


@pytest.mark.parametrize('prefix,length,rule', [('gh'+'p_',36,'github-token'),
    ('github'+'_pat_',82,'github-token'),('sk'+'-proj-',60,'provider-token'),
    ('sk'+'-',48,'provider-token')])
@pytest.mark.parametrize('encoding',['ascii','utf-16-le','utf-16-be'])
def test_standalone_high_confidence_tokens(prefix,length,rule,encoding):
    planted=prefix+'A'*length
    assert rule in screen()(planted.encode(encoding))


@pytest.mark.parametrize('quote',['"', "'"])
@pytest.mark.parametrize('encoding',['ascii','utf-16-le','utf-16-be'])
def test_quoted_credential_keys(quote,encoding):
    raw=('{'+quote+'password'+quote+': '+quote+'synthetic-secret-value'+quote+'}').encode(encoding)
    assert 'credential-assignment' in screen()(raw)
