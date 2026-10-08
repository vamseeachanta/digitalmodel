"""Bounded intake implementation binding, independent of native execution provenance."""
from pathlib import Path
import re
import subprocess
from digitalmodel.ansys.analysis_records import canonical_bytes,digest_bytes

ROOT=Path(__file__).resolve().parents[3]
# Selected executable intake/record validation paths, not native replay execution.
SOURCE_PATHS_V1=tuple('src/digitalmodel/ansys/'+name+'.py' for name in (
    '__init__','analysis_pressure_source','analysis_pressure_inputs','analysis_pressure_observed',
    'analysis_records','analysis_evidence','analysis_matrix_publish','analysis_replay',
    'analysis_replay_inputs','cylinder_criteria','cylinder_results'))+('src/digitalmodel/__init__.py','src/digitalmodel/_compat.py')
SCOPE='selected-pressure-intake-record-validation-v1'
# Historical contracts are versioned separately from the current build selection.
# Future selected-set changes add a new scope; they do not edit the v1 contract.
SOURCE_SCOPES={SCOPE: SOURCE_PATHS_V1}
SOURCE_PATHS=SOURCE_SCOPES[SCOPE]


def source_inventory():
    return {p:digest_bytes((ROOT/p).read_bytes()) for p in SOURCE_PATHS}


def _require(value,message):
    if not value:raise ValueError(message)


def validate_recorded_source(package):
    revision=package.get('source_revision')
    _require(isinstance(revision,str) and re.fullmatch(r'[0-9a-f]{40}',revision),'source revision must be real 40-hex commit')
    files=package.get('code_files')
    scope=package.get('code_inventory_scope')
    _require(isinstance(scope,str) and scope in SOURCE_SCOPES,'unknown intake source scope')
    _require(isinstance(files,dict) and set(files)==set(SOURCE_SCOPES[scope]),'intake code inventory differs')
    _require(all(isinstance(v,str) and re.fullmatch(r'[0-9a-f]{64}',v) for v in files.values()),'intake digest malformed')
    _require(package.get('code_revision')==digest_bytes(canonical_bytes(files)),'intake code revision differs')
    _require(package.get('code_canonicalization')=='raw-sha256-v1','intake canonicalization differs')


def verify_build_source(revision,files):
    validate_recorded_source(dict(source_revision=revision,code_files=files,
        code_revision=digest_bytes(canonical_bytes(files)),code_canonicalization='raw-sha256-v1',code_inventory_scope=SCOPE))
    _require(files==source_inventory(),'intake working source differs')
    commit=subprocess.run(['git','-C',str(ROOT),'rev-parse','--verify',revision+'^{commit}'],
        capture_output=True,check=False,timeout=10)
    _require(commit.returncode==0 and commit.stdout.decode().strip()==revision,'source commit not established')
    for path,expected in files.items():
        blob=subprocess.run(['git','-C',str(ROOT),'cat-file','blob',revision+':'+path],
            capture_output=True,check=False,timeout=10)
        _require(blob.returncode==0 and digest_bytes(blob.stdout)==expected,'source commit bytes differ: '+path)
    _require(files==source_inventory(),'intake working source changed during commit verification')
