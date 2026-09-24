"""Replay retained neutral checker evidence offline; no provider invocation."""
import json
import shutil
from pathlib import Path

import pytest

from digitalmodel.ansys.cylinder_reference_provenance import verify_prepared_reference
from digitalmodel.ansys import cylinder_reference


def captured_bundle(tmp_path,monkeypatch):
    from digitalmodel.ansys import cylinder_reference_provenance as module
    source=Path(cylinder_reference.__file__).resolve().parents[3]
    relative=Path('examples/ansys/cylinder-benchmark')
    evidence=source/relative
    invocation=json.loads((evidence/'checker_invocation.json').read_bytes())
    names=['reference.json','reference_comparison.json','checker_invocation.json',
           'checker_capture.json','checker_response.json','checker_transport.json','checker_process_stderr.txt']
    files={str(relative/name).replace('\\','/'): (evidence/name).read_bytes() for name in names}
    files.update({row['path']:(source/row['path']).read_bytes() for row in invocation['frozen_files']})
    for name,raw in files.items():
        target=tmp_path/name;target.parent.mkdir(parents=True,exist_ok=True);target.write_bytes(raw)
    (tmp_path/'.git').mkdir()
    monkeypatch.setattr(module,'_git_blob',lambda root,commit,name:files[name])
    return tmp_path/relative/'reference.json'


def test_retained_reference_arithmetic_and_capture_replay(tmp_path,monkeypatch):
    path=captured_bundle(tmp_path,monkeypatch)
    assert verify_prepared_reference(path) is True


@pytest.mark.parametrize('fault',['row','comparison','input_commit','frozen','transport','capture','response','single_invocation'])
def test_reference_provenance_mutations_refuse(tmp_path,monkeypatch,fault):
    path=captured_bundle(tmp_path,monkeypatch)
    names={'row':'reference.json','comparison':'reference_comparison.json','input_commit':'checker_invocation.json',
           'frozen':'checker_invocation.json','transport':'checker_transport.json','capture':'checker_capture.json',
           'response':'checker_response.json','single_invocation':'checker_invocation.json'}
    target=path.parent/names[fault];data=json.loads(target.read_bytes())
    if fault=='row':data['rows'][-1]['values']['sigma_theta']='1'
    if fault=='comparison':data['comparisons'][0]['passes']=False
    if fault=='input_commit':data['input_commit']='a'*40
    if fault=='frozen':data['frozen_files']=[]
    if fault=='transport':data['is_error']=True
    if fault=='capture':data['exit_code']=1
    if fault=='response':data['expressions']['sigma_r']='0*p'
    if fault=='single_invocation':data['invocation_ordinal']=2
    target.write_text(json.dumps(data))
    with pytest.raises(ValueError):verify_prepared_reference(path)
