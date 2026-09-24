"""Synthetic subprocess evidence only; no repository mutations."""
import subprocess
import pytest
from digitalmodel.ansys.analysis_records import canonical_bytes,digest_bytes


def test_source_binding_requires_real_revision():
    from digitalmodel.ansys.analysis_pressure_source import verify_build_source
    with pytest.raises(ValueError,match='40-hex'):verify_build_source('fake',{})


def test_commit_bytes_must_equal_working(monkeypatch,tmp_path):
    import digitalmodel.ansys.analysis_pressure_source as module
    p=tmp_path/'source.py';p.write_bytes(b'actual')
    monkeypatch.setattr(module,'SOURCE_PATHS',('source.py',))
    monkeypatch.setitem(module.SOURCE_SCOPES,module.SCOPE,('source.py',))
    monkeypatch.setattr(module,'ROOT',tmp_path)
    monkeypatch.setattr(module.subprocess,'run',lambda *a,**k:subprocess.CompletedProcess(a[0],0,b'different',b''))
    with pytest.raises(ValueError,match='commit'):module.verify_build_source('a'*40,{'source.py':digest_bytes(b'actual')})


def test_recorded_inventory_digest_not_live_files(monkeypatch):
    import digitalmodel.ansys.analysis_pressure_source as module
    monkeypatch.setattr(module,'SOURCE_PATHS',('source.py',))
    monkeypatch.setitem(module.SOURCE_SCOPES,module.SCOPE,('source.py',))
    files={'source.py':'a'*64}
    package=dict(source_revision='a'*40,code_files=files,code_revision=digest_bytes(canonical_bytes(files)),code_canonicalization='raw-sha256-v1',code_inventory_scope=module.SCOPE)
    module.validate_recorded_source(package)
    package['code_revision']='b'*64
    with pytest.raises(ValueError):module.validate_recorded_source(package)


@pytest.mark.parametrize('fault',['none','blob','missing_commit','working','drift'])
def test_build_commit_equality_boundary(monkeypatch,tmp_path,fault):
    import digitalmodel.ansys.analysis_pressure_source as module
    p=tmp_path/'source.py';p.write_bytes(b'actual')
    monkeypatch.setattr(module,'SOURCE_PATHS',('source.py',))
    monkeypatch.setitem(module.SOURCE_SCOPES,module.SCOPE,('source.py',))
    monkeypatch.setattr(module,'ROOT',tmp_path)
    commands=[]
    def run(command,**kwargs):
        commands.append(command)
        if 'rev-parse' in command:
            return subprocess.CompletedProcess(command,1 if fault=='missing_commit' else 0,b'a'*40+b'\n',b'')
        if fault=='drift':p.write_bytes(b'changed')
        return subprocess.CompletedProcess(command,0,b'changed' if fault=='blob' else b'actual',b'')
    monkeypatch.setattr(module.subprocess,'run',run)
    files={'source.py':digest_bytes(b'changed' if fault=='working' else b'actual')}
    if fault=='none':
        module.verify_build_source('a'*40,files)
        assert len(commands)==2 and 'cat-file' in commands[1]
    else:
        with pytest.raises(ValueError):module.verify_build_source('a'*40,files)


@pytest.mark.parametrize('fault',['missing','extra','malformed','canonicalization','revision'])
def test_recorded_inventory_rejects_corruption(monkeypatch,fault):
    import digitalmodel.ansys.analysis_pressure_source as module
    monkeypatch.setattr(module,'SOURCE_PATHS',('source.py',))
    monkeypatch.setitem(module.SOURCE_SCOPES,module.SCOPE,('source.py',))
    files={'source.py':'a'*64}
    if fault=='missing':files.clear()
    if fault=='extra':files['other.py']='b'*64
    if fault=='malformed':files['source.py']='bad'
    package=dict(source_revision='a'*40,code_files=files,code_revision=digest_bytes(canonical_bytes(files)),code_canonicalization='raw-sha256-v1',code_inventory_scope=module.SCOPE)
    if fault=='canonicalization':package['code_canonicalization']='unknown'
    if fault=='revision':package['source_revision']='a'*39
    with pytest.raises(ValueError):module.validate_recorded_source(package)


def test_selected_inventory_covers_parent_import_redirect():
    from digitalmodel.ansys.analysis_pressure_source import source_inventory
    files=source_inventory()
    assert {'src/digitalmodel/__init__.py','src/digitalmodel/_compat.py'}<=set(files)


def test_selected_inventory_covers_baseline_quantity_constants():
    from digitalmodel.ansys.analysis_pressure_source import source_inventory
    assert 'src/digitalmodel/ansys/cylinder_results.py' in source_inventory()
