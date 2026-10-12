"""Synthetic storage fixtures; no retained licensed data."""
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import pytest


def module():
    path = Path(__file__).resolve().parents[2]/'scripts/verify_solver_evidence.py'
    spec = importlib.util.spec_from_file_location('verify_solver_evidence', path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def dataset(tmp_path):
    root = tmp_path/'dataset'; root.mkdir()
    raw = b'synthetic neutral output\n'
    (root/'output.txt').write_bytes(raw)
    manifest = {'files':[{'path':'output.txt','sha256':hashlib.sha256(raw).hexdigest(),'bytes':len(raw)}]}
    (root/'manifest.json').write_text(json.dumps(manifest),encoding='utf-8')
    return root, manifest


def git(*args):
    return subprocess.run(['git',*map(str,args)],check=True,capture_output=True).stdout


def bare(tmp_path, root):
    git('init','-b','main',tmp_path/'repo')
    target=tmp_path/'repo'/'data'; target.mkdir()
    import shutil
    shutil.copytree(root,target/'case')
    git('-C',tmp_path/'repo','add','.')
    git('-C',tmp_path/'repo','-c','user.name=Synthetic Test','-c',
        'user.email=synthetic@example.invalid','commit','-m','Synthetic evidence')
    commit=git('-C',tmp_path/'repo','rev-parse','HEAD').decode().strip()
    git('clone','--bare',tmp_path/'repo',tmp_path/'bare.git')
    return tmp_path/'bare.git',commit


@pytest.mark.parametrize('mode',['filesystem','git'])
@pytest.mark.parametrize('fault',[None,'unlisted','missing','digest','bytes','unsafe','duplicate'])
def test_complete_membership_and_hash_parity(tmp_path,mode,fault):
    root,manifest=dataset(tmp_path)
    if fault=='unlisted': (root/'extra.txt').write_bytes(b'extra')
    elif fault=='missing': (root/'output.txt').unlink()
    elif fault=='digest': manifest['files'][0]['sha256']='a'*64
    elif fault=='bytes': manifest['files'][0]['bytes']=True
    elif fault=='unsafe': manifest['files'][0]['path']='../output.txt'
    elif fault=='duplicate': manifest['files'].append(dict(manifest['files'][0],path='OUTPUT.TXT'))
    (root/'manifest.json').write_text(json.dumps(manifest),encoding='utf-8')
    kwargs={}
    if mode=='git':
        git_dir,revision=bare(tmp_path,root);kwargs=dict(git_dir=git_dir,revision=revision)
        root='data/case'
    if fault:
        with pytest.raises(ValueError): module().verify(root,**kwargs)
    else:
        result=module().verify(root,**kwargs)
        assert result['file_count']==1
        assert result['total_bytes']==25
        assert len(result['manifest_sha256'])==64


@pytest.mark.parametrize('path',['/absolute','C:/drive','a\\b','a:b','a//b','./x','a/../b','manifest.json'])
def test_unsafe_manifest_paths_refuse(tmp_path,path):
    root,manifest=dataset(tmp_path);manifest['files'][0]['path']=path
    (root/'manifest.json').write_text(json.dumps(manifest),encoding='utf-8')
    with pytest.raises(ValueError):module().verify(root)


def test_duplicate_json_keys_refuse(tmp_path):
    root,_=dataset(tmp_path)
    (root/'manifest.json').write_bytes(b'{"files":[],"files":[]}')
    with pytest.raises(ValueError):module().verify(root)


def test_missing_git_or_noncommit_never_falls_back(tmp_path):
    root,_=dataset(tmp_path)
    with pytest.raises(ValueError):module().verify(root,git_dir=tmp_path/'absent',revision='a'*40)
    with pytest.raises(ValueError):module().verify('data/case',git_dir=tmp_path,revision='HEAD')


def test_symlink_payload_refuses(tmp_path):
    root,_=dataset(tmp_path)
    target=root/'output.txt';target.unlink()
    try: target.symlink_to(tmp_path/'outside')
    except OSError: pytest.skip('Symlink capability unavailable')
    with pytest.raises(ValueError):module().verify(root)


@pytest.mark.parametrize('mode',['filesystem','git'])
def test_file_metadata_is_permitted_without_attesting_it(tmp_path,mode):
    root,manifest=dataset(tmp_path)
    manifest['files'][0].update(role='diagnostic',origin='synthetic source',
        evidence_id='synthetic-id',restricted_operational=True)
    (root/'manifest.json').write_text(json.dumps(manifest),encoding='utf-8')
    kwargs={}
    if mode=='git':
        git_dir,revision=bare(tmp_path,root);root='data/case'
        kwargs=dict(git_dir=git_dir,revision=revision)
    assert module().verify(root,**kwargs)['file_count']==1


@pytest.mark.parametrize('mode,kind', [('120000','blob'),('160000','commit')])
def test_git_symlink_and_submodule_entries_refuse(monkeypatch,mode,kind):
    tool=module()
    def fake_git(root,*args):
        if args[0]=='rev-parse':return b'true\n'
        if args[0]=='cat-file':return b'commit\n'
        return (mode+' '+kind+' '+'a'*40+'\tdata/case/output.txt\0').encode()
    monkeypatch.setattr(tool,'_git',fake_git)
    with pytest.raises(ValueError):tool._git_inventory('data/case','unused','b'*40)


def test_git_executable_unavailable_refuses(monkeypatch):
    tool=module()
    def absent(*args,**kwargs):raise FileNotFoundError('synthetic missing Git')
    monkeypatch.setattr(tool.subprocess,'run',absent)
    with pytest.raises(ValueError,match='no filesystem fallback'):tool._git('unused','show','a'*40)


@pytest.mark.parametrize('name',['tail.','tail ','CON','nul.txt','COM1.log','LPT9',
    'a/*','a/?','a/[x]','a|b','a<b','a>b','a"b'])
def test_nonportable_names_refuse(name):
    with pytest.raises(ValueError):module()._path(name)


def test_git_directory_casefold_collision_refuses(monkeypatch):
    tool=module()
    def fake_git(root,*args):
        if args[0]=='rev-parse':return b'true\n'
        if args[0]=='cat-file':return b'commit\n'
        return b'100644 blob '+b'a'*40+b'\tdata/case/A/x\0'+b'100644 blob '+b'b'*40+b'\tdata/case/a/y\0'
    monkeypatch.setattr(tool,'_git',fake_git)
    with pytest.raises(ValueError):tool._git_inventory('data/case','unused','b'*40)


def test_bare_git_ref_rejects_dangling_commit(tmp_path):
    root,_=dataset(tmp_path);git_dir,revision=bare(tmp_path,root)
    tree=git('--git-dir='+str(git_dir),'rev-parse',revision+'^{tree}').decode().strip()
    dangling=git('--git-dir='+str(git_dir),'-c','user.name=Synthetic Test',
        '-c','user.email=synthetic@example.invalid','commit-tree',tree,'-m','Unreachable fixture').decode().strip()
    with pytest.raises(ValueError):module().verify('data/case',git_dir=git_dir,revision=dangling)


def test_bare_git_accepts_ancestor_and_current_tip(tmp_path):
    root,_=dataset(tmp_path);git_dir,revision=bare(tmp_path,root)
    repo=tmp_path/'repo';(repo/'outside.txt').write_bytes(b'outside dataset')
    git('-C',repo,'add','.')
    git('-C',repo,'-c','user.name=Synthetic Test','-c','user.email=synthetic@example.invalid',
        'commit','-m','Next synthetic commit')
    tip=git('-C',repo,'rev-parse','HEAD').decode().strip()
    git('--git-dir='+str(git_dir),'fetch',repo,'main:refs/heads/main')
    for commit in (revision,tip):
        assert module().verify('data/case',git_dir=git_dir,revision=commit)['file_count']==1


def test_bare_git_missing_named_ref_refuses(tmp_path):
    root,_=dataset(tmp_path);git_dir,revision=bare(tmp_path,root)
    with pytest.raises(ValueError):
        module().verify('data/case',git_dir=git_dir,revision=revision,git_ref='refs/heads/absent')


def test_explicit_staging_ref_is_local_reachability_only(tmp_path):
    root,_=dataset(tmp_path);git_dir,revision=bare(tmp_path,root)
    git('--git-dir='+str(git_dir),'update-ref','refs/heads/staging',revision)
    assert module().verify('data/case',git_dir=git_dir,revision=revision,
        git_ref='refs/heads/staging')['file_count']==1
