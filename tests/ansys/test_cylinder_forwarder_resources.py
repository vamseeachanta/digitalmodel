"""Synthetic PE resources and local file-identity checks; no image loading."""
import importlib
import struct
import pytest


def tool():
    return importlib.import_module('digitalmodel.ansys.cylinder_forwarder_resources')


def pe(kind=b'\x02', target=b'C:/synthetic/python.exe'):
    resource=bytearray(144)
    def directory(at, entries, named=0):
        struct.pack_into('<HH',resource,at+12,named,len(entries)-named)
        for i,entry in enumerate(entries):struct.pack_into('<II',resource,at+16+i*8,*entry)
    def name(text):
        position=len(resource);raw=text.encode('utf-16le')
        resource.extend(struct.pack('<H',len(text))+raw);return position|0x80000000
    first=name('UV_TRAMPOLINE_KIND');second=name('UV_PYTHON_PATH')
    directory(0,[(10,0x80000000|24)])
    directory(24,[(first,0x80000000|64),(second,0x80000000|88)],2)
    directory(64,[(0,112)]);directory(88,[(0,128)])
    for position,raw in [(112,kind),(128,target)]:
        struct.pack_into('<IIII',resource,position,0x1000+len(resource),len(raw),0,0)
        resource.extend(raw)
    data=bytearray(512);data[:2]=b'MZ';struct.pack_into('<I',data,60,128)
    data[128:132]=b'PE\0\0';struct.pack_into('<H',data,134,1)
    struct.pack_into('<H',data,148,224);struct.pack_into('<H',data,152,0x10b)
    struct.pack_into('<I',data,244,16)
    struct.pack_into('<II',data,264,0x1000,len(resource))
    struct.pack_into('<IIII',data,384,len(resource),0x1000,len(resource),512)
    return bytes(data+resource)


def test_kind_full_bytes_and_literal_preserved():
    result=tool().parse_forwarder_pe(pe(b'\x02\x00'))
    assert result==dict(resource_kind_hex='0200',embedded_target_literal='C:/synthetic/python.exe')


@pytest.mark.parametrize('raw',[b'',b'MZ',pe(b'\x01'),pe(b''),pe(b'\x02'*17),
    pe(target=b''),pe(target=b'x\x00y'),pe(target=b'\xff'),pe(target=b'x'*4097),pe()[:-1]])
def test_malformed_pe_refuses(raw):
    with pytest.raises(ValueError):tool().parse_forwarder_pe(raw)


def test_declared_path_cache_rechecks_bytes(tmp_path):
    path=tmp_path/'a';path.write_bytes(b'one');cache=tool().DeclaredReads()
    assert cache.read(path)[0]==b'one'
    path.write_bytes(b'two')
    with pytest.raises(ValueError):cache.verify()


def test_redirected_source_refuses(tmp_path):
    source=tmp_path/'a';source.write_bytes(b'one');link=tmp_path/'b'
    try:link.symlink_to(source)
    except OSError:pytest.skip('synthetic symlink unavailable')
    with pytest.raises(ValueError):tool().DeclaredReads().read(link)


@pytest.mark.parametrize('fault',['duplicate','overlap','depth','entries','target_relative','oversized'])
def test_pe_structural_refusal(fault):
    raw=bytearray(pe())
    if fault=='duplicate':raw[552:556]=raw[552-8:556-8]
    elif fault=='overlap':raw[640:648]=raw[624:632]
    elif fault=='depth':struct.pack_into('<I',raw,532,0x80000000)
    elif fault=='entries':struct.pack_into('<H',raw,526,257)
    elif fault=='target_relative':raw=pe(target=b'python.exe')
    else:raw=b'MZ'+b'0'*(16*1024**2)
    with pytest.raises(ValueError):tool().parse_forwarder_pe(bytes(raw))


def test_cached_bytes_still_obey_bounded_request(tmp_path):
    path=tmp_path/'a';path.write_bytes(b'abcd');cache=tool().DeclaredReads();cache.read(path)
    with pytest.raises(ValueError):cache.read(path,maximum=3)


def test_identical_bytes_distinct_declared_paths_both_read(tmp_path,monkeypatch):
    from pathlib import Path
    first=tmp_path/'a';second=tmp_path/'b'
    first.write_bytes(b'same');second.write_bytes(b'same');calls=[];original=Path.read_bytes
    def read(path):calls.append(str(path));return original(path)
    monkeypatch.setattr(Path,'read_bytes',read)
    cache=tool().DeclaredReads();cache.read(first);cache.read(second);cache.verify()
    assert calls.count(str(first))==2 and calls.count(str(second))==2



def test_cached_path_cannot_change_declared_alias(tmp_path):
    path=tmp_path/'a';path.write_bytes(b'one');cache=tool().DeclaredReads();cache.read(path)
    with pytest.raises(ValueError):cache.read(path,alias=tmp_path/'different')


def test_forwarder_reads_alias_and_resolved_target_independently():
    launcher='C:\\synthetic\\launcher.exe';alias='C:\\synthetic\\alias\\python.exe'
    resolved='C:\\synthetic\\version\\python.exe';calls=[]
    class Cache:
        def read(self,path,**kwargs):
            calls.append(path)
            if path==launcher:return pe(target=alias.encode()),{}
            return b'image',dict(alias_identity={'synthetic':True},resolved_path=resolved,sha256='b'*64)
    parent=dict(pid=1,creation_time='1',executable_path=launcher,executable_sha256='a'*64,argv=[launcher,'x.py'])
    child=dict(pid=2,parent_pid=1,creation_time='2',executable_path=alias,
        executable_resolved_path=resolved,executable_sha256='b'*64,argv=[alias,'x.py'])
    candidate=dict(parent_pid=1,child_pid=2,declared_target_alias=alias)
    tool().observe_forwarder(candidate,{1:parent,2:child},Cache())
    assert calls==[launcher,alias,resolved]


def test_hardlinked_image_explicit_observation_keeps_strict_defaults(tmp_path):
    import os
    first=tmp_path/'image.exe';first.write_bytes(b'image');second=tmp_path/'other.exe'
    os.link(first,second)
    with pytest.raises(ValueError):tool().DeclaredReads().read(first)
    cache=tool().DeclaredReads();_,metadata=cache.read(first,allow_hardlinks=True)
    assert metadata['link_count']==2;cache.verify()
    with pytest.raises(ValueError):
        tool().DeclaredReads().read(first,alias=first,allow_hardlinks=True)


def test_image_flag_never_permits_redirects(tmp_path):
    first=tmp_path/'image.exe';first.write_bytes(b'image');link=tmp_path/'link.exe'
    try:link.symlink_to(first)
    except OSError:pytest.skip('synthetic symlink unavailable')
    with pytest.raises(ValueError):tool().DeclaredReads().read(link,allow_hardlinks=True)


def test_actual_system_image_read_only_metadata():
    import os
    from pathlib import Path
    if os.name!='nt':pytest.skip('Windows system image observation')
    path=Path(os.environ['SystemRoot'])/'System32/conhost.exe'
    before=path.stat();cache=tool().DeclaredReads()
    _,metadata=cache.read(path,allow_hardlinks=True);cache.verify()
    assert metadata['link_count']==before.st_nlink==path.stat().st_nlink
    assert metadata['inode']==before.st_ino


def actual_rows(launcher,alias):
    cache=tool().DeclaredReads();_,p=cache.read(str(launcher))
    resolved=alias.resolve();_,c=cache.read(str(resolved),alias=str(alias))
    rows={1:dict(pid=1,creation_time='1',executable_path=str(launcher),
        executable_sha256=p['sha256'],argv=[str(launcher),'synthetic.py']),
        2:dict(pid=2,parent_pid=1,creation_time='2',executable_path=str(resolved),
        executable_sha256=c['sha256'],executable_resolved_path=str(resolved),
        argv=[str(alias),'synthetic.py'])}
    return cache,rows


def test_actual_installed_uv_alias_read_only_file_binding():
    import os,sys
    from pathlib import Path
    if os.name!='nt':pytest.skip('Windows uv fixture')
    launcher=Path(sys.executable)
    try:metadata=tool().parse_forwarder_pe(launcher.read_bytes())
    except ValueError:pytest.skip('Interpreter is not the uv trampoline fixture')
    alias=Path(metadata['embedded_target_literal'])
    if not alias.parent.is_junction():pytest.skip('Installed fixture lacks immediate-parent junction')
    before=alias.parent.lstat();cache,rows=actual_rows(launcher,alias)
    result=tool().observe_forwarder(dict(parent_pid=1,child_pid=2,
        declared_target_alias=str(alias)),rows,cache);cache.verify()
    assert result['alias_identity']['inode']==before.st_ino==alias.parent.lstat().st_ino
    assert result['alias_identity']['junction_path']==str(alias.parent)
    assert result['alias_identity']['raw_link_target']==os.readlink(alias.parent)


def test_real_filesystem_without_required_junction_refuses(tmp_path):
    alias=tmp_path/'python.exe';alias.write_bytes(b'synthetic interpreter')
    launcher=tmp_path/'launcher.exe';launcher.write_bytes(pe(target=str(alias).encode()))
    cache,rows=actual_rows(launcher,alias)
    with pytest.raises(ValueError,match='junction required'):
        tool().observe_forwarder(dict(parent_pid=1,child_pid=2,
            declared_target_alias=str(alias)),rows,cache)


def test_metadata_evidence_is_canonical_and_contains_no_payload(tmp_path):
    from digitalmodel.ansys.analysis_records import canonical_bytes
    path=tmp_path/'image.exe';path.write_bytes(b'private synthetic payload')
    cache=tool().DeclaredReads();_,metadata=cache.read(path,allow_hardlinks=True)
    rows=cache.evidence()
    assert rows==[dict(declared_path=str(path),**metadata,declared_alias=None,allow_hardlinks=True)]
    assert b'private synthetic payload' not in canonical_bytes(rows)
    rows[0]['link_count']=999
    assert cache.evidence()[0]['link_count']==1
