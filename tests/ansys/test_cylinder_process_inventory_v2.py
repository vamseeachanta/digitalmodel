"""Synthetic pure v2 evidence; no native authority or host observation."""
from copy import deepcopy
from pathlib import Path
import pytest
from digitalmodel.ansys.cylinder_process_inventory import classify_process_inventory


def row(pid, parent, name='python.exe', script=None):
    path = 'C:/bin/' + name
    return dict(pid=pid, parent_pid=parent, creation_time=str(pid), name=name,
                executable_path=path, executable_resolved_path=path,
                executable_sha256='a'*64, argv=[path]+([script] if script else []),
                cwd='C:/work', script_sources=([dict(argv_index=1,
                path='C:/work/'+script, sha256='b'*64,
                resolution_basis='observed_interpreter_cwd')] if script else []))


@pytest.fixture
def evidence():
    rows=[row(1,0,script='controller.py'),row(2,1,script='guard.py'),
          row(3,2,'mpiexec.exe'),row(4,3,'smpd.exe'),row(5,4,'interFoam.exe')]
    rows[2]['argv'] += ['-n','1']
    wrapper=row(6,0); wrapper['creation_time']='0.5'
    wrapper['cwd']='C:/TEMP';wrapper['argv'] += ['controller.py']
    rows[0]['parent_pid']=6
    rows[0]['executable_path']='C:/alias/python.exe'
    rows.append(wrapper)
    f=dict(parent_pid=6,parent_creation_time='0.5',child_pid=1,
           child_creation_time='1',launcher_sha256='a'*64,resource_kind_hex='02',
           embedded_target_literal='C:/alias/python.exe',
           target_alias_path='C:/alias/python.exe',
           alias_identity=dict(junction_path='C:/alias',device=1,inode=2,
                               raw_link_target='C:\\bin'),
           resolved_target_path='C:/bin/python.exe',resolved_target_sha256='a'*64)
    s=dict(schema='process-snapshot-2',host='synthetic',observed_at='100',
           enumeration_complete=True,rows=rows,forwarders=[f],errors=[],
           coverage=dict(selector='ansys-mpi-lineage-v1',enumerated_count=10,
             selected_count=6,excluded_count=4,selected_details_complete=True))
    b=dict(schema='cfd-process-binding-2',host='synthetic',controller=1,guard=2,
           mpi=3,helper=4,ranks=[5],wrappers=[6],console_helpers=[],
           processes=deepcopy(rows),forwarders=[deepcopy(f)],
           owner_reference=dict(id='existing-receipt.json',sha256='c'*64))
    return s,b


def classify(s,b):
    return classify_process_inventory(s,expected_host='synthetic',cfd_binding=b,
                                      now='101',maximum_age_seconds='30')


def test_alias_junction_must_match_collector_immediate_parent(evidence):
    s, b = evidence
    for record in (s['forwarders'][0], b['forwarders'][0]):
        record['alias_identity']['junction_path'] = 'C:/'
    with pytest.raises(ValueError, match='junction'):
        classify(s, b)


def test_complete_forwarder_is_conditional_and_pure(evidence,monkeypatch):
    s,b=evidence; original=deepcopy(evidence)
    monkeypatch.setattr(Path,'read_bytes',lambda *_: pytest.fail('filesystem'))
    result=classify(s,b)
    assert result['status']=='CLEAR'
    assert result['evidence_scope']=='conditional_structural_classification'
    assert evidence==original
    assert result['raw_inventory'][0]['executable_path']=='C:/alias/python.exe'


@pytest.mark.parametrize('field,value',[
 ('resource_kind_hex','01'),('child_pid',2),('parent_pid',1),
 ('resolved_target_sha256','d'*64),('embedded_target_literal','../python.exe'),
 ('target_alias_path','C:/wrong/python.exe'),('parent_creation_time','0.4')])
def test_invalid_forwarders_refuse_even_when_pinned(evidence,field,value):
    s,b=evidence;s['forwarders'][0][field]=value;b['forwarders']=deepcopy(s['forwarders'])
    with pytest.raises(ValueError): classify(s,b)


@pytest.mark.parametrize('mutation',['missing','duplicate','wrong_basis','escape','forwarder_sources','tail'])
def test_source_contract_refuses(evidence,mutation):
    s,b=evidence;r=s['rows'][0]
    if mutation=='missing': r['script_sources']=[]
    if mutation=='duplicate': r['script_sources']*=2
    if mutation=='wrong_basis': r['script_sources'][0]['resolution_basis']='absolute_argument'
    if mutation=='escape': r['argv'][1]='../controller.py'
    if mutation=='forwarder_sources': s['rows'][-1]['script_sources']=deepcopy(r['script_sources'])
    if mutation=='tail': s['rows'][-1]['argv'][1]='other.py'
    b['processes']=deepcopy(s['rows'])
    with pytest.raises(ValueError): classify(s,b)


def test_incomplete_is_unknown_no_preserved_rows(evidence):
    s,b=evidence;s['rows'].pop();s['errors']=[dict(pid=6,reason_code='inaccessible_detail')]
    s['coverage']['selected_details_complete']=False
    result=classify(s,b)
    assert result['status']=='UNKNOWN'
    assert all(r['classification']!='PRESERVED_CFD' for r in result['dispositions'])


@pytest.mark.parametrize('mutation',['count','overlap','duplicate','complete','future','mixed','unknown_error'])
def test_malformed_snapshots_refuse(evidence,mutation):
    s,b=evidence
    if mutation=='count': s['coverage']['selected_count']=7
    if mutation=='overlap': s['errors']=[dict(pid=1,reason_code='identity_changed')]
    if mutation=='duplicate': s['rows'].append(deepcopy(s['rows'][0]))
    if mutation=='complete': s['coverage']['selected_details_complete']=False
    if mutation=='future': s['rows'][0]['creation_time']='101'
    if mutation=='mixed': b['schema']='cfd-process-binding-1'
    if mutation=='unknown_error': s['errors']=[dict(pid=8,reason_code='benign')]
    with pytest.raises(ValueError): classify(s,b)


def test_null_binding_keeps_mpi_unknown(evidence):
    assert classify(evidence[0],None)['status']=='UNKNOWN'


def test_snapshot_forwarder_pin_drift_cannot_preserve(evidence):
    s,b=evidence;s['forwarders'][0]['alias_identity']['inode']=3
    result=classify(s,b)
    assert result['status']=='UNKNOWN'


def test_unlisted_descendant_cannot_preserve(evidence):
    s,b=evidence;s['rows'].append(row(7,5,'conhost.exe'))
    s['coverage'].update(selected_count=7,excluded_count=3)
    assert classify(s,b)['status']=='UNKNOWN'


def test_empty_observation_does_not_erase_bound_missing_family(evidence):
    s,b=evidence;s.update(rows=[],forwarders=[])
    s['coverage'].update(selected_count=0,excluded_count=10)
    assert classify(s,b)['status']=='UNKNOWN'


@pytest.mark.parametrize('mutation',['owner','code','indices','resolved','alias_extra','kind_long','bool_count'])
def test_additional_structural_failures(evidence,mutation):
    s,b=evidence
    if mutation=='owner': b['owner_reference']={'id':'anything','verified':True}
    if mutation=='code': s['rows'][1]['argv'] += ['-c','pass']
    if mutation=='indices': s['rows'][0]['script_sources'][0]['argv_index']=True
    if mutation=='resolved': s['rows'][0]['executable_resolved_path']='C:/wrong/python.exe'
    if mutation=='alias_extra': s['forwarders'][0]['alias_identity']['other']='x'
    if mutation=='kind_long': s['forwarders'][0]['resource_kind_hex']='02'*17
    if mutation=='bool_count': s['coverage']['excluded_count']=True
    b['processes']=deepcopy(s['rows']);b['forwarders']=deepcopy(s['forwarders'])
    with pytest.raises(ValueError): classify(s,b)
