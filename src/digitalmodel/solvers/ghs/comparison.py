"""Independent exact box oracle; normalized inputs are synthetic assertions only."""
from fractions import Fraction
from ._canonical import decimal, digest, fail, integer, reference, sha, shape, text
from .contracts import UNITS_FRAME, TOLERANCES, validate_prepared

QUANTITIES=('volume_m3','mass_t','waterplane_m2','lcb_m','lcf_m','tcb_m','tcf_m','kb_m','bmt_m','bml_m')


def box_oracle(*,density_kg_m3='1000'):
    rho=Fraction(decimal(density_kg_m3,positive=True));length=Fraction(20);beam=Fraction(10)
    rows=[]
    for depth in map(Fraction,(1,2,3)):
        volume=length*beam*depth
        rows.append({'depth_m':depth,'volume_m3':volume,'mass_t':rho*volume/1000,
            'waterplane_m2':length*beam,'lcb_m':length/2,'lcf_m':length/2,
            'tcb_m':Fraction(0),'tcf_m':Fraction(0),'kb_m':depth/2,
            'bmt_m':(length*beam**3/12)/volume,'bml_m':(beam*length**3/12)/volume})
    return rows


def oracle_material():
    return {'oracle_version':1,'box_m':['20','10','4'],'density_kg_m3':'1000',
            'rows':[{k:str(v) for k,v in row.items()} for row in box_oracle()]}


def tolerance(key,expected):
    floor=key if key in {'volume_m3','mass_t','waterplane_m2'} else 'coordinate_m'
    return max(Fraction(TOLERANCES['relative'])*abs(expected),Fraction(TOLERANCES[floor]))


def evidence_record(evidence):
    shape(evidence,{'schema_version','evidence_kind','execution_receipt','qualification_profile','units_frame','artifacts'})
    integer(evidence['schema_version'],1,1)
    if evidence['evidence_kind']!='synthetic_normalized' or evidence['units_frame']!=UNITS_FRAME:
        fail()
    shape(evidence['units_frame'],UNITS_FRAME)
    reference(evidence['execution_receipt']);reference(evidence['qualification_profile'])
    if type(evidence['artifacts']) is not list or not 1<=len(evidence['artifacts'])<=4:fail()
    artifacts={};total=0
    for artifact in evidence['artifacts']:
        shape(artifact,{'sha256','byte_count'})
        key=sha(artifact['sha256']);size=integer(artifact['byte_count'],1,8*1024*1024)
        if key in artifacts:fail()
        total+=size;artifacts[key]=size
    if total>32*1024*1024:fail()
    return artifacts


def row_record(row,artifacts):
    shape(row,{'depth_m','values','references'})
    depth=decimal(row['depth_m'],positive=True)
    if depth!=row['depth_m'] or depth not in ('1','2','3'):fail()
    shape(row['values'],QUANTITIES);shape(row['references'],QUANTITIES)
    for key in QUANTITIES:
        value=decimal(row['values'][key])
        if value!=row['values'][key]:fail()
        ref=row['references'][key]
        shape(ref,{'artifact_sha256','line','resolution','transformation_reference'})
        artifact=sha(ref['artifact_sha256'])
        if artifact not in artifacts:fail()
        integer(ref['line'],1,artifacts[artifact]+1)
        resolution=decimal(ref['resolution'],positive=True)
        if resolution!=ref['resolution']:fail()
        text(ref['transformation_reference'],256)
    return depth


def compare_normalized(packet,rows,evidence):
    validate_prepared(packet);artifacts=evidence_record(evidence)
    if type(rows) is not list or len(rows)!=3:fail()
    by_depth={}
    for row in rows:
        depth=row_record(row,artifacts)
        if depth in by_depth:fail()
        by_depth[depth]=row
    if set(by_depth)!=set(packet['depths_m']):fail()
    comparisons=[];failures=[]
    for expected in box_oracle():
        depth=str(expected['depth_m']);row=by_depth[depth]
        for key in QUANTITIES:
            actual=Fraction(row['values'][key]);tol=tolerance(key,expected[key])
            if Fraction(row['references'][key]['resolution'])>tol:fail()
            passed=abs(actual-expected[key])<=tol
            comparisons.append({'depth_m':depth,'quantity':key,'actual':row['values'][key],
                'expected_rational':str(expected[key]),'tolerance_rational':str(tol),
                'passed':passed,'source_reference':dict(row['references'][key])})
            if not passed:failures.append(depth+':'+key)
    return {'schema_version':1,'state':'comparison_failed' if failures else 'comparison_passed_unreviewed',
        'packet_sha256':packet['packet_sha256'],'runtime_profile_sha256':packet['runtime_profile_sha256'],
        'evidence_kind':'synthetic_normalized','licensed_execution_verified':False,
        'execution_receipt':dict(evidence['execution_receipt']),
        'qualification_profile':dict(evidence['qualification_profile']),
        'evidence_sha256':digest(evidence),'normalized_rows_sha256':digest([by_depth[d] for d in ('1','2','3')]),
        'comparisons':comparisons,'failure_reasons':failures,
        'conclusion':('Synthetic normalized assertions fall outside the fixed box tolerance.' if failures else
                      'Synthetic normalized assertions agree with the ideal-box oracle within the fixed tolerance.'),
        'limitations':['No GHS execution, native parsing, evidence truth or engineering approval established.',
                       'No equilibrium, GM/GZ, damage or real-vessel suitability conclusion.']}
