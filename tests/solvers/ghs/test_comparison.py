"""Rational oracle and synthetic normalized evidence, not native GHS parsing."""
import copy
from fractions import Fraction

import pytest

from digitalmodel.solvers.ghs import comparison as cmp
from .test_contracts import packet


def normalized():
    expected=cmp.box_oracle()
    rows=[]
    for row in expected:
        values={k:format(float(v),'.12f').rstrip('0').rstrip('.') for k,v in row.items()}
        refs={k:{'artifact_sha256':'b'*64,'line':i+1,'resolution':'0.000001',
                  'transformation_reference':'synthetic-identity-map'} for i,k in enumerate(cmp.QUANTITIES)}
        rows.append({'depth_m':values.pop('depth_m'),'values':values,'references':refs})
    evidence={'schema_version':1,'evidence_kind':'synthetic_normalized',
        'execution_receipt':{'reference':'synthetic-no-execution','sha256':'a'*64},
        'qualification_profile':{'reference':'synthetic-not-native-qualification','sha256':'c'*64},
        'units_frame':dict(cmp.UNITS_FRAME),
        'artifacts':[{'sha256':'b'*64,'byte_count':1000}]}
    return rows,evidence


def test_independent_exact_oracle():
    rows=cmp.box_oracle()
    for i,t in enumerate((1,2,3)):
        r=rows[i]
        assert r['volume_m3']==r['mass_t']==200*t
        assert r['waterplane_m2']==200
        assert r['lcb_m']==r['lcf_m']==10
        assert r['tcb_m']==r['tcf_m']==0
        assert r['kb_m']==Fraction(t,2)
        assert r['bmt_m']==Fraction(25,3*t)
        assert r['bml_m']==Fraction(100,3*t)
    assert cmp.box_oracle(density_kg_m3='500')[0]['mass_t']==100


def test_synthetic_comparison_never_claims_native_execution():
    rows,evidence=normalized();result=cmp.compare_normalized(packet(),rows,evidence)
    assert result['state']=='comparison_passed_unreviewed'
    assert result['evidence_kind']=='synthetic_normalized'
    assert result['licensed_execution_verified'] is False
    assert len(result['comparisons'])==30
    assert 'synthetic' in result['conclusion'].lower()


@pytest.mark.parametrize('mutation',['missing','duplicate','float','nan','wrong_frame','missing_ref',
    'wrong_hash','bad_line','coarse','boolean','depth','extra'])
def test_normalization_evidence_rejects(mutation):
    rows,evidence=normalized()
    if mutation=='missing':rows.pop()
    elif mutation=='duplicate':rows[1]=copy.deepcopy(rows[0])
    elif mutation=='float':rows[0]['values']['mass_t']=200.0
    elif mutation=='nan':rows[0]['values']['mass_t']='NaN'
    elif mutation=='wrong_frame':evidence['units_frame']['x']='aft'
    elif mutation=='missing_ref':del rows[0]['references']['bmt_m']
    elif mutation=='wrong_hash':rows[0]['references']['mass_t']['artifact_sha256']='0'*64
    elif mutation=='bad_line':rows[0]['references']['mass_t']['line']=True
    elif mutation=='coarse':rows[0]['references']['kb_m']['resolution']='1'
    elif mutation=='boolean':evidence['licensed']=True
    elif mutation=='depth':rows[0]['depth_m']='1.001'
    else:rows[0]['values']['gm_m']='5'
    with pytest.raises(ValueError):cmp.compare_normalized(packet(),rows,evidence)


def test_tolerance_boundary_and_failed_conclusion():
    rows,evidence=normalized();rows[0]['values']['tcb_m']='0.005'
    assert cmp.compare_normalized(packet(),rows,evidence)['state']=='comparison_passed_unreviewed'
    rows[0]['values']['tcb_m']='0.005001'
    result=cmp.compare_normalized(packet(),rows,evidence)
    assert result['state']=='comparison_failed'
    assert result['failure_reasons']
    assert 'agree' not in result['conclusion'].lower()
