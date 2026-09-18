"""Coherent point tensors and fail-closed PRESOL parsing."""
import pytest
from digitalmodel.asset_integrity.assessment.example_vessel_presol import read_presol


def fixture_text(kind='shell',wrapped=True):
    count = 4 if kind == 'shell' else 8
    header = 'SHELL RESULTS FOR TOP/BOTTOM ALSO MID WHERE APPROPRIATE\n' if kind == 'shell' else ''
    text = 'ELEMENT= 1 '+('SHELL181' if kind == 'shell' else 'SOLID185')+'\n'+header+'NODE SX SY SZ SXY SYZ SXZ\n'
    for surface in range(3 if kind == 'shell' else 1):
        for node in range(1,count+1):
            values = ''.join(f'{v:+.12E}' for v in (surface+1, -2,3,4,5,6))
            if wrapped:
                values = values[:-19]+'\n '+values[-19:]
            text += f'{node} {values}\n'
    return text, {'elements':[{'element_id':1,'nodes':list(range(1,count+1))}]}


@pytest.mark.parametrize('kind',['shell','solid'])
@pytest.mark.parametrize('wrapped',[True,False])
def test_complete_tensors(tmp_path,kind,wrapped):
    text,model=fixture_text(kind,wrapped)
    path=tmp_path/'stress.txt';path.write_text(text)
    result=read_presol(path,model,kind)
    if kind=='shell':
        assert result['top'][0]['sx']==1
        assert result['bottom'][0]['sx']==2
        assert result['mid'][0]['sx']==3
        result=result['mid']
    assert result[0]['sxz']==6 and result[0]['sy']==-2
    assert result[0]['element_id']==1 and result[0]['node_id']==1


@pytest.mark.parametrize('damage',['truncate','duplicate','nonfinite','missing','order'])
def test_reject_bad(tmp_path,damage):
    text,model=fixture_text(wrapped=False)
    if damage=='truncate': text=text.rsplit('\n',2)[0]
    if damage=='duplicate': text+=text
    if damage=='nonfinite': text=text.replace('+6.000000000000E+00','NaN',1)
    if damage=='missing': model['elements'].append({'element_id':2,'nodes':[1,2,3,4]})
    if damage=='order': text=text.replace('1 +','3 +',1)
    path=tmp_path/'stress.txt';path.write_text(text)
    with pytest.raises(ValueError): read_presol(path,model)


def test_explicit_surface_order(tmp_path):
    text,model=fixture_text(wrapped=False)
    lines=text.splitlines()
    blocks=[lines[3+i*4:3+(i+1)*4] for i in range(3)]
    reordered=lines[:3]+['MID']+blocks[2]+['BOTTOM']+blocks[1]+['TOP']+blocks[0]
    path=tmp_path/'stress.txt';path.write_text('\n'.join(reordered))
    result=read_presol(path,model)
    assert result['top'][0]['sx']==1
    assert result['mid'][0]['sx']==3


def test_eless_fortran_exponent_rejected(tmp_path):
    text,model=fixture_text('solid',False)
    text=text.replace('+1.000000000000E+00-2.000000000000E+00','1.0000+100',1)
    path=tmp_path/'stress.txt';path.write_text(text)
    with pytest.raises(ValueError): read_presol(path,model,'solid')


def test_unlabeled_shell_without_format_header_rejected(tmp_path):
    text,model=fixture_text(wrapped=False)
    text=text.replace('SHELL RESULTS FOR TOP/BOTTOM ALSO MID WHERE APPROPRIATE\n','')
    path=tmp_path/'stress.txt';path.write_text(text)
    with pytest.raises(ValueError): read_presol(path,model)
