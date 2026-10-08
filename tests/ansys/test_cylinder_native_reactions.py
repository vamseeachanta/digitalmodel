"""Native reaction-layout fixture with blank unconstrained FX, never invented zero."""
from decimal import Decimal
import pytest
from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_validation import _reaction_listing
from tests.ansys.test_cylinder_native_listings import page


def reaction():
    prefix=page().replace(b'NODAL STRESS',b'TOTAL REACTION SOLUTION').replace(
        b'THE FOLLOWING X,Y,Z VALUES ARE IN GLOBAL COORDINATES',
        b'THE FOLLOWING X,Y,Z SOLUTIONS ARE IN THE GLOBAL COORDINATE SYSTEM')
    row=b' '+f'{1:8d}'.encode()+b' '+b' '*24+b' 0.0000000000000000E+000\n'
    return (b'PRINT F REACTION SOLUTIONS PER NODE\n'+prefix+b'NODE FX FY\n'+row+
            b'TOTAL VALUES\n VALUE   0.0000000000000000E+000 0.0000000000000000E+000\n')


def test_blank_unconstrained_fx_does_not_hide_fy():
    _reaction_listing(reaction(),{1:Decimal(0)})


@pytest.mark.parametrize('damage',['missing_fy','present_fx','nonzero_fy','wrong_node','duplicate','total','frame'])
def test_unknown_or_mismatching_reaction_refuses(damage):
    raw=reaction()
    if damage=='missing_fy':raw=raw.replace(b' 0.0000000000000000E+000\n',b' '*24+b'\n',1)
    elif damage=='present_fx':raw=raw.replace(b' '*24,b' 1.0000000000000000E+000',1)
    elif damage=='nonzero_fy':raw=raw.replace(b'0.0000000000000000E+000',b'1.0000000000000000E+000',1)
    elif damage=='wrong_node':raw=raw.replace(f'{1:8d}'.encode(),f'{2:8d}'.encode(),1)
    elif damage=='duplicate':
        row=next(r for r in raw.splitlines() if len(r)==58)
        raw=raw.replace(row+b'\n',row+b'\n'+row+b'\n')
    elif damage=='total':raw=raw.replace(b' VALUE   0.',b' VALUE   1.')
    else:raw=raw.replace(b'GLOBAL COORDINATE',b'LOCAL COORDINATE')
    with pytest.raises(EvidenceError):_reaction_listing(raw,{1:Decimal(0)})
