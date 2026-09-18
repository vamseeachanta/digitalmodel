"""Geometry, traction and output isolation checks for solid vessel decks."""
import math
import numpy as np
import pytest
from digitalmodel.asset_integrity.assessment.example_vessel_solid import build_model, render_deck, write_case


@pytest.fixture(scope='module')
def model():
    return build_model('C', 1.5, local_pitch=100, far_pitch=400)


def test_hex_orientation_and_inner_face(model):
    nodes = {n[0]: np.array(n[1:]) for n in model['nodes']}
    for el in model['elements']:
        a,b,c,d,e,f,g,h = [nodes[n] for n in el['nodes']]
        jac = np.column_stack(((b+c+f+g-a-d-e-h)/8,
                               (c+d+g+h-a-b-e-f)/8,
                               (e+f+g+h-a-b-c-d)/8))
        assert np.linalg.det(jac) > 0
        if el['radial_layer'] == 0:
            normal = np.cross(a-b,d-b)  # face 1: J-I-L-K; outward from material
            centroid = (a+b+c+d)/4
            assert np.dot(normal, np.array([0,centroid[1],centroid[2]])) < 0


def test_positive_jacobians_at_all_integration_points(model):
    signs = np.array([[-1,-1,-1],[1,-1,-1],[1,1,-1],[-1,1,-1],
                      [-1,-1,1],[1,-1,1],[1,1,1],[-1,1,1]])
    xyz = np.array([n[1:] for n in model['nodes']])
    cells = xyz[np.array([e['nodes'] for e in model['elements']])-1]
    for point in signs/math.sqrt(3):
        derivatives = np.empty((8,3))
        for axis in range(3):
            others = [a for a in range(3) if a != axis]
            derivatives[:,axis] = signs[:,axis]*np.prod(1+signs[:,others]*point[others],axis=1)/8
        jacobians = np.einsum('eni,nj->eij',cells,derivatives)
        assert np.all(np.linalg.det(jacobians) > 0)


def test_faceting_and_end_moment_are_bounded(model):
    nmap = {n[0]:n for n in model['nodes']}
    forces = model['end_forces']['right']
    total = sum(f for _,f in forces)
    eccentricity = math.hypot(*(sum(nmap[n][axis]*f for n,f in forces)/total for axis in (2,3)))
    # Nonuniform faceting must not introduce an unintended end bending moment.
    assert eccentricity < 1e-9


def test_inner_radius_and_loads(model):
    nmap = {n[0]: n for n in model['nodes']}
    for el in model['elements']:
        if el['radial_layer'] == 0:
            assert all(math.hypot(*nmap[n][2:]) == pytest.approx(1000) for n in el['nodes'][:4])
    thrust = 1.5 * math.pi * 1000**2
    assert sum(f for _,f in model['end_forces']['left']) == pytest.approx(-thrust)
    assert sum(f for _,f in model['end_forces']['right']) == pytest.approx(thrust)
    assert len(set(round(f,3) for _,f in model['end_forces']['left'])) > 2


def test_deck(model):
    deck = render_deck(model)
    assert 'ET,1,SOLID185' in deck and 'SHELL,' not in deck
    assert 'RSYS,0' in deck and 'stress_solid' in deck
    assert '/OUTPUT,stress_nodes,txt\nPRESOL,S,COMP\n/OUTPUT' in deck
    inner = [e for e in model['elements'] if e['radial_layer'] == 0]
    assert deck.count('SFE,') == len(inner)
    assert ',1,PRES,,1.5' in deck
    assert 'D,1,UZ,0' in deck
    assert 'CE,NEXT,0,1,UZ,' not in deck


def test_exclusive_write(tmp_path):
    out = write_case(tmp_path,'solid','intact',1.5,local_pitch=400,far_pitch=400)
    saved = (out/'vessel.inp').read_bytes()
    with pytest.raises(FileExistsError):
        write_case(tmp_path,'solid','D',1.5)
    assert (out/'vessel.inp').read_bytes() == saved


def test_link_rejected(tmp_path):
    link = tmp_path/'alias'
    try:
        link.symlink_to(tmp_path,target_is_directory=True)
    except OSError:
        pytest.skip('symlink creation unavailable')
    with pytest.raises(ValueError):
        write_case(link,'solid','intact',1.5)


@pytest.mark.parametrize('layers',[0,1.5,True])
def test_bad_layers(layers):
    with pytest.raises(ValueError):
        build_model('C',1.5,radial_layers=layers)
