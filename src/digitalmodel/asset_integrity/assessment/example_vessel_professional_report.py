"""Controlled engineering presentation of separately verified example results."""
import re
from itertools import count
from . import example_vessel_completion_report as base
from .example_vessel_report_style import STYLE, PRINT_SCRIPT

SECTIONS = [('summary','1. Summary'),('introduction','2. Introduction and nomenclature'),
    ('basis','3. Basis for assessment'),('methodology','4. Assessment methodology and thickness data'),
    ('level12','5. Levels 1 and 2 assessment'),('fea-comparisons','6. Finite-element numerical comparisons'),
    ('verification','7. Verification and limitations'),('repair','8. Repair assessment'),
    ('conclusions','9. Conclusions and recommendations'),('references','10. References and revision history'),
    ('appendix-a','Appendix A. Detailed case comparisons'),('appendix-b','Appendix B. Evidence register')]


def _section(index, content):
    identity, title = SECTIONS[index]
    return f'<section id="{identity}"><h2>{title}</h2>{content}</section>'


def _inner(content):
    return re.sub(r'^<h2>.*?</h2>', '', content, count=1)


def _table(headers, rows, caption):
    value = base._table(headers, rows, caption)
    return value.replace('<table>', '<div class="table-wrap"><table>', 1).replace('</table>', '</table></div>', 1)


def _wrap_tables(content):
    return content.replace('<table>','<div class="table-wrap"><table>').replace('</table>','</table></div>')


def _front(payload):
    doc = payload.get('document', {})
    revision, date = base._text(doc.get('revision', '01')), base._text(doc.get('date', 'Not assigned'))
    cover = ('<header class="cover"><div class="brand">AceEngineer</div>'
        '<p class="eyebrow">Asset integrity · fitness-for-service assessment</p>'
        '<h1>Refinery pressure vessel<br>Fitness-for-service assessment</h1>'
        '<p class="subtitle">Four independent areas of local metal loss: assessment, reduced-pressure '
        'operation and welded-insert repair.</p>'
        f'<p class="cover-meta">Revision {revision} · {date}<br>Simulated data based on measurements · INTERNAL TECHNICAL REVIEW</p>'
        '<p>Illustrative measurement-style thickness grids; all values are assumed. '
        'No field measurements were supplied.</p></header>')
    controls = [('Document','Pressure-vessel four-area FFS assessment'),('Revision / date',f'{revision} / {date}'),
        ('Prepared by','AceEngineer'),('Purpose','Engineering assessment using simulated data based on an assumed measurement grid'),
        ('Assessment basis','API 579-1/ASME FFS-1:2007; assumed material properties at '+
         base._number(payload.get('basis',{}).get('assessment_temperature_c'))+' °C'),
        ('Issue status','Internal technical review; formal engineering sign-off not established')]
    control = '<div class="frontmatter doc-control"><h2>Document control</h2>' + _table(
        ('Field','Document particulars'), controls, 'Document-control record. No actual-asset approval is implied.')
    links = ''.join(f'<li><a href="#{key}">{title}</a></li>' for key,title in SECTIONS)
    return cover + control + '</div><nav class="frontmatter" aria-label="Contents"><h2>Contents</h2><ol>'+links+'</ol></nav>'


def _summary(payload):
    paragraphs = ''.join(f'<p>{base._text(v)}</p>' for v in payload.get('conclusions', [])[:4])
    decisions = base._decisions(payload).replace('<h2>', '<h3>').replace('</h2>', '</h3>')
    decisions = decisions.replace('Decision matrix.', 'Table 1.1. Area decision matrix.')
    return ('<p class="notice"><strong>Simulated data based on measurements.</strong> The '
        'measurement grid is assumed; no field measurements were supplied. Geometry, damage and material '
        'inputs remain assumed. The following findings are conditional numerical comparisons; no certified '
        'equipment rating or completed physical repair is established.</p>' + paragraphs + _wrap_tables(decisions))


def _introduction():
    rows = [('FFS','Fitness-for-service assessment'),('LTA','Local thin area; smooth external metal loss'),
        ('CTP','Critical thickness profile retaining spatial order'),('RSF / RSFa','Remaining strength factor / allowable RSF'),
        ('FEA','Finite-element analysis'),('SCL','Stress-classification line through the wall'),
        ('MAWP','Maximum allowable working pressure; conditional when based on assumed inputs'),
        ('NDE / PWHT','Nondestructive examination / postweld heat treatment')]
    return ('<p>A cylindrical refinery pressure vessel is assessed to demonstrate four independent '
        'dispositions: Level 1 acceptance, Level 2 acceptance, reduced-pressure elastic assessment '
        'and restoration by a flush insert. The progression demonstrates how additional analysis '
        'or repair changes the disposition for the simulated assessment.</p><p>The four damage areas are '
        'assumed far apart and noninteracting. No inspection data from an operating vessel is used. '
        'The assessment excludes crack-like damage, cyclic service, creep and external pressure; '
        'supplementary loads are assumed negligible.</p><h3>2.1 Nomenclature</h3>' + _table(
            ('Term','Definition'),rows,'Table 2.1. Nomenclature and abbreviations.'))


def _basis(payload):
    b = payload.get('basis', {}); m = b.get('material', {})
    n = base._number
    diameter = 2*b['inside_radius_mm'] if isinstance(b.get('inside_radius_mm'), (int,float)) else None
    geometry = [('Inside diameter',n(diameter),'mm'),('Tangent length',n(b.get('shell_tangent_length_mm')),'mm'),
        ('Nominal shell thickness',n(b.get('nominal_mm')),'mm'),('Measurement uncertainty deduction',n(b.get('uncertainty_mm')),'mm'),
        ('Future metal-loss deduction',n(b.get('future_loss_mm')),'mm'),('Target pressure',n(b.get('target_pressure_mpa_g')),'MPa(g)'),
        ('Reduced pressure evaluated',n(b.get('executed_reduced_pressure_mpa_g')),'MPa(g)'),
        ('Assessment temperature',n(b.get('assessment_temperature_c')),'°C')]
    h = payload.get('head',{})
    geometry += [('Nominal head thickness',n(h.get('head_nominal_mm')),'mm'),
        ('Internal head height',n(h.get('inside_height_mm')),'mm'),
        ('Assumed head joint efficiency',n(h.get('assumed_joint_efficiency')),'—')]
    material = [('Material','Generic assumed carbon steel','—'),('Elastic modulus',n(m.get('elastic_modulus_mpa')),'MPa'),
        ('Poisson ratio',n(m.get('poisson_ratio')),'—'),('Assumed allowable stress',n(m.get('screening_stress_mpa')),'MPa'),
        ('Assumed yield strength',n(m.get('yield_mpa')),'MPa'),('Executed response','Linear elastic, isotropic','—')]
    content = '<p>The simulated data are based on an assumed measurement grid, with assumed geometry and material properties. '
    content += 'The assumed allowable stress is not established from a qualified material table.</p><div class="two-column"><div>'
    content += '<h3>3.1 Geometry and loading</h3>'+_table(('Parameter','Value','Unit'),geometry,'Table 3.1. Assumed vessel basis.')
    content += '</div><div><h3>3.2 Material model</h3>'+_table(('Property','Value','Unit'),material,
        'Table 3.2. Assumed properties at '+n(b.get('assessment_temperature_c'))+' °C.')+'</div></div>'
    rows = [(a['area_id'],n(a['centre_x_mm']),n(a['theta_deg']),n(a['axial_extent_mm']),
             n(a['circumferential_extent_mm']),n(a['minimum_mm'])) for a in b.get('areas', [])]
    content += _table(('Area','Axial centre (mm)','Angle (°)','Axial extent (mm)','Arc extent (mm)',
                      'Current minimum (mm)'),rows,'Table 3.3. Assumed damage areas; current values precede deductions.')
    return content + '<h3>3.3 Vessel location schematic</h3>'+_inner(base._location(payload)).replace(
        'Schematic only;', 'Figure 3.1. Schematic only;')


def _method(payload):
    grids = _inner(base._grids(payload)); counter = count(2)
    grids = re.sub('<p class="caption">',lambda _: f'<p class="caption">Figure 4.{next(counter)}. ',grids)
    b = payload.get('basis',{}); n=base._number
    return (f'<p>The current wall grid is reduced once by {n(b.get("uncertainty_mm"))} mm uncertainty and '
        f'{n(b.get("future_loss_mm"))} mm future loss. '
        'Spatial critical profiles are assessed under API 579 Part 5. A lower-level non-pass leads '
        'to a more detailed assessment; it does not establish physical collapse.</p>'+base._method_flow()+
        '<p class="caption">Figure 4.1. Assessment routes for the four noninteracting areas.</p>'
        '<p>Levels 1 and 2 use the allowable RSF stated in Table 5.1 with their applicability and circumferential checks. '
        'The selected Level 3 route uses elastic primary-stress and local-failure comparisons. '
        'A separate native run verifies the reduced-pressure case. Where the selected route does '
        'not meet its criteria, the pressure boundary is restored by an ideal flush insert and reassessed.</p>' + grids)


def _case_table(cases, caption, detailed=False):
    rows = []
    for c in cases:
        demands, limits = c.get('demands_mpa', {}), c.get('limits_mpa', {})
        comparisons = [base._number(demands.get(k))+' / '+base._number(limits.get(k)) for k in base.CRITERIA]
        label = c.get('case_id','?') if detailed else c.get('case','?')
        rows.append((label,base._number(c.get('pressure_mpa')),*comparisons,base._fe_status(c)))
    return _table(('Case','Pressure (MPa)','Membrane demand / limit (MPa)',
        'Membrane + bending demand / limit (MPa)','Trace demand / limit (MPa)','Disposition'),rows,caption)


def _primary_cases(payload):
    rows = payload.get('fea', [])
    selected = [c for c in rows if c.get('formulation')=='solid' and c.get('radial_layers')==4
                and not c.get('case_id','').endswith('-right') and c.get('case') in ('C','D','repair')
                and base._number(c.get('pressure_mpa'))!='NOT EVALUATED']
    return sorted(selected,key=lambda c: ({'C':0,'D':1,'repair':2}.get(c.get('case'),3),
                                         -c['pressure_mpa']))


def _figure(item, number, native=False):
    if not item.get('src'):
        return '<p class="no-image">Image not supplied; no screenshot evidence is inferred.</p>'
    caption = f'Figure {number}. '+str(item.get('caption',''))
    return (f'<figure class="{"native-figure" if native else "analysis-figure"}">'
        f'<a href="{base._url(item["src"])}"><img src="{base._url(item["src"])}" '
        f'alt="{base._text(caption)}"></a><figcaption>{base._text(caption)}</figcaption></figure>')


def _case_images(payload, cases):
    content = ''
    for index,case in enumerate(cases,2):
        identity = case.get('case_id','?'); pressure = base._number(case.get('pressure_mpa'))
        label = 'D — ideal insert repair' if case.get('case')=='repair' else str(case.get('case','?'))
        content += f'<div class="case-block"><h3>6.{index} Area {base._text(label)} — {pressure} MPa</h3>'
        content += '<p class="status">'+base._fe_status(case)+'.</p>'
        supplied = [i for i in payload.get('fea_images',[]) if i.get('case_id')==identity]
        images = [i for i in supplied if _image_bound(i) and i.get('pressure_mpa')==case.get('pressure_mpa')]
        if len(images)!=len(supplied):
            content += '<p class="notice">Supplied native image failed provenance binding and is excluded.</p>'
        for j,item in enumerate(images,1):
            content += _figure(item,f'6.{index}{chr(96+j)}',True)
            content += '<p class="case-note">'+base._text(item.get('averaging','Averaging not supplied'))
            content += '; '+base._text(item.get('deformation','Deformation setting not supplied'))+'.</p>'
        if not images:
            content += '<p class="no-image">Native analysis image not supplied for this case.</p>'
        content += ('<p class="case-note">The native equivalent-stress contour is a raw visualization. '
            'Its peak differs from the through-wall linearized demands and trace criterion in Table 6.1; '
            'the contour maximum alone is not the acceptance check.</p></div>')
    return content


def _image_bound(item):
    required = ('src','case_id','quantity','units','averaging','result_set','deformation')
    hashes = ('source_rst_sha256','sha256')
    return (all(item.get(k) for k in required) and all(
        re.fullmatch('[0-9a-f]{64}',str(item.get(k,''))) is not None for k in hashes))


def _fea(payload):
    cases = _primary_cases(payload)
    content = ('<p><strong>Linear elastic analysis.</strong> Full-cylinder solid models use the '
        'assessed wall geometry, internal pressure and closed-end thrust. The refined comparisons '
        'are identified by case in Table 6.1; discretization is recorded in Section 7. The heads are assessed separately; '
        'their junction stiffness is not represented by the local shell-cylinder model.</p>'
        '<p>All membrane stress is compared with the general primary bound. API 579 Annex B2 '
        'linearization retains all six membrane components and only axial/hoop/in-plane shear '
        'bending components. Raw stress traces are also bounded. The elastic non-pass is not '
        'a demonstrated collapse pressure or proof that every Level 3 route fails.</p>')
    content += _case_table(cases,'Table 6.1. Refined numerical comparisons; each cell gives demand / assumed limit.') if cases else '<p>NOT EVALUATED — No finite-element result supplied for the main comparison. No refined finite-element result supplied.</p>'
    content += '<h3>6.1 Model and mesh</h3>'
    figures = payload.get('figures', [])
    if figures and not payload.get('mesh_images') and 'mesh' in figures[0].get('src',''): content += _figure(figures[0],'6.1')
    for index,item in enumerate(payload.get('mesh_images',[]),1):
        if _image_bound(item): content += _figure(item,f'6.1{chr(96+index)}',True)
    content += ('<p class="case-note">Mesh and boundary qualification is reported in Section 7. '
        'Only images with case and result-file identity are included. '
        'The supplied image register in Appendix B records their plotting settings and source identity.</p>')
    return content + _case_images(payload,cases)


def _verification(payload):
    records = payload.get('verification', {})
    items = records.items() if isinstance(records,dict) else records
    rows = [(k,v) for k,v in items if k not in ('Source integrity','Legal/security scan')]
    head = payload.get('head',{}); n=base._number
    head_table = _table(('Head check','Value'),[
        ('Assessed head thickness (mm)',n(head.get('assessed_head_thickness_mm'))),
        ('Required thickness at target pressure (mm)',n(head.get('required_thickness_mm'))),
        ('Conditional pressure capacity (MPa)',n(head.get('mawp_mpa'))),
        ('Qualification',head.get('scope','NOT EVALUATED'))],
        'Table 7.2. Assumed undamaged 2:1 elliptical heads; nominal pressure check.')
    return (_table(('Verification item','Evidence and criterion'),rows,'Table 7.1. Verification and scope of the numerical evidence.')+
        '<p class="notice">Mesh-shape warnings are retained in the assessment record. Benchmark and '
        'two-resolution agreement support the stated comparisons; they do not establish unrestricted '
        'mesh quality or applicability to unmodelled loads and damage mechanisms.</p>'+head_table)


def _references(payload):
    source = base._url(payload.get('source_href',base.SOURCE))
    return ('<ol class="source-list"><li><a href="'+source+'">API 579-1/ASME FFS-1:2007 — verified source locators</a>. '
        'Part 5; Annex A; Annexes B1 and B2. Licensed original retained at its source.</li>'
        '<li>API 510, ninth edition, June 2006: §8.1.5.2.2 flush inserts; welding, heat treatment and examination provisions. '
        'Source identity and digest are recorded with the verified locators.</li>'
        '<li>ANSYS Mechanical APDL 2026 R1: retained input decks, native results and postprocessing records, Appendix B.</li>'
        '</ol><h3>10.1 Revision history</h3>'+_table(('Revision','Description'),[
            (payload.get('document',{}).get('revision','01'),'Updated simulated-data terminology and PDF edition; native FEA images and case evidence. Numerical basis retained.')],
            'Table 10.1. Presentation revision; no new equipment authorization.'))


def _appendix(payload):
    links = ''.join(f'<li><a href="{base._url(i["href"])}">{base._text(i["label"])}</a></li>'
                    for i in payload.get('evidence_links',[]))
    images = []
    case_ids = {c.get('case_id') for c in payload.get('fea',[])}
    for image in payload.get('fea_images',[])+payload.get('mesh_images',[]):
        if image.get('case_id') not in case_ids or not _image_bound(image): continue
        fields = ('case_id','quantity','units','averaging','result_set','deformation','source_rst_sha256','sha256')
        images.append('<details><summary>'+base._text(image.get('caption','Image provenance'))+'</summary>'+_table(
            ('Record','Value'),[(k,image.get(k,'NOT RECORDED')) for k in fields], 'Native image provenance record.')+'</details>')
    return '<ul class="source-list">'+links+'</ul>'+''.join(images)


def _repair(payload):
    content = '<h3>8.1 Assessment escalation and repair route</h3>'+_inner(base._repair())
    content = content.replace('</svg>','</svg><p class="caption">Figure 8.1. Removal, restoration and reassessment sequence.</p>',1)
    content = content.replace('Proposed developed-surface layout','Figure 8.2. Proposed developed-surface layout',1)
    content = content.replace('<h3>Insert geometry and load path</h3>','<h3>8.2 Insert geometry and load path</h3>',1)
    if any(r.get('area_id')=='D' for r in payload.get('level12',[])):
        content = ('<p>Area D follows the sequence: Level 1 non-pass → Level 2 non-pass → '
            'selected elastic Level 3 non-pass → pressure-boundary repair → reassessment. '
            'The numerical non-passes are documented in Sections 5 and 6.</p>')+content
    return content+''.join('<p>'+base._text(v)+'</p>' for v in payload.get('repair_method',[]))


def render_professional(payload):
    body = _front(payload)+_section(0,_summary(payload))+_section(1,_introduction())
    body += _section(2,_basis(payload))+_section(3,_method(payload))
    screening = _inner(base._level12(payload)).replace('Table 2.', 'Table 5.1.')
    screening += base._circumferential(payload).replace('Table 2b.', 'Table 5.2.')
    body += _section(4,_wrap_tables(screening))+_section(5,_fea(payload))+_section(6,_verification(payload))
    body += _section(7,_repair(payload))
    conclusions = ''.join('<p>'+base._text(v)+'</p>' for v in payload.get('conclusions',[]))
    body += _section(8,conclusions+'<p>Application to operating equipment requires measured wall data, '
        'qualified material/design information, complete load and damage-mechanism checks, and qualified '
        'repair fabrication, examination and testing. The current common operating pressure remains '
        'conditional on this simulated-data basis.</p>')
    body += _section(9,_references(payload))
    body += _section(10,'<details><summary>All retained numerical comparisons</summary>'+_case_table(
        payload.get('fea',[]),'Table A.1. Full case register, including shell diagnostics and verification repeats.',True)+'</details>'+
        '<details><summary>Developed-surface thickness and stress views</summary>'+''.join(
            _figure(item,f'A.{i}') for i,item in enumerate(payload.get('figures',[])[1:],1))+'</details>')
    body += _section(11,_appendix(payload))
    return ('<!doctype html><html lang="en"><head><meta charset="utf-8">'
        '<meta name="viewport" content="width=device-width,initial-scale=1">'
        '<title>Refinery pressure vessel | Engineering assessment | AceEngineer</title>'
        f'<style>{STYLE}</style></head><body><main>{body}<footer>AceEngineer · Pressure-vessel '
        'fitness-for-service assessment · Simulated data based on measurements · Internal technical review</footer></main>'+PRINT_SCRIPT+'</body></html>')
