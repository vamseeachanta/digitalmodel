"""Bounded native PLANE182 shape-summary gate; no physical qualification."""
import re


def assess_shape_output(text, expected_elements):
    """Require a complete all-element, default-limit summary with no warnings."""
    if type(expected_elements) is not int or expected_elements <= 0:
        raise ValueError('expected element count must be a positive integer')
    if text.count('SHAPE TESTING SUMMARY') != 1:
        raise ValueError('missing or duplicate shape summary')
    prefix, summary = text.split('SHAPE TESTING SUMMARY')
    states = re.findall(r'ELEMENT SHAPE CHECKING IS ([^\r\n]+)', prefix)
    if not states or any(s.strip() != 'ON WITH DEFAULT LIMITS' for s in states):
        raise ValueError('default shape checking not established')
    if 'FOR ALL SELECTED ELEMENTS' not in summary:
        raise ValueError('all-element shape summary missing')
    counts = re.findall(r'Element count\s+(\d+)\s+PLANE182', summary)
    if counts != [str(expected_elements)]:
        raise ValueError('shape element count differs from native mesh')
    for label in ('Aspect Ratio', 'Parallel Deviation', 'Maximum Angle', 'Jacobian Ratio', 'Any'):
        rows = re.findall(r'^\s*'+label+r'\s+(\d+)\s+(\d+)\s+(\d+)\s+[\d.]+\s*%', summary, re.M)
        if rows != [(str(expected_elements), '0', '0')]:
            raise ValueError(f'incomplete or failed native shape check: {label}')
    for level in ('WARNING', 'ERROR'):
        totals = re.findall(r'NUMBER OF '+level+r'\s+MESSAGES ENCOUNTERED=\s*(\d+)', text)
        if totals != ['0'] or re.search(r'\*\*\*\s*'+level+r'\s*\*\*\*', text):
            raise ValueError(f'native {level.lower()} log is not clean')
    return {'shape_gate_passed': True, 'elements_tested': expected_elements,
            'default_shape_limits': True, 'warning_count': 0, 'error_count': 0,
            'native_qualification_complete': False}
