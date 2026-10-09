"""Closed canonical records for the source-neutral canary."""
import hashlib
import json
import re
from fractions import Fraction

MAX_BYTES = 8 * 1024 * 1024


def fail():
    raise ValueError('invalid, noncanonical or unqualified canary evidence')


def shape(value, keys):
    if type(value) is not dict or len(value) != len(keys) or set(value) != set(keys):
        fail()


def text(value, limit=128):
    if type(value) is not str or len(value)>limit or not value.strip() or not value.isprintable():
        fail()
    return value


def sha(value):
    if type(value) is not str or re.fullmatch('[0-9a-f]{64}',value) is None:
        fail()
    return value


def integer(value, low, high):
    if type(value) is not int or not low<=value<=high:
        fail()
    return value


def decimal(value, *, positive=False):
    if type(value) is not str or len(value)>64 or re.fullmatch(r'-?(0|[1-9][0-9]*)(\.[0-9]+)?',value) is None:
        fail()
    number=Fraction(value)
    if (value.startswith('-') and number==0) or abs(number)>10**12 or (positive and number<=0):
        fail()
    normalized=value.rstrip('0').rstrip('.') if '.' in value else value
    return normalized


def encoded(value):
    return json.dumps(value,sort_keys=True,separators=(',',':'),ensure_ascii=False,allow_nan=False).encode('utf-8')


def digest(value):
    data=encoded(value)
    if len(data)>MAX_BYTES:
        fail()
    return hashlib.sha256(data).hexdigest()


def bytes_hash(value):
    if type(value) is not bytes or not 0<len(value)<=MAX_BYTES:
        fail()
    return hashlib.sha256(value).hexdigest()


def reference(value):
    shape(value,{'reference','sha256'})
    return {'reference':text(value['reference'],256),'sha256':sha(value['sha256'])}
