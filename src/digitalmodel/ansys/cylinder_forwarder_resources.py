"""Bounded PE metadata and declared-path observations; no image loading."""
import hashlib
from copy import deepcopy
import os
from pathlib import Path, PureWindowsPath
import stat
import struct


class PEReader:
    def __init__(self, raw):
        if not isinstance(raw, bytes) or not 64 <= len(raw) <= 16 * 1024**2:
            raise ValueError('bounded PE bytes required')
        self.raw, self.spans, self.visited = raw, [], set()
        if raw[:2] != b'MZ':
            raise ValueError('missing DOS signature')
        pe = self.unpack('<I', 60)[0]
        if raw[pe:pe+4] != b'PE\0\0':
            raise ValueError('missing PE signature')
        count = self.unpack('<H', pe+6)[0]
        size = self.unpack('<H', pe+20)[0]
        optional = pe+24
        magic = self.unpack('<H', optional)[0]
        if magic not in (0x10b, 0x20b) or not 1 <= count <= 96:
            raise ValueError('unsupported PE header')
        delta = 96 if magic == 0x10b else 112
        if size < delta+24 or self.unpack('<I', optional+delta-4)[0] < 3:
            raise ValueError('missing resource data directory')
        self.sections = [self.unpack('<IIII', optional+size+40*i+8) for i in range(count)]
        rva, self.resource_size = self.unpack('<II', optional+delta+16)
        self.base = self.offset(rva, self.resource_size)

    def unpack(self, fmt, pos):
        if pos < 0 or pos+struct.calcsize(fmt) > len(self.raw):
            raise ValueError('out-of-file PE span')
        return struct.unpack_from(fmt, self.raw, pos)

    def offset(self, rva, size):
        matches = [raw+rva-va for virtual,va,length,raw in self.sections
                   if va <= rva and rva+size <= va+length]
        if len(matches) != 1 or size < 1 or matches[0]+size > len(self.raw):
            raise ValueError('ambiguous or invalid PE section span')
        return matches[0]

    def span(self, relative, size):
        if relative < 0 or size < 1 or relative+size > self.resource_size:
            raise ValueError('out-of-resource span')
        end = relative+size
        if any(relative < b and a < end for a,b in self.spans):
            raise ValueError('overlapping resource records')
        self.spans.append((relative,end))
        return self.base+relative

    def entries(self, relative):
        if relative in self.visited:
            raise ValueError('cyclic resource directory')
        self.visited.add(relative)
        pos = self.base+relative
        named, ids = self.unpack('<HH', pos+12)
        if not 1 <= named+ids <= 256:
            raise ValueError('resource entry count outside bounds')
        self.span(relative,16+8*(named+ids))
        result, names = [], set()
        for i in range(named+ids):
            name,target = self.unpack('<II',pos+16+i*8)
            if name & 0x80000000:
                start = name & 0x7fffffff
                length = self.unpack('<H',self.base+start)[0]
                if not 1 <= length <= 4096:
                    raise ValueError('resource name length outside bounds')
                at = self.span(start,2+length*2)
                name = self.raw[at+2:at+2+length*2].decode('utf-16le')
            if name in names:
                raise ValueError('duplicate resource key')
            names.add(name)
            result.append((name,target))
        return result

    def walk(self, relative=0, keys=()):
        if len(keys) >= 3:
            raise ValueError('resource depth exceeds three')
        found = {}
        for name,target in self.entries(relative):
            path = keys+(name,)
            if target & 0x80000000:
                found.update(self.walk(target & 0x7fffffff,path))
            else:
                if len(path) != 3:
                    raise ValueError('resource leaf depth differs')
                at = self.span(target,16)
                rva,size,_,_ = self.unpack('<IIII',at)
                position = self.offset(rva,size)
                self.span(position-self.base,size)
                found[path] = self.raw[position:position+size]
        return found


def parse_forwarder_pe(raw):
    try:
        rows = PEReader(raw).walk()
        values = {}
        for name in ('UV_TRAMPOLINE_KIND','UV_PYTHON_PATH'):
            matches = [value for key,value in rows.items() if key[:2] == (10,name)]
            if len(matches) != 1:
                raise ValueError('unique trampoline resources required')
            values[name] = matches[0]
        kind,target = values['UV_TRAMPOLINE_KIND'],values['UV_PYTHON_PATH']
        if not 1 <= len(kind) <= 16 or kind[0] != 2 or not 1 <= len(target) <= 4096:
            raise ValueError('unsupported resource kind or target length')
        literal = target.decode('utf-8')
        if '\0' in literal or not PureWindowsPath(literal).is_absolute():
            raise ValueError('absolute non-NUL interpreter target required')
        return dict(resource_kind_hex=kind.hex(),embedded_target_literal=literal)
    except (UnicodeError,struct.error) as exc:
        raise ValueError('invalid PE resource encoding') from exc


def path_identity(path, alias=None, allow_hardlinks=False):
    if type(allow_hardlinks) is not bool or alias is not None and allow_hardlinks:
        raise ValueError('alias/interpreter targets retain strict link identity')
    path = Path(path).absolute()
    permitted = Path(alias).absolute().parent if alias else None
    junction = None
    for part in reversed((path,*path.parents)):
        info = part.lstat()
        if stat.S_ISLNK(info.st_mode) or getattr(info,'st_file_attributes',0) & 0x400:
            if part != permitted or not part.is_junction() or junction is not None:
                raise ValueError('undeclared redirected path')
            raw = os.readlink(part)
            if not raw or len(raw) > 4096:
                raise ValueError('invalid raw junction target')
            junction = dict(junction_path=str(part),device=info.st_dev,
                            inode=info.st_ino,raw_link_target=raw)
    resolved = path.resolve(strict=True)
    if resolved != path:
        # The permitted alias is one junction only, never a second redirected target.
        for part in (resolved,*resolved.parents):
            if part.is_symlink() or part.is_junction():
                raise ValueError('redirected resolved target')
    info = path.stat()
    if not stat.S_ISREG(info.st_mode) or info.st_nlink < 1 or not allow_hardlinks and info.st_nlink != 1:
        raise ValueError('regular unaliased file required')
    return dict(resolved_path=str(resolved),device=info.st_dev,inode=info.st_ino,
                bytes=info.st_size,mtime_ns=info.st_mtime_ns,link_count=info.st_nlink,alias_identity=junction)


class DeclaredReads:
    def __init__(self):
        self.cache = {}

    def read(self, path, alias=None, maximum=None, allow_hardlinks=False):
        key = str(path)
        if key not in self.cache:
            identity = path_identity(path,alias,allow_hardlinks)
            if maximum is not None and identity['bytes'] > maximum:
                raise ValueError('file exceeds bounded read')
            raw = Path(path).read_bytes()
            if len(raw) != identity['bytes'] or path_identity(path,alias,allow_hardlinks) != identity:
                raise ValueError('file identity changed during read')
            sha = hashlib.sha256(raw).hexdigest()
            self.cache[key] = (raw,identity,sha,alias,allow_hardlinks)
        raw,identity,sha,declared_alias,declared_links = self.cache[key]
        if (declared_alias != alias or declared_links != allow_hardlinks
                or maximum is not None and len(raw) > maximum):
            raise ValueError('cached declaration or read bound differs')
        return raw,dict(identity,sha256=sha)

    def verify(self):
        for path,(_,identity,sha,alias,allow_hardlinks) in self.cache.items():
            if (path_identity(path,alias,allow_hardlinks) != identity
                    or hashlib.sha256(Path(path).read_bytes()).hexdigest() != sha
                    or path_identity(path,alias,allow_hardlinks) != identity):
                raise ValueError('declared path changed after collection')

    def evidence(self):
        """Return immutable-by-copy metadata, never retained executable bytes."""
        return [dict(declared_path=path,**deepcopy(identity),sha256=sha,
                     declared_alias=str(alias) if alias is not None else None,
                     allow_hardlinks=allow_hardlinks)
                for path,(_,identity,sha,alias,allow_hardlinks) in sorted(self.cache.items())]


def observe_forwarder(candidate, rows, cache):
    parent,child = rows[candidate['parent_pid']],rows[candidate['child_pid']]
    raw,_ = cache.read(parent['executable_path'],maximum=16*1024**2)
    result = parse_forwarder_pe(raw)
    target = str(PureWindowsPath(result['embedded_target_literal']))
    alias = candidate['declared_target_alias']
    if target != str(PureWindowsPath(alias)):
        raise ValueError('declared alias differs from PE target')
    _,observed = cache.read(target,alias=alias)
    _,resolved = cache.read(observed['resolved_path'],alias=alias)
    if resolved['sha256'] != observed['sha256']:
        raise ValueError('declared alias and resolved bytes differ')
    if observed['alias_identity'] is None:
        raise ValueError('exact declared interpreter junction required')
    if (child['parent_pid'] != parent['pid'] or parent['argv'][1:] != child['argv'][1:]
            or child['executable_resolved_path'] != observed['resolved_path']
            or child['executable_sha256'] != observed['sha256']
            or str(PureWindowsPath(child['executable_path'])) not in
                (target,str(PureWindowsPath(observed['resolved_path'])))):
        raise ValueError('forwarded child identity differs')
    return dict(parent_pid=parent['pid'],parent_creation_time=parent['creation_time'],
        child_pid=child['pid'],child_creation_time=child['creation_time'],
        launcher_sha256=parent['executable_sha256'],**result,target_alias_path=target,
        alias_identity=observed['alias_identity'],resolved_target_path=observed['resolved_path'],
        resolved_target_sha256=observed['sha256'])
