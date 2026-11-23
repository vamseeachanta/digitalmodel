"""Verify complete payloads on disk or in one exact bare-Git commit."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import subprocess


def _path(value):
    if (not isinstance(value, str) or not value or value.startswith('/')
            or any(c in value for c in ('\\', ':', '\x00'))
            or any(ord(c) < 32 for c in value)
            or any(part in ('', '.', '..') for part in value.split('/'))):
        raise ValueError('Invalid dataset-relative POSIX path')
    for part in value.split('/'):
        if (part.endswith(('.', ' ')) or any(c in part for c in '*?[]<>|"')
                or re.fullmatch(r'(?i:CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])', part.split('.')[0])):
            raise ValueError('Nonportable dataset path component')
    return value


def _unique(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError('Duplicate JSON key')
        result[key] = value
    return result


def _manifest(raw):
    try:
        manifest = json.loads(raw, object_pairs_hook=_unique)
    except (UnicodeError, json.JSONDecodeError) as error:
        raise ValueError('Invalid manifest JSON') from error
    if not isinstance(manifest, dict) or not isinstance(manifest.get('files'), list):
        raise ValueError('Manifest files must be a list')
    result, folded = {}, set()
    for row in manifest['files']:
        if not isinstance(row, dict) or not {'path', 'sha256', 'bytes'} <= set(row):
            raise ValueError('Invalid manifest file row')
        name = _path(row['path'])
        if name.casefold() == 'manifest.json' or name.casefold() in folded:
            raise ValueError('Duplicate or self-referential manifest path')
        if (not isinstance(row['sha256'], str) or not re.fullmatch('[0-9a-f]{64}', row['sha256'])
                or type(row['bytes']) is not int or row['bytes'] < 0):
            raise ValueError('Invalid digest or byte count')
        folded.add(name.casefold())
        result[name] = row
    return result


def _regular(path, directory=False):
    try:
        info = path.lstat()
    except OSError as error:
        raise ValueError('Missing dataset path') from error
    if stat.S_ISLNK(info.st_mode) or getattr(info, 'st_file_attributes', 0) & 0x400:
        raise ValueError('Symlink or reparse-point redirection refused')
    valid = stat.S_ISDIR(info.st_mode) if directory else stat.S_ISREG(info.st_mode)
    if not valid:
        raise ValueError('Nonregular dataset entry')
    return info


def _root(path):
    root = Path(os.path.abspath(path))
    for parent in reversed((root, *root.parents)):
        _regular(parent, directory=True)
    return root


def _disk_inventory(root):
    found, folded, pending = {}, set(), [root]
    while pending:
        directory = pending.pop()
        _regular(directory, directory=True)
        for entry in directory.iterdir():
            is_dir = stat.S_ISDIR(entry.lstat().st_mode)
            _regular(entry, directory=is_dir)
            name = _path(entry.relative_to(root).as_posix())
            if name.casefold() in folded:
                raise ValueError('Casefold-colliding dataset entries')
            folded.add(name.casefold())
            if is_dir:
                pending.append(entry)
            else:
                found[name] = entry
    return found


def _disk_read(root, path):
    _root(path.parent)
    if not path.is_relative_to(root):
        raise ValueError('Dataset path escaped root')
    before = _regular(path)
    raw = path.read_bytes()
    after = _regular(path)
    keys = ('st_dev', 'st_ino', 'st_size', 'st_mtime_ns', 'st_ctime_ns')
    if any(getattr(before, k) != getattr(after, k) for k in keys):
        raise ValueError('Dataset bytes changed during read')
    return raw


def _git(git_dir, *args):
    try:
        proc = subprocess.run(['git', '--no-replace-objects', '--git-dir='+str(git_dir), *args],
            capture_output=True, check=False)
    except OSError as error:
        raise ValueError('Git unavailable; no filesystem fallback') from error
    if proc.returncode:
        raise ValueError('Git evidence read failed; no filesystem fallback')
    return proc.stdout


def _git_inventory(dataset, git_dir, revision):
    _path(dataset)
    if not isinstance(revision, str) or not re.fullmatch('[0-9a-fA-F]{40}', revision):
        raise ValueError('Exact 40-hex commit required')
    if _git(git_dir, 'rev-parse', '--is-bare-repository').strip() != b'true':
        raise ValueError('Bare Git repository required')
    if _git(git_dir, 'cat-file', '-t', revision).strip() != b'commit':
        raise ValueError('Revision must identify a commit')
    raw = _git(git_dir, 'ls-tree', '-rz', revision, '--', dataset+'/')
    found, folded, prefixes = {}, set(), {}
    for item in raw.split(b'\0'):
        if not item:
            continue
        try:
            meta, encoded = item.split(b'\t', 1)
            mode, kind, oid = meta.split()
            full = encoded.decode('utf-8')
        except (ValueError, UnicodeError) as error:
            raise ValueError('Invalid Git tree entry') from error
        if mode not in (b'100644', b'100755') or kind != b'blob':
            raise ValueError('Symlink, submodule or unsupported Git mode')
        if not full.startswith(dataset+'/'):
            raise ValueError('Git tree path outside dataset')
        name = _path(full[len(dataset)+1:])
        if name.casefold() in folded:
            raise ValueError('Casefold-colliding Git paths')
        parts = name.split('/')
        for index in range(1, len(parts)+1):
            prefix = '/'.join(parts[:index])
            if prefixes.setdefault(prefix.casefold(), prefix) != prefix:
                raise ValueError('Casefold-colliding Git directory components')
        folded.add(name.casefold())
        found[name] = full
    return found


def _reachable(git_dir, revision, git_ref):
    if not isinstance(git_ref, str) or not git_ref.startswith('refs/heads/'):
        raise ValueError('Explicit branch ref required')
    _git(git_dir, 'check-ref-format', git_ref)
    tip = _git(git_dir, 'rev-parse', '--verify', git_ref+'^{commit}').decode('ascii').strip()
    if not re.fullmatch('[0-9a-fA-F]{40}', tip):
        raise ValueError('Named branch commit is invalid')
    if _git(git_dir, 'cat-file', '-t', tip).strip() != b'commit':
        raise ValueError('Named branch must identify a commit')
    _git(git_dir, 'merge-base', '--is-ancestor', revision, tip)


def verify(dataset_dir, git_dir=None, revision=None, git_ref='refs/heads/main'):
    """Return verified counts and manifest digest; never infer qualification."""
    if git_dir is None:
        if revision is not None:
            raise ValueError('Revision requires Git mode')
        root = _root(dataset_dir)
        inventory = _disk_inventory(root)
        read = lambda name: _disk_read(root, inventory[name])
    else:
        inventory = _git_inventory(str(dataset_dir), _root(git_dir), revision)
        _reachable(git_dir, revision, git_ref)
        read = lambda name: _git(git_dir, 'show', revision+':'+inventory[name])
    if 'manifest.json' not in inventory:
        raise ValueError('Manifest missing')
    raw_manifest = read('manifest.json')
    manifest = _manifest(raw_manifest)
    if set(inventory) - {'manifest.json'} != set(manifest):
        raise ValueError('Missing or unlisted dataset payload')
    total = 0
    for name, row in manifest.items():
        raw = read(name)
        if len(raw) != row['bytes'] or hashlib.sha256(raw).hexdigest() != row['sha256']:
            raise ValueError('Payload digest or byte count differs')
        total += len(raw)
    if git_dir is None and set(_disk_inventory(root)) != set(inventory):
        raise ValueError('Dataset membership changed during verification')
    return dict(file_count=len(manifest), total_bytes=total,
                manifest_sha256=hashlib.sha256(raw_manifest).hexdigest())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('dataset_dir')
    parser.add_argument('--git-dir')
    parser.add_argument('--revision')
    parser.add_argument('--git-ref', default='refs/heads/main')
    args = parser.parse_args()
    try:
        result = verify(args.dataset_dir, args.git_dir, args.revision, args.git_ref)
    except (ValueError, OSError) as error:
        parser.exit(1, str(error)+'\n')
    print(json.dumps(result, sort_keys=True))


if __name__ == '__main__':
    main()
