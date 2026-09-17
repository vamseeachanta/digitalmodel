"""One collection's bounded name facts; no process exemption or execution authority."""
from copy import deepcopy


def _codec():
    # Lazy import: the existing resolver imports the base snapshot module.
    from . import cylinder_absence_collection
    return cylinder_absence_collection


def _validate_rows(rows):
    fields = {'pid', 'parent_pid', 'name', 'creation_time', 'executable_path'}
    if not isinstance(rows, list) or len(rows) > 65536:
        raise ValueError('Invalid bounded process-name inventory')
    seen = set()
    for row in rows:
        if not isinstance(row, dict) or set(row) != fields:
            raise ValueError('Invalid process-name fields')
        for key in ('pid', 'parent_pid'):
            if type(row[key]) is not int or not 0 <= row[key] < 2**32:
                raise ValueError('Invalid process-name identifier')
        if row['pid'] in seen:
            raise ValueError('Duplicate process-name identifier')
        seen.add(row['pid'])
        for key, limit in [('name', 4096), ('creation_time', 64)]:
            if not isinstance(row[key], str) or len(row[key]) > limit:
                raise ValueError('Invalid bounded process-name text')
        image = row['executable_path']
        if image is not None and (not isinstance(image, str) or len(image) > 4096):
            raise ValueError('Invalid bounded process image')
        _codec()._psutil_epoch(row['creation_time'])


def parent_map_evidence(maps):
    """Project integer-keyed working maps into the existing canonical evidence shape."""
    return _codec()._parent_map_evidence(maps)


class ActiveNameResolution:
    """Resolve only A blanks, then join later blanks to the same initial identity."""

    def __init__(self):
        self.originals = []
        self.completed = 0
        self.failed = False
        self.query = _codec()._no_query_evidence()
        self.resolved = {}

    def _initial(self, rows, blanks):
        codec = _codec()
        if blanks:
            try:
                self.query = deepcopy(codec._query_blank_names([row['pid'] for row in blanks]))
            except codec.CollectionError as error:
                self.query = deepcopy(error.evidence)
                raise
            self.resolved = deepcopy(codec._reconcile(rows, self.query))

    def observe(self, rows):
        if self.failed or self.completed >= 3:
            raise ValueError('Name-resolution collection cannot retry or exceed three stages')
        self.failed = True
        self.originals.append(deepcopy(rows))
        _validate_rows(rows)
        blanks = [row for row in rows if not row['name'].strip()]
        if self.completed == 0:
            self._initial(rows, blanks)
        for row in blanks:
            resolved = self.resolved.get(row['pid'])
            if resolved is None:
                raise ValueError('New blank identity after initial name resolution')
            if row['parent_pid'] != resolved['ParentProcessId']:
                raise ValueError('Resolved process parent identity differs')
        normalized = _codec()._normalize(rows, self.resolved)
        self.completed += 1
        self.failed = False
        return normalized

    def evidence(self):
        return deepcopy(dict(schema='active-name-resolution-1',
            original_inventories=self.originals, query=self.query,
            completed_stages=self.completed, failed=self.failed,
            limitation='Resolution supplies names only; selected detail, identity and resource checks remain required.'))
