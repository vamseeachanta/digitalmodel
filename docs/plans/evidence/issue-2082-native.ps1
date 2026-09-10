# Explicitly invoked Windows acceptance only. Raw logs/artifacts are private.
# No installations, task changes, queue actions or native invocation by tests.
# Requires Windows PowerShell 5.1+ and the existing licensed Python environment.
[CmdletBinding()]
param(
    [Parameter(Mandatory=$true)][string]$Python,
    [Parameter(Mandatory=$true)][string]$RepoRoot,
    [string]$OutputDirectory = '',
    [ValidateRange(1,90)][int]$TimeoutSeconds = 90
)
$ErrorActionPreference = 'Stop'
if ($PSVersionTable.PSVersion -lt [version]'5.1' -or $env:OS -ne 'Windows_NT') {
    throw 'Windows PowerShell 5.1 or later is required.'
}
$Python = (Resolve-Path -LiteralPath $Python).ProviderPath
$RepoRoot = (Resolve-Path -LiteralPath $RepoRoot).ProviderPath
if (!(Test-Path -LiteralPath $Python -PathType Leaf) -or
    !(Test-Path -LiteralPath (Join-Path $RepoRoot 'scripts/solver_smoke_test.py') -PathType Leaf)) {
    throw 'Existing Python executable and repository CLI are required.'
}
if (!$OutputDirectory) {
    $OutputDirectory = Join-Path ([IO.Path]::GetTempPath()) ('orcaflex-proof-' + [guid]::NewGuid())
}
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $OutputDirectory) { throw 'Evidence directory must not exist.' }
# The selected Python performs orchestration too, without importing OrcFxAPI.
# A suspended process enters a kill-on-close job before any user code executes.
$worker = @'
import _winapi
import ctypes as c
from ctypes import wintypes as w
import datetime
import hashlib
import json
import math
import msvcrt
import os
from pathlib import Path
import subprocess
import sys
import time
import uuid

class BasicLimits(c.Structure):
    _fields_ = [('process_time', c.c_longlong), ('job_time', c.c_longlong),
                ('flags', w.DWORD), ('min_ws', c.c_size_t), ('max_ws', c.c_size_t),
                ('active', w.DWORD), ('affinity', c.c_size_t),
                ('priority', w.DWORD), ('scheduling', w.DWORD)]

class IoCounters(c.Structure):
    _fields_ = [(name, c.c_ulonglong) for name in
                ('read_ops', 'write_ops', 'other_ops', 'read_bytes', 'write_bytes', 'other_bytes')]

class ExtendedLimits(c.Structure):
    _fields_ = [('basic', BasicLimits), ('io', IoCounters),
                ('process_memory', c.c_size_t), ('job_memory', c.c_size_t),
                ('peak_process', c.c_size_t), ('peak_job', c.c_size_t)]

def kernel_api():
    kernel = c.WinDLL('kernel32', use_last_error=True)
    definitions = {
        'CreateJobObjectW': ([c.c_void_p, w.LPCWSTR], w.HANDLE),
        'SetInformationJobObject': ([w.HANDLE, c.c_int, c.c_void_p, w.DWORD], w.BOOL),
        'AssignProcessToJobObject': ([w.HANDLE, w.HANDLE], w.BOOL),
        'ResumeThread': ([w.HANDLE], w.DWORD),
        'CloseHandle': ([w.HANDLE], w.BOOL),
    }
    for name, (args, result) in definitions.items():
        function = getattr(kernel, name)
        function.argtypes, function.restype = args, result
    return kernel

def job_object(kernel):
    job = kernel.CreateJobObjectW(None, None)
    if not job:
        raise c.WinError(c.get_last_error())
    limits = ExtendedLimits()
    limits.basic.flags = 0x2000  # JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
    if not kernel.SetInformationJobObject(job, 9, c.byref(limits), c.sizeof(limits)):
        kernel.CloseHandle(job)
        raise c.WinError(c.get_last_error())
    return job

def run_owned(argv, root, output, timeout):
    kernel = kernel_api()
    job = job_object(kernel)
    process = thread = None
    try:
        with open(output/'stdout.log', 'wb') as stdout, open(output/'stderr.log', 'wb') as stderr:
            info = subprocess.STARTUPINFO()
            info.dwFlags = subprocess.STARTF_USESTDHANDLES
            with open(os.devnull, 'rb') as stdin:
                handles = [msvcrt.get_osfhandle(f.fileno()) for f in (stdin, stdout, stderr)]
                for handle in handles:
                    os.set_handle_inheritable(handle, True)
                info.hStdInput, info.hStdOutput, info.hStdError = handles
                info.lpAttributeList = {'handle_list': handles}
                process, thread, _, _ = _winapi.CreateProcess(
                    sys.executable, subprocess.list2cmdline(argv), None, None, True,
                    0x4 | subprocess.CREATE_NO_WINDOW, None, str(root), info)
            if not kernel.AssignProcessToJobObject(job, process):
                raise c.WinError(c.get_last_error())
            if kernel.ResumeThread(thread) == 0xffffffff:
                raise c.WinError(c.get_last_error())
            timed_out = _winapi.WaitForSingleObject(process, timeout*1000) == 258
            code = None if timed_out else _winapi.GetExitCodeProcess(process)
            return code, timed_out
    finally:
        if process is not None and _winapi.WaitForSingleObject(process, 0) == 258:
            _winapi.TerminateProcess(process, 1)
        kernel.CloseHandle(job)  # also kills descendants when the parent already exited
        if process is not None:
            _winapi.WaitForSingleObject(process, 5000)
            _winapi.CloseHandle(process)
        if thread is not None:
            _winapi.CloseHandle(thread)

FLAGS = ('static_finite', 'dynamic_finite', 'simulation_complete', 'saved_sim_reloaded',
         'reloaded_simulation_complete', 'reloaded_dynamic_finite')
NUMBERS = ('static_tension_kN', 'dynamic_samples', 'reloaded_dynamic_samples', 'sim_bytes')

def validate_report(report, scratch):
    if report.get('ok') is not True or len(report.get('results', [])) != 1:
        raise ValueError('solver report failed')
    result = report['results'][0]
    if result.get('solver') != 'orcaflex' or result.get('ok') is not True:
        raise ValueError('OrcaFlex result missing or failed')
    if any(result.get(key) is not True for key in FLAGS):
        raise ValueError('finite/completion/reload proof missing')
    counts = result.get('thread_counts_observed', {})
    values = [result.get('thread_count_requested')] + [counts.get(k) for k in
                                                    ('solve', 'data_reader', 'simulation_reader')]
    if any(type(value) is not int or value != 1 for value in values):
        raise ValueError('one-thread proof failed')
    for key in ('dynamic_samples', 'reloaded_dynamic_samples', 'sim_bytes'):
        if type(result.get(key)) is not int or result[key] < (1 if key == 'sim_bytes' else 2):
            raise ValueError('sample or file-size proof failed')
    if result['dynamic_samples'] != result['reloaded_dynamic_samples']:
        raise ValueError('readback sample count differs')
    values = result.get('dynamic_tension_kN', [])
    if len(values) != 2 or not all(math.isfinite(float(v)) for v in values + [result['static_tension_kN']]):
        raise ValueError('nonfinite or missing numeric proof')
    for suffix in ('dat', 'sim'):
        if (scratch/'orcaflex'/('smoke.'+suffix)).stat().st_size <= 0:
            raise ValueError('saved artifact empty')
    if (scratch/'orcaflex/smoke.sim').stat().st_size != result['sim_bytes']:
        raise ValueError('simulation file size disagrees')
    keys = ('solver', 'ok', 'dll_version', 'thread_count_requested', 'thread_counts_observed',
            'dynamic_tension_kN') + FLAGS + NUMBERS
    return {key: result[key] for key in keys}

def hashes(paths, root):
    return {p.relative_to(root).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in paths if p.is_file()}

def reject_constant(value):
    raise ValueError('nonfinite JSON constant')

def main():
    root, output, timeout = Path(sys.argv[1]), Path(sys.argv[2]), int(sys.argv[3])
    output.mkdir(parents=True, exist_ok=False)
    scratch = output/('scratch-'+uuid.uuid4().hex)
    scratch.mkdir()
    started = time.monotonic()
    proof = dict(ok=False, timed_out=False, python_version=sys.version.split()[0],
                 utc=datetime.datetime.now(datetime.timezone.utc).isoformat())
    sources = [root/'scripts/solver_smoke_test.py', root/'src/digitalmodel/solvers/smoke/probes.py',
               root/'src/digitalmodel/solvers/smoke/workflow.py']
    stage = 'revision'
    try:
        revision = subprocess.run(['git', 'rev-parse', 'HEAD'], cwd=root, check=True,
                                  capture_output=True, text=True, timeout=10)
        proof['revision'] = revision.stdout.strip()
        if not all(path.is_file() for path in sources):
            raise ValueError('mandatory proof source missing')
        proof['input_hashes'] = hashes(sources, root)
        argv = [sys.executable, '-B', str(sources[0]), '--solver', 'orcaflex',
                '--json', '--output-dir', str(scratch)]
        stage = 'subprocess'
        code, timed_out = run_owned(argv, root, output, timeout)
        proof.update(exit_code=code, timed_out=timed_out)
        if timed_out or code != 0:
            raise ValueError('child timeout' if timed_out else 'child failed')
        stage = 'report-validation'
        report = json.loads((output/'stdout.log').read_text(encoding='utf-8-sig'),
                            parse_constant=reject_constant)
        proof['result'] = validate_report(report, scratch)
        proof['ok'] = True
    except Exception as error:
        proof['error_type'] = type(error).__name__  # raw diagnostics stay private
        proof['failure_stage'] = stage
        (output/'harness-error.log').write_text(str(error), encoding='utf-8')
    finally:
        proof['elapsed_s'] = round(time.monotonic()-started, 3)
        proof['output_hashes'] = hashes(scratch.rglob('*'), scratch)
        (output/'proof.json').write_text(json.dumps(proof, indent=2, allow_nan=False), encoding='utf-8')
    return 0 if proof['ok'] else 1

sys.exit(main())
'@
& $Python -B -X utf8 -c $worker $RepoRoot $OutputDirectory $TimeoutSeconds
exit $LASTEXITCODE
