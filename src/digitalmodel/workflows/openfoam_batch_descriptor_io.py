"""Small descriptor-relative atomic I/O primitives for owned batch layouts."""

import os
import secrets

_DIRECTORY_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW


def write_case_file(work_fd: int, case: str, name: str, data: bytes) -> None:
    try:
        os.mkdir(case, 0o700, dir_fd=work_fd)
    except FileExistsError:
        pass
    case_fd = os.open(case, _DIRECTORY_FLAGS, dir_fd=work_fd)
    temporary = f".{name}.tmp-{secrets.token_hex(16)}"
    try:
        fd = os.open(
            temporary,
            os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
            0o600,
            dir_fd=case_fd,
        )
        try:
            os.write(fd, data)
            os.fsync(fd)
        finally:
            os.close(fd)
        os.rename(temporary, name, src_dir_fd=case_fd, dst_dir_fd=case_fd)
    finally:
        try:
            os.unlink(temporary, dir_fd=case_fd)
        except FileNotFoundError:
            pass
        os.close(case_fd)
