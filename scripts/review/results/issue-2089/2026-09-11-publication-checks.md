# Publication checks

- Approved plan SHA256 unchanged: bbd0bc72328bb62b3f311ade8b476a47235c0b2ce318fd3dc46393fccab43a6c.
- Final Windows test-log source hashes match all five current production files.
- JSON artifacts parse; source/test file and function size limits pass; report local links resolve.
- Staged whitespace check passes with core.whitespace=cr-at-eol to recognize existing Windows line endings; production bytes were preserved. Trailing whitespace in text logs was normalized without changing results.
- Canonical legal scanner exited zero but printed an empty resolved repository. This is the known workspace-hub issue 3804 failure and is NOT counted as valid coverage.
- The existing issue-3804 corrected scanner resolved the actual task worktree and passed the staged diff scan with exit zero. No scanner code was modified for this task.
- Cleanup audit is scoped and sanitized; unrelated machine inventory was not published.
