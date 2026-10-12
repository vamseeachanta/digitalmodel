# Independent Codex code review
Verdict: MAJOR; changes required. Reviewed six new source files and new tests against approved addendum SHA256 1e049153d18a37165aad32f781caac3615cce66806ae0bc15717458dda82c262. No native/vendor execution, writes or marker access.

1. MAJOR: crash() calls ready(parent.json,item.role), but item is undefined. Running-controller scenario cannot pass. Use fixed parent role and inject complete tests of both crash branches. Correction: current run_scenario catches BaseException, so NameError DOES reach close_all and leaves recovery_required when census incomplete.
2. MAJOR: timeout observation has no timeout-specific deadline or expired wait; it immediately closes the job after readiness. Add real bounded expiration while parent/child remain alive; preserve watchdog margin, reject early completion and cleanup failure.
3. MINOR: failed observation with confirmed process cleanup leaves scenario directory behind while qualify finalizes marker. Preserve diagnostics outside disposable work, clean failed-scenario directory too, and block full cleanup status on removal failure.

The tests do not exercise both crash orchestration branches, explaining the NameError miss. Native API failure coverage needs explicit attribute initialization/update, creation, membership and resume cases before the Linux count can substantiate approved coverage.

Reviewed source hashes:
_windows_job.py 3254a9ade060ae2b05ef84482fcb041c0fe3ddfc200d46a4ee5ddc69e3fb725d
_owned_process.py 4ec142b0bd4818269a04e323f54ab4063383acdea4787dcb999bc17e7a0c4e81
_sentinel.py 749f28339d1f0a6f875c2efd13c016a1f28f6f4e6dbbc26070ce127e19d7b288
_qualification_state.py 4e942b5995ad9175e5c8789d36465b8bcc6d7e5567279174b475ae63bde320be
_qualification_scenarios.py ea2d0ed7e906a6c0f49bbb9b9902aea8cc8c555977291dc5d4207e00ee0318e1
qualification.py ac165740ca76820e35a43db904502c8132e49aff7d5211223d3f012506a88267
