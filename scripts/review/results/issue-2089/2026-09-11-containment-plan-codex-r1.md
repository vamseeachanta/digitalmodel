# Independent Codex adversarial plan review
Verdict: MAJOR; revise before approval.
Reviewed plan SHA256: 95fb2c92e362d17413862a71882a6aa17554d7f121dbffbccd65750e057dca6d.
Reviewer: independent template_review subagent. No writes or sentinel/vendor execution.

1. MAJOR: Last-handle cleanup conflicts with post-cleanup accounting. An observer job handle prevents kill-on-last-close; closing every handle prevents querying the unnamed job. Define sole controller ownership at trigger, close observer job duplicates, check membership/accounting before closure and retain process handles for waits afterward. Add a deliberately retained second-handle negative control.
2. MAJOR: Suspended-before-resume scenario contradicts universal readiness/resume/watchdog promises. Require controller-origin suspended identity publication and an observer barrier, no ResumeThread or sentinel readiness. A never-resumed process has no watchdog; failed cleanup requires identity-grounded disposition.
3. MAJOR: Natural watchdog exits could falsely qualify ineffective containment. Pin monotonic timing and enforce remaining sentinel life longer than cleanup plus margin. Add an ineffective-kill test that must fail before natural exit. Treat the normal-exit parent separately.
4. MAJOR: Interlock namespace and create-before-record crash gap are unspecified. Use stable non-output-root-dependent state; persist creation-may-have-occurred before launch; incomplete/empty identities must remain unresolved. Test different output roots and observer loss during identity persistence.

Atomic assignment and separation from licensed execution require no additional scope expansion. These blockers concern the experiment's proof validity. Read-only cleanup left no artifacts.
