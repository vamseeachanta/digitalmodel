From: Orcina <orcina@orcina.com>
Sent: Tuesday, November 26, 2024 3:38 AM
To: Vamsee Achanta <vamseea@acma-inc.com>
Cc: Scott McClure <scottm@acma-inc.com>; <sivakumar.pabolu@gmail.com>
Subject: RE: OrcaFlex | Startup Scripts

Dear Vamsee,

The default number of cores can be set using a registry entry, so there is no need for a startup script.

I have mine set to 12, out of my sixteen total cores:

Windows Registry Editor Version 5.00

[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex]
"DefaultThreadCount"=dword:0000000c

If there are other settings that you would want to make, I expect that we can help in a similar way, so please do ask.

Regards,

Colin Lewis.

T :  +44(0)1229 584742
E :  <orcina@orcina.com>
W:  <www.orcina.com>

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: 26 November 2024 03:50
To: Orcina <orcina@orcina.com>
Cc: Scott McClure <scottm@acma-inc.com>; <sivakumar.pabolu@gmail.com>
Subject: OrcaFlex | Startup Scripts

Dear Support Representative,

Is there a way to run a start-up python scripts on the OrcaFlex opening to set some simple initial settings?

For example, I want to set the number of threads to be always 0.9 * Maximum possible cores to avoid hanging the computer etc.

Thank you,
Vamsee
