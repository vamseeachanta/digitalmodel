Vamsee

The documentation says this: "If user specified seeds is checked then you must give a seed for each wave train; if it is not checked, seeds are chosen automatically."

You are correct that if you don't check user-specified seeds, then the same seeds will be used. I used seeds plural because you may have multiple wave trains. The first wave train will always have seed 12345. The second wave train will have seed -1567012174 and so it goes on.

When user-specified seeds is checked, you have to provide them. We don't recommend what seeds to use. If you have multiple analyses with the same wave parameters, and the same seeds, then the sea states will be identical. If the seeds differ, then the sea states will be from a different realisation, and so will also differ.

Best regards,

David Heffernan

From: Vamsee Achanta <vamseea@acma-inc.com> 
Sent: 21 February 2025 13:28
To: Orcina <orcina@orcina.com>
Cc: Scott McClure <scottm@acma-inc.com>
Subject: RE: OrcaFlex | Environment JONSWAP | Seed

David,


Please see screenshot below. 

Scenario 1: User Specified seed unchecked. Seem to be defaulting to a WaveSeed: 12345

Scenario 2: User Specified seed checked. If my design requires to run 5 random seeds, do you have a recommendation on the user specified seeds we can be using? 

If this needs more clarification, please let me know

 

Thank you again,
Vamsee

From: Orcina <orcina@orcina.com> 
Sent: Friday, February 21, 2025 7:22 AM
To: Vamsee Achanta <vamseea@acma-inc.com>
Cc: Scott McClure <scottm@acma-inc.com>
Subject: RE: OrcaFlex | Environment JONSWAP | Seed

Vamsee

I'm afraid I do not understand the question you are asking. Would you be able to provide some more details.

Best regards,

David Heffernan

From: Vamsee Achanta <vamseea@acma-inc.com> 
Sent: 21 February 2025 13:17
To: Orcina <orcina@orcina.com>
Cc: Scott McClure <scottm@acma-inc.com>
Subject: OrcaFlex | Environment JONSWAP | Seed

Dear Support Representative,

I am unable to reproduce the result for the default random seed selected by program. Which seed will provide me this result when specified by the user?

Also, I am also running additional seeds as part of design. Any guidance specifying seeds when performing 5 runs, 10 runs, 20 runs etc.

Thank you very much,
Vamsee
