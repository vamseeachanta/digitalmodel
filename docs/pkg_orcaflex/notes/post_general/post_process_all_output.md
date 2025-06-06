## Summary
Working Keys:
- Line
- Vessel
- ?

### COmmunications

Dear Vamsee,

For a results output point in OrcaFlex, you can use API functions "vars" and "varDetails" to get these lists.

Note that output points are sometimes whole objects, such as a vessel, but can also be even more specific, such as a line end, or a vessel with defined morison element index, etc. It is for this reason that the vars function takes object extra data.

Available variables can also be different for time domain or frequency domain analysis, and then for history results or range graphs. For this reason vars function takes the result type.

Documentation is here:
https://www.orcina.com/webhelp/OrcFxAPI/Redirector.htm?Pythonreference,OrcaFlexObject.htm

As you note, when new results appear in OrcaFlex, these lists will indeed grow.

I hope that this helps.

Regards,

Colin Lewis.

 
T :  +44(0)1229 584742
E :  orcina@orcina.com
W:  www.orcina.com
     



From: Vamsee Achanta <vamseea@acma-inc.com> 
Sent: 09 May 2025 14:46
To: Orcina <orcina@orcina.com>
Subject: RE: Orcaflex | Postprocess all available variables for an object

Completing and correcting the below email

From: Vamsee Achanta 
Sent: Friday, May 9, 2025 8:42 AM
To: Orcina <orcina@orcina.com>
Subject: Orcaflex | Postprocess all available variables for an object

Is there any command with which I can find all the available variables to assess for an object (eg Line object or Vessel Object)?  I can copy them directly from UI. However, if you add any additional quantity in future revisions, I would like to capture them as well.

The main objective is to extract the variable properties for StaticState, Timetrace (min, max, mean, other?) etc using OrcfxAPI. Alternatively, is there an OrcfxAPI command to get this data in 1 shot?

Any guidance is appreciated.

 


Thank you very much,
Vamsee

Orcina Ltd., Daltongate, Ulverston, Cumbria LA12 7AJ, UK. Company no. 1996191, registered in England & Wales. 
