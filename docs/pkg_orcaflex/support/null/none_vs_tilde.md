**RE: YML FIles | Tilde or null or None**

====================

Dear Vamsee,

The code I usually use for yml processing includes

import yaml

def represent_none(self,_):
    return self.represent_scalar('tag:yaml.org,2002:null', '~')

yaml.add_representer(type(None), represent_none)
in the topmatter, before I start loading and emitting yml for OrcaFlex use.

It might be that this is all you need to adapt into your own code. Otherwise, if you need more help, can you please share the smallest possible code / OrcaFlex yml files that I can run to see the specific problem that you are facing?

Regards,

Colin Lewis.

====================

Saipem Classification - General Use
From: Achanta Vamsee <Vamsee.Achanta.guest@saipem.com>
Sent: Sunday, March 24, 2024 10:50 PM
To: Orcina <orcina@orcina.com>
Subject: YML FIles | Tilde or null or None

In yaml files the Tilde and null seem to be not interchangeable.  I am injecting ~ as a string with single quotation marks. Any comment on why this is so?

Thank you,
Vamsee
