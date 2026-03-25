## Summary


https://realpython.com/python-yaml/

Possible solution is to use a LoaderClass (BaseLoader) to identify the main keys.

From there, we may need to resort to ascii encoding to read the file.


Also, with recent advances, we need to iimprove our yaml utilities and try to use the latest version of pyyaml and its best practices. Check link above.


## Communications

Vamsee

I'm not sure what is causing your issue, because I don't enough of your code. Just including a single line is often not sufficient for us to debug. Ideally we would like to see a cutdown, minimal, but complete program that demonstrates the issue. Then we know we are talking about the same thing.

But based on the error message it seems likely that ymlfile is a closed file.

Your YAML file is 36MB in size. Trying to load that with pyyaml is going to be a performance challenge. That's because pyyaml will do this by creating a Python dictionary that contains the entire structure. That's going to be very expensive for this type and size of data.

OrcaFlex is able to load your YAML in around 45s because it uses an different event based paradigm for processing structured YAML and uses those events to synthesise efficiently an OrcaFlex model. So I fairly rapidly gave up working with your file and instead looked at a simpler model. 

I made a model with a single default vessel in and loaded it like this:

import yaml
doc = yaml.safe_load(open("test.yml", "r", encoding="utf-8-sig"))

The encoding is important because our files contain a UTF8 BOM and that information needs to be conveyed to the Python libraries so that they don't treat the BOM as part of the document.

Now, your YAML file will load, eventually, but it will take an age. This is really a perfect example of where variation files come into their own. I don't know precisely what your motivations and goals are in this work, but I would imagine that it would be possible to separate the data into that which is static, and that which you modify. I'm guessing that the large 3D seabed data is static, and that the vessel type properties are too. And the actual positions and configurations of the model are what you are wanting to manipulate.

So you could put the 3D seabed and the vessel type data into a binary .dat base model file, and then have the rest of the data specified in a variation YAML file. This would make working with those YAML files much more palatable. Your original model will load in around 5 minutes, but the signs are, for me at least, that the techniques I have described to segregate the data will be worthwhile.

As a final aside, we're currently working on a development that will allow 3D seabed data to be specified in an external text file which will make working with models containing large 3D seabed data sets more efficient. But that's for the next release!

Best regards,

David Heffernan

From: Vamsee Achanta <vamseea@acma-inc.com> 
Sent: 11 March 2025 16:48
To: Orcina <orcina@orcina.com>
Subject: YML File | Read Using Python

Dear Support Representative,

What is the best way to read the attached .yml file that is obtained from OrcaFlex. I tried to load It back using python and ended up with the below error.

Code : Using pyyaml
yaml.safe_load(ymlfile)

Error:
data = self.stream.read(size) ^^^^^^^^^^^^^^^^^^^^^^ ValueError: I/O operation on closed file.

Thank you,
