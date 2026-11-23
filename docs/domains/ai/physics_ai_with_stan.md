
https://www.linkedin.com/feed/update/urn:li:activity:7290073985701011459?commentUrn=urn%3Ali%3Acomment%3A%28activity%3A7290073985701011459%2C7291828535990571008%29&dashCommentUrn=urn%3Ali%3Afsd_comment%3A%287291828535990571008%2Curn%3Ali%3Aactivity%3A7290073985701011459%29

This is great!
Will you share your instructions to locally install Deepseek?
I have read that Deepseek require a large amount of RAM and GPUs to run.

Like
like
1

Reply
11 Replies
11 Replies on Alfonso R. Reyes? comment
View ?? Alastair Muir, PhD, BSc, BEd, MBB?s profile
?? Alastair Muir, PhD, BSc, BEd, MBB
 ? 2nd
Data Science Consultant | @alastairmuir.bsky.social | Risk Analysis and Optimization
19h

Alfonso R. Reyes I posted twice on local implementations of DeepSeek. I don?t know how to repost or search

Like

Reply
View Alfonso R. Reyes? profile
Alfonso R. Reyes
 ? 1st
VP Artificial Intelligence Engineering - Energy Division
19h

?? Alastair Muir, PhD, BSc, BEd, MBB Thanks. I will look it up. From what I read so far, the installation procedures are not encouraging because of the hardware demands. Like minimum 128 GB RAM and 4 GPUs. Don?t have that kind of rig at home. Anyway, I am liking so far the results of Deepseek in generating code and explaining physics learning machines.

Like

Reply
View Alfonso R. Reyes? profile
Alfonso R. Reyes
 ? 1st
VP Artificial Intelligence Engineering - Energy Division
18h

?? Alastair Muir, PhD, BSc, BEd, MBB I think this table in the Medium article is key for hardware vs model requirements:

# Default 7B model (4.7GB - ideal for consumer GPUs)
ollama run deepseek-r1

# Larger 70B model (requires 24GB+ VRAM)
ollama run deepseek-r1:70b

# Actual DeepSeek-R1 (requires 336GB+ VRAM for 4-bit quantization) 
ollama run deepseek-r1:671b


Deepseek-R1 - the good one - is way out of my computer league.. the other two are not so reliable.

Like

Reply
View ?? Alastair Muir, PhD, BSc, BEd, MBB?s profile
?? Alastair Muir, PhD, BSc, BEd, MBB
 ? 2nd
Data Science Consultant | @alastairmuir.bsky.social | Risk Analysis and Optimization
18h

Alfonso R. Reyes I have spent two days now getting Stan to compile with CUDA/OpenCL to use GPUs. I think I?m nearly there

Like

Reply
View Alfonso R. Reyes? profile
Alfonso R. Reyes
 ? 1st
VP Artificial Intelligence Engineering - Energy Division
18h

?? Alastair Muir, PhD, BSc, BEd, MBB how silly of me. I just asked DeepSeek about minimum requirements for its installation:

?Alright, the user is asking about the minimum hardware requirements to install DeepSeek-R1 locally. Let me start by recalling what DeepSeek-R1 is. It's an AI model, likely a large language model similar to others in the field.

Large language models usually require significant computational resources, especially GPU memory. The size of the model is a critical factor here. If DeepSeek-R1 is a 7B parameter model, it would need around 14-16GB of GPU memory in FP16 precision. If it's larger, like a 70B model, the requirements would be much higher, possibly over 140GB, which isn't feasible on consumer hardware.

The user mentioned "minimum" requirements. need to consider different scenarios. For example, running the model in FP16 vs. quantized versions (like 4-bit or 8-bit).

VRAM is a key component. For a 7B model in FP16, 16GB VRAM is a starting point. But if they use quantization, maybe 8GB could work. However, performance might be slower. For CPU-only setups, RAM becomes important, but inference would be much slower. They might need 16-32GB of RAM and use libraries like llama.cpp to offload to the CPU.

Like

Reply
View Alfonso R. Reyes? profile
Alfonso R. Reyes
 ? 1st
VP Artificial Intelligence Engineering - Energy Division
18h

?? Alastair Muir, PhD, BSc, BEd, MBB Ok. Hold on. But Stan is not a requirement, right? How much VRAM does your GPU have?

Like

Reply
View Alfonso R. Reyes? profile
Alfonso R. Reyes
 ? 1st
VP Artificial Intelligence Engineering - Energy Division
18h

?? Alastair Muir, PhD, BSc, BEd, MBB
This is doable:
Comment image, no alternative text available

Like

Reply
View ?? Alastair Muir, PhD, BSc, BEd, MBB?s profile
?? Alastair Muir, PhD, BSc, BEd, MBB
 ? 2nd
Data Science Consultant | @alastairmuir.bsky.social | Risk Analysis and Optimization
18h

Alfonso R. Reyes No, not a requirement for DeepSeek, just my own project to keep me from getting too distracted. Just some Sunday morning musings on what I do to do something relevant (incorporation of physics in models) in the midst of the storm

Like
like
1

Reply
View Alfonso R. Reyes? profile
Alfonso R. Reyes
 ? 1st
VP Artificial Intelligence Engineering - Energy Division
18h

?? Alastair Muir, PhD, BSc, BEd, MBB Beautiful!

Like

Reply
View Alfonso R. Reyes? profile
Alfonso R. Reyes
 ? 1st
VP Artificial Intelligence Engineering - Energy Division
18h

??amazing! Stan can do physics as well and solve differential equations!
Comment image, no alternative text available

Like
like
1

Reply
View ?? Alastair Muir, PhD, BSc, BEd, MBB?s profile
?? Alastair Muir, PhD, BSc, BEd, MBB
 ? 2nd
Data Science Consultant | @alastairmuir.bsky.social | Risk Analysis and Optimization
17h

Alfonso R. Reyes differential equations: this is part of my cunning plan

Like
insightful
1

Reply

Collapse replies