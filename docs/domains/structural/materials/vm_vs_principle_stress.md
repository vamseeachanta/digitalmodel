https://www.linkedin.com/posts/activity-7098211931072716800-_jWX?utm_source=share&utm_medium=member_desktop

Which one should I Use #Von Mises Stress Plot Or #Principal Stress Plot?

Mechanical parts or assemblies can be predicted to fail by various modes determined by buckling, deflection, natural frequency, strain, or stress.  Strain or stress failure criteria are different depending on whether they are considered brittle or ductile materials.  In FEA for most ductile material failure can be predicted using Von Mises stress criteria and the Principal Stress theory can be used to predict the failure of brittle material.

💡 The Von Mises Stress is #NOT actual #stress, It is a theoretical value, It’s a measure of energy density & based on the Distortion energy theory. As energy density has the same unit as Stress, So we called von Mises Stress. Von Mises essentially calculates what is known as the distortion energy density at a particular point in the system. Von Mises stress is also called equivalent stress. It’s a special measure of stress that serves as an approach to combine all stress components into one value. One Vonmises stress plot can represent
• Equivalent stress allows one to view the stress of the structure in one plot.
• Equivalent stress can be used as a scalar indicator to determine material failure.

💡 The #principal Stress is a #real Stress, It’s refers to the maximum and minimum normal stresses acting on a specific plane within a matter & these normal stress (Tensile and Compressive) called principal stresses, By determining the principal stresses, engineers can identify the directions (Tensile and Compressive) in which a material is most likely to fail or deform It is obtained by suitably transforming (rotating) the stress element such that the rotated element is subjected to no shear stress. Principal stress is theoretically performed using Mohr’s circle, a graphical method that helps visualize the stress transformation and determine the principal stress magnitudes and directions. There is a failure theory based on the Principal Stresses Max's Principal Stress theory.

💡 Von Mises can be calculated from Principal Stresses but Principal Stresses cannot calculate from von Mises stress.

💡 However, Vonmises stress has many big advantages compared to principal stress especially because it helps a lot in visualizing stresses concentration in critical areas such as notches, holes, Fillets, sharp corners, load and boundary conditions location or cracks in a material, By calculating the stress distribution around these areas, engineers can assess the potential for failure and make design modifications to mitigate stress concentrations but only looking to Vonmises stress it’s difficult to say that these high stress generating in components is due to only von misses stress because Vonmises stress can not exhibit tensile and compressive stress so in this situation we recommendation to check the Minimum and Maximum principal stress also even for ductile material as well.

== Response

The short answer is- all of them!

But in a logical sequence to understand the stress state.
Von Mises gives an overall indication of where critical stresses may be . It does not tell us what type of stress, in particular tension/compression or direct/shear.

Next use Cartesian or local directional stress to get a sense of the loading action and stress flow. This may be sufficient to identify a critical stress level.

Next check max/min principal stresses. This helps understand stress flow further and can be used directly for local fatigue life, brittle failure or local compressive buckling or crippling.

Check max shear to see if it is a shear dominant stress region.

By the time you have covered all that lot you should have a real understanding of the stress state and then what failure theory to apply.

It might even be a Von Mises criteria after all that. But now it’s a clearer judgment call.

It might sound like a lot of work, but with a good post processor it’s pretty quick and slick.

