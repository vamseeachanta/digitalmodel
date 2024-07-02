function [tMinimum] = API16QTMinEvaluation( pipe, allowableFactor);
% getFilenames : Get filename list in directory "pth" with extension "ext"
% INPUT : Pipe properties
% OUTPUTS: Minimum Wall Thickness Required Per API 16Q VM Stress Criteria of 0.67 times Yield
pipe.minimumID_m=pipe.nominalOD_m - 2*pipe.minimumWT_m;
pipe.A = pi()/4*(pipe.nominalOD_m^2 - pipe.minimumID_m^2);
pipe.Ai = pi()/4*pipe.minimumID_m^2;
pipe.Ao = pi()/4*pipe.nominalOD_m^2;
pipe.I = pi()/64*(pipe.nominalOD_m^4 - pipe.minimumID_m^4);

pipe.tensionTrue = pipe.tensionEffective + pipe.internalPressure*pipe.Ai - pipe.externalPressure*pipe.Ao % N

sigma.radial = -(pipe.externalPressure*pipe.nominalOD_m + pipe.internalPressure*pipe.minimumID_m)/(pipe.nominalOD_m + pipe.minimumID_m)
sigma.circuferential = (pipe.internalPressure - pipe.externalPressure)*pipe.nominalOD_m /(2*pipe.minimumWT_m) - pipe.internalPressure
sigma.axial_1 = pipe.tensionTrue/pipe.A + pipe.moment/(2*pipe.I)*(pipe.nominalOD_m - pipe.minimumWT_m)
sigma.axial_2 = pipe.tensionTrue/pipe.A - pipe.moment/(2*pipe.I)*(pipe.nominalOD_m - pipe.minimumWT_m)

LHS_Squared1 = ((sigma.radial - sigma.circuferential)^2 + (sigma.circuferential - sigma.axial_2)^2 + (sigma.axial_2 - sigma.radial)^2)/2
LHS_Squared2 = ((sigma.radial - sigma.circuferential)^2 + (sigma.circuferential - sigma.axial_2)^2 + (sigma.axial_2 - sigma.radial)^2)/2

RHS_Squared = (allowableFactor*pipe.yieldStrength)^2

tMinimum = pipe.minimumWT_m;

end
