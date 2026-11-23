pipe.nominalOD_m = 0.24765;     % m
pipe.nominalWT_m = 0.034925;    % m
pipe.minimumWT_m = 0.034925;   % m
pipe.nominalID = pipe.nominalOD_m - 2*pipe.nominalWT_m; % m

pipe.futureCorrosionRateFloor = 0.0762; % minimum corrosion rate floor in mm per year (3 mpy)
pipe.age = 8;   % age in years
pipe.asessLengthErrorRatio = 0.01; % Acceptable error ratio in assessment length
pipe.dataQualityRatioLimit = 0.2; % data quality ratio to determine data reject threshold when there are too many NANs in a set
pipe.assessmentLengthCeiling = 500; % Averaging length ceiling limit in mm

pipe.externalPressure = 0; % Pa
pipe.internalPressure = 0; % Pa
pipe.tensionEffective = 6674724; % N
pipe.moment = 1277.044974; % N.m

pipe.yieldStrength = 551.5796E6; % Pa

pipe.minimumWT_m = API16QTMinEvaluation( pipe, 0.667);
