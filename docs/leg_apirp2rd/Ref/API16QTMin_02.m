pipe.nominalOD_m = 0.127;     % m
pipe.nominalWT_m = 0.0127;    % m
pipe.minimumWT_m = 0.005875;   % m
pipe.nominalID = pipe.nominalOD_m - 2*pipe.nominalWT_m; % m
pipe.futureCorrosionRateFloor = 0.0762; % minimum corrosion rate floor in mm per year (3 mpy)
pipe.age = 8;   % age in years
pipe.asessLengthErrorRatio = 0.01; % Acceptable error ratio in assessment length
pipe.dataQualityRatioLimit = 0.2; % data quality ratio to determine data reject threshold when there are too many NANs in a set
pipe.assessmentLengthCeiling = 500; % Averaging length ceiling limit in mm

pipe.externalPressure = 0; % Pa
pipe.internalPressure = 41.37E6; % Pa
pipe.tensionEffective = 0; % N
pipe.moment = 0; % N.m
pipe.yieldStrength = 551.5796E6; % Pa

pipe.minimumWT_m = API16QTMinEvaluation( pipe, 0.667);
