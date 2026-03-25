@REM Preprocess analysis files

CALL conda activate digitalmodel

CALL python -m digitalmodel RAOS_F1_L00.yml
CALL python -m digitalmodel RAOS_F1_L15.yml
CALL python -m digitalmodel RAOS_F1_L50.yml
CALL python -m digitalmodel RAOS_F1_L95.yml

CALL python -m digitalmodel RAOS_F2_L00.yml
CALL python -m digitalmodel RAOS_F2_L15.yml
CALL python -m digitalmodel RAOS_F2_L50.yml
CALL python -m digitalmodel RAOS_F2_L95.yml

CALL python -m digitalmodel RAOS_180K_LNGC_L00.yml
CALL python -m digitalmodel RAOS_180K_LNGC_L99.yml

CALL python -m digitalmodel RAOS_125K_LNGC_L00.yml
CALL python -m digitalmodel RAOS_125K_LNGC_L99.yml
