echo Start: %date% %time% > time.txt
bemrosetta_cl -orca -numtries 10 -numthread 12 -rw "Desktop.wave.yml" "Desktop.flex.yml"
echo End:   %date% %time% >> time.txt
