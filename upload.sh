#!/bin/sh

STUDENT_NUMBER="01427123"
HOST="g0.complang.tuwien.ac.at"

filename="AufgabeFFP$1.hs"
dir="exercise-$1"

echo Trying to upload ${dir}/${filename}

scp "${dir}/${filename}" "ffp${STUDENT_NUMBER}@${HOST}:Gruppe/"
