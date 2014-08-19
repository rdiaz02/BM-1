#!/bin/bash
runKnitr.sh Lesson-2.Rnw
rm Lesson-2-files.zip
mkdir Lesson-2-files
cp Lesson-2.pdf P53.txt MYC.txt BRCA2.txt ./Lesson-2-files/.
zip -r Lesson-2-files.zip Lesson-2-files

runKnitr.sh Lesson-3.Rnw
rm Lesson-3-files.zip
mkdir Lesson-3-files
cp Lesson-3.pdf MIT.txt Cholesterol.txt AnAge_birds_reptiles.txt CystFibr2.txt ./Lesson-3-files/.
zip -r Lesson-3-files.zip Lesson-3-files

