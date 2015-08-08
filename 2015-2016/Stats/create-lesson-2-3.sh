#!/bin/bash
runKnitr-2.sh Lesson-2.Rnw 1
rm Lesson-2-files.zip
mkdir Lesson-2-files
rm Lesson-2-files/*
cp P53.txt MYC.txt BRCA2.txt ./Lesson-2-files/.
zip -r Lesson-2-files.zip Lesson-2-files

runKnitr-2.sh Lesson-3.Rnw 1
rm Lesson-3-files.zip
mkdir Lesson-3-files
rm Lesson-3-files/*
cp MIT.txt Cholesterol.txt AnAge_birds_reptiles.txt CystFibr2.txt ./Lesson-3-files/.
zip -r Lesson-3-files.zip Lesson-3-files

