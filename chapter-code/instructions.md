---
layout: chapter-code
title: Code instructions
---

## Instructions for chapter code usage

For each chapter a script is provided which generates all results and figures of that chapter.
Most of the commands can simply be copied-and-pasted into an R session where spatstat is loaded.
However, several chunks of code require that one or more auxiliary R scripts have been loaded first.
To be able to run all chapter scripts from the current working directory it must contain a subdirectory called 'R' containing the following files:

- [startup.R](R/startup.html) (required by all chapters)
- [figurelayout.R](R/figurelayout.html) (required by all chapters)
- [fijiquakes.R](R/fijiquakes.html) (required by chapter 6)
- [rSpecialMC.R](R/rSpecialMC.html) (required by chapter 12)
- [rSpecialNS.R](R/rSpecialNS.html) (required by chapter 12)
- [short.output.R](R/short.output.html) (required by chapters 16 and 17)
- [shortestpath.R](R/shortestpath.html) (required by chapter 17)

You will also need a subdirectory called 'data' containing the following files:

- [subWinElev.dput](data/subWinElev.dput) (required by chapter 4)
- [potatoes.rda](data/potatoes.rda) (required by chapter 5)
- [simpletree.rda](data/simpletree.rda) (required by chapter 17)
- [dendriteLam10.rda](data/dendriteLam10.rda) (required by chapter 17)
- [dendriteSplitLam10.rda](data/dendriteSplitLam10.rda) (required by chapter 17)

The code for each chapter is here: 

- [Chapter 1](chapter01.html)
- [Chapter 2](chapter02.html)
- [Chapter 3](chapter03.html)
- [Chapter 4](chapter04.html)
- [Chapter 5](chapter05.html)
- [Chapter 6](chapter06.html)
- [Chapter 7](chapter07.html)
- [Chapter 8](chapter08.html)
- [Chapter 9](chapter09.html)
- [Chapter 10](chapter10.html)
- [Chapter 11](chapter11.html)
- [Chapter 12](chapter12.html)
- [Chapter 13](chapter13.html)
- [Chapter 14](chapter14.html)
- [Chapter 15](chapter15.html)
- [Chapter 16](chapter16.html)
- [Chapter 17](chapter17.html)
