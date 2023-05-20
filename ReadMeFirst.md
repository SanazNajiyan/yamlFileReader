* the application is run with the command 
cabal run yamltree
* the test suite is run with the command
cabal test
* the proof is in READMe.md
challenges of the project:
finishing all questions of the assignment, I got interested in putting my tests in a more professional format. Knowing that my quick checks did work in Main.hs properly, I changed my yamltree.cabal to introduce test suits and added testMe.hs. First problem was that both tests can't be implemented simultaneously. so I did test it one by one and the test suite each time passed. Yet, later my test suite tests didn't work and it ran forever. I couldn't fix this issue and since it wasn't hard requirement of the project, I decided to step forward and submit the assignment

possible typos:
in question 7 the desired output warning for instruments-hierarchy.yaml is given which is correct. However in `instruments-interaction.log` the log indicates that the output for `instruments-hierarchy.yaml` is different. I believe this is a typo and both outputs are the same if the input file is `instruments-hierarchy.yaml`.


