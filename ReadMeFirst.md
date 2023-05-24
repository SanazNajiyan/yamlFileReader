Some things to share with you:
* the application is run with the command 
cabal run yamltree
* the test suite is run with the command
cabal test (this command is not working currently but at the end of the program I left the command to run the test through main)
* the proof is in READMe.md
* the other explanatory question regarding `find` is answered in Main.hs while commented out. 
* as discussed the float numbers are formatted to two decimal places
* the question e.g.`Equal weight VIX: V2X: VNKI ? (Type "no" or "NO" or "n" or "N" to give custom weights. Any other input will result in equal weights.)` will prompt the user to input the weights in case non-equal weights are chosen in the first place. However, if the equal weights are 
inputted afterwards, the program is not going to complain and assumes it as the change of mind.
* some questions were long to keep track of so I kept them in the code for my reference
Challenges of the project:
finishing all questions of the assignment, I got interested in putting my tests in a more professional format. I modified my yamltree.cabal file to introduce test suites and added `testThis.hs`. However, I encountered an issue where both tests couldn't be implemented simultaneously. To work around this, I tested them one by one, and each test suite passed individually. However, later on, my test suites didn't work and ran indefinitely. Another approach I took was introducing modules and trying to import the functions to `Main.hs` and `testThis.hs`_I worked on it in my branch `bardia` on git. However I encountered many errors indicating that specific functions are not exported and the `Variable is not in scope`. 
I also tried to do it simple way by just including all functions in my `testThis.hs` in order not to import Main to make sure that only `testThis.hs` is run when `cabal tet` commanded. Yet, this didn't work either. 
All said, I am confident in the QuickChecks of my application and I got pass for 100 tests for both tests. Also if I only implement putStrLn " " in my Main.hs, one test suit at a time will pass. 

During parsing (parse :: FilePath -> IO Y.YamlValue
), I realized that the way the Yaml library works to read YAML files produces keys that look like this: "VIX V2X VNKI". This became problematic when dealing with the leaf nodes for tree regularization and when working with weighted YAML trees. To address this issue, I wrote a function to post-process the YAML tree.

possible typos:
-In question 7, the desired output warning for instruments-hierarchy.yaml is given correctly. However, in instruments-interaction.log, the log indicates that the output for instruments-hierarchy.yaml is different. I believe this is a typo, and both outputs should be the same if the input file is instruments-hierarchy.yaml.
-There were inconsistencies in instruments_hierarchy_weighted.yaml, which was an example output of pretty printing. This was discussed with Matthijs and seems to be a typo.

I would like to take this opportunity to thank you for your time and designing this assignment for me. It has been a valuable learning experience and I did enjoy the time I was busy with it.
