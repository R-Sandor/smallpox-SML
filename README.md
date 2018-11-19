Background:
This project was my first time writing code in SML or any functional language at all. The main challenge was thinking in terms of recursion.

The project was to create a simulation of the impact of weaponized small pox on a town or city. We worked with a population of 5000 which we had to generate randomized data that reflected U.S. Census information in C++, then we were tasked with using the statistical model from the research paper (here for more Information) .

The program would create totals for the number of people who have been contaminated each day, infected, and died, or the rare case surved the 15+ days.

Objective:
Functional Programming
Thinkining in terms of recursion
Strongly Typed Programming Language Principles



To run:
call make in source directory to build the c++ code.
then run the population generator
./population
This will generate a fairly accurate population model of the united states for a town of 5000 people.

install sml and execute the file with SML.
cd toContainingFolder
$sml
use "smallpox.sml";
Then the simulations runs and shows the results of the disease
