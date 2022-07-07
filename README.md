This program can be run as is or edited for use.

The user can edit the square grid size(N) per a processor and the number of processors vertically(H) and horizontally(W). 
H times W must equal the total number of processors. The total number of processors(HxW) times the square grid size(N) per 
a processor will equal the total grid size(HxWxN). The grid size per a processor will be square but the user and can make
the prcessor height and and width as they please. If a user wants a specific grid size they must make all of these calculations
and adjustments themselves. The program will take care of the rest however. As is the program will test a glider on a 20x20
grid devided between 4 processors where each prcessor has a subgrid size of 10. If the user wanted to add a third row of 
processors, the H value would be changed to 3. This would create a 30x20 grid devided up evenly into 6 processors. This
scheme easily handles any load balancing issues and odd user defined dimensions. The program can also be easily edited 
to be filled with randomly distributed 1's and 0's instead of a glider. As is the program iterates 80 times and the glider
returns to its original position. 

To run this program in grape you need to run the command
    
    mpif90 GameOfLife.f90 -o game

and then 

    qsub GameOfLife.cmd

The output will be placed in a game.o[job#]

