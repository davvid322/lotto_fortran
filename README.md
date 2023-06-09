# lotto_longshot
This is a small program that simulates a lottery, similar to the Lotto 6/49 in Canada.
I created this for two reasons:
- To re-learn the Fortran programming language (though modern Fortran is way different than
the Fortran IV / 77 I learned in my first university computer science course many decades ago), and
- To show the statistical futility of playing lotteries.
### Usage Notes
- This is a text-only program that runs in a terminal.
- The user gets to choose 6 numbers between 1 and 49, or they can let the system
do a 'quick pick' to generate a random selection for them.
- The user can specify how many games to simulate, and the system will keep
track of how many times they got 0, 1, 2...6 correct.
- At the end the program will produce some summary statistics.
- If the user chooses 100 or fewer games, the system will show the details
of each game.
- There is no limit to the number of games to simulate (unsigned 64-bit integer).
- This was created using GNU Fortran 11.3.0 on Ubuntu Linux 22.04.
