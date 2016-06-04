File Structure
==============
This archive contains one pdf file (`report.pdf`) with our report, as well as 
folders for the code of our solutions. 

Sudoku
------
* `sudoku.ecl`: Viewpoint 1 implemented with ECLiPSe. Runnable.
* `sudoku_row.ecl`: Viewpoint 2 implemented with ECLiPSe. Runnable.
* `sudoku.chr`: Viewpoint 1 implemented with CHR. Runnable.
* `sudoku_row.chr`: Viewpoint 2 implemented with CHR. Runnable.
* `sudex_toledo.pl`: Contains the puzzles which get imported by our code

Shikaku
-------
* `shikaku.ecl`: Shikaku solver in ECLiPSe. Runnable.
* `shikaku.chr`: Shikaku solver in CHR. Runnable.
* `puzzles.pl`: Contains the puzzles which get imported by our code
* `print.pl`: Contains the provided functionality to print Shikaku puzzles


Running the code
================
Every file marked above as "Runnable" can be executed and uses the same interface

* `solve(Name).`: Solve and show the puzzle with the given name `Name`.
* `solve_all.`: Solve all puzzles, but only one at a time. It will continue when the user presses `;`
* `solve_all_auto.`: Also solves all puzzles, but will do so without waiting for input.

Please note that the ECLiPSe code has only been tested using TkEclipse 6.1.
Use that version if you run into issues.

Documentation
=============
All our code has been documented in the source files to some extent. It is more detailed
in some areas than in others, but combined with the report we hope that the structure
of our code is easy to follow.