This file contains two callable functions, classify and canProgress.

canProgress:
This function accepts a list of ModuleResults and determines whether the student has passed that year with those given modules. This is in accordance with the University of Southampton calender last updated on 01/04/19. This function is called with "canProgress moduleList" where moduleList is the list of modules.

classify:
This function accpets a list of lists of the full number of modules a student has completed over their course and then classify their qualification according to the University of Southampton calender. This function can be called with "classify yearlyModules" where yearlyModules is of the form [year1, year2, ..] and year1,year2,... are the list of modules of that year in the course.
