README

The token files are in the format 

.I <did>

.W

<textline>+

<blankline>

The did here should match those in the grels files, which are just simple triplet representations.  As far as i can tell, the hierarchy information is implicit in these matrix representations.  e.g., in the first document E11 is a child of ECAT and both are listed:

E11 2286 1
ECAT 2286 1
M11 2286 1
M12 2286 1
MCAT 2286 1

Thus, we only need the hierarchy information to define these categories.