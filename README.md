RLS Habitat Quiz
------------------

A flashcard and quiz app to learn Reef Life Survey habitat labels

It launches a Shiny app from R that runs in a web interface. Habitat images and clickable label options are presented. Optionally, users can record quiz scores or add a folder of their own images and labels to learn.

Forked from: coralquiz by Matthew Doherty:  https://github.com/dohertyml/coralquiz
This version has been adapted to include RLS images, new folder structure, and renamed functions.


Installation
------------------
Install remotes if not already installed:

install.packages("remotes")

Install the RLS Habitat Quiz package from GitHub:

remotes::install_github("LizziOh/rls_habitat_quiz")

Usage
------------------
Load the package:

library(rlsquiz)

Launch practice mode (default folder: "rls_catalogue"):

rls_practice()   *this launches the standard Reef Life Survey label scheme exemplar image set*

rls("rls_catalogue")   *this launches the standard Reef Life Survey label scheme exemplar image set*

rls("rls_RRH")    *this launches the Reef Repair Hub label scheme exemplar image set*

Launch quiz mode with 20 questions:

rls_quiz()   *this launches the standard Reef Life Survey label scheme exemplar image set with 20 questions*

rls_quiz("rls_catalogue", n = 20)   *this launches the standard Reef Life Survey label scheme exemplar image set*

rls_quiz("rls_RRH", n = 20)   *this launches the Reef Repair Hub label scheme exemplar image set*

Adding new image folders
------------------
Users can adapt this without code changes by referencing their own new top-level folder containing other labels and associated images. Requires ≥ 4 species per top-level folder, with sub-folders named as label names containing associated images. Supports jpg, jpeg, JPEG, png, webp. The folder will be automatically recognised when placed in the r package library under ...rlsquiz/app/www/photos

eg. rls_practice("*top-level folder*")

Acknowledgements
------------------
Original coralquiz package by Matthew Doherty
Thanks for sharing the coralquiz framework that we adapted for Reef Life Survey habitats.
