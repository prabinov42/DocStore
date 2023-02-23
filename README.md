# DocStore

This repo is for a system that does text mining on pdf, docx, and pptx documents.

Create a directory for these files, and (ideally) creat a subdirectory withing that houses all the above mentioned documents.

Run _PrepDocs.R_ to create a _tidydocs.csv_ that is then read and used by the _app.R_ shiny app.

Pls read the comments at the start of PrepDocs, as there is still some manual work that needs to be done to map the (long) file names to something shorter and more usable in the app.