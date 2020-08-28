Val: A Minimum Dataset Validator (for BACPAC)
=============================================

This library builds validators.

This will eventually be a conforming R package but at present is in an
early development stage.

Development
===========

This repo comes with its own development environment in the form of a
Dockerfile.

    > source docker-aliases.sh
    > bu ## build 
    > r ## run r
    > rss ## run an r studio server on port 8787
    
Running Tests
-------------

Tests are defined in the tests/testthat directory. You can run them
like so:

    > r
    >> devtools::test()
    
Tests should be grouped by context in some semi-meaningful way. 

