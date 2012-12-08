ngram
=====

Final project for CSE 5522 @ Ohio State University

Predictive typing

Reads a file keeping track of 1,2,3-grams and uses data
to make predicitions based on previous entries



Counter.hs
    1,2,3-gram counters, weights, and function for linear interpolation

Format.hs
    strip punctuation (except for apostrophes) from text and split into words
    
Interact.hs
    main method, enter file to read in and start predicting
    
Predictor.hs
    convert 1,2,3-gram counters into predictors
    
WordBuffer.hs
    holds last 3 words for both counting and predicting

Build.sh, run.sh
    ghc commands to build and run