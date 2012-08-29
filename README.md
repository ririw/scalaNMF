scalaNMF
========

A scala NMF library.

Use
===
Set up the matrix you wish to factorize to be an instance of <i>AbstractMatrix</i>. Then create a new instance of
EuclidianNMF, with <i>r</i> as the inner dimension for <i>W</i> and <i>H</i>.

    v = new SparseMatrix(...)
    // set up v
    val factored = new EuclidianNMF(v, r, minimumDistance, maxIterations)
    val wmat = factored.w
    val hmat = factored.h

This may take some time.

The MultiEuclidianNMF class executes several of the same problems, possibly in parallel, with different random initializations,
which can sometimes be useful for situations where the eventual factorization is heavily dependent on initial values.

Todo
====
 * Fix and upload the distributed library
 * Switch to the Breeze linear algebra library
 * Add more objective functions
 * Use the pimp-my-library pattern to make breeze more useful
 * Write more tests
 * Allow for the use of temporary files for very large matrices
 * Automate tests against matlab or octave - at the moment the test code outputs files that are trivial to read into matlab,
 where it is also trivial to compare the results.

Issues
======
 * Not fully tested
 * Only useful for smaller matrices

Dependencies/acknowledgements
=============================
 * Scalacheck
 * Scalatest
 * Google matrix toolkits
 * Breeze-math