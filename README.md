# pcgen
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

A fast pseudorandom number generator, as presented by M.E. O'Neill on
[http://www.pcg-random.org](http://www.pcg-random.org). See that site for
information on the particulars of the technique used.

This implementation uses two Word64 of internal data and produces a Word32 of
output per step. It's two to three times as fast as StdGen.

The generator implements the RandomGen typeclass from the
[random](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
package, but also provides its step function as a stand alone function that you
can use without the typeclass.
