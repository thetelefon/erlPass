<!-- Version control

     X.Y.Z

     X - For API changes - functions, dependencies or interface

     Y - For internal or backend changes

     Z - For documentation, comments, small bugfixes, other one-two line fixes

-->

### v0.2.0

- It's now possible to specify the **maximum** number of occerrences for each of `upper`, `lower`, `number`, and `symbol` as a two-tuple e.g. `{upper, 3}`.
- `generate/1` is now implemented.

### v0.1.9

- Removed deprecated function `generate/5`
- Added more extensive tests
- Small bugfix

### v0.1.8

- Added badges for tests and code coverage

### v0.1.7

- Unit and Integration tests
- Small bugfix

### v0.1.6

Implementation of `generate/2` which support a list of the atoms

`upper`, `lower`, `number` and `symbol`

to represent which selections is made to generate a password.


### v0.1.5

Reimplemented help function for `generate/5` for pattern matching


### v0.1.4

Reimplemented the `generate/5` function to suit functional programming instead of imperative


### v0.1.0

A full implementation of erlPass.
