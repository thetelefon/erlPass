
### v0.1.6

Implementation of `generate/2` which support a list of the atoms

`upper`, `lower`, `number` and `symbol`

to represent which selections is made to generate a password.


#### Example
```
1> erlPass:generate(10, [upper,lower]).
"dwjbOuaADp"
2> 
```


### v0.1.5

Reimplemented help function for `generate/5` for pattern matching


### v0.1.4

Reimplemented the `generate/5` function to suit functional programming instead of imperative

**Issues solevd**

- *Issue 1. solevd*


### v0.1.0

A full implementation of erlPass.

**Known issues**

1. *If the length is zero it will recurse for ever.*

