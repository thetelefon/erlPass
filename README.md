erlPass
=====

A simple password generator.

Simple to use, just call the `generate/2` function with the 

desired attributes in a list

Usage
-----

#### Example

```
1> erlPass:generate(10, [upper,number,symbol]).
"4A0LE6\\_W@"
2> 
```

##### ***Deprecated***

```
1> erlPass:generate(10, true, true, false, true).
"mfNAA}$S,^"
2> 
```


Build
-----

    $ rebar3 compile
