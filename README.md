# erlPass

An OTP library to generate passwords.


## Build

    $ rebar3 compile


## Test

Simply run eunit with rebar3

    $ rebar3 eunit


## Usage

Simple to use, just call the `generate/2` function with the 

desired attributes in list.

### Example

```
1> erlPass:generate(10, [upper,number,symbol]).
"4A0LE6\\_W@"
2> 
```

#### ***Deprecated***

```
1> erlPass:generate(10, true, true, false, true).
"mfNAA}$S,^"
2> 
```
