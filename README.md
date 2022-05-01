# erlPass

[![build](https://github.com/thetelefon/erlPass/actions/workflows/erlang.yml/badge.svg)](https://github.com/thetelefon/erlPass)
[![codecov](https://codecov.io/gh/thetelefon/erlPass/branch/master/graph/badge.svg?token=GWYPMBYL96)](https://codecov.io/gh/thetelefon/erlPass)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg?logo=apache&logoColor=red)](https://www.apache.org/licenses/LICENSE-2.0)


An OTP library to generate passwords.

## Installation

### Rebar3

`erlPass` is built using **[rebar3](https://rebar3.readme.io/docs/getting-started)**,
if this library is used with rebar3 simply put it in your rebar.config file:

```
{deps, [
    erlPass
    ]}.
```

### Other

If you don't use rebar3 it's possibly to get the latest version from **[github](https://github.com/thetelefon/erlPass/releases)**.


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
