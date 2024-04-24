# erlPass

[![release](https://img.shields.io/github/v/release/thetelefon/erlPass)](https://github.com/thetelefon/erlPass/releases/latest)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg?logo=apache&logoColor=red)](https://www.apache.org/licenses/LICENSE-2.0)
[![build](https://github.com/thetelefon/erlPass/actions/workflows/erlang.yml/badge.svg)](https://github.com/thetelefon/erlPass)
[![codecov](https://codecov.io/gh/thetelefon/erlPass/branch/master/graph/badge.svg?token=GWYPMBYL96)](https://codecov.io/gh/thetelefon/erlPass)
[![Changelog](https://img.shields.io/badge/changelog-@hex.pm-purple)](https://hexdocs.pm/erlPass/changelog.html)
[![Changelog](https://img.shields.io/badge/changelog-@github-ligthpurple)](https://github.com/thetelefon/{{name}}/blob/master/CHANGELOG.md)


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

To run the tests call eunit via rebar3

    $ rebar3 eunit


## Usage

Call `generate/1` specifying the length of the password or to specify the attributes of the generated password, use `generate/2`

### Example

```
1> erlPass:generate(10, [upper,{number,3},symbol]).
"4A0LE6\\_W@"
2> erlPass:generate(5).
"iM{37"
3>
```

Note that `erlPass:generate(10, [upper, lower]).` is the same as `erlPass:generate(10, [{upper, 10}, {lower, 10}]).`
and `erlPass:generate(10, [{upper, 0}]).` is the same as `erlPass:generate(10, []).`.
