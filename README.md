# ReverseRegexes

[![Build Status](https://travis-ci.org/carlobaldassi/ReverseRegexes.jl.png)](https://travis-ci.org/carlobaldassi/ReverseRegexes.jl)

Quick example:

```
julia> using ReverseRegexes

julia> rr"\b(using|import)\s+\w+\b" # example of reverse-regex
ReverseRegex(r"\b\w+\s+(gnisu|tropmi)\b")

julia> rsearch("Foo bar baz", rr"ba[rz]")
9:11
```
