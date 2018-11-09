KVX: Abstract Chain Storage
===========================
[![Build Status](https://travis-ci.org/synrc/kvx.svg?branch=master)](https://travis-ci.org/synrc/kvx)

Features
--------

* Polymorphic Tuples aka Extensible Records
* Basic Schema for Storing Chains
* Backends: MNESIA, FS
* Extremely Compact: 420 LOC

Usage
-----

In rebar.config:

```erlang
{kvs, ".*", {git, "git://github.com/synrc/kvx", []}}
```

Credits
-------

* Maxim Sokhatsky

OM A HUM
