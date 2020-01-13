# Changelog

## 0.6.1.0

* Code now compiles with Stackage LTS 14.20

## 0.6.0.0

* Code now compiles with the lastest Stackage LTS (14.3)
* Incorporates underlying Vinyl framework typeclasses instead of proxies. This has resulted in a few
  breaking changes to the composite API, mostly in the `-base` project.
* [reifyVal](http://hackage.haskell.org/package/composite-base-0.6.0.0/docs/Composite-Record.html#v:reifyVal)
  provided to assist with compiler inference.
