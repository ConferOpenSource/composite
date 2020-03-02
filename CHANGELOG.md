# Changelog

## 0.7.1.0

* Split out fieldJsonFormat and sumJsonFormat into encode-only and decode-only versions.

## 0.7.0.0

* Round out functions for working with decode-only or encode-only records.
* Rename ToField to ToJsonField and FromField to FromJsonField.

## 0.6.2.0

* Widen vinyl bound to >= 0.5.3 && < 0.13 (was < 0.12)
* Unbreak deriveOpaleyeEnum for newer PostgreSQL which won't implicitly convert text to an enum.

## 0.6.1.0

* Code now compiles with Stackage LTS 14.20

## 0.6.0.0

* Code now compiles with the lastest Stackage LTS (14.3)
* Incorporates underlying Vinyl framework typeclasses instead of proxies. This has resulted in a few
  breaking changes to the composite API, mostly in the `-base` project.
* [reifyVal](http://hackage.haskell.org/package/composite-base-0.6.0.0/docs/Composite-Record.html#v:reifyVal)
  provided to assist with compiler inference.
