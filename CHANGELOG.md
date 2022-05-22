# Changelog

## 0.7.5.0

* Update stack resolver and lens upper bound.
* Added travis CI support.

Thanks to @locallycompact @peterbecich!

## 0.7.4.0

* Update Stackage nightly support to 2020-08-13, widended dependency version bounds.
* Add NFData instances for `(:->)` and `Record` / `Rec Identity`.
* Add `composite-aeson-path` with JSON formats for the `path` package.
* Add `composite-binary` with orphan instances for the `binary` package.
* Add `composite-hashable` with orphan instances for the `hashable` package.
* Add `IsoHKD Identity (s :-> a)` instance.

Thanks to @locallycompact!

## 0.7.3.0 

* Support GHC 8.10 / Stackage nightly 2020-07-19, widened dependency version bounds.

Thanks to @locallycompact!

## 0.7.2.0

* Support GHC 8.8 / LTS-16.5, widened dependency version bounds.

Thanks to @locallycompact!

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

Thanks to @dfithian!
