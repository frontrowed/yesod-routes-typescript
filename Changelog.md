## 3.0.0.1

* Starting in `yesod-core-1.6.2`, `yesod-core` started deriving `Show` for `ResourceTree` and `FlatResource`. To prevent duplicate instance errors, this package now only derives `Show` for `yesod-core < 1.6.2`.
* An implication of this is that building anything less than `yesod-routes-flow-3.0.0.1` with `yesod-core-1.6.2` will cause duplicate instance compiler errors.
