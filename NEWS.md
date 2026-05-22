# multiscales 0.1.1

Compatibility update for **ggplot2 >= 3.5.0** (tested against ggplot2 4.0.3).
The original 0.1.0 release depended on ggplot2 internals that were removed or
renamed during the 3.5.0 guide-system rewrite. The package now installs and the
custom `colourbox` / `colourfan` guides render again.

## Bug fixes

* **Installation failure (`object 'justify_grobs' not found`).** `justify_grobs()`
  was removed from ggplot2. It is now vendored into the package (along with its
  `rotate_just()` helper, both taken from ggplot2 3.4.4) instead of being pulled
  from `ggplot2:::`.

* **`is.waive()` removed.** ggplot2 renamed the internal `is.waive()` to
  `is_waiver()`. A self-contained `is.waive()` (checking the stable `"waiver"`
  S3 class) is now defined locally.

* **`ggname()` / `element_render()`.** These still exist as ggplot2 internals but
  are now accessed defensively, so a future rename degrades to a clear error
  rather than failing at package load time.

* **Empty / missing legend.** Under the new `GuideOld` compatibility shim,
  ggplot2 calls the legacy `guide_geom()` method with `default_mapping = NULL`.
  The package's `matched_aes()` previously depended on that default mapping to
  detect which layers used the guide's aesthetic, so the guide was silently
  dropped. `matched_aes()` now reads `layer$computed_mapping` (which already
  includes inherited plot aesthetics in ggplot2 >= 3.5.0) and is robust to a
  `NULL` default mapping.

* **Legend layout crash (`Unknown input` in `width_cm()`).** The guides assumed
  `theme$legend.key.width` / `legend.key.height` were populated directly on the
  theme. In ggplot2 >= 3.5.0 these must be resolved with `calc_element()` (with
  inheritance from `legend.key.size`). A `legend_key_dim()` helper now resolves
  them, and `legend.spacing.*` / `legend.margin` are resolved via a new
  `calc_theme_element()` helper.

* **`colourfan` non-finite viewport.** Breaks censored at the scale limits are
  represented as `NA`, which propagated through the fan's radial label
  positions into `NA` legend-layout dimensions. The `min()` calls over label
  positions are now `NA`-safe.

## Internal / dependency changes

* Dropped the dependency on the retired **plyr** package; `plyr::llply()` /
  `plyr::compact()` are replaced with base R (with a small local `compact()`).
* `DESCRIPTION` now requires `ggplot2 (>= 3.5.0)`.

## Known issues

* `pal_vsup_carto()` calls `colorspace::carto_hcl()`, which is not part of the
  exported colorspace API in recent colorspace releases. The other palette
  constructors are unaffected.
