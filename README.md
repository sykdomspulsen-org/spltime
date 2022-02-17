# spltime <a href="https://docs.sykdomspulsen.no/spltime"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[spltime](https://docs.sykdomspulsen.no/spltime) provides date and time functions for public health purposes.

`spltime` provides convenient and consistent conversion between

- isoyear
- isoweek
- calyear
- season week

In addition, fhitime provides calendar data for reference.

Read the introduction vignette [here](http://docs.sykdomspulsen.no/spltime/articles/spltime.html) or run `help(package="spltime")`.

## splverse

<a href="https://docs.sykdomspulsen.no/packages"><img src="https://docs.sykdomspulsen.no/packages/splverse.png" align="right" width="120" /></a>

The [splverse](https://docs.sykdomspulsen.no/packages) is a set of R packages developed to help solve problems that frequently occur when performing infectious disease surveillance.

If you want to install the dev versions (or access packages that haven't been released on CRAN), run `usethis::edit_r_profile()` to edit your `.Rprofile`. 

Then write in:

```
options(
  repos = structure(c(
    SPLVERSE  = "https://docs.sykdomspulsen.no/drat/",
    CRAN      = "https://cran.rstudio.com"
  ))
)
```

Save the file and restart R.

You can now install [splverse](https://docs.sykdomspulsen.no/packages) packages from our [drat registry](https://docs.sykdomspulsen.no/drat).

```
install.packages("spltime")
```

