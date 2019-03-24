
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visualize-dots-usages-in-ggplot2

## Background

To introduce [ellipsis package](https://github.com/r-lib/ellipsis) in
ggplot2 (see
[tidyverse/ggplot2\#3196](https://github.com/tidyverse/ggplot2/issues/3196)
for the context), we need to review the usages of `...` in ggplot2,
which seems impossible by hand. This repository is an attempt to
visualize it.

## Define functions

``` r
`%||%` <- rlang::`%||%`

# if x (a call) has ... in its arguments, return TRUE
has_dots <- function(x) {
  call_args <- rlang::call_args(x)
  any(purrr::map_lgl(call_args, identical, quote(...)))
}

# find the names of functions that take dots
pursue_dots <- function(x) {
  if (!rlang::is_call(x)) {
    return(NULL)
  }
  
  # if x has ..., return the function name
  if (has_dots(x)) {
    # rlang::call_name() returns NULL for anonymous functions and R6 methods
    x_call_name <- rlang::call_name(x)

    # if the call is lapply, actuall function is the second argument    
    if (identical(x_call_name, "lapply")) {
      x_call_name <- rlang::as_string(rlang::call_args(x)[[2]])
    }
    return(x_call_name %||% "")
  }
  
  # if x doesn't has ... directly, try to find ... on each argument recursively
  res <- lapply(x, pursue_dots)
  
  # remove NULL
  res <- purrr::compact(res)
  
  purrr::flatten_chr(res)
}
```

### Test

``` r
pursue_dots(body(function(...) foo(...)))
#> [1] "foo"
pursue_dots(body(function(...) bar(foo(...))))
#> [1] "foo"
pursue_dots(body(function(...) stop("not implemented")))
#> character(0)
```

## Get data

package version:

``` r
packageDescription("ggplot2")[c("Version", "GithubSHA1")]
#> $Version
#> [1] "3.1.0.9000"
#> 
#> $GithubSHA1
#> [1] "20b4b075007cab8189745293cd172b7af724e2dc"
```

``` r
library(ggplot2)

# get all objects in ggplot2's namespace
ggplot2_ns <- rlang::ns_env("ggplot2")
obj_names <- ls(ggplot2_ns)
obj_names <- purrr::set_names(obj_names)
objs <- lapply(obj_names, get, envir = ggplot2_ns)

# keep functions that takes ...
funs <- purrr::keep(objs, ~ rlang::is_function(.) && any(names(formals(.)) == "..."))

# investigate the bodies of the functions
fun_bodies <- lapply(funs, body)
res <- lapply(fun_bodies, pursue_dots)

# bind them together
res_df <- purrr::map_dfr(res, ~ data.frame(to = ., stringsAsFactors = FALSE), .id = "from")
```

## Result

### Table

``` r
knitr::kable(res_df)
```

| from                        | to                        |
| :-------------------------- | :------------------------ |
| aes                         | enquos                    |
| aes\_                       | list                      |
| aes\_q                      | list                      |
| aes\_string                 | list                      |
| annotate                    | list                      |
| annotate                    | list                      |
| annotation\_logticks        | list                      |
| annotation\_map             | list                      |
| as.list.ggproto             | as.list.environment       |
| borders                     | geom\_polygon             |
| collide                     | dapply                    |
| collide                     | dapply                    |
| collide2                    | pos                       |
| coord\_map                  | list                      |
| cut\_interval               | cut                       |
| cut\_number                 | cut                       |
| dapply                      | fun                       |
| data\_frame                 | list                      |
| datetime\_scale             | continuous\_scale         |
| dispatch\_args              | list                      |
| element\_grob.element\_line | polylineGrob              |
| element\_grob.element\_rect | rectGrob                  |
| element\_render             | element\_grob             |
| expand\_limits              | list                      |
| find\_args                  | list                      |
| geom\_abline                | list                      |
| geom\_area                  | list                      |
| geom\_bar                   | geom\_histogram           |
| geom\_bar                   | list                      |
| geom\_bin2d                 | list                      |
| geom\_blank                 | list                      |
| geom\_boxplot               | list                      |
| geom\_col                   | list                      |
| geom\_contour               | list                      |
| geom\_count                 | list                      |
| geom\_crossbar              | list                      |
| geom\_curve                 | list                      |
| geom\_density               | list                      |
| geom\_density\_2d           | list                      |
| geom\_density2d             | list                      |
| geom\_dotplot               | list                      |
| geom\_errorbar              | list                      |
| geom\_errorbarh             | list                      |
| geom\_freqpoly              | list                      |
| geom\_hex                   | list                      |
| geom\_histogram             | list                      |
| geom\_hline                 | list                      |
| geom\_jitter                | list                      |
| geom\_label                 | list                      |
| geom\_line                  | list                      |
| geom\_linerange             | list                      |
| geom\_map                   | list                      |
| geom\_path                  | list                      |
| geom\_point                 | list                      |
| geom\_pointrange            | list                      |
| geom\_polygon               | list                      |
| geom\_qq                    | list                      |
| geom\_qq\_line              | list                      |
| geom\_quantile              | list                      |
| geom\_raster                | list                      |
| geom\_rect                  | list                      |
| geom\_ribbon                | list                      |
| geom\_rug                   | list                      |
| geom\_segment               | list                      |
| geom\_sf                    | list                      |
| geom\_sf\_label             | list                      |
| geom\_sf\_text              | list                      |
| geom\_smooth                | list                      |
| geom\_spoke                 | list                      |
| geom\_step                  | list                      |
| geom\_text                  | list                      |
| geom\_tile                  | list                      |
| geom\_violin                | list                      |
| geom\_vline                 | list                      |
| ggplot.default              | fortify                   |
| ggproto                     | list                      |
| ggsave                      | dev                       |
| guide\_colorbar             | list                      |
| guide\_colourbar            | list                      |
| guide\_legend               | list                      |
| guides                      | list                      |
| interleave.default          | list                      |
| interleave.unit             | list                      |
| labeller                    | list                      |
| labs                        | list2                     |
| lims                        | list                      |
| make\_scale                 | scale                     |
| manual\_scale               | discrete\_scale           |
| map\_data                   | map                       |
| mean\_cl\_boot              | list                      |
| mean\_cl\_normal            | list                      |
| mean\_sdl                   | list                      |
| median\_hilow               | list                      |
| message\_wrap               | paste                     |
| print.ggproto               |                           |
| qplot                       | enquos                    |
| quickplot                   | enquos                    |
| scale\_alpha                | continuous\_scale         |
| scale\_alpha\_continuous    | continuous\_scale         |
| scale\_alpha\_date          | datetime\_scale           |
| scale\_alpha\_datetime      | datetime\_scale           |
| scale\_alpha\_discrete      | scale\_alpha\_ordinal     |
| scale\_alpha\_identity      | continuous\_scale         |
| scale\_alpha\_manual        | manual\_scale             |
| scale\_alpha\_ordinal       | discrete\_scale           |
| scale\_color\_brewer        | discrete\_scale           |
| scale\_color\_continuous    | scale\_colour\_gradient   |
| scale\_color\_continuous    | scale\_colour\_viridis\_c |
| scale\_color\_discrete      | discrete\_scale           |
| scale\_color\_distiller     | continuous\_scale         |
| scale\_color\_gradient      | continuous\_scale         |
| scale\_color\_gradient2     | continuous\_scale         |
| scale\_color\_gradientn     | continuous\_scale         |
| scale\_color\_grey          | discrete\_scale           |
| scale\_color\_hue           | discrete\_scale           |
| scale\_color\_identity      | discrete\_scale           |
| scale\_color\_manual        | manual\_scale             |
| scale\_color\_viridis\_c    | continuous\_scale         |
| scale\_color\_viridis\_d    | discrete\_scale           |
| scale\_colour\_brewer       | discrete\_scale           |
| scale\_colour\_continuous   | scale\_colour\_gradient   |
| scale\_colour\_continuous   | scale\_colour\_viridis\_c |
| scale\_colour\_date         | datetime\_scale           |
| scale\_colour\_datetime     | datetime\_scale           |
| scale\_colour\_discrete     | discrete\_scale           |
| scale\_colour\_distiller    | continuous\_scale         |
| scale\_colour\_gradient     | continuous\_scale         |
| scale\_colour\_gradient2    | continuous\_scale         |
| scale\_colour\_gradientn    | continuous\_scale         |
| scale\_colour\_grey         | discrete\_scale           |
| scale\_colour\_hue          | discrete\_scale           |
| scale\_colour\_identity     | discrete\_scale           |
| scale\_colour\_manual       | manual\_scale             |
| scale\_colour\_ordinal      | discrete\_scale           |
| scale\_colour\_viridis\_c   | continuous\_scale         |
| scale\_colour\_viridis\_d   | discrete\_scale           |
| scale\_continuous\_identity | continuous\_scale         |
| scale\_discrete\_identity   | discrete\_scale           |
| scale\_discrete\_manual     | manual\_scale             |
| scale\_fill\_brewer         | discrete\_scale           |
| scale\_fill\_continuous     | scale\_fill\_gradient     |
| scale\_fill\_continuous     | scale\_fill\_viridis\_c   |
| scale\_fill\_date           | datetime\_scale           |
| scale\_fill\_datetime       | datetime\_scale           |
| scale\_fill\_discrete       | discrete\_scale           |
| scale\_fill\_distiller      | continuous\_scale         |
| scale\_fill\_gradient       | continuous\_scale         |
| scale\_fill\_gradient2      | continuous\_scale         |
| scale\_fill\_gradientn      | continuous\_scale         |
| scale\_fill\_grey           | discrete\_scale           |
| scale\_fill\_hue            | discrete\_scale           |
| scale\_fill\_identity       | discrete\_scale           |
| scale\_fill\_manual         | manual\_scale             |
| scale\_fill\_ordinal        | discrete\_scale           |
| scale\_fill\_viridis\_c     | continuous\_scale         |
| scale\_fill\_viridis\_d     | discrete\_scale           |
| scale\_linetype             | discrete\_scale           |
| scale\_linetype\_discrete   | discrete\_scale           |
| scale\_linetype\_identity   | discrete\_scale           |
| scale\_linetype\_manual     | manual\_scale             |
| scale\_shape                | discrete\_scale           |
| scale\_shape\_discrete      | discrete\_scale           |
| scale\_shape\_identity      | continuous\_scale         |
| scale\_shape\_manual        | manual\_scale             |
| scale\_shape\_ordinal       | scale\_shape              |
| scale\_size\_area           | continuous\_scale         |
| scale\_size\_date           | datetime\_scale           |
| scale\_size\_datetime       | datetime\_scale           |
| scale\_size\_discrete       | scale\_size\_ordinal      |
| scale\_size\_identity       | continuous\_scale         |
| scale\_size\_manual         | manual\_scale             |
| scale\_size\_ordinal        | discrete\_scale           |
| scale\_x\_discrete          | discrete\_scale           |
| scale\_x\_log10             | scale\_x\_continuous      |
| scale\_x\_reverse           | scale\_x\_continuous      |
| scale\_x\_sqrt              | scale\_x\_continuous      |
| scale\_y\_discrete          | discrete\_scale           |
| scale\_y\_log10             | scale\_y\_continuous      |
| scale\_y\_reverse           | scale\_y\_continuous      |
| scale\_y\_sqrt              | scale\_y\_continuous      |
| stat\_bin                   | list                      |
| stat\_bin\_2d               | list                      |
| stat\_bin\_hex              | list                      |
| stat\_bin2d                 | list                      |
| stat\_binhex                | list                      |
| stat\_boxplot               | list                      |
| stat\_contour               | list                      |
| stat\_count                 | list                      |
| stat\_density               | list                      |
| stat\_density\_2d           | list                      |
| stat\_density2d             | list                      |
| stat\_ecdf                  | list                      |
| stat\_ellipse               | list                      |
| stat\_function              | list                      |
| stat\_identity              | list                      |
| stat\_qq                    | list                      |
| stat\_qq\_line              | list                      |
| stat\_quantile              | list                      |
| stat\_sf                    | list                      |
| stat\_sf\_coordinates       | list                      |
| stat\_smooth                | list                      |
| stat\_spoke                 | geom\_spoke               |
| stat\_sum                   | list                      |
| stat\_summary               | list                      |
| stat\_summary\_2d           | list                      |
| stat\_summary\_bin          | list                      |
| stat\_summary\_hex          | list                      |
| stat\_summary2d             | stat\_summary\_2d         |
| stat\_unique                | list                      |
| stat\_ydensity              | list                      |
| summarise\_by\_x            | dapply                    |
| tapply\_df                  | fun                       |
| theme                       | find\_args                |
| theme\_replace              | theme                     |
| theme\_update               | theme                     |
| transform\_position         | trans\_x                  |
| transform\_position         | trans\_y                  |
| vars                        | quos                      |
| warning\_wrap               | paste                     |
| xlim                        | c                         |
| ylim                        | c                         |

### Visualize

``` r
library(DiagrammeR)

nodes <- tibble::tibble(
  fun = unique(c(res_df$from, res_df$to)),
  type = dplyr::case_when(
    fun %in% ls(rlang::pkg_env("ggplot2")) &
      fun %in% ls(rlang::ns_env("ggplot2"))~ "exported",
    fun %in% ls(rlang::ns_env("ggplot2")) ~ "internal",
    TRUE ~ "external"
  )
)


graph <- create_graph() %>% 
  add_nodes_from_table(nodes,
                       label_col = fun, type_col = type) %>%
  colorize_node_attrs(node_attr_from = type,
                      node_attr_to = fillcolor) %>% 
  set_node_attrs(fontcolor, "black") %>%
  add_edges_from_table(res_df, from_col = from, to_col = to, from_to_map = label)
#> Warning: Prefixing `UQ()` with the rlang namespace is deprecated as of rlang 0.3.0.
#> Please use the non-prefixed form or `!!` instead.
#> 
#>   # Bad:
#>   rlang::expr(mean(rlang::UQ(var) * 100))
#> 
#>   # Ok:
#>   rlang::expr(mean(UQ(var) * 100))
#> 
#>   # Good:
#>   rlang::expr(mean(!!var * 100))
#> 
#> This warning is displayed once per session.

export_graph(graph, file_name = "result.svg", width = 1200, height = 1200)
```

Here’s result:

[![](result.svg)](result.svg)
