#' nish
#'
#' nish provides discrete and continuous scales and different themes to be used
#' with ggplot2
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/rrunner/nish}
#'   \item Report issues at \url{https://github.com/rrunner/nish/issues}
#' }
#'
#' @docType package
#' @name nish
NULL

# palettes ----------

#' nish (main) colour palette that includes the majority of the colours used
#' @export
#' @examples
#'nish_colours
nish_colours <- c("#0000a0", "#3399ff", "#99ccff", "#8b8a8d", "#c9c7c7",
                  "#474748", "#f0c1ae", "#a66a5d", "#6e3629", "#bc92ed",
                  "#8548c9", "#ed8ec3", "#d6549e", "#ffda66", "#ff9873",
                  "#ff5959", "#40bfa3", "#2e8c77", "#aad369", "#8aad4c")

nish_blue_pal_col  <- nish_colours[1:7]
nish_grey_pal_col  <- nish_colours[c(4:6, 1:3, 7)]
nish_pink_pal_col  <- nish_colours[c(7, 1:3, 4:6)]
nish_mixed_pal_col <- nish_colours
nish_pos_zero_neg1 <- c(nish_colours[17], "#1d1d1b", nish_colours[16])
nish_pos_zero_neg2 <- c(nish_colours[1], "#1d1d1b", nish_colours[4])
nish_lt <- c("solid", "dashed", "solid", "dotted", "dashed", "solid", "dotted")

list_of_pals <- list(
                  blue  = nish_blue_pal_col,
                  grey  = nish_grey_pal_col,
                  pink  = nish_pink_pal_col,
                  mixed = nish_mixed_pal_col,
                  pn1   = nish_pos_zero_neg1,
                  pn2   = nish_pos_zero_neg2,
                  lt    = nish_lt
                  )

manual_palettes <- lapply(list_of_pals, scales::manual_pal)



# discrete colour and fill scales ----------

#' Create a function that returns a discrete scale
#'
#' Create a discrete scale using nish colour scheme.
#'
#' @param aes A character aesthetic. Allowed values are "colour", "fill" and
#'   "linetype" (partial matching).
#' @param pal A character vector containing the palette.
#'
#' @return A function that returns a discrete scale when called.
#'
#' @importFrom ggplot2 discrete_scale
create_discrete_scale <- function(aes = c("colour", "fill", "linetype"),
                                  pal = NULL) {

  aes <- match.arg(aes)

  # scale_name is used in error handling
  function(...) {
    ggplot2::discrete_scale(
      aesthetics = aes,
      scale_name = "nish",
      palette = pal,
      ...
      )
  }
}

#' Discrete nish scales
#'
#' @param ... Additional arguments are passed to
#'   \code{\link[ggplot2:discrete_scale]{ggplot2::discrete_scale}}().
#' @name disc
NULL

# blue

#' @rdname disc
#' @export
scale_colour_nish_blue <- create_discrete_scale(
                            aes = "colour",
                            pal = manual_palettes[["blue"]]
                            )
#' @rdname disc
#' @export
scale_color_nish_blue <- scale_colour_nish_blue

#' @rdname disc
#' @export
scale_fill_nish_blue <- create_discrete_scale(
                          aes = "fill",
                          pal = manual_palettes[["blue"]]
                          )

# grey

#' @rdname disc
#' @export
scale_colour_nish_grey <- create_discrete_scale(
                            aes = "colour",
                            pal = manual_palettes[["grey"]]
                            )

#' @rdname disc
#' @export
scale_color_nish_grey <- scale_colour_nish_grey

#' @rdname disc
#' @export
scale_fill_nish_grey <- create_discrete_scale(
                          aes = "fill",
                          pal = manual_palettes[["grey"]]
                          )

# pink

#' @rdname disc
#' @export
scale_colour_nish_pink <- create_discrete_scale(
                            aes = "colour",
                            pal = manual_palettes[["pink"]]
                            )

#' @rdname disc
#' @export
scale_color_nish_pink <- scale_colour_nish_pink

#' @rdname disc
#' @export
scale_fill_nish_pink <- create_discrete_scale(
                          aes = "fill",
                          pal = manual_palettes[["pink"]]
                          )

# mixed

#' @rdname disc
#' @export
scale_colour_nish_mixed <- create_discrete_scale(
                             aes = "colour",
                             pal = manual_palettes[["mixed"]]
                             )

#' @rdname disc
#' @export
scale_color_nish_mixed <- scale_colour_nish_mixed

#' @rdname disc
#' @export
scale_fill_nish_mixed <- create_discrete_scale(
                           aes = "fill",
                           pal = manual_palettes[["mixed"]]
                           )

# primary scale for positive, zero, negative values

#' @rdname disc
#' @export
scale_colour_nish_pos_neg <- create_discrete_scale(
                               aes = "colour",
                               pal = manual_palettes[["pn1"]]
                               )

#' @rdname disc
#' @export
scale_color_nish_pos_neg <- scale_colour_nish_pos_neg

#' @rdname disc
#' @export
scale_fill_nish_pos_neg <- create_discrete_scale(
                             aes = "fill",
                             pal = manual_palettes[["pn1"]]
                             )

# alternative scale for positive, zero, negative values

#' @rdname disc
#' @export
scale_colour_nish_pos_neg2 <- create_discrete_scale(
                                aes = "colour",
                                pal = manual_palettes[["pn2"]]
                                )

#' @rdname disc
#' @export
scale_color_nish_pos_neg2 <- scale_colour_nish_pos_neg2

#' @rdname disc
#' @export
scale_fill_nish_pos_neg2 <- create_discrete_scale(
                              aes = "fill",
                              pal = manual_palettes[["pn2"]]
                              )

# scale for linetypes (according to brand description)

#' @rdname disc
#' @export
scale_linetype_nish <- create_discrete_scale(
                        aes = "linetype",
                        pal = manual_palettes[["lt"]]
                        )



# continuous (gradient) colour and fill scales ----------


## two colour gradient (low-high) -----

#' Create a function that returns a gradient scale
#'
#' Create a gradient scale using nish colour scheme.
#'
#' If options ggplot2.continuous.colour and ggplot2.continuous.fill are unset,
#' gradient is used instead of continuous.
#'
#' @param col A character string of gradient nuance. Allowed values are "blue",
#'   "grey" and "pink" (partial matching).
#' @param aes A character aesthetic. Allowed values are "colour" and "fill"
#'   (partial matching).
#'
#' @return A function that returns a two colour gradient scale when called.
#'
#' @importFrom ggplot2 scale_colour_gradient scale_fill_gradient
create_gradient_scale <- function(col = c("blue", "grey", "pink"),
                                  aes = c("colour", "fill")) {

  aes <- match.arg(aes)
  col <- match.arg(col)

  if (col == "blue") {
    low  <- "#99ccff"
    high <- "#0000a0"
    na_value <- "#ff5959"
  } else if (col == "grey") {
    low  <- "#c9c7c7"
    high <- "#474748"
    na_value <- "#ff5959"
  } else if (col == "pink") {
    low  <- "#fcebe3"
    high <- "#fad9c8"
    na_value <- "#1d1d1b"
  }

  function(...) {
    if (aes == "colour") {
      ggplot2::scale_colour_gradient(
        low = low,
        high = high,
        na.value = na_value,
        aesthetics = aes,
        ...
        )
    } else if (aes == "fill") {
      ggplot2::scale_fill_gradient(
        low = low,
        high = high,
        na.value = na_value,
        aesthetics = aes,
        ...
        )
    }
  }
}

#' Gradient nish scales
#'
#' @param ... Additional arguments are passed to
#'   \code{\link[ggplot2:scale_gradient]{ggplot2::scale_colour_gradient}}() or
#'   \code{\link[ggplot2:scale_gradient]{ggplot2::scale_fill_gradient}}().
#' @name grad
NULL

# blue

#' @rdname grad
#' @export
scale_colour_gradient_nish_blue <- create_gradient_scale( # nolint
                                     col = "blue",
                                     aes = "colour"
                                     )

#' @rdname grad
#' @export
scale_color_gradient_nish_blue <- scale_colour_gradient_nish_blue

#' @rdname grad
#' @export
scale_fill_gradient_nish_blue <- create_gradient_scale(
                                   col = "blue",
                                   aes = "fill"
                                   )

# grey

#' @rdname grad
#' @export
scale_colour_gradient_nish_grey <- create_gradient_scale( # nolint
                                     col = "grey",
                                     aes = "colour"
                                     )

#' @rdname grad
#' @export
scale_color_gradient_nish_grey  <- scale_colour_gradient_nish_grey

#' @rdname grad
#' @export
scale_fill_gradient_nish_grey <- create_gradient_scale(
                                   col = "grey",
                                   aes = "fill"
                                   )

# pink

#' @rdname grad
#' @export
scale_colour_gradient_nish_pink <- create_gradient_scale( # nolint
                                     col = "pink",
                                     aes = "colour"
                                     )

#' @rdname grad
#' @export
scale_color_gradient_nish_pink  <- scale_colour_gradient_nish_pink

#' @rdname grad
#' @export
scale_fill_gradient_nish_pink <- create_gradient_scale(
                                   col = "pink",
                                   aes = "fill"
                                   )


## diverging colour gradient (low-mid-high) -----

#' Create a function that returns a gradient2 scale
#'
#' Create a gradient2 scale (diverging colour gradient) using nish colour
#' scheme.
#'
#' If options ggplot2.continuous.colour and ggplot2.continuous.fill are unset,
#' gradient is used instead of continuous.
#'
#' @param type A character denoting the gradient2 type. Allowed values are "pn1"
#'   and "pn2".
#' @param aes A character aesthetic. Allowed values are "colour" and "fill"
#'   (partial matching).
#'
#' @return A function that returns a diverging colour gradient scale when
#'   called.
#'
#' @importFrom ggplot2 scale_colour_gradient2 scale_fill_gradient2
create_gradient2_scale <- function(type = NULL,
                                   aes = c("colour", "fill")) {

  aes <- match.arg(aes)

  if (type == "pn1") {
    # perhaps not the best choice of divergent colours
    low  <- nish_pos_zero_neg1[3]
    mid  <- "white"
    high <- nish_pos_zero_neg1[1]
    na_value <- "grey50"
  } else if (type == "pn2") {
    # perhaps not the best choice of divergent colours
    low  <- nish_pos_zero_neg2[3]
    mid  <- "white"
    high <- nish_pos_zero_neg2[1]
    na_value <- "#ff5959"
  } else {
    stop("Type must be any of 'pn1' or 'pn2'", call. = FALSE)
  }

  function(...) {
    if (aes == "colour") {
      ggplot2::scale_colour_gradient2(
        low = low,
        mid = mid,
        high = high,
        na.value = na_value,
        aesthetics = aes,
        ...
        )
    } else if (aes == "fill") {
      ggplot2::scale_fill_gradient2(
        low = low,
        mid = mid,
        high = high,
        na.value = na_value,
        aesthetics = aes,
        ...
        )
    }
  }
}

#' Gradient2 nish scales
#'
#' @param ... Additional arguments are passed to
#'   \code{\link[ggplot2:scale_gradient]{ggplot2::scale_colour_gradient2}}() or
#'   \code{\link[ggplot2:scale_gradient]{ggplot2::scale_fill_gradient2}}().
#' @name grad2
NULL

# primary pos-zero-negative

#' @rdname grad2
#' @export
scale_colour_gradient2_nish_pos_neg <- create_gradient2_scale( # nolint
                                         type = "pn1",
                                         aes = "colour"
                                         )

#' @rdname grad2
#' @export
scale_color_gradient2_nish_pos_neg <- scale_colour_gradient2_nish_pos_neg # nolint

#' @rdname grad2
#' @export
scale_fill_gradient2_nish_pos_neg <- create_gradient2_scale( # nolint
                                       type = "pn1",
                                       aes = "fill"
                                       )
# alternative pos-zero-negative

#' @rdname grad2
#' @export
scale_colour_gradient2_nish_pos_neg2 <- create_gradient2_scale( # nolint
                                          type = "pn2",
                                          aes = "colour"
                                          )

#' @rdname grad2
#' @export
scale_color_gradient2_nish_pos_neg2 <- scale_colour_gradient2_nish_pos_neg2 # nolint

#' @rdname grad2
#' @export
scale_fill_gradient2_nish_pos_neg2 <- create_gradient2_scale( # nolint
                                        type = "pn2",
                                        aes = "fill"
                                        )


## n-colour gradient -----

#' Create a function that returns a gradientn scale
#'
#' Create a gradientn scale using nish colour scheme.
#'
#' If options ggplot2.continuous.colour and ggplot2.continuous.fill are unset,
#' gradient is used instead of continuous.
#'
#' @param aes A character aesthetic. Allowed values are "colour" and "fill"
#'   (partial matching).
#'
#' @return A function that returns a gradientn scale when called. Defaults to
#'   use the following colours, \code{nish_colours[c(7, 5, 3:1)]}.
#'
#' @importFrom ggplot2 scale_colour_gradientn scale_fill_gradientn
create_gradientn_scale <- function(aes = c("colour", "fill")) {

  aes <- match.arg(aes)

  function(...) {
    nc <- nish_colours[c(7, 5, 3:1)]
    if (aes == "colour") {
      ggplot2::scale_colour_gradientn(colors = nc, ...)
    } else if (aes == "fill") {
      ggplot2::scale_fill_gradientn(colors = nc, ...)
    }
  }
}

#' Gradientn nish scales
#'
#' @param ... Additional arguments are passed to
#'   \code{\link[ggplot2:scale_gradient]{ggplot2::scale_colour_gradientn}}() or
#'   \code{\link[ggplot2:scale_gradient]{ggplot2::scale_fill_gradientn}}(). For
#'   example, use argument 'colours' to provide your own vector of colours for
#'   the n-colour gradient.
#' @name gradn
NULL

#' @rdname gradn
#' @export
scale_colour_gradientn_nish <- create_gradientn_scale(aes = "colour")

#' @rdname gradn
#' @export
scale_color_gradientn_nish <- scale_colour_gradientn_nish

#' @rdname gradn
#' @export
scale_fill_gradientn_nish <- create_gradientn_scale(aes = "fill")



# themes ----------

#' Change theme settings conditional on 'geom_*' used in the plot
#'
#' Method to change theme settings conditional on 'geom_*' used in the plot
#'
#' The original idea was to use:
#' \code{plot$theme <- ggplot2:::add_theme(plot$theme, object, "object")}
#' for this purpose but \code{ggplot2:::add_theme}() is an internal ggplot2
#' function and thus not exported.
#'
#' Instead, the current theme in \code{object} consist of \code{theme_nish_*}
#' settings. These theme properties are further modified based on 'geom_*' in
#' this method.
#'
#' \code{plot$theme} may initially (as input to this method) be an empty list if
#' no \code{theme}() calls have been performed or set in working environment.
#' \code{object} always replaces an existing theme in \code{plot$theme}.
#'
#' Note: The conditional part of the theme settings does not work with
#' \code{ggplot2::theme_set}(). Explicit \code{theme_nish_*()} calls required to
#' enable conditional theme settings.
#'
#' @param object An object (theme) to add to the plot.
#' @param plot The ggplot object to add \code{object} to.
#' @param object_name The name of the object to add.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot_add %+replace%
ggplot_add.conditional_theme <- function(object, # nolint
                                         plot,
                                         object_name) {

  if (inherits(plot$layers[[1]]$geom, "GeomLine")) {
    object <- object +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank()
      )
  } else if (inherits(plot$layers[[1]]$geom, "GeomBar")) {
    object <- object +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank()
      )
  }

  if (ggplot2::is.theme(plot$theme)) {
    # replace (overwrite) current theme by 'nish' theme
    plot$theme <- plot$theme %+replace% object
  } else {
    # otherwise insert 'nish' theme
    plot$theme <- object
  }

  plot
}

#' nish themes
#'
#' nish themes with either light blue ("#e4f2ff"), light pink ("#fcebe3") or
#' white background. All nish themes are based on the same underlying internal
#' base theme.
#'
#' Theme settings are conditional on 'geom_*' used in the plot.
#'
#' Note:
#' \itemize{
#'   \item The conditional part of the theme settings does not work with
#' \code{ggplot2::theme_set}(). For example, by using
#' \code{old_theme <- theme_set(theme_nish_blue())}.
#' However, \code{theme_nish_*()} can be be called explicitly to enable
#' conditional theme settings.
#'   \item axis.ticks are set which does not agree to description.}
#' @name theme_nish
NULL

# base theme (white grid)
#' @noRd
theme_nish_base <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "sans",
                                   colour = "#1d1d1b"),
      axis.ticks = ggplot2::element_line(colour = "#1d1d1b"),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "white"),
      panel.grid.major.y = ggplot2::element_line(colour = "white")
      )
}

# blue theme (white grid)
#' @rdname theme_nish
#' @export
theme_nish_blue <- function() {
  t <- theme_nish_base() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#e4f2ff"),
      panel.border = ggplot2::element_rect(colour = "#e4f2ff",
                                           fill = NA)
      )

  class(t) <- c("conditional_theme", class(t))
  t
}

# pink theme (white grid)
#' @rdname theme_nish
#' @export
theme_nish_pink <- function() {
  t <- theme_nish_base() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#fcebe3"),
      panel.border = ggplot2::element_rect(colour = "#fcebe3",
                                           fill = NA)
      )

  class(t) <- c("conditional_theme", class(t))
  t
}

# white theme (blue grid)
#' @rdname theme_nish
#' @export
theme_nish_white <- function() {
  t <- theme_nish_base() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_rect(colour = "#e4f2ff",
                                           fill = NA),
      panel.grid.major.x = ggplot2::element_line(colour = "#e4f2ff"),
      panel.grid.major.y = ggplot2::element_line(colour = "#e4f2ff")
      )

  class(t) <- c("conditional_theme", class(t))
  t
}
