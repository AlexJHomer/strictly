#' \emph{Strictly Come Dancing} dances dataset
#'
#' A dataset of every distinct dance on the BBC TV series \emph{Strictly Come
#' Dancing}.
#'
#' @format ## `dances`
#' A data frame with 2,832 rows and 17 columns:
#' \describe{
#'   \item{id}{
#'     A unique ID number for each "dance". Each dance can only have one
#'     couple performing. For most dances, the format is `S00W00D00`, where the
#'     `00`s are replaced by the series, week, and dance number in sequence. For
#'     dances where the multiple couples danced simultaneously (e.g. the
#'     "Swing-a-thon"), the format is `S00W00D00X00`, where the last two digits
#'     distinguish the different performers in the same dance.
#'   }
#'   \item{series_num}{Series number}
#'   \item{week_num}{Week number}
#'   \item{week_descrip}{The theme or special description of the week, if any}
#'   \item{couple_name}{
#'     The name of the couple as used on \emph{Strictly}. This will usually be
#'     the first name of the celebrity, then "&", then the first name of the
#'     professional dancer.
#'   }
#'   \item{total_score}{The total score received from the judges for that dance}
#'   \item{dance}{The dance style (e.g. "Cha-cha")}
#'   \item{result}{
#'     The outcome of that week's show(s) for that couple. This may be blank if
#'     no couple was eliminated.
#'   }
#'   \item{n_judges}{
#'     The number of judges awarded scores that week. Does not count "mentors"
#'     who sit on the panel but do not award scores.
#'   }
#'   \item{nth_dance}{
#'     Which number dance this is for the couple in question in the week in
#'     question
#'   }
#'   \item{group_dance_flag}{
#'     Logical: `TRUE` if multiple couples were dancing at once (corresponds to
#'     having a 12-character `id`); `FALSE` otherwise (corresponds to having a
#'     9-character `id`)
#'   }
#'   \item{dance_num}{
#'     Serial number for that week's dances; corresponds to the two digits
#'     following "D" in `id`
#'   }
#'   \item{theme, theme_detail}{
#'     A theme for the week that requires additional detail, and that additional
#'     detail—for example, `theme_detail` might identify the name of a musical.
#'     Both are `NA` if no additional detail is required, so (e.g.) "Halloween"
#'     is not one of the possible values of `theme`—use `week_descrip` instead.
#'   }
#'   \item{instant_dance_flag}{
#'     Logical: indicates whether or not the dance in question was an "Instant
#'     Dance"
#'   }
#' }
#' @source Wikipedia contributors, [Strictly Come Dancing series
#' 23](https://en.wikipedia.org/wiki/Strictly_Come_Dancing_series_23), and
#' articles for earlier series. Used under the [Creative Commons
#' Attribution-ShareAlike 4.0 International
#' licence](https://creativecommons.org/licenses/by-sa/4.0/deed.en).
"dances"
