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
#'     Name of the couple as used on \emph{Strictly}. This will usually be the
#'     first name of the celebrity, then "&", then the first name of the
#'     professional dancer.
#'   }
#'   \item{total_score}{The total score received from the judges for that dance}
#'   \item{dance}{The dance style (e.g. "Cha-cha")}
#'   \item{result}{
#'     Outcome of that week's show(s) for that couple. This may be `NA` if no
#'     couple was eliminated.
#'   }
#'   \item{n_judges}{
#'     Number of judges awarding scores that week. Does not count "mentors" who
#'     sat on the panel but did not award scores.
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

#' \emph{Strictly Come Dancing} dataset of judges' scores
#'
#' A dataset of individual judges' scores on the BBC TV series \emph{Strictly
#' Come Dancing}.
#'
#' @format ## `judge_scores`
#' A data frame with 10,913 rows and 4 columns:
#' \describe{
#'   \item{id}{ID number for the dance in the `dances` table}
#'   \item{judge}{Judge name}
#'   \item{score}{Score: an integer from 1 to 10}
#'   \item{guest_judge_flag}{
#'     Logical: `TRUE` for guest judges that were not considered one of the
#'     "main" judges for the series, and `FALSE` otherwise
#'   }
#' }
#' @source Wikipedia contributors, [Strictly Come Dancing series
#' 23](https://en.wikipedia.org/wiki/Strictly_Come_Dancing_series_23), and
#' articles for earlier series. Used under the [Creative Commons
#' Attribution-ShareAlike 4.0 International
#' licence](https://creativecommons.org/licenses/by-sa/4.0/deed.en).
"judge_scores"

#' Dataset of \emph{Strictly Come Dancing} judges
#'
#' A dataset of which judges awarded scores in each week, for BBC TV series
#' \emph{Strictly Come Dancing}.
#'
#' @format ## `weekly_judges_lookup`
#' A data frame with 279 rows and 4 columns:
#' \describe{
#'   \item{series_num}{Series number}
#'   \item{week_num}{Week number}
#'   \item{weekly_judges}{
#'     A list column. Each row's entry in the list is a factor, giving the names
#'     of the relevant judges on the panel. Does not count "mentors" who sat on
#'     the panel but did not award scores.
#'   }
#'   \item{n_judges}{
#'     Number of judges awarding scores that week. Does not count "mentors" who
#'     sat on the panel but did not award scores.
#'   }
#' }
#' @source Wikipedia contributors, [Strictly Come Dancing series
#' 23](https://en.wikipedia.org/wiki/Strictly_Come_Dancing_series_23), and
#' articles for earlier series. Used under the [Creative Commons
#' Attribution-ShareAlike 4.0 International
#' licence](https://creativecommons.org/licenses/by-sa/4.0/deed.en).
"weekly_judges_lookup"

#' Dataset of couples on \emph{Strictly Come Dancing}
#'
#' A dataset of celebrities and their professional partners on the BBC TV series
#' \emph{Strictly Come Dancing}.
#'
#' @format ## `couple_lookup`
#' A data frame with 330 rows and 4 columns:
#' \describe{
#'   \item{series_num}{Series number}
#'   \item{couple_name}{
#'     Name of the couple as used on \emph{Strictly}. This will usually be the
#'     first name of the celebrity, then "&", then the first name of the
#'     professional dancer.
#'   }
#'   \item{celebrity}{Name of celebrity}
#'   \item{professional_partner}{Name of professional dancer}
#' }
#' @source Wikipedia contributors, [Strictly Come Dancing series
#' 23](https://en.wikipedia.org/wiki/Strictly_Come_Dancing_series_23), and
#' articles for earlier series. Used under the [Creative Commons
#' Attribution-ShareAlike 4.0 International
#' licence](https://creativecommons.org/licenses/by-sa/4.0/deed.en).
"couple_lookup"

#' Dataset of songs used on \emph{Strictly Come Dancing}
#'
#' A dataset of musical pieces as used on \emph{Strictly Come Dancing}.
#'
#' @format ## `music`
#' A data frame with 2,878 rows and 4 columns:
#' \describe{
#'   \item{id}{
#'     ID number for the dance in the `dances` table. Note that, while most
#'     values appear only once in this table, some appear multiple times when
#'     a dance was performed to a medley of pieces.
#'   }
#'   \item{song}{Name of musical piece}
#'   \item{artist}{
#'     Name of original artist, in whose style the piece is typically performed
#'     (but see `theme_artist`)
#'   }
#'   \item{theme_artist}{
#'     Logical. In some cases the "artist" is not given in the source, and
#'     instead some other detail is provided—usually this is the name of a wider
#'     creative work (film, musical, etc.) in which the music is featured. To
#'     flag this, these cases are indicated by `TRUE` in this column (all other
#'     rows are `FALSE`).
#'   }
#' }
#' @source Wikipedia contributors, [Strictly Come Dancing series
#' 23](https://en.wikipedia.org/wiki/Strictly_Come_Dancing_series_23), and
#' articles for earlier series. Used under the [Creative Commons
#' Attribution-ShareAlike 4.0 International
#' licence](https://creativecommons.org/licenses/by-sa/4.0/deed.en).
"music"
