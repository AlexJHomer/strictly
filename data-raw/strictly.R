## code to prepare `strictly` dataset goes here

library(magrittr)
library(tidyverse)
library(WikipediR)
library(httr)
library(jsonlite)
library(rvest)

`%!in%` <- function(x, table) !(x %in% table)

max_series <- 23L

head_judge_lookup_prelim <- tibble(series_num = 1:max_series) |>
  mutate(
    head_judge = case_when(
      series_num <= 14L ~ "Len Goodman",
      series_num <= 22L ~ "Shirley Ballas"
    )
  )

page_text <- 1L |>
  seq(max_series) |>
  (\(x) paste("Strictly Come Dancing series", x))() |>
  map_chr(
    \(x) page_content(
      language = "en",
      project = "wikipedia",
      page_name = x,
      as_wikitext = TRUE
    ) |>
      extract2("parse") |>
      extract2("wikitext") |>
      extract2("*")
  )

extract_template_call <- function(text, template_name) {
  text |>
    str_split_1("(?=(\\{\\{|\\}\\}))") |>
    tibble(part = _) |>
    mutate(
      which_bracket = case_when(
        str_detect(part, "^\\{\\{") ~ "open",
        str_detect(part, "^\\}\\}") ~ "close",
        .default                    = "neither"
      ),
      level =
        cumsum(which_bracket == "open") - cumsum(which_bracket == "close"),
      template_started = part |>
        str_detect(paste0("^\\{\\{", str_escape(template_name), "[\n ]*\\|")) |>
        cumsum()
    ) |>
    filter(template_started == 1) |>
    mutate(
      level_been_too_low = level |>
        magrittr::extract(1) |>
        is_greater_than(level) |>
        cumsum()
    ) |>
    filter(level_been_too_low == 0) |>
    pull(part) |>
    c("}}") |>
    paste(collapse = "")
}

extract_template_argument <- function(text, arg_name, strip = TRUE) {
  start_of_arg_regex <- arg_name |>
    str_escape() |>
    (\(x) paste0("^\\| *", x, " *\\="))()
  out <- text |>
    str_split_1("(?=(\\{\\{|\\}\\}|\\|))") |>
    tibble(part = _) |>
    mutate(
      which_bracket = case_when(
        str_detect(part, "^\\{\\{") ~ "open",
        str_detect(part, "^\\}\\}") ~ "close",
        str_detect(part, "^\\|")    ~ "bar",
        .default                    = "none"
      ),
      level =
        cumsum(which_bracket == "open") - cumsum(which_bracket == "close"),
      new_arg = (which_bracket == "bar" & level == 1L),
      which_arg = cumsum(new_arg),
      n_matching_arg = part |>
        str_detect(start_of_arg_regex) |>
        and(new_arg) |>
        cumsum()
    ) |>
    filter(n_matching_arg == 1, level > 0) |>
    filter(which_arg == which_arg[1]) |>
    pull(part) |>
    paste(collapse = "")
  if (strip == TRUE) {
    out <- str_remove(out, start_of_arg_regex)
  }
  out
}

get_judges <- . %>%
  extract_template_call("Infobox reality competition season") %>%
  extract_template_argument("judges") %>%
  str_split_1("\\<br\\>") %>%
  magrittr::extract(!str_detect(., "guest")) %>%
  str_squish() %>%
  str_remove("^[^\\[]*\\[\\[") %>%
  str_remove("\\]\\][^\\]]*$")

judge_list_prelim <- map(page_text, get_judges)

poss_judges <- judge_list_prelim |>
  unlist() |>
  unique()

judge_list <- map(judge_list_prelim, \(x) factor(x, levels = poss_judges))

get_guest_judges <- . %>%
  extract_template_call("Infobox reality competition season") %>%
  extract_template_argument("judges") %>%
  str_split_1("\\<br\\>") %>%
  magrittr::extract(str_detect(., "guest")) %>%
  str_squish() %>%
  str_remove("^[^\\[]*\\[\\[") %>%
  str_remove("\\]\\][^\\]]*$")

guest_judge_list_prelim <- map(page_text, get_guest_judges)

poss_guest_judges <- guest_judge_list_prelim |>
  unlist() |>
  unique()

guest_judge_list <- map(
  guest_judge_list_prelim,
  \(x) factor(x, levels = poss_guest_judges)
)

poss_judges_all <- poss_judges |>
  c(poss_guest_judges) |>
  unique()

head_judge_lookup <- head_judge_lookup_prelim |>
  mutate(head_judge = factor(head_judge, levels = poss_judges_all))

get_section_table <- function(num) {
  GET(
    url = "https://en.wikipedia.org/w/api.php",
    query = list(
      action = "parse",
      page = paste("Strictly Come Dancing series", num),
      format = "json",
      prop = "sections"
    )
  ) |>
    parse_json() |>
    extract2("parse") |>
    extract2("sections") |>
    list_transpose() |>
    as_tibble() |>
    add_column(series_num = num, .before = 1)
}

series_sections <- 1L |>
  seq(max_series) |>
  map(get_section_table) |>
  list_rbind() |>
  mutate(across(c(level, index), parse_integer))

couples_section_tib <- filter(series_sections, line == "Couples")

get_section_by_num <- function(series_num, section_index, html = TRUE) {
  GET(
    url = "https://en.wikipedia.org/w/api.php",
    query = list(
      action = "parse",
      page = paste("Strictly Come Dancing series", series_num),
      format = "json",
      section = section_index,
      prop = if (html == TRUE) "text" else "wikitext"
    )
  ) |>
    parse_json() |>
    extract2("parse") |>
    extract2(if (html == TRUE) "text" else "wikitext") |>
    extract2("*")
}

couples_sections <- map2_chr(
  couples_section_tib$series_num,
  couples_section_tib$index,
  get_section_by_num
)

get_couples_section <- function(series_num, section_index) {
  series_num |>
    get_section_by_num(section_index) |>
    str_replace_all(str_escape("<br />"), "NEWLINE") |>
    read_html() |>
    html_table() |>
    extract2(1) |>
    rename_with(\(x) str_remove(x, "\\[[0-9]+\\]")) |>
    mutate(across(everything(), \(x) str_replace_all(x, "NEWLINE", "\n"))) |>
    add_column(series_num = series_num, .before = 1)
}

couples <- couples_section_tib$series_num |>
  map2(couples_section_tib$index, get_couples_section) |>
  list_rbind()

couples_names <- couples |>
  janitor::clean_names() |>
  separate_longer_delim(professional_partner, delim = "\n") |>
  mutate(
    professional_partner = str_remove(professional_partner, " *\\(.*\\)")
  ) |>
  separate_wider_delim(
    c(celebrity, professional_partner),
    delim = regex("(?<!(Rev\\.|Dr\\.|Judge|DJ|La)) "),
    names = c("first_name", "surname"),
    names_sep = "_",
    too_many = "merge",
    too_few = "align_start",
    cols_remove = FALSE
  ) |>
  mutate(
    n_celebs = n_distinct(celebrity_celebrity),
    .by = c(series_num, celebrity_first_name)
  ) |>
  mutate(
    start_of_couple_name = if_else(
      n_celebs == 1,
      true = celebrity_first_name,
      false = paste0(
        celebrity_first_name, " ", str_sub(celebrity_surname, 1, 1), "."
      )
    ),
    couple_name = paste(
      start_of_couple_name,
      "&",
      professional_partner_first_name
    )
  ) |>
  rename(
    celebrity = celebrity_celebrity,
    professional_partner = professional_partner_professional_partner
  )

couple_lookup <- select(
  couples_names,
  series_num, couple_name, celebrity, professional_partner
)

celeb_couple_lookup <- select(couple_lookup, -professional_partner)

judges_in_order_list <- page_text |>
  str_extract("Unless indicated otherwise[^\\.]*\\.") |>
  str_remove(".*:") |>
  str_remove_all("['.\\[\\]]") |>
  str_squish() |>
  str_split(", ") |>
  map(\(x) factor(x, levels = poss_judges))

judges_in_order_lookup <- judges_in_order_list |>
  tibble(series_judges = _) |>
  mutate(series_num = seq_along(series_judges), .before = 1)

get_weekly_table <- function(series_num, section_index) {
  series_num |>
    get_section_by_num(section_index) |>
    str_replace_all("<br />", "NEWLINE") |>
    read_html() |>
    html_table() |>
    map(\(x) rename_with(x, \(y) str_remove(y, "(\\[[0-9]+\\])+"))) |>
    list_rbind() |>
    mutate(
      across(where(is.character), \(x) str_replace_all(x, "NEWLINE", "\n"))
    )
}

# get_section_by_num(1, 5, html = FALSE)

uncleaned_scraped_weekly_results <- series_sections |>
  filter(str_detect(line, "^Week ")) |>
  mutate(
    series_table = map2(series_num, index, get_weekly_table, .progress = TRUE)
  ) |>
  unnest_longer(series_table) |>
  unnest_wider(series_table)

uncleaned_scraped_weekly_wikitext <- series_sections |>
  filter(str_detect(line, "^Week ")) |>
  mutate(
    wikitext = map2(
      series_num,
      index,
      \(x, y) get_section_by_num(x, y, html = FALSE),
      .progress = TRUE
    )
  ) |>
  unnest_longer(wikitext)

weekly_wikitext_with_weeks <- uncleaned_scraped_weekly_wikitext |>
  left_join(judges_in_order_lookup, by = "series_num") |>
  rename(week = line) |>
  mutate(
    week_num = week |> str_extract("(?<=^Week )[0-9]+") |> as.integer(),
    .after = series_num
  )

weekly_judges_lookup <- weekly_wikitext_with_weeks |>
  mutate(
    series_judges = series_judges |>
      map(\(x) fct_expand(x, poss_judges_all)),
    weekly_judges = wikitext |>
      str_extract("order from left.*\\."),
    weekly_judges = if_else(
      is.na(weekly_judges),
      true = series_judges,
      false = weekly_judges |>
        str_remove(".*:") |>
        str_remove_all("['.\\[\\]]") |>
        str_squish() |>
        str_split(", ") |>
        map(\(x) factor(x, levels = poss_judges_all))
    ),
    n_judges = map_int(weekly_judges, length)
  ) |>
  select(series_num, week_num, weekly_judges, n_judges)


prelim_output_1 <- uncleaned_scraped_weekly_results |>
  select(
    -c(
      toclevel, level, number, index, fromtitle, byteoffset, anchor, linkAnchor
    )
  ) |>
  janitor::clean_names() |>
  rename(couple_name = couple, week = line) |>
  # mutate(couple_name = str_replace_all(couple_name, "(?<= )and(?= )", "&")) |>
  # mutate(
  #   couple_name = if_else(
  #     str_count(couple_name, "&") > 1,
  #     true = str_replace_all(couple_name, "(?<=[a-z])(?=[A-Z])", "\n"),
  #     false = couple_name
  #   )
  # ) |>
  separate_longer_delim(couple_name, "\n") |>
  left_join(celeb_couple_lookup, by = c("series_num", "couple_name")) |>
  mutate(
    dance = dance |>
      str_replace_all("\n", " ") |>
      str_replace_all("[‘’]", "'") |>
      str_replace_all("[“”]", '"') |>
      str_replace_all("(?<!^|(&|Group|Lindy) |\\(|/).", str_to_lower),
    scores = scores |>
      str_replace_all("\n", " "),
    scores = if_else(scores == "No scores received", true = NA, false = scores),
    result = if_else(str_detect(result, '^"'), true = NA, false = result)
  ) |>
  separate_wider_regex(
    week,
    c("^Week ", week_num = "[0-9]+", ": ?", week_descrip = ".*$"),
    too_few = "align_start"
  ) |>
  mutate(
    orig_ix = seq_along(series_num),
    week_num = as.integer(week_num),
    week_descrip = str_remove_all(week_descrip, "</?i>")
  ) |>
  mutate(
    result = if (length(unique(result[!is.na(result)])) == 1) {
      unique(result[!is.na(result)])
    } else {
      result
    },
    .by = c(series_num, week_num, couple_name)
  ) |>
  left_join(weekly_judges_lookup, by = c("series_num", "week_num")) |>
  add_count(series_num, week_num, celebrity, name = "n_dances") |>
  mutate(
    nth_dance = seq_along(series_num),
    .by = c(series_num, week_num, celebrity)
  ) |>
  arrange(series_num, week_num, nth_dance, orig_ix) |>
  mutate(
    group_dance_flag = str_detect(str_to_lower(dance), "thon|group"),
    group_dance_name = if_else(group_dance_flag, true = dance, false = NA)
  ) |>
  mutate(
    same_group_dance = group_dance_flag & !is.na(lag(group_dance_name)) &
      group_dance_name == lag(group_dance_name),
    dance_num = seq_along(orig_ix) - cumsum(same_group_dance),
    line_id = if_else(
      group_dance_flag,
      true = cumsum(group_dance_flag),
      false = NA
    ),
    .by = c(series_num, week_num)
  ) |>
  mutate(
    across(
      c(series_num, week_num, dance_num, line_id),
      \(x) str_pad(x, 2, side = "left", pad = "0"),
      .names = "{.col}_pad"
    ),
    id = "S{series_num_pad}W{week_num_pad}D{dance_num_pad}" |>
      glue::glue() |>
      as.character(),
    id = if_else(
      group_dance_flag,
      true = "{id}X{line_id_pad}" |>
        glue::glue() |>
        as.character(),
      false = id
    ),
    music_id = str_sub(id, start = 1, end = 9)
  ) |>
  select(
    -c(group_dance_name, same_group_dance, line_id, orig_ix, ends_with("_pad"))
  ) |>
  relocate(id, music_id)

themes <- c("film", "broadway_musical", "musical", "country", "celebrating_bbc")

theme_details <- prelim_output_1 |>
  select(id, all_of(themes)) |>
  pivot_longer(-id, names_to = "theme", values_to = "theme_detail") |>
  filter(!is.na(theme_detail)) |>
  mutate(
    theme = theme |>
      str_replace_all("_", " ") |>
      str_to_sentence() |>
      str_replace("bbc", str_to_upper),
    theme_detail = theme_detail |>
      str_replace_all("\n", " ") |>
      str_squish()
  )

test_theme_details <- theme_details |>
  count(id) |>
  pull(n) |>
  is_greater_than(1L) |>
  any()

if (test_theme_details) {
  stop("Some weeks have multiple themes")
}

prelim_output_2 <- prelim_output_1 |>
  select(-all_of(themes)) |>
  left_join(theme_details, by = "id") |>
  separate_wider_delim(
    scores,
    " ",
    names = c("total_score", "breakdown"),
    too_few = "align_start",
    too_many = "merge"
  ) |>
  mutate(
    breakdown = breakdown |>
      str_remove_all("[()]") |>
      str_split(", ?")
  )

judge_scores_prelim <- prelim_output_2 |>
  select(id, series_num, total_score, weekly_judges, breakdown) |>
  unnest_longer(c(weekly_judges, breakdown)) |>
  filter(!is.na(breakdown)) |>
  rename(judge = weekly_judges, score = breakdown) |>
  mutate(score = as.integer(score))

judge_score_test <- judge_scores_prelim |>
  summarise(total_score = unique(total_score), score = sum(score), .by = id) |>
  filter(total_score != score) |>
  nrow() |>
  is_greater_than(0)

if (judge_score_test) {
  stop("Some score breakdowns don't add to the provided total.")
}

judge_scores <- judge_scores_prelim |>
  left_join(judges_in_order_lookup, by = "series_num") |>
  mutate(guest_judge_flag = map2_lgl(judge, series_judges, `%!in%`)) |>
  select(-c(starts_with("series_"), total_score))



# judge_scores |>
#   filter(!guest_judge_flag) |>
#   left_join(select(prelim2, series_num, id), by = "id") |>
#   group_by(judge, series_num) |>
#   summarise(mean_score = mean(score), .groups = "drop_last") |>
#   mutate(
#     label = if_else(series_num == max(series_num), true = judge, false = NA)
#   ) |>
#   ungroup() |>
#   mutate(judge = judge |> fct_rev() |> fct_drop()) |>
#   complete(judge, series_num) |>
#   ggplot(aes(x = series_num, y = mean_score, colour = judge, label = label)) +
#   geom_line(lwd = 2, alpha = 0.8, show.legend = FALSE, na.rm = TRUE) +
#   geom_text(colour = "black", hjust = 0, nudge_x = 0.3, na.rm = TRUE) +
#   scale_x_continuous(expand = expansion(add = c(0, 5))) +
#   theme_minimal() +
#   labs(
#     x = "Series",
#     y = "Average score"
#   )


# dat <- judge_scores |>
#   filter(!guest_judge_flag) |>
#   left_join(select(prelim2, series_num, id), by = "id") |>
#   group_by(judge, series_num) |>
#   filter(series_num >= 7, judge != "Craig Revel Horwood")
#
# lm(score ~ series_num * judge, dat) |>
#   anova()

# Add exception for group merengues in series 7

# prelim_output_2 |>
#   filter(!is.na(scores)) |>
#   separate_wider_delim(
#     scores, " ",
#     names = c("tot", "breakdown"),
#     too_few = "align_start",
#     too_many = "merge"
#   ) |>
#   select(id, tot, breakdown) |>
#   mutate(
#     tot = as.integer(tot),
#     breakdown = str_remove_all(breakdown, "[()]")
#   ) |>
#   separate_longer_delim(breakdown, ", ") |>
#   mutate(breakdown = as.integer(breakdown)) |>
#   filter(!is.na(breakdown)) |>
#   group_by(id, tot) |>
#   summarise(breakdown = sum(breakdown), .groups = "drop") |>
#   filter(tot != breakdown)


music_prelim <- prelim_output_2 |>
  select(music_id, music, theme_detail) |>
  distinct()

music_prelim_test <- music_prelim |>
  count(music_id) |>
  filter(n > 1) |>
  nrow() |>
  is_greater_than(0)

if (music_prelim_test) {
  stop(
    "Some things identified as simultaneous performances have different music.")
}

music <- music_prelim |>
  mutate(
    music = music |>
      str_remove_all("\\[[0-9]+\\]") |>
      str_replace_all("\n(?= *[Aa]ll)", " — ") |>
      str_replace_all("\n", " ")
  ) |>
  separate_longer_delim(
    music,
    regex('(\\,| (&|/)) (?=(\\"[^\\"]*\\"[^\\"]*)+[^\\"]*$)')
  ) |>
  separate_wider_delim(
    music,
    regex(" ?\u2014 ?| \u2013 "),
    names = c("song", "artist"),
    too_few = "align_start"
  ) |>
  group_by(music_id) |>
  fill(artist, .direction = "up") |>
  ungroup() |>
  mutate(
    song = song |>
      str_remove('^"') |>
      str_remove('"$'),
    theme_artist = is.na(artist) & !is.na(theme_detail),
    artist = if_else(
      theme_artist,
      true = paste0('from "', theme_detail, '"'),
      false = artist
    ),
    .keep = "unused"
  )

prelim_output_3 <- prelim_output_2 |>
  select(-c(breakdown, weekly_judges, music, celebrity)) |>
  mutate(
    instant_dance_flag =
      series_num == 23L & week_num == 10L & id %!in% judge_scores$id
  )

dance_off_lookup <- weekly_wikitext_with_weeks |>
  mutate(
    dance_off_flag = wikitext |> str_to_lower() |> str_detect("votes to save")
  ) |>
  select(series_num, week_num, dance_off_flag)

dance_off_participants <- prelim_output_3 |>
  semi_join(
    dance_off_lookup |> filter(dance_off_flag),
    by = c("series_num", "week_num")
  ) |>
  filter(result %in% c("Eliminated", "Bottom two")) |>
  distinct(series_num, week_num, couple_name)

dance_off_participants_stop <- dance_off_participants |>
  count(series_num, week_num) |>
  filter(n != 2L) |>
  nrow() |>
  is_greater_than(0L)

if (dance_off_participants_stop) {
  stop("Some dance-offs do not have exactly two participating couples.")
}

dance_off_votes_prelim <- weekly_wikitext_with_weeks |>
  semi_join(
    dance_off_lookup |> filter(dance_off_flag),
    by = c("series_num", "week_num")
  ) |>
  mutate(
    dance_off_section = wikitext |>
      str_extract("votes to save:?(\\<[^\\<\\>]*\\>)?\n(.|\n)+") |>
      str_remove("^votes to save:?(\\<[^\\<\\>]*\\>)?\n"),
    casting_vote = wikitext |>
      str_extract("(?<=\\[\\[)[A-Za-z ]+(?=\\]\\] had the casting vote)") |>
      factor(levels = poss_judges_all)
  ) |>
  left_join(head_judge_lookup, by = "series_num") |>
  mutate(
    casting_vote = if_else(is.na(casting_vote), head_judge, casting_vote)
  ) |>
  separate_longer_delim(dance_off_section, "\n") |>
  mutate(dance_off_section = str_remove(dance_off_section, "\\<ref.*$")) |>
  separate_wider_delim(
    dance_off_section,
    names = c("judge", "vote"),
    delim = regex(": ?")
  ) |>
  mutate(
    judge = judge |>
      str_remove("^\\*") |>
      str_squish()
  ) |>
  left_join(weekly_judges_lookup, by = c("series_num", "week_num")) |>
  unnest_longer(weekly_judges) |>
  mutate(
    judge_match = str_detect(weekly_judges, paste0(str_escape(judge), "$"))
  )

dance_off_votes_stop <- dance_off_votes_prelim |>
  summarise(test = sum(judge_match), .by = c(series_num, week_num, judge)) |>
  filter(test != 1L) |>
  nrow() |>
  is_greater_than(0L)

if (dance_off_votes_stop) {
  stop(
    "Some judge names in the dance-off votes match with multiple judges ",
    "or with none."
  )
}

dance_off_votes_prelim_2 <- dance_off_votes_prelim |>
  filter(judge_match) |>
  select(series_num, week_num, judge = weekly_judges, casting_vote, vote) |>
  mutate(
    vote = vote |>
      str_remove("^'*") |>
      str_remove("\\.?'*$") |>
      str_remove(" '*\\([^\\(\\)]*\\)$") |>
      str_replace(" and ", " & ") |>
      str_squish(),
    actually_voted_flag = !str_detect(vote, "Did not vote"),
    vote = if_else(
      actually_voted_flag,
      true = vote,
      false = str_extract(vote, "(?<=(for|to save) ).*$")
    )
  )

dance_off_couple_stop <- dance_off_votes_prelim_2 |>
  anti_join(couple_lookup, by = c("series_num", "vote" = "couple_name")) |>
  nrow() |>
  is_greater_than(1L)

if (dance_off_couple_stop) {
  stop("Some votes have not been correctly parsed.")
}

dance_off_casting_stop <- dance_off_votes_prelim_2 |>
  summarise(
    n_casting = sum(casting_vote == judge),
    .by = c(series_num, week_num)
  ) |>
  filter(n_casting != 1L) |>
  nrow() |>
  is_greater_than(1L)

if (dance_off_casting_stop) {
  stop(
    "Some weeks with a dance-off do not (uniquely) identify the judge with ",
    "the casting vote."
  )
}

dance_offs <- dance_off_votes_prelim_2 |>
  mutate(casting_vote_flag = judge == casting_vote) |>
  select(-casting_vote)

dances <- prelim_output_3 |>
  left_join(
    dance_off_participants |> mutate(dance_off_flag = TRUE),
    by = c("series_num", "week_num", "couple_name")
  ) |>
  replace_na(replace = list(dance_off_flag = FALSE))

usethis::use_data(dances, overwrite = TRUE)
usethis::use_data(judge_scores, overwrite = TRUE)
usethis::use_data(weekly_judges_lookup, overwrite = TRUE)
usethis::use_data(couple_lookup, overwrite = TRUE)
usethis::use_data(music, overwrite = TRUE)
usethis::use_data(dance_offs, overwrite = TRUE)
