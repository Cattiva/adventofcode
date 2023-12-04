input <- readLines(here::here("2023", "data", "day4.txt"))

parsed_input <- strsplit(
  trimws(
    gsub(
      "Card\\s+\\d+:",
      "",
      input
    )
  ),
  "\\s\\|\\s"
)

extract_numbers <- function(number_string) {
  as.integer(unlist(strsplit(number_string, "\\s+")))
}

calculate_points <- function(wins) {
  if (wins > 0) {
    points <- 2^(wins - 1)
  } else {
    points <- 0
  }
  return(points)
}

extract_matches <- function(card) {
  winning_numbers <- extract_numbers(card[1])
  drawn_numbers <- extract_numbers(card[2])

  wins <- sum(drawn_numbers %in% winning_numbers)
  return(wins)
}

part_1 <- sum(
  purrr::map_dbl(
    purrr::map_dbl(
      parsed_input,
      extract_matches
    ),
    calculate_points
  )
)

cards_stack <- c()

get_new_cards <- function(no) {
  card <- parsed_input[[no]]
  matches <- extract_matches(card)

  new_cards <- seq_len(matches) + no
  return(new_cards)
}

get_copies <- function(card_no) {
  cards_stack <- c(cards_stack, get_new_cards(card_no))

  new_cards <- c(cards_stack, unlist(purrr::map(get_new_cards(card_no), get_copies)))

  return(new_cards)
}

part_2 <- length(unlist(purrr::map(seq_len(length(parsed_input)), get_copies))) + length(parsed_input)
