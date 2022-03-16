# Functions---------------------------------------------------------------------

## Checks a guess against the right answer and returns exact/approx matches.
wordle_checker <- function(guess, word) {
  
  guess_vector <- strsplit(guess, '')[[1]]
  word_vector <- strsplit(word, '')[[1]]
  
  exact_match_flag <- guess_vector == word_vector
  approx_match_flag <- guess_vector %in% word_vector
  
  return(exact_match_flag + approx_match_flag)
  
}

# Takes vector and gives same length vector with probabilities substituted instead.
inline_sub_probs <- function(vector) {
  
  freq_table <- vector |> 
    table() |> 
    prop.table()
  
  freq_table[fastmatch::fmatch(vector, rownames(freq_table))]
}

# Given data frame of ngrams, find probability of occurrence for each record.
pos_score_prob <- function(data) {
  
  data |>
    as.list() |> 
    vapply(FUN = inline_sub_probs,
           FUN.VALUE = numeric(nrow(data))) |>
    (\(x) x*(1-x))() |> 
    rowSums(na.rm = T)
    
}

# Generates ngram vector given word vector and mask.
generate_ngram <- function(word_vec, mask) {
  
 mask <- as.logical(mask) 
  
 word_vec[!mask] <- "_"
 
 word_vec |>
   paste0(collapse = '') |> 
   strsplit(split = "_") |>
   magrittr::extract2(1)
}

## 'Scores' words based on frequency of n-grams. Highest score is best.
score_words <- function(word_list) {
  
  # Creates character dataframe of letters where each row is a word.
  word_vec_list <- word_list |> 
    strsplit('') |> 
    do.call(what = rbind.data.frame) |> 
    as.matrix()
  
  # # Frequency of letters for each position (1st letter, 2nd letter, etc.).
  # freq_table_pos <- word_vec_list |> 
  #   apply(2, \(x) table(x) |> prop.table()) 
  # 
  # # Score for each word based on frequency of letter for positions.
  # word_score_pos <- word_vec_list |>
  #   as.data.frame() |> 
  #   as.list() |> 
  #   mapply(FUN = \(x, y) x[match(y, names(x))],
  #          x = freq_table_pos,
  #          SIMPLIFY = F) |> 
  #   sapply(\(x) setNames(x, NULL)) |> 
  #   apply(1, \(x) prod(x*(1-x)))
  
  ngrams_word_list <- replicate(5, c(0, 1), simplify = F) |> 
    expand.grid() |>
    t() |>
    as.data.frame() |> 
    as.list() |> 
    lapply(\(x) apply(word_vec_list,
                      1,
                      generate_ngram,
                      mask = x,
                      simplify = F)) |>
    lapply(\(y) do.call(what = rbind.data.frame, y))
  
  word_score_probs <- sapply(ngrams_word_list, pos_score_prob) |> 
    rowSums()
  
  # Frequency of letters in general.
  freq_table_gen <- word_vec_list |> 
    as.vector() |>
    table() |> 
    prop.table()
  
  # Score for each word based on frequency of letters in general.
  word_score_approx <- word_vec_list |>
    apply(1, unique) |> 
    sapply(\(x, y) x[fastmatch::fmatch(y, names(x))], x= freq_table_gen) |> 
    vapply(\(x) sum(x*(1-x)), numeric(1))
  
  # Return value that combines both positional and general scores.
  word_score <- magrittr::add(word_score_probs, word_score_approx) |> 
    cbind(word_list) |> 
    as.data.frame() |> 
    dplyr::mutate(`V1` = as.numeric(`V1`))
  
  return(word_score)
  
}

## Filter word_list based on wordle_flags
word_list_filter <- function(word_list, guess, flags_for_guess) {
  
  # Get logical vectors from the info. returned by wordle_checker().
  exact_matches <- flags_for_guess == 2
  approx_matches <- flags_for_guess == 1
  no_matches <- flags_for_guess == 0
  
  # RETURN HERE if the word is correct.
  if(all(exact_matches)) return(guess)
  
  # Creates character vector/dataframe of letters where each row is a word.
  word_vec_list <- word_list |> 
    strsplit('') |> 
    do.call(what = rbind)
  guess_vec <- strsplit(guess, '')[[1]]
  
  
  # See what letters exact match the guess for each word in word_list, and then
  #   see if that logical matches the logical for exact_matches returned by
  #   wordle_checker(). The right word will have the same logical pattern to the
  #   guess as the logical returned by wordle_checker().
  exact_match_filter_flag <- apply(word_vec_list,
                                   1,
                                   \(x) x == guess_vec,
                                   simplify = F) |>
    do.call(what = rbind) |>
    apply(1, \(x) x == exact_matches, simplify = F) |>
    do.call(what = rbind) |>
    apply(1, all)
  
  # If any letters in each word match the approx. match logical returned by
  #   wordle_checker().
  check_common_letters <- function(wordvec1, wordvec2) {
    
    if (length(wordvec2) == 0) {
      return(rep(TRUE, length.out = length(wordvec1)))
    } else {
      return(all(wordvec2 %in% wordvec1))
    }
    
  }
  
  approx_match_filter_flag <-
    apply(word_vec_list,
          1,
          \(x) check_common_letters(x, guess_vec[approx_matches])
    )
  # If any of the words in word_list have letters which *aren't* excluded by the
  #   no_match logical returned by wordle_checker().
  no_match_filter_flag <- !apply(
    word_vec_list,
    1,
    \(x) any(x %in% guess_vec[no_matches],
             simplify = F)
  )
  
  # Total flag is (exact_matches + approx_matches)*no_matches.
  total_flag <- exact_match_filter_flag |> 
    magrittr::and(approx_match_filter_flag) |> 
    magrittr::and(no_match_filter_flag) |> 
    as.logical()
  
  new_word_list <- word_list |> 
    subset(total_flag) |> 
    {\(x) magrittr::extract(x, x != guess)}()
  
  return(new_word_list)
  
}

## Solves wordle based on hidden word. Hidden word is never compared to
##  anything. Recursive function that narrows down available words.
wordle_solver <- function(word_list, hidden_word) {

  # Guess is highest scoring word from word_list. Ties are broken.
  guess <- score_words(word_list) |> 
    dplyr::slice_max(`V1`, n = 1, with_ties = F) |>
    magrittr::use_series("word_list")
  
  # For information.
  print(guess)
  
  flags_for_guess <- wordle_checker(guess, hidden_word)
  
  # print(flags_for_guess)
  
  new_word_list <- word_list_filter(word_list, guess, flags_for_guess)
  
  print(new_word_list)
  
  if (length(new_word_list) == 1) {
    return(c(guess, new_word_list))
  } else {
    return(c(guess, wordle_solver(new_word_list, hidden_word = hidden_word)))
  }
  
}

# Import word list -------------------------------------------------------------

# word_list <- readLines("words.txt")
# 
# random_word <- word_list |> sample(1)
# 
# wordle_solver(word_list, random_word)
