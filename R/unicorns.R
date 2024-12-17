#' Creates a Unicorn Data Set from simulated models.
#'
#' @param n Number of unicorns used
#'
#' @export
#'

unicorns <- function(n){
    colors <- c("White", "Black", "Gray", "Brown", "Pink", "Gold", "Silver")
    genders <- c("Female", "Male", "Non-binary", "Genderfluid", "Agender")
    Age = sample(1:20, n, replace = TRUE)
    Type_of_Unicorn = sample(c("Rainbow", "Jewel", "Ember", "Ruvas"), n, replace = TRUE)
    Elusiveness_Score = (Type_of_Unicorn == "Rainbow") * stats::rnorm(n, 34.3, 3) +
      (Type_of_Unicorn == "Jewel") * stats::rnorm(n, 34.2, 3) +
      (Type_of_Unicorn == "Ember") * stats::rnorm(n, 35.5, 3) +
      (Type_of_Unicorn == "Ruvas") * stats::rnorm(n, 35.4, 3) + stats::rnorm(n, sd = 1)
    Horn_Length = stats::rnorm(n, mean = 5, sd = 0.25)
    Type_of_Horn = sample(c("Opal", "Aquamarine"), n, replace = TRUE)
    Gentle_Score = (Type_of_Horn == "Opal") * stats::rnorm(n, 21.5, 30) +
      (Type_of_Horn == "Aquamarine") * stats::rnorm(n, 25.5, 24) +
      stats::rnorm(n, sd = 6)

    Nature_Connection_Score = (Type_of_Unicorn == "Rainbow") * (-2) +
      (Type_of_Unicorn == "Jewel") * 3 +
      (Type_of_Unicorn == "Ember") * 0 +
      (Type_of_Unicorn == "Ruvas") * 0.5 +
      (Type_of_Horn == "Opal") * 3.3 +
      (Type_of_Horn == "Aquamarine") * 0 +
      909 +
      Age * 3.4 +
      stats::rnorm(n, sd = 1.5)

    # Create a dataset of n unicorns with Magical_Score, Personality_Score, Mythical_Score, and Type_of_Unicorn
    unicorns <- data.frame(
      Unicorn_ID = 1:n,
      Age = Age,
      Gender = sample(genders, n, replace = TRUE),
      Color = sample(colors, n, replace = TRUE),
      Type_of_Unicorn = Type_of_Unicorn,
      Type_of_Horn = Type_of_Horn,
      Horn_Length = Horn_Length,
      Horn_Strength = 30 - (4 * Horn_Length / (Horn_Length + 8)) + stats::rnorm(n, 0, 2.3),
      Weight = 100 + 5.8 * Horn_Length + stats::rnorm(n, 0, 25),
      Health_Score = sample(1:10, n, replace = TRUE),
      Personality_Score = stats::rgamma(n, shape = 1, scale = 1),
      Magical_Score = 3423 + 8 * Nature_Connection_Score + stats::rnorm(n, 0, 1.8),
      Elusiveness_Score = Elusiveness_Score,
      Gentleness_Score = Gentle_Score,
      Nature_Score = Nature_Connection_Score
    )
    return(unicorns)
}

