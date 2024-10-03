# First 10 problems from Project Euler


library(numbers)
library(gmp)
library(Zseq)
library(gtools)

# Problem 1 ----

numbers <- 1:999

numbers[numbers %% 3 == 0 | numbers %% 5 == 0] |>
   sum()


# Problem 2 ----

index <- 1
fib_last_term <- 0

while (fib_last_term <= 4e6) {
   fib_sequence <- fibonacci(index, sequence = TRUE)
   fib_last_term <- fib_sequence[index]
   index <- index + 1
}

fib_sequence <- fib_sequence[fib_sequence <= 4e6]

fib_sequence[fib_sequence %% 2 == 0] |>
   sum()

# Problem 3 ----

max_prime_factor <- 
   gsub(",", "", "600,851,475,143") |>
   as.numeric() |>
   primeFactors() |> 
   max() 

max_prime_factor

# Problem 4 ----

num_vec_one <- 100:999
num_vec_two <- 100:999

number_vector <- outer(num_vec_one, num_vec_two) |>
   as.vector() |>
   sort(decreasing = TRUE)

for (i in number_vector) {
   word <- as.character(i)
   word_reversed <- strsplit(word, "") |>
      unlist() |>
      rev() |>
      paste(collapse = "")
   if (word == word_reversed) break
}

as.numeric(word)

# Problem 5 ----

candidate <- 2520
remainder <- 1

while (remainder != 0) {
   rem_vec <- candidate %% 1:20
   remainder <- sum(rem_vec)
   candidate <- candidate + 2520
}
solution <- candidate - 2520; solution

# Problem 6 ---- 

first_number <- sum(1:100)**2
second_number <- (1:100)**2 |> sum()
first_number - second_number

# Problem 7 ----

index_number <- 1  
prime_count <- 1 
while (prime_count <= 10001) {
   if (isPrime(index_number)) {
      prime_count <- prime_count + 1
      index_number <- index_number + 1
   } else {
      index_number <- index_number + 1
   } 
}

index_number - 1

# Problem 8 ----

digits_1k <- "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846106495514929086256932197846862248283972241375657056057490261407972968652414535100474821663704844031998900089524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

step_size <- 12
highest_product <- 0

for (i in 1: nchar(digits_1k)) {
   start_point <- i
   prod_string <- substr(digits_1k, start_point, start_point + step_size)
   prod_numeric <- strsplit(prod_string, "") |>
      unlist() |>
      as.numeric() |>
      prod()
   if (prod_numeric > highest_product) {
      highest_product <- prod_numeric
   }
}

highest_product

# Problem 9 ----

poss_a <- 1: floor(1000 / 3)
poss_b <- 1: floor(1000 / 2)
a <- 0
b <- 0
c <- 0
found_solution <- FALSE

for (i in seq_along(poss_a)) {
   for (j in seq_along(poss_b)) {
      k <- 1000 - i - j
      if (i**2 + j**2 == k**2) {
         a <- i
         b <- j
         c <- k
         found_solution <- TRUE
         break
      }
   }
   if (found_solution == TRUE) {
      break
   }
}

prod(a, b, c)

# Problem 10 ----

atkin_sieve(2e6) |>
   sum()









