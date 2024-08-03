beads <- rep(c("cyan", "magenta", "yellow"), times = c(2, 5, 7))    # create an urn with 2 red, 3 blue
mean(beads != "cyan") * 0.2

nrow(permutations(3, 3))
6/336

# Assessment Question 1
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
check_jamaica <- function(x){
  all(sample(runners, x) == rep("Jamaica", x))
}
sum(replicate(10000, check_jamaica(3)))

# Assessment Question 2
meals <- function(x){
  entree <- nrow(combinations(6, 1))
  side <- nrow(combinations(x, 2))
  drink <- nrow(combinations(3, 1))
  nrow(expand.grid(seq(entree), seq(side), seq(drink)))
}
options <- data.frame("side" = 2:12)
options <- options %>%
  mutate(option = sapply(side, function(e) meals(e)))
options

# Assessment Question 3-6
head(esoph)
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

high_alc <- esoph %>% filter(alcgp == max(esoph$alcgp))
sum(high_alc$ncases) / sum(high_alc$ncases, high_alc$ncontrols)

low_alc <- esoph %>% filter(alcgp == min(esoph$alcgp))
sum(low_alc$ncases) / sum(low_alc$ncases, low_alc$ncontrols)

case <- esoph %>% filter(ncases > 0)
min_tab <- case %>% filter(case$tobgp == min(case$tobgp))
1 - sum(min_tab$ncases) / sum(case$ncases)

max_alc <- case %>% filter(case$alcgp == max(case$alcgp))
sum(max_alc$ncases) / all_cases
max_tab <- case %>% filter(case$tobgp == max(case$tobgp))
sum(max_tab$ncases) / all_cases

max_alc_tab <- case %>% filter(case$alcgp == max(case$alcgp) | case$tobgp == max(case$tobgp))
sum(max_alc_tab$ncases) / all_cases

# Controls
min_tab <- case %>% filter(case$tobgp == min(case$tobgp))
1 - sum(min_tab$ncases) / sum(case$ncases)

max_alc <- esoph %>% filter(esoph$alcgp == max(esoph$alcgp))
sum(max_alc$ncontrols) / all_controls
sum(max_alc$ncases) / all_cases
0.225 / 0.0284

sum(max_alc$ncases) / sum(max_alc$ncontrols)

max_tab <- esoph %>% filter(esoph$tobgp == max(esoph$tobgp))
sum(max_tab$ncontrols) / all_controls

max_alc_tab <- esoph %>% filter(esoph$alcgp == max(esoph$alcgp) | esoph$tobgp == max(esoph$tobgp))
sum(max_alc_tab$ncontrols) / all_controls
sum(max_alc_tab$ncases) / all_cases

0.33/0.0903
