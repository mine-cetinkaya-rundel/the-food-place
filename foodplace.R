library(robotstxt)
library(tidyverse)
library(rvest)
library(tidytext)
library(glue)
library(tools)
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
#devtools::install_github("wilkelab/ggtext")
library(ggtext)

# thanks! ----------------------------------------------------------------------
# lynn fisher / the food place / data: https://thefoodplace.cafe
# claus wilke / ggtext: https://github.com/wilkelab/ggtext 
# emil hvitfeld / avg color of img: https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/

# set URL ----------------------------------------------------------------------
url <- "https://thefoodplace.cafe/"

# check if scraping is ok ------------------------------------------------------
paths_allowed(url)

# scrape -----------------------------------------------------------------------
page <- read_html(url)

nodes <- page %>%
  html_nodes("#cake .menu-item , #bottomless .menu-item , #theselection .menu-item , #brunch .menu-item , #soups .menu-item , .menu-item+ .menu-item , #entrees .menu-item , .menu-item:nth-child(1)")

names <- nodes %>% 
  html_node(".name") %>%
  html_text()

seasons_episodes <- nodes %>% 
  html_node(".episode") %>%
  html_text()

descriptions <- nodes %>% 
  html_node(".description") %>%
  html_text() %>%
  str_trim()

# construct tibble -------------------------------------------------------------
foodplace <- tibble(
  name           = names,
  description    = descriptions,
  season_episode = seasons_episodes,
) %>%
  separate(season_episode, into = c("season", "episode"))

write_csv(foodplace, "foodplace.csv")

# characters -------------------------------------------------------------------

# source: https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/
mean_img_color <- function(x) {
  data <- png::readPNG(x)
  color_freq <- names(sort(table(rgb(data[,,1], data[,,2], data[,,3])), 
                           decreasing = TRUE))
  setdiff(color_freq, c("#FFFFFF", "#000000", "#010101"))[1] # with janet fix
}

characters_vec <- c(
  "chidi",   
  "eleanor",
  "jason",
  "derek",
  "janet",
  "tahani",
  "michael",
  "simone",
  "trevor"
)

characters <- tibble(
  character = characters_vec,
  img       = glue("img/{character}.png"),
  color     = map_chr(img, mean_img_color)
) %>%
  mutate(color = if_else(character == "simone", "#AB868B", color)) # simone's pink dress

# words ------------------------------------------------------------------------

foodplace_words <- foodplace %>%
  unnest_tokens(word, name) %>%
  anti_join(get_stopwords()) %>%
  mutate(word  = str_remove(word, "’s")) %>%
  count(word, sort = TRUE) %>%
  mutate(
    type  = if_else(word %in% characters$character, "Character", "Food item")
  )

# character plot ---------------------------------------------------------------

labels <- c(
  Chidi   = "<img src='img/chidi.png' width='30' /> <br>Chidi",
  Eleanor = "<img src='img/eleanor.png' width='30' /> <br>Eleanor",
  Jason   = "<img src='img/jason.png' width='30' /> <br>Jason",
  Tahani  = "<img src='img/tahani.png' width='30' /> <br>Tahani",
  Derek   = "<img src='img/derek.png' width='30' /> <br>Derek",
  Janet   = "<img src='img/janet.png' width='30' /> <br>Janet",
  Michael = "<img src='img/michael.png' width='30' /> <br>Michael",
  Trevor  = "<img src='img/trevor.png' width='30' /> <br>Trevor",
  Simone  = "<img src='img/simone.png' width='30' /> <br>Simone"
)

plot_character <- foodplace_words %>%
  filter(type == "Character") %>%
  left_join(characters, by = c("word" = "character")) %>%
  mutate(word = toTitleCase(word)) %>%
  ggplot(aes(x = fct_reorder(word, n, .desc = TRUE), y = n, fill = color)) +
  geom_col() +
  scale_fill_identity() +
  scale_x_discrete(name = NULL, labels = labels) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(lineheight = 1.2),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
    ) +
  labs(
    y = "", 
    title = "How often do character names appear on the menu?"
    )

# food items plot --------------------------------------------------------------

plot_food <- foodplace_words %>%
  filter(type == "Food item") %>%
  slice(1:20) %>%
  ggplot(aes(x = fct_reorder(word, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "#31995C80", high = "#31995C") +
  theme_minimal() + 
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    ) +
  guides(fill = "none") +
  labs(
    x = "", 
    y = "",
    title = "What are the most common words on the menu?"
    ) +
  coord_flip() 

# patch together ---------------------------------------------------------------

plot_together <- plot_character + plot_food +
  plot_annotation(
    title = "**<span style='font-size:24pt;color:#FAD22D'>The Food Place</span>**, visualised", 
    subtitle = "*Enjoy the food and drinks you remember from Eleanor and friends’ time on Earth,<br>in the fake Good Place, the Bad Place, and the real Good Place.*",
    caption = "Source: thefoodplace.cafe by **@lynnandtonic**",
    theme = theme(
      plot.title = element_markdown(size = 16), 
      plot.subtitle = element_markdown(), 
      plot.caption = element_markdown()
    )
  )

# save 
ggsave("the-food-place.png", plot_together, width = 12, height = 5)
