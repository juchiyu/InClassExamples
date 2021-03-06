# US Authors and punctuation data
#===================================
# This code was first written by Kharanshu Patel and edited by Ju-Chi Yu
graphics.off()
rm(list = ls())

# Packages and functions --------------------------------------------------
library(devtools)
library(ggplot2)
library(corrplot)
library(ExPosition)
library(gutenbergr)
library(dplyr)
library(tidytext)
library(magrittr)
library(stringr)
library(stringi)
library(pbapply)

source("GetPuncCount.Author.R")

# All authors -----------------------------------------------------------------
aut <- gutenberg_authors
# authorOI <- c("Austen, Jane", "Dickens, Charles")#,

# Authors of interest ----
authorOI <- c("Christie, Agatha",
              "Einstein, Albert",
              "Huxley, Aldous",
              "Bierce, Ambrose",
              "Potter, Beatrix",
              "Franklin, Benjamin",
              "Russell, Bertrand",
              "Darwin, Charles",
              "Dickens, Charles",
              "Herr, Charlotte B. (Charlotte Bronte)",
              "Defoe, Daniel",
              "Lawrence, D. H. (David Herbert)",
              "Poe, Edgar Allan",
              "Burroughs, Edgar Rice",
              "Brontë, Emily",
              "Shaw, Bernard",
              "Washington, George",
              "James, Henry",
              "Wells, H. G. (Herbert George)",
              "Melville, Herman",
              "Asimov, Isaac",
              "London, Jack",
              "Joyce, James",
              "Austen, Jane",
              "Mill, John Stuart",
              "Swift, Jonathan",
              "Carroll, Lewis",
              # "Byron, Lord",
              "Twain, Mark",
              "Shelley, Mary Wollstonecraft",
              "Faraday, Michael",
              "Wilde, Oscar",
              "Wodehouse, P. G. (Pelham Grenville)",
              "Frost, Robert",
              "Stevenson, Robert Louis",
              "Kipling, Rudyard",
              "Doyle, Arthur Conan",
              "Galton, Francis",
              "Saki", # Here you are! "Munro, Hector Hugh"
              "Davy, Humphry, Sir",
              "Newton, Isaac, Sir",
              "Churchill, Winston",
              "Hardy, Thomas",
              "Eliot, T. S. (Thomas Stearns)",
              "Woolf, Virginia",
              "Maugham, W. Somerset (William Somerset)"
              )

authorOI_Fr <- c("Stendhal",
                 "Verne, Jules",
                 "Rostand, Edmond",
                 "Dumas, Alexandre",
                 "Proust, Marcel",
                 "Voltaire",
                 "Molière",
                 "Renard, Jules",
                 "Mallarmé, Stéphane",
                 "Louÿs, Pierre",
                 "Loti, Pierre",
                 "Daudet, Alphonse",
                 "France, Anatole",
                 "Zola, Émile",
                 "Boileau Despréaux, Nicolas",
                 "Hugo, Victor",
                 "Alain-Fournier",
                 "Baudelaire, Charles",
                 "Colette",
                 "Diderot, Denis",
                 "Danton, Georges Jacques",
                 "Gide, André",
                 "Mistral, Frédéric",
                 "Staël, Madame de (Anne-Louise-Germaine)",
                 "Vigny, Alfred de",
                 "Duhamel, Georges",
                 "Gautier, Théophile",
                 "Maupassant, Guy de",
                 "Maspero, G. (Gaston)",
                 "Achard, Amédée",
                 "Flaubert, Gustave",
                 "Balzac, Honoré de",
                 "Ségur, Sophie, comtesse de",
                 "Binet, Alfred",
                 "Anonymous",
                 "Prévost, Marcel",
                 "Bonaparte, Pierre Napoléon, prince",
                 "Lautréamont, comte de",
                 "Napoleon I, Emperor of the French",
                 "Villon, François",
                 "Sand, George",
                 "Cartier, Jacques",
                 "Poe, Edgar Allan",
                 "Musset, Alfred de",
                 "Sainte-Beuve, Charles Augustin",
                 "Allais, Alphonse",
                 "Descartes, René",
                 "Barbey d'Aurevilly, J. (Jules)",
                 "Fénelon, François de Salignac de La Mothe-",
                 "Pergaud, Louis",
                 "Various",
                 "Musset, Paul de",
                 "Vallès, Jules",
                 "Heredia, José-Maria de",
                 "La Rochefoucauld, François duc de",
                 "Verlaine, Paul",
                 "Sue, Eugène",
                 "Apollinaire, Guillaume",
                 "Bernard, Claude",
                 "Barrès, Maurice",
                 "Jarry, Alfred",
                 "Sade, marquis de",
                 "Taine, Hippolyte",
                 "Perrault, Charles",
                 "Villiers de L'Isle-Adam, Auguste, comte de",
                 "Goncourt, Edmond de",
                 "La Fontaine, Jean de",
                 "Talleyrand-Périgord, Charles Maurice de, prince de Bénévent",
                 "Beaumarchais, Pierre Augustin Caron de",
                 "Itard, Jean Marc Gaspard",
                 "Le Bon, Gustave",
                 "Restif de La Bretonne",
                 "Mirabeau, Honoré-Gabriel de Riquetti, comte de",
                 "Rimbaud, Arthur",
                 "Robespierre, Maximilien",
                 "Tocqueville, Alexis de",
                 "Rosny, Lucien de",
                 "Corneille, Pierre",
                 "Nerval, Gérard de",
                 "L'Isle-Adam, Auguste de Villiers de",
                 "Montaigne, Michel de",
                 "Voltaire, Francois-Marie de",
                 "Mauriac, François",
                 "Montesquiou-Fézensac, Robert de")

# Punctuations of interest ------------------------------------------------
matches.all <- c(",",".","?","!",":",";","-","\u2013","\'","\"") 


# Extract data ------------------------------------------------------------
AllPunctCount.Author <- pbsapply(authorOI,GetPuncCount.Author,punct = matches.all) %>% t()
AllPunctCount.AuthorFr <- pbsapply(authorOI_Fr, GetPuncCount.Author,punct = matches.all,language = 'fr') %>% t()


# Missing book  -----------------------------------------------------------
## ID 4688
id_4688 <- readtext("gutenberg_id4688.txt")
text_strip_4688 <- id_4688$text %>% paste(collapse = '\n') %>% str_replace_all("[\n\n]","")
AutPunc.count_4688 <- sapply(matches.all,  function(x) length(gregexpr(x, text_strip_4688, fixed = TRUE)[[1]]))
AutPunc.count_4688[grepl('^1$',AutPunc.count_4688)] <- 0
## Add back
AllPunctCount.AuthorFr['Mallarmé, Stéphane',] <- AutPunc.count_4688
