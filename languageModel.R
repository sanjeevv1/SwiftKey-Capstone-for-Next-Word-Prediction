###########  LANGUAGE MODEL CREATION STAND ALONE CODE V2  #####################
## This code will run the in RStudion console and create 
## the matrices for the language model in a stand alone
##  fashion and these matrices will be loaded separately for the Shiny app
##  that will do the next word prediction
###############################################################################


library(dplyr)
library(ggplot2)
library(tm)
library(SnowballC)
library(caret)
library(wordcloud)
library(RWeka)
library(stringr)

##########################################
# read  text files for blogs,news and tweets
##########################################
b <- readLines(con = "data/en_US.blogs.txt", ok = TRUE, warn = TRUE,
                  encoding = "UTF-8", skipNul = FALSE)

n <- readLines(con = "data/news.txt", ok = TRUE, warn = TRUE,
               encoding = "UTF-8", skipNul = FALSE)

t <- readLines(con = "data/en_US.twitter.txt", ok = TRUE, warn = TRUE,
               encoding = "UTF-8", skipNul = FALSE)

########################################################
# create indices for random sampling of all 3 text files
########################################################
set.seed(123)
bi <- sample(1:899288,89928,replace=FALSE)
set.seed(123)
ni <- sample(1:1010242,101024,replace=FALSE)
set.seed(123)
ti <- sample(1:2360148,236014,replace=FALSE)

###############################################################
# create randomly sampled data subset,save in file,read as Corpus
###############################################################

data <- c(b[bi],n[ni],t[ti])
writeLines(data,con="~/predictv1/data/subset/subset.txt")
case <- Corpus(DirSource("~/predictv1/data/subset/"),readerControl = list(reader=readPlain,lang="en",load=TRUE,encoding="UTF-8"))


  
 BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 5)) # create n-grams
 tdm <- TermDocumentMatrix(case, control = list(tokenize = BigramTokenizer)) # creat
  # tdms <- removeSparseTerms(tdm,0.1)
  m <- as.matrix(tdm)
 rows <- rownames(m)
 
##################################################################################################
## Use TM package functions to clean data, remove special chars, convert to lower, remove numbers etc.
###################################################################################################

remString <- content_transformer(function(x, pattern) gsub(pattern, " ", x))  ####create remove string function to remove specific chars###########

case <- tm_map(case,remString, "[^[:graph:]]")
case <- tm_map(case,removeNumbers)
case <- tm_map(case,removePunctuation)
case <- tm_map(case, content_transformer(tolower))
case <- tm_map(case, remString, "\"")
case <- tm_map(case, remString, "&")
case <- tm_map(case, remString, "-")
case <- tm_map(case, remString, "--")
case <- tm_map(case, remString, "---")
case <- tm_map(case, remString, "- -")
case <- tm_map(case, remString, "$") # size increases
case <- tm_map(case, remString, "<")
case <- tm_map(case, remString, ">")
case <- tm_map(case, remString, "#")
case <- tm_map(case, remString, "!!!!")
case <- tm_map(case, remString, "!!!")
case <- tm_map(case, remString, "!!")
case <- tm_map(case, remString, "(")
case <- tm_map(case, remString, ")")
case <- tm_map(case, remString, "@")
case <- tm_map(case, remString, "%")



####################################################################################################
# create list of stopwords from list of obscene words downloaded from
# shutterstock and then remove from Corpus
###################################################################################################

 mystopwords <- c("2g1c",
                  "2 girls 1 cup",
                  "acrotomophilia",
                  "alabama hot pocket",
                  "anal",
                  "anilingus",
                  "anus",
                  "arsehole",
                  "ass",
                  "asshole",
                  "assmunch",
                  "auto erotic",
                  "autoerotic",
                  "babeland",
                  "baby batter",
                  "baby juice",
                  "ball gag",
                  "ball gravy",
                  "ball kicking",
                  "ball licking",
                  "ball sack",
                  "ball sucking",
                  "bangbros",
                  "bareback",
                  "barenaked",
                  "bastardo",
                  "bastinado",
                  "bbw",
                  "bdsm",
                  "beaver cleaver",
                  "beaver lips",
                  "bestiality",
                  "big black",
                  "big breasts",
                  "big knockers",
                  "big tits",
                  "bimbos",
                  "birdlock",
                  "bitch",
                  "bitches",
                  "black cock",
                  "blonde action",
                  "blonde on blonde action",
                  "blowjob",
                  "blow job",
                  "blow your load",
                  "blue waffle",
                  "blumpkin",
                  "bollocks",
                  "bondage",
                  "boner",
                  "boob",
                  "boobs",
                  "booty call",
                  "brown showers",
                  "brunette action",
                  "bukkake",
                  "bulldyke",
                  "bullet vibe",
                  "bullshit",
                  "bung hole",
                  "bunghole",
                  "busty",
                  "butt",
                  "buttcheeks",
                  "butthole",
                  "camel toe",
                  "camgirl",
                  "camslut",
                  "camwhore",
                  "carpet muncher",
                  "carpetmuncher",
                  "chocolate rosebuds",
                  "circlejerk",
                  "cleveland steamer",
                  "clit",
                  "clitoris",
                  "clover clamps",
                  "clusterfuck",
                  "cock",
                  "cocks",
                  "coprolagnia",
                  "coprophilia",
                  "cornhole",
                  "creampie",
                  "cum",
                  "cumming",
                  "cunnilingus",
                  "cunt",
                  "darkie",
                  "date rape",
                  "daterape",
                  "deep throat",
                  "deepthroat",
                  "dendrophilia",
                  "dick",
                  "dildo",
                  "dirty pillows",
                  "dirty sanchez",
                  "doggie style",
                  "doggiestyle",
                  "doggy style",
                  "doggystyle",
                  "dog style",
                  "dolcett",
                  "domination",
                  "dominatrix",
                  "dommes",
                  "donkey punch",
                  "double dong",
                  "double penetration",
                  "dp action",
                  "dry hump",
                  "dvda",
                  "eat my ass",
                  "ecchi",
                  "ejaculation",
                  "erotic",
                  "erotism",
                  "ethical slut",
                  "eunuch",
                  "faggot",
                  "fecal",
                  "felch",
                  "fellatio",
                  "feltch",
                  "female squirting",
                  "femdom",
                  "figging",
                  "fingerbang",
                  "fingering",
                  "fisting",
                  "foot fetish",
                  "footjob",
                  "frotting",
                  "fuck",
                  "fuck buttons",
                  "fucking",
                  "fudge packer",
                  "fudgepacker",
                  "futanari",
                  "gang bang",
                  "gay sex",
                  "genitals",
                  "giant cock",
                  "girl on",
                  "girl on top",
                  "girls gone wild",
                  "goatcx",
                  "goatse",
                  "god damn",
                  "gokkun",
                  "golden shower",
                  "goodpoop",
                  "goo girl",
                  "goregasm",
                  "grope",
                  "group sex",
                  "g-spot",
                  "guro",
                  "hand job",
                  "handjob",
                  "hard core",
                  "hardcore",
                  "hentai",
                  "homoerotic",
                  "honkey",
                  "hooker",
                  "hot carl",
                  "hot chick",
                  "how to kill",
                  "how to murder",
                  "huge fat",
                  "humping",
                  "incest",
                  "intercourse",
                  "jack off",
                  "jail bait",
                  "jailbait",
                  "jelly donut",
                  "jerk off",
                  "jigaboo",
                  "jiggaboo",
                  "jiggerboo",
                  "jizz",
                  "juggs",
                  "kike",
                  "kinbaku",
                  "kinkster",
                  "kinky",
                  "knobbing",
                  "leather restraint",
                  "leather straight jacket",
                  "lemon party",
                  "lolita",
                  "lovemaking",
                  "make me come",
                  "male squirting",
                  "masturbate",
                  "menage a trois",
                  "milf",
                  "missionary position",
                  "motherfucker",
                  "mound of venus",
                  "mr hands",
                  "muff diver",
                  "muffdiving",
                  "nambla",
                  "nawashi",
                  "negro",
                  "neonazi",
                  "nigga",
                  "nigger",
                  "nig nog",
                  "nimphomania",
                  "nipple",
                  "nipples",
                  "nsfw images",
                  "nude",
                  "nudity",
                  "nympho",
                  "nymphomania",
                  "octopussy",
                  "omorashi",
                  "one cup two girls",
                  "one guy one jar",
                  "orgasm",
                  "orgy",
                  "paedophile",
                  "panties",
                  "panty",
                  "pedobear",
                  "pedophile",
                  "pegging",
                  "penis",
                  "phone sex",
                  "piece of shit",
                  "pissing",
                  "piss pig",
                  "pisspig",
                  "playboy",
                  "pleasure chest",
                  "pole smoker",
                  "ponyplay",
                  "poof",
                  "poon",
                  "poontang",
                  "punany",
                  "poop chute",
                  "poopchute",
                  "porn",
                  "porno",
                  "pornography",
                  "prince albert piercing",
                  "pthc",
                  "pubes",
                  "pussy",
                  "queaf",
                  "raghead",
                  "raging boner",
                  "rape",
                  "raping",
                  "rapist",
                  "rectum",
                  "reverse cowgirl",
                  "rimjob",
                  "rimming",
                  "rosy palm",
                  "rosy palm and her 5 sisters",
                  "rusty trombone",
                  "sadism",
                  "santorum",
                  "scat",
                  "schlong",
                  "scissoring",
                  "semen",
                  "sex",
                  "sexo",
                  "sexy",
                  "shaved beaver",
                  "shaved pussy",
                  "shemale",
                  "shibari",
                  "shit",
                  "shitty",
                  "shota",
                  "shrimping",
                  "skeet",
                  "slanteye",
                  "slut",
                  "s&m",
                  "smut",
                  "snatch",
                  "snowballing",
                  "sodomize",
                  "sodomy",
                  "spic",
                  "splooge",
                  "splooge moose",
                  "spooge",
                  "spread legs",
                  "spunk",
                  "strap on",
                  "strapon",
                  "strappado",
                  "strip club",
                  "style doggy",
                  "suck",
                  "sucks",
                  "suicide girls",
                  "sultry women",
                  "swastika",
                  "swinger",
                  "tainted love",
                  "taste my",
                  "tea bagging",
                  "threesome",
                  "throating",
                  "tied up",
                  "tight white",
                  "tit",
                  "tits",
                  "titties",
                  "titty",
                  "tongue in a",
                  "topless",
                  "tosser",
                  "towelhead",
                  "tranny",
                  "tribadism",
                  "tub girl",
                  "tubgirl",
                  "tushy",
                  "twat",
                  "twink",
                  "twinkie",
                  "two girls one cup",
                  "undressing",
                  "upskirt",
                  "urethra play",
                  "urophilia",
                  "vagina",
                  "venus mound",
                  "vibrator",
                  "violet blue",
                  "violet wand",
                  "vorarephilia",
                  "voyeur",
                  "vulva",
                  "wank",
                  "wetback",
                  "wet dream",
                  "white power",
                  "women rapping",
                  "wrapping men",
                  "wrinkled starfish",
                  "xx",
                  "xxx",
                  "yaoi",
                  "yellow showers",
                  "yiffy",
                  "zoophilia")
case <- tm_map(case,removeWords,mystopwords)   ####### remove stopwords
case <- tm_map(case,stripWhitespace)           ####### now take out white space  

#case <- tm_map(case,stemDocument) Decided against stemming the Corpus, to increase prediction accuracy


######################################################################################
## Create language model matrices for various n-grams
#######################################################################################

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) # create n-grams
tdm <- TermDocumentMatrix(case, control = list(tokenize = BigramTokenizer))
m2 <- as.matrix(tdm)
r2 <- rownames(m2)
write.matrix(m2,file="~/predictv2/data/m2")
write.matrix(r2,file="~/predictv2/data/r2")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) # create n-grams
tdm <- TermDocumentMatrix(case, control = list(tokenize = BigramTokenizer))
m3 <- as.matrix(tdm)
r3 <- rownames(m3)
write.matrix(m3,file="~/predictv2/data/m3")
write.matrix(r3,file="~/predictv2/data/r3")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4)) # create n-grams
tdm <- TermDocumentMatrix(case, control = list(tokenize = BigramTokenizer))
m4 <- as.matrix(tdm)
r4 <- rownames(m4)
write.matrix(m4,file="~/predictv2/data/m4")
write.matrix(r4,file="~/predictv2/data/r4")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5)) # create n-grams
tdm <- TermDocumentMatrix(case, control = list(tokenize = BigramTokenizer))
m5 <- as.matrix(tdm)
r5 <- rownames(m5)
write.matrix(m5,file="~/predictv2/data/m5")
write.matrix(r5,file="~/predictv2/data/r5")

