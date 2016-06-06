# dealing with 
library(distrom)

vocab <- read.table("data/vocab.csv", row.names=1,sep=",",
	col.names=c("phrase","count","docs"))

fit <- c()
for(part in 0:20) 
	fit <- c(fit, readRDS(sprintf("output/noindustry/fit-%05d.rds",part)))

class(fit) <- "dmr"
Bbic <- coef(fit, k=log(804414))
Baic <- coef(fit)

getnz <- function(v, w=1, n=10){
	b <- exp(Bbic[v,])*w
	print(names(sort(b[b!=0],decreasing=TRUE))[1:n])
}

w = vocab[,"docs"]^{0.6}
getnz("M142", w=w)  # metals trading
getnz("GENV", w=w)  # environment
getnz("GDEF", w=w)  # defense
getnz("ECAT", w=w)  # economics
getnz("E12", w=w)  # monetary economics
getnz("E121", w=w)  # money supply


# > w = vocab[,"docs"]^{0.6}
# > getnz("M142", w=w)  # metals trading
#  [1] "gold"    "lme"     "cop"     "metal"   "comex"   "p  allad"  "silf"
#  [8] "alumin"  "bullion" "platin"
# > getnz("GENV", w=w)  # environment
#  [1] "epa"     "pollut"  "sulphur" "environ" "wildlif" "emit"    "soot"
#  [8] "soybean" "dioxid"  "specy"
# > getnz("GDEF", w=w)  # defense
#  [1] "aberdeen" "ldd"      "uld"      "chemic"   "defend"   "base"
#  [7] "forc"     "milit"    "army"     "arm"
# > getnz("ECAT", w=w)  # economics
#  [1] "nondur"  "adj"     "unadj"   "percent" "year"    "econom"  "statist"
#  [8] "month"   "growth"  "billion"
# > getnz("E12", w=w)  # monetary economics
#  [1] "polic"    "market"   "interest" "bank"     "cent"     "rate"
#  [7] "govern"   "make"     "meet"     "shar"
# > getnz("E121", w=w)  # money supply
#  [1] "avg"   "suppl" "m2"    "trln"  "wk"    "yr"    "unq"   "unch"  "nil"
# [10] "m1"

# > w = 1#vocab[,"docs"]
# > getnz("M142", w=w)  # metals trading
#  [1] "indium"  "a7e"     "tael"    "hussey"  "brandei" "bismuth" "shg"
#  [8] "spoor"   "dla"     "three"   "pgm"     "alloy"   "sharad"  "poletz"
# [15] "comex"   "pallad"  "onstad"  "lme"     "inco"    "cobalt"
# > getnz("GENV", w=w)  # environment
#  [1] "epa"         "soot"        "wildlif"     "turtl"       "greenhous"
#  [6] "denr"        "dioxid"      "orchid"      "kyot"        "co2"
# [11] "pollut"      "specy"       "environment" "habitat"     "eleph"
# [16] "zoo"         "extinct"     "emit"        "whal"        "conservat"
# > getnz("GDEF", w=w)  # environment
#  [1] "ralston"      "mckinney"     "aberdeen"     "ldd"          "uld"
#  [6] "mclachl"      "ordn"         "warhead"      "pla"          "ctbt"
# [11] "cadet"        "conscrib"     "pluton"       "serge"        "ota"
# [16] "consensual"   "bliss"        "martial"      "landmin"      "shalikashvil"
# > getnz("ECAT", w=w)  # economics
#  [1] "nondur"   "unadj"    "inven"    "adj"      "insee"    "loyn"
#  [7] "philpot"  "ons"      "acct"     "ine"      "cip"      "prel"
# [13] "hicp"     "unadjust" "manufac"  "greb"     "prelim"   "m0"
# [19] "gne"      "dec95"
# > getnz("E12", w=w)  # monetary economics
#  [1] "guill"      "harkin"     "crr"        "trichet"    "preempt"
#  [6] "wolfensohn" "gaddum"     "gehrig"     "koebnick"   "camdessus"
# [11] "endes"      "jochims"    "giscard"    "ipo"        "frenkel"
# [16] "djiwandon"  "zeitl"      "thiess"     "esta"       "sudradjad"
# > getnz("E121", w=w)  # money supply
#  [1] "trln"     "wks"      "m2"       "unq"      "wk"       "m1"
#  [7] "prelim"   "unch"     "adj"      "chg"      "m4"       "unadj"
# [13] "m0"       "avg"      "unadjust" "bsa"      "giro"     "pvs"
# [19] "m3"       "p1"

# > w = vocab[,"docs"]
# > getnz("M142", w=w)  # metals trading
#  [1] "gold"    "bank"    "metal"   "cop"     "lead"    "silf"    "lme"
#  [8] "alumin"  "invest"  "shar"    "bullion" "ounc"    "year"    "comex"
# [15] "offic"   "pallad"  "mine"    "platin"  "zinc"    "financ"
# > getnz("GENV", w=w)  # environment
#  [1] "year"     "environ"  "epa"      "million"  "percent"  "stat"
#  [7] "level"    "trad"     "high"     "sulphur"  "group"    "activ"
# [13] "end"      "month"    "pollut"   "soybean"  "expect"   "low"
# [19] "clos"     "newsroom"
# > getnz("GDEF", w=w)  # environment
#  [1] "year"    "stat"    "base"    "month"   "offic"   "report"  "forc"
#  [8] "defend"  "told"    "end"     "gener"   "add"     "million" "high"
# [15] "sale"    "expect"  "tuesday" "week"    "secur"   "clos"
# >
# > getnz("ECAT", w=w)  # economics
#  [1] "year"     "percent"  "month"    "econom"   "newsroom" "stat"
#  [7] "million"  "billion"  "expect"   "financ"   "rise"     "offic"
# [13] "market"   "rate"     "cent"     "increas"  "pric"     "growth"
# [19] "report"   "govern"
# > getnz("E12", w=w)  # monetary economics
#  [1] "market"   "year"     "bank"     "interest" "stat"     "shar"
#  [7] "polic"    "cent"     "compan"   "govern"   "rate"     "told"
# [13] "make"     "add"      "high"     "time"     "percent"  "offic"
# [19] "meet"     "plan"
# > getnz("E121", w=w)  # money supply
#  [1] "suppl"    "million"  "avg"      "week"     "billion"  "end"
#  [7] "year"     "note"     "rose"     "money"    "percent"  "newsroom"
# [13] "net"      "pct"      "total"    "month"    "yr"       "figur"
# [19] "broad"    "june"