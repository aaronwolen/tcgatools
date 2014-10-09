# Barcode component patterns
.pattern <- list(
  project     = fixed("TCGA"),
  tss         = "^[[:alnum:]]{2}$",
  participant = "^[[:alnum:]]{4}$",
  sample      = "^[[:digit:]]{2}[[:upper:]]?$",
  portion     = "^[[:digit:]]{2}[DGHRTWX]?$",
  drug        = "^[CDHIT][[:digit:]]$",
  exam        = "^E[[:digit:]]+$",
  surgery     = "^S[[:digit:]]+$",
  radiation   = "^R[[:digit:]]+$",
  slide       = "^[TBM]S[[:alnum:]]$",
  center      = "^[0-3][[:digit:]]$",
  plate       = "^[[:alnum:]]{4}$"
)

# Barcode types
.type <- list()
# 2 components
.type$tss <- .pattern[c("project", "tss")]
# 3 components
.type$participant <- c(.type$tss, .pattern["participant"])
# 4 components
.type$drug      <- c(.type$participant, .pattern["drug"])
.type$exam      <- c(.type$participant, .pattern["exam"])
.type$surgery   <- c(.type$participant, .pattern["surgery"])
.type$radiation <- c(.type$participant, .pattern["radiation"])
.type$sample    <- c(.type$participant, .pattern["sample"])
# 5 components (portion/analyte)
.type$portion   <- c(.type$sample,  .pattern["portion"])
# 6 components
.type$slide     <- c(.type$portion, .pattern["slide"]) 
# 7 components 
.type$aliquot   <- c(.type$portion, .pattern["plate"], .pattern["center"])
