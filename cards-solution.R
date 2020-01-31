### Cards

Deck <- R6Class("Deck",
                
    public = list(
      
      deck = NULL,
      farbe = c("G", "H", "E", "S"),
      wert = c(6:10, "U", "O", "K", "A"),
      cards = paste0(rep(farbe, each = 9), rep(wert, times = 4)),
      
      initialize = function(deck = cards) {    # every time new initialization
        self$deck <- sample(cards)
      },
      
      reshuffle = function() {                 # reshuffle remaining cards in deck
        if (self$n() == 0) stop("No cards remained in the deck.")
        self$deck <- sample(self$deck)
        self$deck
      },
      
      n = function() {                         # how many cards remain in the deck?
        length(self$deck)
      },
      
      draw = function(n = 1) {                # draw <n> cards from deck
        checkmate::assert_integerish(n, lower = 1, upper = 36, 
                                  len = 1, any.missing = FALSE)
        if (n > self$n()) {
          stop("Only ", self$n(), " cards remaining.", call. = FALSE)
        } 
        
        output <- self$deck[seq_len(n)]
        self$deck <- self$deck[-seq_len(n)]
        output
      },
      
      refill = function() {                    # refill the deck again
        self$deck <- sample(cards)
        invisible(self)
      },
      
      cut = function() {                       # cut the deck randomly
        checkmate::assert_true(self$n() > 0)
        n <- self$n()
        allowed <- seq_len(n)
        my_cut <- sample(allowed, 1)
        self$deck <- c(self$deck[(my_cut + 1):n], self$deck[1:my_cut])
        self$deck
      }
))

deck_new <- Deck$new()
deck_new$deck

deck_new$reshuffle()
deck_new$draw(10)
deck_new$n()
deck_new$deck
deck_new$draw(15)
deck_new$reshuffle()
deck_new$refill()
deck_new$deck
deck_new$draw(15)
deck_new$cut()
deck_new$n()
deck_new$deck
deck_new$cut()
deck_new$deck
deck_new$draw(22)
deck_new$draw(21)
deck_new$draw(1)
deck_new$n()
deck_new$cut()
deck_new$reshuffle()
