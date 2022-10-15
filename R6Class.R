library(R6)

# FIFO => First In, First Out
Queue <- R6::R6Class(classname = "Queue",
  public = list(
    initialize = function(...) {
      for (item in list(...)) {
        self$add(item)
      }
    },
    public_queue = list(),
    public_length = function() base::length(self$public_queue),
    add = function(x) {
      private$queue = c(private$queue, list(x))
      self$public_queue = c(self$public_queue, list(x))
      invisible(self)
    },
    remove = function() {
      if (private$length() == 0) return(NULL)
      # Can use private$queue for explicit access
      head <- private$queue[[1]] # same head between private and public queue
      private$queue <- private$queue[-1]
      self$public_queue <- self$public_queue[-1]
      head
    }
  ),
  private = list(
    queue = list(),
    length = function() base::length(private$queue)
  )
)

# inspect class's methods and fields
Queue$private_methods
Queue$private_fields
Queue$public_fields
Queue$public_methods$public_length()

# some basic operations

a = Queue$new(5, 6, "foo")
a$public_queue # private queue is not accessible!
a$add(3)
a$add('42')
a$public_queue
a$public_length()
a$remove() # prints the removed element!
a$remove()
a$public_queue # only 3 elements remain
a$public_length()

b = a
b$remove() # REMOVED IN BOTH, NEW CLASS IS A REFERENCE NOT A DEEP COPY
b$public_length()
a$public_length()

deepa = a$clone(deep = TRUE)
deepa$add('345')
a$public_length() # didn't change
deepa$public_length() # has one more element! DEEP CLONING ACHIEVED!
