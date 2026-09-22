#' mathsolver: BYOK AI math solver with independent verification
#' An answer is only verified=TRUE when the model's verification expression
#' (pure arithmetic) is evaluated locally and matches the answer.

.system_prompt <- paste(
  "You are a precise math solver.",
  "Reply with STRICT JSON only, no markdown fences, in this exact shape:",
  '{"answer": <number>, "steps": [<string>, ...], "verification": {"expression": "<string>"}}',
  "Rules:",
  '- "answer" must be a single number (the final result).',
  '- "steps" must be an array of short plain-language explanation strings.',
  '- "verification.expression" must be a pure arithmetic expression that',
  "  evaluates to the answer. Allowed: numbers, + - * / % ^ ( ), and the",
  "  functions abs sqrt sin cos tan ln log exp floor ceil round min max",
  "  (log is base 10, ln is natural), and the constants pi and e.",
  "- The expression must recompute the answer independently.",
  sep = "\n"
)

solver_error <- function(code, message) {
  err <- structure(list(code = code, message = message), class = c("solver_error", "error", "condition"))
  stop(err)
}

# ---------------- expression evaluator ----------------

.funs <- list(
  abs = abs, sqrt = sqrt, sin = sin, cos = cos, tan = tan,
  ln = log, log = function(x) log10(x), exp = exp,
  floor = function(x) floor(x) + 0,
  ceil = function(x) ceiling(x) + 0,
  round = function(x) round(x) + 0,
  min = min, max = max
)
.consts <- list(pi = pi, e = exp(1))

.tokenize <- function(src) {
  re <- "\\s*(?:(\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?|\\.\\d+)|([a-zA-Z_][a-zA-Z_0-9]*)|([-+*/%^(),]))"
  matches <- regmatches(src, gregexpr(re, src, perl = TRUE))[[1]]
  tokens <- list()
  for (m in matches) {
    m <- trimws(m)
    if (nzchar(m)) tokens[[length(tokens) + 1]] <- m
  }
  # coverage check: everything non-space must be consumed
  covered <- sum(nchar(gsub("\\s", "", matches)))
  total <- nchar(gsub("\\s", "", src))
  if (covered != total) solver_error("EXPR_BAD_CHAR", "unexpected character")
  tokens
}

#' Evaluate a pure arithmetic expression string.
#' @export
eval_expression <- function(src) {
  if (!is.character(src) || length(src) != 1 || !nzchar(trimws(src))) {
    solver_error("EXPR_EMPTY", "empty expression")
  }
  tokens <- .tokenize(src)
  pos <- 1
  peek <- function() if (pos <= length(tokens)) tokens[[pos]] else NULL
  eat <- function() {
    if (pos > length(tokens)) solver_error("EXPR_SYNTAX", "expected more tokens")
    t <- tokens[[pos]]
    pos <<- pos + 1
    t
  }
  is_num <- function(t) grepl("^[0-9.]", t)
  is_id <- function(t) grepl("^[a-zA-Z_]", t)

  expr <- function() {
    v <- term()
    while (!is.null(peek()) && peek() %in% c("+", "-")) {
      op <- eat()
      r <- term()
      v <- if (op == "+") v + r else v - r
    }
    v
  }
  term <- function() {
    v <- unary()
    while (!is.null(peek()) && peek() %in% c("*", "/", "%")) {
      op <- eat()
      r <- unary()
      v <- if (op == "*") v * r else if (op == "/") v / r else v %% r
    }
    v
  }
  unary <- function() {
    if (!is.null(peek()) && peek() == "-") { eat(); return(-unary()) }
    if (!is.null(peek()) && peek() == "+") { eat(); return(unary()) }
    power()
  }
  power <- function() {
    base <- atom()
    if (!is.null(peek()) && peek() == "^") {
      eat()
      return(base^unary()) # right associative
    }
    base
  }
  atom <- function() {
    t <- eat()
    if (is_num(t)) return(as.numeric(t))
    if (is_id(t)) {
      name <- tolower(t)
      if (!is.null(peek()) && peek() == "(") {
        eat()
        args <- c(expr())
        while (!is.null(peek()) && peek() == ",") { eat(); args <- c(args, expr()) }
        if (eat() != ")") solver_error("EXPR_SYNTAX", "expected )")
        fn <- .funs[[name]]
        if (is.null(fn)) solver_error("EXPR_UNKNOWN_FUNC", paste("unknown function", name))
        return(fn(args))
      }
      if (!is.null(.consts[[name]])) return(.consts[[name]])
      solver_error("EXPR_UNKNOWN_ID", paste("unknown identifier", name))
    }
    if (t == "(") {
      v <- expr()
      if (eat() != ")") solver_error("EXPR_SYNTAX", "expected )")
      return(v)
    }
    solver_error("EXPR_SYNTAX", paste("unexpected token", t))
  }

  value <- expr()
  if (pos != length(tokens) + 1) solver_error("EXPR_TRAILING", "trailing tokens")
  if (!is.finite(value)) solver_error("EXPR_NON_FINITE", "non-finite result")
  value
}

.numerically_equal <- function(a, b) {
  isTRUE(all.equal(a, b, tolerance = 1e-6, check.attributes = FALSE))
}

.parse_model_reply <- function(text) {
  start <- regexpr("\\{", text)
  end <- gregexpr("\\}", text)[[1]]
  if (start < 1 || length(end) == 0 || max(end) <= start) {
    solver_error("INVALID_JSON", "no JSON object in reply")
  }
  body <- substr(text, start, max(end))
  data <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(data)) solver_error("INVALID_JSON", "reply was not valid JSON")

  answer <- data$answer
  if (is.character(answer)) {
    m <- regmatches(answer, regexpr("-?[0-9.]+(?:[eE][+-]?[0-9]+)?", answer))
    answer <- if (length(m) == 1) as.numeric(m) else NULL
  }
  if (is.null(answer) || !is.numeric(answer)) solver_error("INVALID_JSON", "missing numeric answer")

  expression <- data$verification$expression
  if (!is.character(expression)) solver_error("INVALID_JSON", "missing verification.expression")

  steps <- if (is.list(data$steps)) unlist(data$steps) else character(0)
  list(answer = as.numeric(answer), steps = as.character(steps), expression = expression)
}

.default_transport <- function(url, body, api_key) {
  res <- tryCatch({
    httr2_available <- requireNamespace("httr2", quietly = TRUE)
    if (httr2_available) {
      httr2::request(url) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "Authorization" = paste("Bearer", api_key)
        ) |>
        httr2::req_body_raw(body, "application/json") |>
        httr2::req_perform()
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(res)) solver_error("HTTP_ERROR", "API call failed (install httr2 for HTTP support)")
  content <- tryCatch(jsonlite::fromJSON(rawToChar(res$body), simplifyVector = FALSE), error = function(e) NULL)
  text <- content$choices[[1]]$message$content
  if (!is.character(text)) solver_error("HTTP_ERROR", "missing message content")
  text
}

#' BYOK client factory for an OpenAI-compatible endpoint.
#'
#' Instantiate once, solve many:
#'
#'   solver <- math_solver(api_key = "sk-...", base_url = "https://api.deepseek.com/v1", model = "deepseek-chat")
#'   r <- solver$solve("2x + 3 = 11, solve for x")
#'   # r$verified / r$answer / r$steps / r$evaluated / r$retries
#'
#' @param api_key user's own key (BYOK)
#' @param base_url any OpenAI-compatible endpoint (default OpenAI)
#' @param model model id (default gpt-4o-mini)
#' @param transport test injection: function(url, body_json, api_key) -> model reply string
#' @return list with $solve(problem) closure
#' @export
math_solver <- function(api_key = "", base_url = "https://api.openai.com/v1",
                        model = "gpt-4o-mini", transport = NULL) {
  if (!nzchar(api_key)) solver_error("NO_API_KEY", "api_key is required (BYOK)")
  base <- sub("/+$", "", base_url)
  if (!grepl("^https?://", base)) {
    solver_error("BAD_BASE_URL", "base_url must be an http(s) URL, e.g. https://api.deepseek.com/v1")
  }
  if (is.null(transport)) transport <- .default_transport

  list(
    solve = function(problem) {
      if (!is.character(problem) || length(problem) != 1 || !nzchar(trimws(problem))) {
        solver_error("NO_PROBLEM", "problem must be non-empty")
      }
      url <- paste0(base, "/chat/completions")
      messages <- list(
        list(role = "system", content = .system_prompt),
        list(role = "user", content = problem)
      )
      call <- function() {
        body <- jsonlite::toJSON(list(model = model, messages = messages, temperature = 0), auto_unbox = TRUE)
        transport(url, body, api_key)
      }

      parsed <- tryCatch(
        .parse_model_reply(call()),
        solver_error = function(e) {
          if (e$code != "INVALID_JSON") stop(e)
          messages <<- c(messages,
            list(role = "assistant", content = "invalid JSON"),
            list(role = "user", content = "Your reply was not valid JSON. Reply again with the exact strict JSON shape.")
          )
          .parse_model_reply(call())
        }
      )

      evaluate <- function(p) {
        tryCatch({
          ev <- eval_expression(p$expression)
          list(ev = ev, ok = .numerically_equal(ev, p$answer))
        }, solver_error = function(e) list(ev = NULL, ok = FALSE))
      }

      result <- evaluate(parsed)
      evaluated <- result$ev
      verified <- result$ok
      retries <- 0

      if (!verified) {
        retries <- 1
        messages <<- c(messages, list(role = "user", content = sprintf(
          "Your verification expression evaluated to %s, which does not match your answer %s. Re-derive carefully and reply again with the same strict JSON shape.",
          if (is.null(evaluated)) "an error" else format(evaluated), format(parsed$answer)
        )))
        tryCatch({
          second <- .parse_model_reply(call())
          r2 <- evaluate(second)
          if (!is.null(r2$ev)) evaluated <- r2$ev
          if (r2$ok) { parsed <- second; verified <- TRUE }
        }, solver_error = function(e) NULL)
      }

      list(
        answer = parsed$answer, steps = parsed$steps, expression = parsed$expression,
        evaluated = evaluated, verified = verified, retries = retries
      )
    }
  )
}
