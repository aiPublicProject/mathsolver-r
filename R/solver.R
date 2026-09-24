#' mathsolver: BYOK AI math solver with execution-based verification (v0.2)
#' Correctness model (PAL-style): the model never states the answer.
#' It returns a small JavaScript-like PROGRAM; this package executes the
#' program deterministically and the execution output IS the answer.
#' For equations, a CHECK expression ({x} placeholder) must evaluate to 0
#' when the computed answer is substituted back into the original equation.

.system_prompt <- paste(
  "You are a precise math solver.",
  "Reply with STRICT JSON only, no markdown fences, in this exact shape:",
  '{"program": "<string>", "steps": [<string>, ...], "check": "<string>"}',
  "Rules:",
  '- "program" is a small JavaScript-like program that computes the final answer.',
  '  One statement per line (or ; separated). Allowed statements:',
  '      let NAME = EXPRESSION',
  '      result = EXPRESSION',
  '  EXPRESSIONs may use numbers, + - * / % ^ ( ), the functions',
  '  abs sqrt sin cos tan ln log exp floor ceil round min max',
  '  (log is base 10, ln is natural), the constants pi and e, and any',
  '  variable defined by an earlier let. The value assigned to "result"',
  '  is the answer. Never state the answer as a number in text.',
  '- "steps" is an array of short plain-language explanation strings.',
  '- "check" is a verification expression containing the placeholder {x}.',
  '  After solving, {x} is replaced by the computed answer and the whole',
  '  expression must evaluate to 0.',
  '  For equations, substitute the answer back into the original equation',
  '  (e.g. 2x+3=11 -> "2*{x}+3-11").',
  '  For arithmetic, recompute via a different path and subtract the answer',
  '  (e.g. 15% of 80 -> "80*15/100-{x}"). Provide "check" whenever possible.',
  sep = "\n"
)

.correction_prompt <- function(reason) {
  paste0(
    "Your submission failed verification: ", reason,
    ". Re-derive the problem carefully and reply again with the same strict JSON shape."
  )
}

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
#' @param src expression source
#' @param env named list of variable bindings (case-sensitive, shadow pi/e)
#' @export
eval_expression <- function(src, env = list()) {
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
      if (!is.null(env[[t]])) return(env[[t]]) # env binds raw name, shadows constants
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

# ---------------- program interpreter ----------------

.let_re <- "^let\\s+([a-zA-Z_]\\w*)\\s*=\\s*(.+)$"
.assign_re <- "^([a-zA-Z_]\\w*)\\s*=\\s*(.+)$"

#' Execute a model-generated program. Statements (one per line or ;
#' separated): let NAME = EXPR | NAME = EXPR | bare EXPR. The answer is the
#' value of `result`, else the last bare expression. The model never states
#' the answer as a number — execution output IS the answer.
#' @param src program source
#' @export
run_program <- function(src) {
  if (!is.character(src) || length(src) != 1 || !nzchar(trimws(src))) {
    solver_error("PROGRAM_EMPTY", "empty program")
  }
  env <- list()
  result_defined <- FALSE
  last_defined <- FALSE
  last_value <- NULL
  for (raw in strsplit(src, "[;\n]+")[[1]]) {
    line <- trimws(raw)
    if (!nzchar(line)) next
    m <- regexec(.let_re, line, perl = TRUE)
    if (m[[1]][1] != -1) {
      parts <- regmatches(line, m)[[1]]
      env[[parts[2]]] <- eval_expression(parts[3], env)
      if (identical(parts[2], "result")) result_defined <- TRUE
      next
    }
    m <- regexec(.assign_re, line, perl = TRUE)
    if (m[[1]][1] != -1) {
      parts <- regmatches(line, m)[[1]]
      env[[parts[2]]] <- eval_expression(parts[3], env)
      if (identical(parts[2], "result")) result_defined <- TRUE
      next
    }
    last_value <- eval_expression(line, env)
    last_defined <- TRUE
  }
  if (result_defined) return(env[["result"]])
  if (last_defined) return(last_value)
  solver_error("PROGRAM_NO_RESULT", "program produced no result")
}

#' Substitute the computed answer into a check expression ({x} placeholder)
#' and evaluate it. Returns list(value=, passed=); passed when ~0 (scaled
#' tolerance).
#' @param check_src check expression containing {x}
#' @param answer computed answer substituted for {x}
#' @export
run_check <- function(check_src, answer) {
  substituted <- gsub("\\{\\s*x\\s*\\}", sprintf("(%s)", format(answer, digits = 17, trim = TRUE)),
                      check_src, ignore.case = TRUE, perl = TRUE)
  value <- eval_expression(substituted)
  list(value = value, passed = abs(value) <= 1e-6 * max(1, abs(answer)))
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

  program <- data$program
  if (!is.character(program) || !nzchar(trimws(program))) {
    solver_error("INVALID_JSON", "missing program")
  }

  steps <- if (is.list(data$steps)) unlist(data$steps) else character(0)

  check <- data$check
  if (!is.character(check) || length(check) != 1 || !nzchar(trimws(check))) check <- NULL

  list(program = program, steps = as.character(steps), check = check)
}

# ---------------- HTTP interface ----------------

#' Real HTTP POST (httr2). Returns list(status=, body=). Optional dependency:
#' tests inject their own http_post seam, so CI never needs the network.
.real_http_post <- function(url, headers, body) {
  ok <- requireNamespace("httr2", quietly = TRUE)
  if (!ok) solver_error("HTTP_ERROR", "API call failed (install httr2 for HTTP support)")
  req <- httr2::request(url)
  req <- httr2::req_method(req, "POST")
  req <- do.call(httr2::req_headers, c(list(req), headers))
  req <- httr2::req_body_raw(req, body, "application/json")
  req <- httr2::req_error(req, FALSE) # surface status instead of raising
  res <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(res)) solver_error("HTTP_ERROR", "API call failed")
  list(status = res$status_code, body = rawToChar(res$body))
}

.content_from_response <- function(status, raw) {
  if (status >= 300) solver_error("HTTP_ERROR", paste("API responded", status))
  content <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  text <- if (is.list(content)) content$choices[[1]]$message$content else NULL
  if (!is.character(text)) solver_error("HTTP_ERROR", "missing message content")
  text
}

.default_transport <- function(url, body, api_key) {
  reply <- .real_http_post(
    url,
    list("Content-Type" = "application/json", "Authorization" = paste("Bearer", api_key)),
    body
  )
  .content_from_response(reply$status, reply$body)
}

# ---------------- client ----------------

#' BYOK client factory for an OpenAI-compatible endpoint.
#'
#' Instantiate once, solve many:
#'
#'   solver <- math_solver(api_key = "sk-...", base_url = "https://api.deepseek.com/v1", model = "deepseek-chat")
#'   r <- solver$solve("2x + 3 = 11, solve for x")
#'   # r$answer (from executing the model's program) / r$verified / r$check_value / r$retries
#'
#' @param api_key user's own key (BYOK)
#' @param base_url any OpenAI-compatible endpoint (default OpenAI)
#' @param model model id (default gpt-4o-mini)
#' @param transport test injection: function(url, body_json, api_key) -> model reply string
#' @param http_post test seam below the default transport:
#'   function(url, headers, body_json) -> list(status=, body=) (no sockets)
#' @return list with $solve(problem) closure
#' @export
math_solver <- function(api_key = "", base_url = "https://api.openai.com/v1",
                        model = "gpt-4o-mini", transport = NULL, http_post = NULL) {
  if (!nzchar(api_key)) solver_error("NO_API_KEY", "api_key is required (BYOK)")
  base <- sub("/+$", "", base_url)
  if (!grepl("^https?://", base)) {
    solver_error("BAD_BASE_URL", "base_url must be an http(s) URL, e.g. https://api.deepseek.com/v1")
  }
  if (is.null(transport)) {
    hp <- if (is.null(http_post)) .real_http_post else http_post
    transport <- function(url, body, api_key) {
      reply <- hp(url, list("Content-Type" = "application/json", "Authorization" = paste("Bearer", api_key)), body)
      .content_from_response(reply$status, reply$body)
    }
  }

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
          if (!identical(e$code, "INVALID_JSON")) stop(e)
          messages <<- c(messages,
            list(role = "assistant", content = "invalid JSON"),
            list(role = "user", content = "Your reply was not valid JSON. Reply again with the exact strict JSON shape.")
          )
          .parse_model_reply(call())
        }
      )

      attempt <- function(p) {
        tryCatch({
          answer <- run_program(p$program)
          check_value <- NULL
          verified <- FALSE
          if (!is.null(p$check)) {
            r <- run_check(p$check, answer)
            check_value <- r$value
            verified <- isTRUE(r$passed)
          }
          list(ok = TRUE, answer = answer, check_value = check_value, verified = verified)
        }, solver_error = function(e) list(ok = FALSE, error = e))
      }

      outcome <- attempt(parsed)
      retries <- 0

      if (!isTRUE(outcome$ok) || !isTRUE(outcome$verified)) {
        retries <- 1
        reason <- if (!isTRUE(outcome$ok)) {
          sprintf("program failed to execute (%s: %s)", outcome$error$code, outcome$error$message)
        } else {
          cv <- if (is.null(outcome$check_value)) "none" else format(outcome$check_value)
          sprintf("check evaluated to %s instead of 0", cv)
        }
        # direct body scope: plain <- updates the local messages (<<- would
        # skip the current frame and leave the retry request without context)
        messages <- c(messages, list(
          list(role = "assistant", content = jsonlite::toJSON(
            list(program = parsed$program, steps = parsed$steps, check = if (is.null(parsed$check)) NULL else parsed$check),
            auto_unbox = TRUE
          )),
          list(role = "user", content = .correction_prompt(reason))
        ))
        second_parsed <- .parse_model_reply(call()) # second failure propagates
        second <- attempt(second_parsed)
        if (!isTRUE(second$ok)) stop(second$error) # PROGRAM_* error persisted after retry
        parsed <- second_parsed
        outcome <- second
      }

      list(
        answer = outcome$answer, steps = parsed$steps, program = parsed$program,
        check = if (is.null(parsed$check)) NULL else parsed$check,
        check_value = if (is.null(outcome$check_value)) NULL else outcome$check_value,
        verified = isTRUE(outcome$verified), retries = retries
      )
    }
  )
}
