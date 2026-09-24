# Zero-dep test runner: Rscript -e 'source("tests/run.R")'
# (jsonlite needed, declared in DESCRIPTION Imports)
suppressWarnings(suppressMessages(library(methods)))

repo_dir <- dirname(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
if (!nzchar(repo_dir)) repo_dir <- getwd()
source(file.path(repo_dir, "R", "solver.R"))

failures <- 0
check <- function(name, cond) {
  if (isTRUE(cond)) {
    cat("  ok  ", name, "\n", sep = "")
  } else {
    failures <<- failures + 1
    cat("FAIL  ", name, "\n", sep = "")
  }
}
throws <- function(expr) {
  tryCatch({ force(expr); FALSE }, solver_error = function(e) TRUE)
}
throws_code_prefix <- function(expr, prefix) {
  tryCatch({ force(expr); FALSE }, solver_error = function(e) startsWith(e$code, prefix))
}
eq <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-9, check.attributes = FALSE))

# v0.2 protocol fixtures: the model returns program/steps/check — never an answer.
# (\\n stays the two-char JSON escape; jsonlite turns it into real newlines on parse)
good <- '{"program": "let d = 11 - 3;\\nlet x = d / 2;\\nresult = x", "steps": ["Subtract 3: 2x = 8", "Divide by 2: x = 4"], "check": "2*{x} + 3 - 11"}'
no_check <- '{"program": "result = 0.15 * 80", "steps": ["Compute 15% of 80"]}'
wrong_check <- '{"program": "let d = 11 - 3;\\nresult = d / 2", "steps": ["..."], "check": "2*{x} + 3 - 12"}'
broken_program <- '{"program": "result = undefinedvar + 1", "steps": []}'

# ---- expression evaluator ----
check("2*3+4=10", eq(eval_expression("2*3+4"), 10))
check("2+3*4=14", eq(eval_expression("2+3*4"), 14))
check("(2+3)*4=20", eq(eval_expression("(2+3)*4"), 20))
check("2^3^2=512", eq(eval_expression("2^3^2"), 512))
check("-3^2=-9", eq(eval_expression("-3^2"), -9))
check("sqrt(16)=4", eq(eval_expression("sqrt(16)"), 4))
check("min(3,5)=3", eq(eval_expression("min(3,5)"), 3))
check("pi", eq(eval_expression("pi"), pi))
for (bad in c("system('x')", "1+2)", "foo(1)", "")) {
  check(paste0("rejects <", bad, ">"), throws(eval_expression(bad)))
}

# ---- env variables ----
check("env resolves vars", eq(eval_expression("d / 2", list(d = 8)), 4))
check("env resolves two vars", eq(eval_expression("x + y", list(x = 1.5, y = 2.5)), 4))
check("undefined var errors", throws(eval_expression("d")))
check("env shadows pi", eq(eval_expression("pi", list(pi = 3)), 3))

# ---- program interpreter ----
check("runProgram let+result=4", eq(run_program("let d = 11 - 3;\nlet x = d / 2;\nresult = x"), 4))
check("runProgram semicolons+bare=12", eq(run_program("let a = 3; let b = 4; a * b"), 12))
check("runProgram bare=12", eq(run_program("0.15 * 80"), 12))
check("runProgram rejects undefined var", throws(run_program("result = undefinedvar + 1")))
check("runProgram rejects empty", throws_code_prefix(run_program(""), "PROGRAM_EMPTY"))
check("runProgram rejects no-result", throws_code_prefix(run_program("let a = 1; let b = 2"), "PROGRAM_NO_RESULT"))

# ---- check substitution ----
pass <- run_check("2*{x} + 3 - 11", 4)
failc <- run_check("2*{x} + 3 - 12", 4)
alt <- run_check("80*15/100 - {x}", 12)
check("runCheck pass", isTRUE(pass$passed) && eq(pass$value, 0))
check("runCheck fail", isTRUE(!failc$passed) && eq(failc$value, -1))
check("runCheck recompute path", isTRUE(alt$passed))

# ---- constructor validation ----
check("NO_API_KEY at construct", throws(math_solver(api_key = "")))
check("BAD_BASE_URL at construct", throws(math_solver(api_key = "sk", base_url = "not-a-url")))

# ---- solve: answer comes from program execution ----
calls <- 0
seen <- list()
tr_ok <- function(url, body, key) {
  calls <<- calls + 1
  seen$url <<- url; seen$key <<- key; seen$body <<- body
  good
}
solver <- math_solver(api_key = "sk-test", base_url = "https://api.deepseek.com/v1", model = "deepseek-chat", transport = tr_ok)
r <- solver$solve("2x + 3 = 11, solve for x")
# 答案=执行产物(4), 代回检验=0; 模型 JSON 里没有 answer 字段
check("verified first try, answer from execution", isTRUE(r$verified) && identical(r$retries, 0) && eq(r$answer, 4) && eq(r$check_value, 0))
check("calls=1", calls == 1)
check("url exact", identical(seen$url, "https://api.deepseek.com/v1/chat/completions"))
check("key passed", identical(seen$key, "sk-test"))
check("body carries model+temperature", grepl('"deepseek-chat"', seen$body, fixed = TRUE) && grepl('"temperature":0', seen$body, fixed = TRUE))
check("protocol: no answer field in model JSON", !grepl('"answer"', good, fixed = TRUE))

# no check provided
r <- math_solver(api_key = "sk", transport = function(u, b, k) no_check)$solve("15% of 80")
check("no check -> unverified, answer from execution", eq(r$answer, 12) && isTRUE(!r$verified) && is.null(r$check) && is.null(r$check_value))

# check fails -> retry recovers
n <- 0
r <- math_solver(api_key = "sk", transport = function(u, b, k) {
  n <<- n + 1
  if (n == 1) wrong_check else good
})$solve("2x+3=11")
check("check fail retry recovers", isTRUE(r$verified) && r$retries == 1 && eq(r$answer, 4))

# program execution error -> retry -> fixed
n <- 0
r <- math_solver(api_key = "sk", transport = function(u, b, k) {
  n <<- n + 1
  if (n == 1) broken_program else good
})$solve("2x+3=11")
check("program error retry recovers", isTRUE(r$verified) && eq(r$answer, 4))

# program error persists -> PROGRAM_ or EXPR_ error thrown
err_code <- tryCatch({
  math_solver(api_key = "sk", transport = function(u, b, k) broken_program)$solve("2x+3=11")
  ""
}, solver_error = function(e) e$code)
check("program error persists throws", startsWith(err_code, "PROGRAM_") || startsWith(err_code, "EXPR_"))

# invalid json then ok
n <- 0
r <- math_solver(api_key = "sk", transport = function(u, b, k) {
  n <<- n + 1
  if (n == 1) "no json" else good
})$solve("1+1")
check("invalid json then ok", isTRUE(r$verified))

# invalid twice raises
check("invalid twice raises", throws(math_solver(api_key = "sk", transport = function(u, b, k) "nothing")$solve("1+1")))

# http error no retry
calls2 <- 0
check("http error no retry", throws(local({
  math_solver(api_key = "sk", transport = function(u, b, k) {
    calls2 <<- calls2 + 1
    solver_error("HTTP_ERROR", "401")
  })$solve("1+1")
})))
check("http calls=1", calls2 == 1)

# check still failing after retry -> unverified, answer kept
r <- math_solver(api_key = "sk", transport = function(u, b, k) wrong_check)$solve("2x+3=11")
check("still failing unverified", eq(r$answer, 4) && isTRUE(!r$verified) && r$retries == 1)

# ---- HTTP-interface mock: inject http_post seam so the DEFAULT transport ----
# ---- runs its real code path without sockets ----
mocked_solver <- function(api_key, contents, statuses = integer(0)) {
  state <- new.env(parent = emptyenv())
  state$n <- 0
  state$calls <- list()
  solver <- math_solver(
    api_key = api_key,
    base_url = "https://mock.test/v1",
    model = "mock-model",
    http_post = function(url, headers, body) {
      i <- state$n
      state$n <- state$n + 1
      state$calls[[length(state$calls) + 1]] <- list(url = url, headers = headers, body = body)
      content <- if (i < length(contents)) contents[[i + 1]] else good
      status <- if (i < length(statuses)) statuses[[i + 1]] else 0
      if (status >= 300) {
        list(status = status, body = "upstream boom")
      } else {
        list(status = 200, body = sprintf('{"choices":[{"message":{"content":%s}}]}',
                                          jsonlite::toJSON(content, auto_unbox = TRUE)))
      }
    }
  )
  list(solver = solver, state = state)
}

mock <- mocked_solver("sk-mock", c(good))
r <- mock$solver$solve("2x + 3 = 11, solve for x")
check("http mock: round trip verified", isTRUE(r$verified) && identical(r$retries, 0) && eq(r$answer, 4))
check("http mock: one call", length(mock$state$calls) == 1)
check("http mock: url joined", identical(mock$state$calls[[1]]$url, "https://mock.test/v1/chat/completions"))
check("http mock: bearer auth", identical(mock$state$calls[[1]]$headers[["Authorization"]], "Bearer sk-mock"))
check("http mock: model + temperature",
      grepl('"mock-model"', mock$state$calls[[1]]$body, fixed = TRUE) &&
      grepl('"temperature":0', mock$state$calls[[1]]$body, fixed = TRUE))
check("http mock: system prompt shape",
      grepl('"role":"system"', mock$state$calls[[1]]$body, fixed = TRUE) &&
      grepl("STRICT JSON", mock$state$calls[[1]]$body, fixed = TRUE))

mock <- mocked_solver("sk", c(wrong_check, good))
r <- mock$solver$solve("2x+3=11")
retry_has_reason <- length(mock$state$calls) == 2 &&
  grepl("failed verification", mock$state$calls[[2]]$body, fixed = TRUE)
check("http mock: retry recovers + carries reason", isTRUE(r$verified) && r$retries == 1 && retry_has_reason)

mock <- mocked_solver("sk", c("certainly not json", good))
r <- mock$solver$solve("1+1")
check("http mock: invalid json re-ask", isTRUE(r$verified) && length(mock$state$calls) == 2)

mock <- mocked_solver("sk", character(0), c(500L))
check("http mock: 500 -> HTTP_ERROR no retry",
      throws_code_prefix(mock$solver$solve("1+1"), "HTTP_ERROR") && length(mock$state$calls) == 1)

mock <- mocked_solver("sk-bad", character(0), c(401L))
check("http mock: 401 -> HTTP_ERROR", throws_code_prefix(mock$solver$solve("1+1"), "HTTP_ERROR"))

# ---- smoke: real API (SMOKE_API_KEY via env, never in git) ----
smoke_key <- Sys.getenv("SMOKE_API_KEY")
if (nzchar(smoke_key)) {
  smoke_base <- Sys.getenv("SMOKE_BASE_URL", "https://api.openai.com/v1")
  solver <- math_solver(api_key = smoke_key, base_url = smoke_base)
  r <- solver$solve("2x + 3 = 11, solve for x")
  cat(sprintf("smoke: answer=%s verified=%s retries=%s\n", r$answer, r$verified, r$retries))
  check("smoke real API", isTRUE(r$verified) && abs(r$answer - 4) < 1e-9)
}

cat(if (failures == 0) "\nALL PASS\n" else paste0("\n", failures, " FAILURES\n"))
quit(status = if (failures == 0) 0 else 1)
