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

good <- '{"answer": 4, "steps": ["Subtract 3: 2x = 8", "Divide by 2: x = 4"], "verification": {"expression": "(11-3)/2"}}'
wrong <- '{"answer": 4, "steps": ["..."], "verification": {"expression": "(11-3)/3"}}'

# evaluator
check("2*3+4=10", isTRUE(all.equal(eval_expression("2*3+4"), 10, tolerance = 1e-9)))
check("2+3*4=14", isTRUE(all.equal(eval_expression("2+3*4"), 14, tolerance = 1e-9)))
check("(2+3)*4=20", isTRUE(all.equal(eval_expression("(2+3)*4"), 20, tolerance = 1e-9)))
check("2^3^2=512", isTRUE(all.equal(eval_expression("2^3^2"), 512, tolerance = 1e-9)))
check("-3^2=-9", isTRUE(all.equal(eval_expression("-3^2"), -9, tolerance = 1e-9)))
check("sqrt(16)=4", isTRUE(all.equal(eval_expression("sqrt(16)"), 4, tolerance = 1e-9)))
check("min(3,5)=3", isTRUE(all.equal(eval_expression("min(3,5)"), 3, tolerance = 1e-9)))
check("pi", isTRUE(all.equal(eval_expression("pi"), pi, tolerance = 1e-12)))
for (bad in c("system('x')", "1+2)", "foo(1)", "")) {
  check(paste0("rejects <", bad, ">"), throws(eval_expression(bad)))
}

# constructor validation
check("NO_API_KEY at construct", throws(math_solver(api_key = "")))
check("BAD_BASE_URL at construct", throws(math_solver(api_key = "sk", base_url = "not-a-url")))

# solve: verified first try
calls <- 0
seen <- list()
tr_ok <- function(url, body, key) {
  calls <<- calls + 1
  seen$url <<- url; seen$key <<- key
  good
}
solver <- math_solver(api_key = "sk-test", base_url = "https://api.deepseek.com/v1", model = "deepseek-chat", transport = tr_ok)
r <- solver$solve("2x + 3 = 11, solve for x")
check("verified first try", isTRUE(r$verified) && identical(r$retries, 0) || (r$retries == 0 && r$verified))
check("evaluated=4", isTRUE(all.equal(r$evaluated, 4)))
check("calls=1", calls == 1)
check("url exact", identical(seen$url, "https://api.deepseek.com/v1/chat/completions"))
check("key passed", identical(seen$key, "sk-test"))

# retry recovers
n <- 0
r <- math_solver(api_key = "sk", transport = function(u, b, k) {
  n <<- n + 1
  if (n == 1) wrong else good
})$solve("2x+3=11")
check("retry recovers", isTRUE(r$verified) && r$retries == 1)

# invalid json then ok
n <- 0
r <- math_solver(api_key = "sk", transport = function(u, b, k) {
  n <<- n + 1
  if (n == 1) "no json" else good
})$solve("1+1")
check("invalid json then ok", isTRUE(r$verified))

# invalid twice raises
check("invalid twice raises", throws(math_solver(api_key = "sk", transport = function(u, b, k) "nothing")$solve("1+1")))

# no api key

# http error no retry
calls2 <- 0
check("http error no retry", throws(local({
  math_solver(api_key = "sk", transport = function(u, b, k) {
    calls2 <<- calls2 + 1
    solver_error("HTTP_ERROR", "401")
  })$solve("1+1")
})))
check("http calls=1", calls2 == 1)

# still wrong unverified
r <- math_solver(api_key = "sk", transport = function(u, b, k) wrong)$solve("2x+3=11")
check("still wrong unverified", isTRUE(!r$verified) && r$retries == 1)

# smoke: real API (SMOKE_API_KEY via env, never in git)
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
