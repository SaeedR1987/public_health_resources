devtools::load_all()
library(testthat)

new_priv <- function() {
  inst <- Asset$new()
  list(inst = inst, priv = inst$.__enclos_env__$private)
}

test_that("translations are loaded once at initialization", {
  x <- new_priv()
  expect_true(is.list(x$priv$..translations))
  expect_true(all(c("en", "fr", "es", "ar") %in% names(x$priv$..translations)))
})

test_that("every translated key also exists in English", {
  tr <- new_priv()$priv$..translations
  for (lang in c("fr", "es", "ar")) {
    expect_true(all(names(tr[[lang]]) %in% names(tr$en)))
  }
})

test_that("..lang defaults to en and is gettable/settable", {
  x <- new_priv()
  expect_identical(x$inst$get("..lang"), "en")
  x$inst$set("..lang", "fr")
  expect_identical(x$inst$get("..lang"), "fr")
})

test_that("..phr_txt returns English by default", {
  x <- new_priv()
  expect_identical(
    x$priv$..phr_txt("target_field path is invalid."),
    "target_field path is invalid."
  )
})

test_that("..phr_txt uses the object language or an explicit lang", {
  x <- new_priv()
  key <- "target_field path is invalid."

  x$inst$set("..lang", "es")
  expect_identical(x$priv$..phr_txt(key), "La ruta de target_field no es válida.")

  expect_identical(
    x$priv$..phr_txt(key, lang = "fr"),
    "Le chemin de target_field n'est pas valide."
  )
  expect_identical(x$priv$..phr_txt(key, lang = "ar"), "مسار target_field غير صالح.")
})

test_that("..phr_txt falls back to English for unknown languages", {
  x <- new_priv()
  key <- "target_field path is invalid."
  expect_identical(x$priv$..phr_txt(key, lang = "zz"), key)

  x$inst$set("..lang", "xx")
  expect_identical(x$priv$..phr_txt(key), key)
})

test_that("..phr_txt interpolates glue expressions from the caller", {
  x <- new_priv()
  role <- "abc"
  field <- "xyz"
  expect_identical(
    x$priv$..phr_txt("Role '{role}' was not found in field '{field}'."),
    "Role 'abc' was not found in field 'xyz'."
  )
  expect_identical(
    x$priv$..phr_txt("Role '{role}' was not found in field '{field}'.", lang = "es"),
    "No se encontró el rol 'abc' en el campo 'xyz'."
  )
})

test_that("..phr_txt handles missing keys with marker or default", {
  x <- new_priv()
  expect_identical(x$priv$..phr_txt("Not in json"), "\u29EBNot in json\u29EB")
  expect_identical(
    x$priv$..phr_txt("Not in json", default = "fallback"),
    "fallback"
  )
})

test_that("..append_translations merges extra translations from a JSON file", {
  x <- new_priv()
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  writeLines(
    '{"en":{"extra_msg":"Extra {1+1}"},"fr":{"extra_msg":"Supplement {1+1}"},"de":{"extra_msg":"Zusatz"}}',
    f
  )
  n_before <- length(x$priv$..translations$en)
  x$priv$..append_translations(path = f)
  expect_identical(length(x$priv$..translations$en), n_before + 1L)
  expect_identical(x$priv$..phr_txt("extra_msg"), "Extra 2")
  expect_identical(x$priv$..phr_txt("extra_msg", lang = "fr"), "Supplement 2")
  expect_identical(x$priv$..phr_txt("extra_msg", lang = "de"), "Zusatz")
  expect_error(x$priv$..append_translations(path = "missing.json"), "not found")
  expect_error(x$priv$..append_translations(), "path")
})

test_that("..append_translations respects overwrite = FALSE", {
  x <- new_priv()
  key <- names(x$priv$..translations$en)[1]
  original <- x$priv$..translations$en[[key]]
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  writeLines(sprintf('{"en":{"%s":"changed"}}', key), f)
  x$priv$..append_translations(path = f, overwrite = FALSE)
  expect_identical(x$priv$..translations$en[[key]], original)
  x$priv$..append_translations(path = f)
  expect_identical(x$priv$..translations$en[[key]], "changed")
})

test_that("..phr_text_to_key normalizes text", {
  priv <- new_priv()$priv
  expect_identical(priv$..phr_text_to_key(""), "")
  expect_identical(priv$..phr_text_to_key(NULL), "")
  expect_identical(priv$..phr_text_to_key("Hello, World!"), "hello_world")
  expect_identical(priv$..phr_text_to_key("1 Hello"), "txt_1_hello")
  long <- priv$..phr_text_to_key(strrep("a", 150))
  expect_lte(nchar(long), 95L)
})

