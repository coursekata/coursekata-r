test_that("generated aliases leave unused caller promises lazy through the help gate", {
  for (fn in list(gf_coef, gf_squareduce, gf_squaresid)) {
    forced <- FALSE
    dangerous_plot <- function() {
      forced <<- TRUE
      stop("the plot promise was forced")
    }

    expect_no_error(suppressMessages(fn(object = dangerous_plot(), show.help = TRUE)))
    expect_false(forced)
  }
})

test_that("generated front doors bind private pre helpers without changing ggformula scope", {
  helpers <- list(
    gf_b = c("gf_b_warn_unreachable", "gf_b_spec", "gf_b_layer_fun"),
    gf_coef = c("gf_b_warn_unreachable", "gf_b_spec", "gf_b_layer_fun"),
    gf_model = c(
      "implied_model_spec", "model_layer_spec", "implied_layer_fun", "model_layer_fun"
    ),
    gf_reduce = c("reduce_spec", "resid_jitter", "resid_layer_fun"),
    gf_square_reduce = c("reduce_spec", "resid_jitter", "resid_layer_fun"),
    gf_squareduce = c("reduce_spec", "resid_jitter", "resid_layer_fun"),
    gf_resid_fun = c("resid_jitter", "resid_fun_spec", "resid_layer_fun"),
    gf_resid = c("resid_jitter", "resid_spec", "resid_layer_fun"),
    gf_square_resid = c("resid_jitter", "resid_spec", "resid_layer_fun"),
    gf_squaresid = c("resid_jitter", "resid_spec", "resid_layer_fun"),
    gf_square_resid_fun = c("resid_jitter", "resid_fun_spec", "resid_layer_fun"),
    gf_squareplot = "squareplot_check",
    gf_sd_ruler = c("check_ruler_where", "sd_ruler_inherited")
  )
  coursekata_namespace <- rlang::ns_env("coursekata")
  ggformula_namespace <- rlang::ns_env("ggformula")

  for (function_name in names(helpers)) {
    front_door <- rlang::env_get(coursekata_namespace, function_name)
    factory_environment <- rlang::get_env(front_door)
    expect_identical(
      rlang::env_parent(factory_environment),
      ggformula_namespace,
      info = function_name
    )

    for (helper_name in helpers[[function_name]]) {
      expect_identical(
        rlang::env_get(factory_environment, helper_name),
        rlang::env_get(coursekata_namespace, helper_name),
        info = paste(function_name, helper_name)
      )
    }
  }
})
