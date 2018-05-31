context("tests FARS Data")


# test fars_read
test_that("fars_read returns an error if filename does not exist",{
        filename_not_exist <- "filename_does_not_exist"
        error_message <- paste("file '", filename_not_exist , "' does not exist", sep = "")
        expect_error(fars_read(filename_not_exist), error_message)
})

test_that("fars_read returns an expected data ",{
        filename_fars <- "accident_2014.csv.bz2"
        dim_expected <- c(30056L, 50L)
        data_fars <- fars_read(filename_fars)
        expect_false(is.null(data_fars))
        expect_is(data_fars, "tbl_df")
        expect_identical(dim(data_fars), dim_expected)
})

test_that("fars_read does not return  output",{
        filename_fars <- "accident_2014.csv.bz2"
        expect_silent(fars_read(filename_fars))
})


# test make_filename
test_that("make_filename generates the file name",{
        expect_match("accident_2014.csv.bz2", make_filename("2014"))
})

test_that("make_filename returns a warning when passing an invalid character",{
        invalid_year = "a_string"
        expect_warning(make_filename(invalid_year), "NAs introduced by coercion")
})



