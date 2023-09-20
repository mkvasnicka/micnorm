test_that("parse point line works", {
    expected_output <- function(input = "",
                                call_up_points = 0,
                                number_of_non_excused_call_ups = 0L,
                                number_of_excused_call_ups = 0L,
                                raised_hand_points = 0,
                                errors = 0L) {
        tibble::tibble(
            input = input,
            call_up_points = call_up_points,
            number_of_non_excused_call_ups = number_of_non_excused_call_ups,
            number_of_excused_call_ups = number_of_excused_call_ups,
            raised_hand_points = raised_hand_points,
            errors = errors
        )
    }

    # only call-up points
    expect_identical(
        parse_point_line(""),
        expected_output("")
    )

    expect_equal(
        parse_point_line("1"),
        expected_output("1", 1, 1)
    )

    expect_equal(
        parse_point_line("1 2 3"),
        expected_output("1 2 3", 6, 3)
    )

    expect_equal(
        parse_point_line(" 1 2 3 "),
        expected_output(" 1 2 3 ", 6, 3)
    )

    expect_equal(
        parse_point_line("1 2 3|"),
        expected_output("1 2 3|", 6, 3)
    )

    expect_equal(
        parse_point_line("1 2 3 |   "),
        expected_output("1 2 3 |   ", 6, 3)
    )

    # call-up points and raised hand points
    expect_equal(
        parse_point_line("1|2"),
        expected_output("1|2", 1, 1, 0L, 2)
    )

    expect_equal(
        parse_point_line("1 2 3|1 2 3"),
        expected_output("1 2 3|1 2 3", 6, 3, 0L, 6)
    )

    expect_equal(
        parse_point_line(" 1   2   3  |  1   2 3 "),
        expected_output(" 1   2   3  |  1   2 3 ", 6, 3, 0L, 6)
    )

    # call-up points and excused call-ups
    expect_equal(
        parse_point_line("X"),
        expected_output("X", 0, 0, 1L)
    )

    expect_equal(
        parse_point_line("X|"),
        expected_output("X|", 0, 0, 1L)
    )

    expect_equal(
        parse_point_line("1 2 X"),
        expected_output("1 2 X", 3, 2, 1L)
    )

    # call-up points, excused call-ups, and raised hand points
    expect_equal(
        parse_point_line("X|2"),
        expected_output("X|2", 0, 0, 1L, 2)
    )

    expect_equal(
        parse_point_line("1 2 X|3 4 5"),
        expected_output("1 2 X|3 4 5", 3, 2, 1L, 12)
    )

    # correct decimal separator
    expect_equal(
        parse_point_line("1,2"),
        expected_output("1,2", 1.2, 1)
    )

    expect_equal(
        parse_point_line("1,2|3,4"),
        expected_output("1,2|3,4", 1.2, 1, 0L, 3.4)
    )

    expect_equal(
        parse_point_line("1,2|3.4"),
        expected_output("1,2|3.4", 1.2, 1, 0L, 3.4)
    )

    expect_equal(
        parse_point_line("1,22|3.43"),
        expected_output("1,22|3.43", 1.22, 1, 0L, 3.43)
    )


    # error: extra tube
    expect_equal(
        parse_point_line("1 2 X|3 4 5|"),
        expected_output("1 2 X|3 4 5|", 3, 2, 1L, 7, 1L)
    )

    # error: too large number -- probably missing space or decimal separator
    expect_equal(
        parse_point_line("12 3|1 2"),
        expected_output("12 3|1 2", 15, 2, 0L, 3, 1L)
    )

    expect_equal(
        parse_point_line("1 2 3|12 3"),
        expected_output("1 2 3|12 3", 6, 3, 0L, 15, 1L)
    )

    # error: two decimal separators
    expect_equal(
        parse_point_line("1,2,3|1 2 3"),
        expected_output("1,2,3|1 2 3", 0, 0, 0L, 6, 1L)
    )

    # error: not a number or X
    expect_equal(
        parse_point_line("1 2 3|a b c"),
        expected_output("1 2 3|a b c", 6, 3, 0L, 0, 3L)
    )
})
