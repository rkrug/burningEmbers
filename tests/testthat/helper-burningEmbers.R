colRangeDefault <-
  structure(
    list(
      bot = c(
        0.841470984807897,
        0.909297426825682,
        0.141120008059867,
        -0.756802495307928,
        -0.958924274663138
      ),
      top = c(
        6.56120922347256,
        2.73541265381143,
        0.440030013598219,
        1.78542551654555,
        5.53464874185291
      )
    ),
    .Names = c("bot", "top"),
    row.names = c("RFC1", "RFC2", "RFC3",
                  "RFC4", "RFC5"),
    class = "data.frame"
  )

colRangeVal <-
  structure(
    list(
      Fritz = c(1, -30, 1.3, -23.5),
      Eddie = c(22.4,
                -5, 43, -22.4)
    ),
    .Names = c("Fritz", "Eddie"),
    row.names = c(NA,
                  -4L),
    class = "data.frame"
  )
